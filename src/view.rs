use crate::doc::{Document, Object};
use crate::geo::{Geo, Pt};
use eframe::egui::{self, Align2, Color32, FontId, Painter, Pos2, Rect, Shape, Stroke, pos2, vec2};
use plotwerk_math as m;

pub struct View {
    pub cx: f64,
    pub cy: f64,
    pub scale: f64,
    pub show_axes: bool,
    pub show_grid: bool,
}

impl Default for View {
    fn default() -> Self {
        Self { cx: 0.0, cy: 0.0, scale: 50.0, show_axes: true, show_grid: true }
    }
}

const GRID_MINOR: Color32 = Color32::from_rgb(238, 240, 244);
const GRID_MAJOR: Color32 = Color32::from_rgb(214, 218, 226);
const AXIS: Color32 = Color32::from_rgb(70, 76, 90);
const LABEL: Color32 = Color32::from_rgb(96, 102, 116);

fn nice_step(raw: f64) -> (f64, f64) {
    let p = 10f64.powf(raw.log10().floor());
    let mant = raw / p;
    let (n, div) = if mant < 1.5 {
        (1.0, 5.0)
    } else if mant < 3.5 {
        (2.0, 4.0)
    } else if mant < 7.5 {
        (5.0, 5.0)
    } else {
        (10.0, 5.0)
    };
    (n * p, n * p / div)
}

fn fmt_tick(v: f64, step: f64) -> String {
    if v.abs() < step * 1e-6 {
        return "0".into();
    }
    if step >= 1.0 && v.abs() < 1e6 {
        return format!("{}", v.round() as i64).replace('-', "−");
    }
    if step < 1.0 && step > 1e-6 {
        let decimals = (-step.log10()).ceil().max(0.0) as usize;
        return format!("{:.*}", decimals, v).replace('-', "−");
    }
    m::format_number(v)
}

impl View {
    pub fn to_screen(&self, rect: Rect, p: Pt) -> Pos2 {
        let c = rect.center();
        pos2(c.x + ((p[0] - self.cx) * self.scale) as f32, c.y - ((p[1] - self.cy) * self.scale) as f32)
    }

    pub fn to_world(&self, rect: Rect, p: Pos2) -> Pt {
        let c = rect.center();
        [self.cx + (p.x - c.x) as f64 / self.scale, self.cy - (p.y - c.y) as f64 / self.scale]
    }

    pub fn world_rect(&self, rect: Rect) -> (f64, f64, f64, f64) {
        let a = self.to_world(rect, rect.left_bottom());
        let b = self.to_world(rect, rect.right_top());
        (a[0], b[0], a[1], b[1])
    }

    pub fn zoom_at(&mut self, rect: Rect, pos: Pos2, factor: f64) {
        let before = self.to_world(rect, pos);
        self.scale = (self.scale * factor).clamp(1e-4, 1e7);
        let after = self.to_world(rect, pos);
        self.cx += before[0] - after[0];
        self.cy += before[1] - after[1];
    }

    pub fn pan_pixels(&mut self, delta: egui::Vec2) {
        self.cx -= delta.x as f64 / self.scale;
        self.cy += delta.y as f64 / self.scale;
    }

    pub fn reset(&mut self) {
        self.cx = 0.0;
        self.cy = 0.0;
        self.scale = 50.0;
    }

    pub fn snap(&self, p: Pt) -> Pt {
        let (major, minor) = nice_step(90.0 / self.scale);
        let _ = major;
        let tol = 6.0 / self.scale;
        let s = |v: f64| {
            let r = (v / minor).round() * minor;
            if (r - v).abs() < tol { r } else { v }
        };
        [s(p[0]), s(p[1])]
    }

    pub fn draw_background(&self, painter: &Painter, rect: Rect) {
        painter.rect_filled(rect, 0.0, Color32::WHITE);
        let (x0, x1, y0, y1) = self.world_rect(rect);
        let (major, minor) = nice_step(90.0 / self.scale);
        if self.show_grid {
            for (step, color) in [(minor, GRID_MINOR), (major, GRID_MAJOR)] {
                let mut x = (x0 / step).floor() * step;
                while x <= x1 {
                    let sx = self.to_screen(rect, [x, 0.0]).x;
                    painter.line_segment([pos2(sx, rect.top()), pos2(sx, rect.bottom())], Stroke::new(1.0, color));
                    x += step;
                }
                let mut y = (y0 / step).floor() * step;
                while y <= y1 {
                    let sy = self.to_screen(rect, [0.0, y]).y;
                    painter.line_segment([pos2(rect.left(), sy), pos2(rect.right(), sy)], Stroke::new(1.0, color));
                    y += step;
                }
            }
        }
        if !self.show_axes {
            return;
        }
        let origin = self.to_screen(rect, [0.0, 0.0]);
        let ax_y = origin.y.clamp(rect.top(), rect.bottom());
        let ax_x = origin.x.clamp(rect.left(), rect.right());
        let stroke = Stroke::new(1.4, AXIS);
        painter.line_segment([pos2(rect.left(), ax_y), pos2(rect.right(), ax_y)], stroke);
        painter.line_segment([pos2(ax_x, rect.top()), pos2(ax_x, rect.bottom())], stroke);
        painter.add(Shape::convex_polygon(
            vec![pos2(rect.right() - 1.0, ax_y), pos2(rect.right() - 9.0, ax_y - 4.0), pos2(rect.right() - 9.0, ax_y + 4.0)],
            AXIS,
            Stroke::NONE,
        ));
        painter.add(Shape::convex_polygon(
            vec![pos2(ax_x, rect.top() + 1.0), pos2(ax_x - 4.0, rect.top() + 9.0), pos2(ax_x + 4.0, rect.top() + 9.0)],
            AXIS,
            Stroke::NONE,
        ));
        let font = FontId::proportional(11.5);
        let below = ax_y + 18.0 < rect.bottom();
        let mut x = (x0 / major).floor() * major;
        while x <= x1 {
            if x.abs() > major * 1e-6 {
                let sx = self.to_screen(rect, [x, 0.0]).x;
                painter.line_segment([pos2(sx, ax_y - 3.0), pos2(sx, ax_y + 3.0)], stroke);
                let (anchor, pos) = if below { (Align2::CENTER_TOP, pos2(sx, ax_y + 5.0)) } else { (Align2::CENTER_BOTTOM, pos2(sx, ax_y - 5.0)) };
                painter.text(pos, anchor, fmt_tick(x, major), font.clone(), LABEL);
            }
            x += major;
        }
        let left = ax_x - 30.0 > rect.left();
        let mut y = (y0 / major).floor() * major;
        while y <= y1 {
            if y.abs() > major * 1e-6 {
                let sy = self.to_screen(rect, [0.0, y]).y;
                painter.line_segment([pos2(ax_x - 3.0, sy), pos2(ax_x + 3.0, sy)], stroke);
                let (anchor, pos) = if left { (Align2::RIGHT_CENTER, pos2(ax_x - 6.0, sy)) } else { (Align2::LEFT_CENTER, pos2(ax_x + 6.0, sy)) };
                painter.text(pos, anchor, fmt_tick(y, major), font.clone(), LABEL);
            }
            y += major;
        }
        if rect.contains(origin) {
            painter.text(pos2(origin.x - 5.0, origin.y + 5.0), Align2::RIGHT_TOP, "0", font, LABEL);
        }
    }

    fn stroke_path(&self, painter: &Painter, pts: Vec<Pos2>, stroke: Stroke, dash: u8) {
        if pts.len() < 2 {
            return;
        }
        match dash {
            1 => {
                painter.extend(Shape::dashed_line(&pts, stroke, 10.0, 6.0));
            }
            2 => {
                painter.extend(Shape::dotted_line(&pts, stroke.color, 7.0, stroke.width * 0.55));
            }
            _ => {
                painter.add(Shape::line(pts, stroke));
            }
        }
    }

    fn label_at(&self, painter: &Painter, pos: Pos2, text: &str, color: Color32) {
        painter.text(pos, Align2::LEFT_BOTTOM, text, FontId::proportional(14.0), color.gamma_multiply(0.9));
    }

    fn arc_points(&self, rect: Rect, c: Pt, r: f64, a0: f64, sweep: f64) -> Vec<Pos2> {
        let n = ((sweep * r * self.scale / 3.0).ceil() as usize).clamp(8, 720);
        (0..=n)
            .map(|i| {
                let a = a0 + sweep * i as f64 / n as f64;
                self.to_screen(rect, [c[0] + r * a.cos(), c[1] + r * a.sin()])
            })
            .collect()
    }

    pub fn draw_object(&self, painter: &Painter, rect: Rect, obj: &Object, highlight: bool) {
        let Some(geo) = &obj.geo else { return };
        if !obj.def.visible {
            return;
        }
        let color = obj.def.color;
        let width = if highlight { obj.def.width + 1.6 } else { obj.def.width };
        let stroke = Stroke::new(width, color);
        let dash = obj.def.dash;
        let name = crate::expr_util::display_prime(&obj.def.name);
        let show_label = obj.def.label;
        match geo {
            Geo::Number(_) => {}
            Geo::Point(p) => self.draw_point(painter, rect, *p, obj, highlight, show_label.then_some(name.as_str())),
            Geo::Points(ps) => {
                for (i, p) in ps.iter().enumerate() {
                    let label = format!("{}{}", name, subscript(i + 1));
                    self.draw_point(painter, rect, *p, obj, highlight, show_label.then_some(label.as_str()));
                }
            }
            Geo::Function { .. } => {
                let pts = self.function_paths(rect, &|x| geo.eval_fn(x));
                let mut label_pos = None;
                for path in pts {
                    if label_pos.is_none() {
                        label_pos = path.iter().find(|p| rect.shrink(30.0).contains(**p)).copied();
                    }
                    self.stroke_path(painter, path, stroke, dash);
                }
                if let (true, Some(p)) = (show_label, label_pos) {
                    self.label_at(painter, p + vec2(6.0, -6.0), &name, color);
                }
            }
            Geo::Line { a, b, c } => {
                if let Some((p, q)) = self.clip_line(rect, *a, *b, *c) {
                    self.stroke_path(painter, vec![p, q], stroke, dash);
                    if show_label {
                        self.label_at(painter, p + (q - p) * 0.8 + vec2(6.0, -6.0), &name, color);
                    }
                }
            }
            Geo::Segment(p, q) => {
                let (sp, sq) = (self.to_screen(rect, *p), self.to_screen(rect, *q));
                self.stroke_path(painter, vec![sp, sq], stroke, dash);
                if show_label {
                    self.label_at(painter, sp + (sq - sp) * 0.5 + vec2(6.0, -6.0), &name, color);
                }
            }
            Geo::Ray(p, q) => {
                let (x0, x1, y0, y1) = self.world_rect(rect);
                let d = [q[0] - p[0], q[1] - p[1]];
                let far = ((x1 - x0).abs() + (y1 - y0).abs() + crate::geo::dist(*p, [(x0 + x1) / 2.0, (y0 + y1) / 2.0])) / (d[0].hypot(d[1]).max(1e-12));
                let end = [p[0] + d[0] * far, p[1] + d[1] * far];
                let (sp, se) = (self.to_screen(rect, *p), self.to_screen(rect, end));
                self.stroke_path(painter, vec![sp, se], stroke, dash);
                if show_label {
                    let sq = self.to_screen(rect, *q);
                    self.label_at(painter, sq + vec2(6.0, -6.0), &name, color);
                }
            }
            Geo::Vector(p, q) => {
                let (sp, sq) = (self.to_screen(rect, *p), self.to_screen(rect, *q));
                let dir = (sq - sp).normalized();
                if (sq - sp).length() > 1.0 {
                    let head = 9.0 + width * 1.5;
                    let base = sq - dir * head;
                    let perp = vec2(-dir.y, dir.x) * (head * 0.45);
                    self.stroke_path(painter, vec![sp, base], stroke, dash);
                    painter.add(Shape::convex_polygon(vec![sq, base + perp, base - perp], color, Stroke::NONE));
                }
                if show_label {
                    self.label_at(painter, sp + (sq - sp) * 0.5 + vec2(6.0, -6.0), &name, color);
                }
            }
            Geo::Circle { c, r } => {
                let center = self.to_screen(rect, *c);
                let radius = (*r * self.scale) as f32;
                if radius < 1e6 {
                    if dash == 0 {
                        painter.circle_stroke(center, radius, stroke);
                    } else {
                        let pts = self.arc_points(rect, *c, *r, 0.0, std::f64::consts::TAU);
                        self.stroke_path(painter, pts, stroke, dash);
                    }
                    if show_label {
                        self.label_at(painter, center + vec2(radius * 0.72, -radius * 0.72), &name, color);
                    }
                }
            }
            Geo::Arc { c, r, a0, sweep, sector } => {
                let pts = self.arc_points(rect, *c, *r, *a0, *sweep);
                let center = self.to_screen(rect, *c);
                if *sector {
                    let mut poly = vec![center];
                    poly.extend(pts.iter().copied());
                    fill_polygon(painter, &poly, color.gamma_multiply(0.2));
                    let mut outline = poly.clone();
                    outline.push(center);
                    self.stroke_path(painter, outline, stroke, dash);
                } else {
                    let mid = pts[pts.len() / 2];
                    self.stroke_path(painter, pts, stroke, dash);
                    if show_label {
                        self.label_at(painter, mid + vec2(6.0, -6.0), &name, color);
                    }
                }
            }
            Geo::Angle { v, a0, sweep } => {
                let center = self.to_screen(rect, *v);
                let rpx = 30.0f64;
                let right = (sweep.to_degrees() - 90.0).abs() < 1e-6;
                if right {
                    let u = [a0.cos(), a0.sin()];
                    let w = [(a0 + sweep).cos(), (a0 + sweep).sin()];
                    let s = rpx * 0.7 / self.scale;
                    let p1 = [v[0] + u[0] * s, v[1] + u[1] * s];
                    let p2 = [v[0] + (u[0] + w[0]) * s, v[1] + (u[1] + w[1]) * s];
                    let p3 = [v[0] + w[0] * s, v[1] + w[1] * s];
                    let poly = vec![center, self.to_screen(rect, p1), self.to_screen(rect, p2), self.to_screen(rect, p3)];
                    painter.add(Shape::convex_polygon(poly.clone(), color.gamma_multiply(0.22), Stroke::NONE));
                    painter.add(Shape::line(poly[1..].to_vec(), Stroke::new(1.6, color)));
                    painter.circle_filled(self.to_screen(rect, [v[0] + (u[0] + w[0]) * s * 0.5, v[1] + (u[1] + w[1]) * s * 0.5]), 1.8, color);
                } else {
                    let r = rpx / self.scale;
                    let pts = self.arc_points(rect, *v, r, *a0, *sweep);
                    let mut poly = vec![center];
                    poly.extend(pts.iter().copied());
                    fill_polygon(painter, &poly, color.gamma_multiply(0.22));
                    painter.add(Shape::line(pts, Stroke::new(1.6, color)));
                }
                let mid = a0 + sweep / 2.0;
                let lp = self.to_screen(rect, [v[0] + mid.cos() * (rpx + 18.0) / self.scale, v[1] + mid.sin() * (rpx + 18.0) / self.scale]);
                let text = if show_label {
                    format!("{name} = {}°", crate::doc::fmt_deg(*sweep))
                } else {
                    format!("{}°", crate::doc::fmt_deg(*sweep))
                };
                painter.text(lp, Align2::CENTER_CENTER, text, FontId::proportional(13.0), color);
            }
            Geo::Polygon(pts) => {
                let screen: Vec<Pos2> = pts.iter().map(|p| self.to_screen(rect, *p)).collect();
                fill_polygon(painter, &screen, color.gamma_multiply(0.18));
                let mut closed = screen.clone();
                closed.push(screen[0]);
                self.stroke_path(painter, closed, stroke, dash);
                if show_label {
                    let n = screen.len() as f32;
                    let c = screen.iter().fold(vec2(0.0, 0.0), |acc, p| acc + p.to_vec2()) / n;
                    painter.text(c.to_pos2(), Align2::CENTER_CENTER, &name, FontId::proportional(14.0), color);
                }
            }
            Geo::Implicit { f, .. } => self.draw_implicit(painter, rect, &|x, y| f.eval(&[x, y]), stroke),
            Geo::Region { cond, bounds } => {
                let cell = 3.0f32;
                let fill = color.gamma_multiply(0.22);
                let cols = (rect.width() / cell).ceil() as usize;
                let rows = (rect.height() / cell).ceil() as usize;
                let mut mesh = egui::Mesh::default();
                for j in 0..rows {
                    let y = rect.top() + j as f32 * cell;
                    let mut run: Option<usize> = None;
                    for i in 0..=cols {
                        let inside = i < cols && {
                            let w = self.to_world(rect, pos2(rect.left() + (i as f32 + 0.5) * cell, y + cell * 0.5));
                            cond.eval(&[w[0], w[1]]) > 0.5
                        };
                        match (inside, run) {
                            (true, None) => run = Some(i),
                            (false, Some(start)) => {
                                let r = Rect::from_min_max(pos2(rect.left() + start as f32 * cell, y), pos2(rect.left() + i as f32 * cell, y + cell));
                                mesh.add_colored_rect(r, fill);
                                run = None;
                            }
                            _ => {}
                        }
                    }
                }
                painter.add(Shape::mesh(mesh));
                for (b, strict) in bounds {
                    let st = Stroke::new(width * 0.8, color);
                    if *strict {
                        self.draw_implicit(painter, rect, &|x, y| b.eval(&[x, y]), Stroke::new(st.width * 0.7, color.gamma_multiply(0.7)));
                    } else {
                        self.draw_implicit(painter, rect, &|x, y| b.eval(&[x, y]), st);
                    }
                }
            }
            Geo::Area { f, a, b, .. } => {
                let (lo, hi) = if a <= b { (*a, *b) } else { (*b, *a) };
                let (vx0, vx1, _, _) = self.world_rect(rect);
                let lo_v = lo.max(vx0);
                let hi_v = hi.min(vx1);
                if hi_v > lo_v {
                    let n = (((hi_v - lo_v) * self.scale) as usize).clamp(2, 4000);
                    let fill = color.gamma_multiply(0.28);
                    let axis_y = self.to_screen(rect, [0.0, 0.0]).y;
                    let mut mesh = egui::Mesh::default();
                    for i in 0..=n {
                        let x = lo_v + (hi_v - lo_v) * i as f64 / n as f64;
                        let y = f.eval(&[x]);
                        let sx = self.to_screen(rect, [x, 0.0]).x;
                        let sy = if y.is_finite() { self.to_screen(rect, [x, y]).y.clamp(rect.top() - 10.0, rect.bottom() + 10.0) } else { axis_y };
                        mesh.colored_vertex(pos2(sx, axis_y), fill);
                        mesh.colored_vertex(pos2(sx, sy), fill);
                        if i > 0 {
                            let k = (i * 2) as u32;
                            mesh.add_triangle(k - 2, k - 1, k);
                            mesh.add_triangle(k - 1, k, k + 1);
                        }
                    }
                    painter.add(Shape::mesh(mesh));
                    for path in self.function_paths(rect, &|x| if x >= lo_v && x <= hi_v { f.eval(&[x]) } else { f64::NAN }) {
                        painter.add(Shape::line(path, Stroke::new(width * 0.8, color)));
                    }
                }
            }
        }
    }

    fn draw_point(&self, painter: &Painter, rect: Rect, p: Pt, obj: &Object, highlight: bool, label: Option<&str>) {
        let pos = self.to_screen(rect, p);
        if !rect.expand(20.0).contains(pos) {
            return;
        }
        let r = if highlight { 6.5 } else { 5.0 };
        if obj.free_point {
            painter.circle_filled(pos, r + 1.8, Color32::WHITE);
            painter.circle_filled(pos, r, crate::theme::ACCENT);
        } else {
            painter.circle_filled(pos, r + 1.5, Color32::WHITE);
            painter.circle_filled(pos, r - 0.5, obj.def.color);
        }
        if highlight {
            painter.circle_stroke(pos, r + 4.0, Stroke::new(1.5, crate::theme::ACCENT.gamma_multiply(0.5)));
        }
        if let Some(label) = label {
            painter.text(pos + vec2(8.0, -8.0), Align2::LEFT_BOTTOM, label, FontId::proportional(13.0), Color32::from_rgb(40, 44, 54));
        }
    }

    pub fn function_paths(&self, rect: Rect, f: &dyn Fn(f64) -> f64) -> Vec<Vec<Pos2>> {
        let (x0, x1, _, _) = self.world_rect(rect);
        let n = (rect.width() * 1.5).max(10.0) as usize;
        let limit_top = rect.top() - rect.height() * 3.0;
        let limit_bottom = rect.bottom() + rect.height() * 3.0;
        let jump = rect.height() * 1.2;
        let mut out = Vec::new();
        let mut path: Vec<Pos2> = Vec::with_capacity(n + 1);
        let mut prev: Option<f32> = None;
        for i in 0..=n {
            let x = x0 + (x1 - x0) * i as f64 / n as f64;
            let y = f(x);
            if !y.is_finite() {
                if path.len() > 1 {
                    out.push(std::mem::take(&mut path));
                } else {
                    path.clear();
                }
                prev = None;
                continue;
            }
            let sx = rect.left() + rect.width() * i as f32 / n as f32;
            let sy_raw = self.to_screen(rect, [x, y]).y;
            if let Some(py) = prev {
                if (sy_raw - py).abs() > jump && ((py < rect.top() || py > rect.bottom()) || (sy_raw < rect.top() || sy_raw > rect.bottom())) {
                    if path.len() > 1 {
                        out.push(std::mem::take(&mut path));
                    } else {
                        path.clear();
                    }
                }
            }
            prev = Some(sy_raw);
            path.push(pos2(sx, sy_raw.clamp(limit_top, limit_bottom)));
        }
        if path.len() > 1 {
            out.push(path);
        }
        out
    }

    fn draw_implicit(&self, painter: &Painter, rect: Rect, f: &dyn Fn(f64, f64) -> f64, stroke: Stroke) {
        let cell = 4.0f32;
        let cols = (rect.width() / cell).ceil() as usize + 1;
        let rows = (rect.height() / cell).ceil() as usize + 1;
        let mut grid = vec![0.0f64; cols * rows];
        for j in 0..rows {
            for i in 0..cols {
                let w = self.to_world(rect, pos2(rect.left() + i as f32 * cell, rect.top() + j as f32 * cell));
                grid[j * cols + i] = f(w[0], w[1]);
            }
        }
        let mut segs: Vec<[Pos2; 2]> = Vec::new();
        for j in 0..rows - 1 {
            for i in 0..cols - 1 {
                let v = [grid[j * cols + i], grid[j * cols + i + 1], grid[(j + 1) * cols + i + 1], grid[(j + 1) * cols + i]];
                if v.iter().any(|t| !t.is_finite()) {
                    continue;
                }
                let x = rect.left() + i as f32 * cell;
                let y = rect.top() + j as f32 * cell;
                let corners = [pos2(x, y), pos2(x + cell, y), pos2(x + cell, y + cell), pos2(x, y + cell)];
                let mut pts: Vec<Pos2> = Vec::with_capacity(4);
                for k in 0..4 {
                    let (a, b) = (v[k], v[(k + 1) % 4]);
                    if (a < 0.0) != (b < 0.0) {
                        let t = (a / (a - b)) as f32;
                        let pa = corners[k];
                        let pb = corners[(k + 1) % 4];
                        pts.push(pa + (pb - pa) * t);
                    }
                }
                let maxabs = v.iter().fold(0.0f64, |acc, t| acc.max(t.abs()));
                let minabs = v.iter().fold(f64::INFINITY, |acc, t| acc.min(t.abs()));
                if maxabs > 1e6 * (minabs + 1.0) {
                    continue;
                }
                if pts.len() == 2 {
                    segs.push([pts[0], pts[1]]);
                } else if pts.len() == 4 {
                    segs.push([pts[0], pts[1]]);
                    segs.push([pts[2], pts[3]]);
                }
            }
        }
        for s in segs {
            painter.line_segment(s, stroke);
        }
    }

    pub fn clip_line(&self, rect: Rect, a: f64, b: f64, c: f64) -> Option<(Pos2, Pos2)> {
        let (x0, x1, y0, y1) = self.world_rect(rect);
        let mut pts: Vec<Pt> = Vec::new();
        if b.abs() > 1e-14 {
            for x in [x0, x1] {
                pts.push([x, (c - a * x) / b]);
            }
        }
        if a.abs() > 1e-14 {
            for y in [y0, y1] {
                pts.push([(c - b * y) / a, y]);
            }
        }
        let margin_x = (x1 - x0) * 1e-6;
        let margin_y = (y1 - y0) * 1e-6;
        let inside: Vec<Pt> = pts
            .into_iter()
            .filter(|p| p[0] >= x0 - margin_x && p[0] <= x1 + margin_x && p[1] >= y0 - margin_y && p[1] <= y1 + margin_y)
            .collect();
        if inside.len() < 2 {
            return None;
        }
        let mut best = (inside[0], inside[1]);
        let mut best_d = 0.0;
        for i in 0..inside.len() {
            for j in i + 1..inside.len() {
                let d = crate::geo::dist(inside[i], inside[j]);
                if d > best_d {
                    best_d = d;
                    best = (inside[i], inside[j]);
                }
            }
        }
        Some((self.to_screen(rect, best.0), self.to_screen(rect, best.1)))
    }

    pub fn distance_px(&self, rect: Rect, obj: &Object, pos: Pos2) -> Option<f32> {
        let geo = obj.geo.as_ref()?;
        if !obj.def.visible {
            return None;
        }
        let w = self.to_world(rect, pos);
        let s = self.scale as f32;
        match geo {
            Geo::Number(_) => None,
            Geo::Point(p) => Some(self.to_screen(rect, *p).distance(pos)),
            Geo::Points(ps) => ps.iter().map(|p| self.to_screen(rect, *p).distance(pos)).reduce(f32::min),
            Geo::Function { .. } | Geo::Area { .. } => {
                let mut best = f32::INFINITY;
                for dx in -8..=8 {
                    let sx = pos.x + dx as f32;
                    let wx = self.to_world(rect, pos2(sx, pos.y))[0];
                    let y = geo.eval_fn(wx);
                    if y.is_finite() {
                        let sy = self.to_screen(rect, [wx, y]).y;
                        best = best.min(pos2(sx, sy).distance(pos));
                    }
                }
                Some(best)
            }
            Geo::Line { a, b, c } => Some(((a * w[0] + b * w[1] - c).abs() / (a * a + b * b).sqrt()) as f32 * s),
            Geo::Segment(p, q) | Geo::Vector(p, q) => Some(seg_dist(pos, self.to_screen(rect, *p), self.to_screen(rect, *q))),
            Geo::Ray(p, q) => {
                let (sp, sq) = (self.to_screen(rect, *p), self.to_screen(rect, *q));
                let far = sp + (sq - sp).normalized() * 1e5;
                Some(seg_dist(pos, sp, far))
            }
            Geo::Circle { c, r } => Some(((crate::geo::dist(w, *c) - r).abs()) as f32 * s),
            Geo::Arc { c, r, a0, sweep, sector } => {
                let rel = crate::geo::norm_angle(crate::geo::angle_of(*c, w) - a0);
                let on = rel <= *sweep;
                let d_arc = if on { ((crate::geo::dist(w, *c) - r).abs()) as f32 * s } else { f32::INFINITY };
                if *sector && on && crate::geo::dist(w, *c) <= *r {
                    return Some(6.0);
                }
                Some(d_arc)
            }
            Geo::Angle { v, a0, sweep } => {
                let d = self.to_screen(rect, *v).distance(pos);
                let rel = crate::geo::norm_angle(crate::geo::angle_of(*v, w) - a0);
                (d < 34.0 && rel <= *sweep).then_some(6.0)
            }
            Geo::Polygon(pts) => {
                let screen: Vec<Pos2> = pts.iter().map(|p| self.to_screen(rect, *p)).collect();
                if point_in_polygon(pos, &screen) {
                    return Some(6.0);
                }
                (0..screen.len()).map(|i| seg_dist(pos, screen[i], screen[(i + 1) % screen.len()])).reduce(f32::min)
            }
            Geo::Region { cond, .. } => (cond.eval(&[w[0], w[1]]) > 0.5).then_some(8.0),
            Geo::Implicit { f, .. } => {
                let h = 1e-5 * (1.0 + w[0].abs() + w[1].abs());
                let v = f.eval(&[w[0], w[1]]);
                let gx = (f.eval(&[w[0] + h, w[1]]) - f.eval(&[w[0] - h, w[1]])) / (2.0 * h);
                let gy = (f.eval(&[w[0], w[1] + h]) - f.eval(&[w[0], w[1] - h])) / (2.0 * h);
                let g = (gx * gx + gy * gy).sqrt();
                if g > 0.0 && v.is_finite() { Some((v.abs() / g) as f32 * s) } else { None }
            }
        }
    }

    pub fn hit(&self, rect: Rect, doc: &Document, pos: Pos2, accept: &dyn Fn(&Geo) -> bool) -> Option<usize> {
        let mut best: Option<(usize, f32)> = None;
        for (i, obj) in doc.objects.iter().enumerate() {
            let Some(geo) = &obj.geo else { continue };
            if !accept(geo) {
                continue;
            }
            let Some(d) = self.distance_px(rect, obj, pos) else { continue };
            let bonus = if geo.is_point_like() { 4.0 } else { 0.0 };
            let limit = 8.0 + bonus;
            let score = d - bonus;
            if d <= limit && best.map(|(_, b)| score < b).unwrap_or(true) {
                best = Some((i, score));
            }
        }
        best.map(|(i, _)| i)
    }
}

fn subscript(n: usize) -> String {
    const SUB: [char; 10] = ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'];
    n.to_string().chars().map(|c| SUB[c.to_digit(10).unwrap_or(0) as usize]).collect()
}

fn seg_dist(p: Pos2, a: Pos2, b: Pos2) -> f32 {
    let ab = b - a;
    let len2 = ab.length_sq();
    if len2 == 0.0 {
        return p.distance(a);
    }
    let t = ((p - a).dot(ab) / len2).clamp(0.0, 1.0);
    p.distance(a + ab * t)
}

fn point_in_polygon(p: Pos2, poly: &[Pos2]) -> bool {
    let mut inside = false;
    let n = poly.len();
    let mut j = n - 1;
    for i in 0..n {
        let (a, b) = (poly[i], poly[j]);
        if (a.y > p.y) != (b.y > p.y) && p.x < (b.x - a.x) * (p.y - a.y) / (b.y - a.y) + a.x {
            inside = !inside;
        }
        j = i;
    }
    inside
}

pub fn fill_polygon(painter: &Painter, pts: &[Pos2], color: Color32) {
    let n = pts.len();
    if n < 3 {
        return;
    }
    let area: f32 = (0..n).map(|i| {
        let (a, b) = (pts[i], pts[(i + 1) % n]);
        a.x * b.y - b.x * a.y
    }).sum();
    let ccw = area > 0.0;
    let mut idx: Vec<usize> = (0..n).collect();
    let mut mesh = egui::Mesh::default();
    for p in pts {
        mesh.colored_vertex(*p, color);
    }
    let cross = |o: Pos2, a: Pos2, b: Pos2| (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x);
    let mut guard = 0;
    while idx.len() > 3 && guard < n * n {
        guard += 1;
        let m = idx.len();
        let mut clipped = false;
        for k in 0..m {
            let (ia, ib, ic) = (idx[(k + m - 1) % m], idx[k], idx[(k + 1) % m]);
            let (a, b, c) = (pts[ia], pts[ib], pts[ic]);
            let cr = cross(a, b, c);
            if (cr > 0.0) != ccw || cr == 0.0 {
                continue;
            }
            let contains = idx.iter().any(|&j| {
                if j == ia || j == ib || j == ic {
                    return false;
                }
                let p = pts[j];
                let d1 = cross(a, b, p);
                let d2 = cross(b, c, p);
                let d3 = cross(c, a, p);
                if ccw { d1 >= 0.0 && d2 >= 0.0 && d3 >= 0.0 } else { d1 <= 0.0 && d2 <= 0.0 && d3 <= 0.0 }
            });
            if contains {
                continue;
            }
            mesh.add_triangle(ia as u32, ib as u32, ic as u32);
            idx.remove(k);
            clipped = true;
            break;
        }
        if !clipped {
            break;
        }
    }
    if idx.len() == 3 {
        mesh.add_triangle(idx[0] as u32, idx[1] as u32, idx[2] as u32);
    }
    painter.add(Shape::mesh(mesh));
}
