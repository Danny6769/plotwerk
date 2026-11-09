use crate::doc::{Env, PALETTE};
use crate::expr_util::{contains_var, prime_notation, sub, var};
use crate::theme;
use eframe::egui::{self, Align2, Color32, CornerRadius, FontId, Key, Margin, Pos2, Rect, RichText, Sense, Shape, Stroke, TextEdit, pos2, vec2};
use plotwerk_math::{self as m, Compiled, Expr, Statement};
use std::collections::HashMap;

type P3 = [f64; 3];

#[derive(Clone)]
enum K3 {
    Surface(Compiled),
    Point(P3),
    Plane { n: P3, d: f64 },
    Sphere { c: P3, r: f64 },
    Segment(P3, P3),
    Line(P3, P3),
    Vector(P3, P3),
    Curve(Vec<P3>),
    Number(f64),
}

struct Obj3 {
    raw: String,
    name: String,
    source: String,
    color: Color32,
    visible: bool,
    kind: Option<K3>,
    error: Option<String>,
}

pub struct Space3D {
    objs: Vec<Obj3>,
    input: String,
    error: Option<String>,
    yaw: f32,
    pitch: f32,
    zoom: f32,
    range: f64,
    color_index: usize,
    focus: bool,
}

impl Default for Space3D {
    fn default() -> Self {
        Self { objs: Vec::new(), input: String::new(), error: None, yaw: -0.65, pitch: 0.42, zoom: 1.0, range: 5.0, color_index: 0, focus: true }
    }
}

enum Prim {
    Tri([Pos2; 3], Color32, f32),
    Line([Pos2; 2], Stroke, f32),
    Dot(Pos2, Color32, f32, String),
}

impl Prim {
    fn depth(&self) -> f32 {
        match self {
            Prim::Tri(_, _, d) | Prim::Line(_, _, d) | Prim::Dot(_, _, d, _) => *d,
        }
    }
}

fn sub3(a: P3, b: P3) -> P3 {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

fn cross(a: P3, b: P3) -> P3 {
    [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]
}

fn dot(a: P3, b: P3) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn norm(a: P3) -> f64 {
    dot(a, a).sqrt()
}

fn fmt3(p: P3) -> String {
    format!("({}, {}, {})", m::format_number(p[0]), m::format_number(p[1]), m::format_number(p[2]))
}

fn key(name: &str) -> String {
    name.to_lowercase().replace('ä', "ae").replace('ö', "oe").replace('ü', "ue")
}

impl Space3D {
    pub fn clear(&mut self) {
        self.objs.clear();
        self.color_index = 0;
    }

    pub fn sources(&self) -> Vec<String> {
        self.objs.iter().map(|o| o.raw.clone()).collect()
    }

    fn next_name(&self, upper: bool) -> String {
        let pool: Vec<char> = if upper { ('A'..='Z').collect() } else { "fghpqrsuvwabcdk".chars().collect() };
        for round in 0.. {
            for c in &pool {
                let n = if round == 0 { c.to_string() } else { format!("{c}_{round}") };
                if !self.objs.iter().any(|o| o.name == n) && !["x", "y", "z", "e", "t"].contains(&n.as_str()) {
                    return n;
                }
            }
        }
        unreachable!()
    }

    pub fn add_input(&mut self, raw: &str) -> Result<(), String> {
        let mut text = prime_notation(raw.trim());
        if text.is_empty() {
            return Ok(());
        }
        let mut forced_name: Option<String> = None;
        if let Some((n, rest)) = text.split_once(':') {
            let n = n.trim();
            if !n.is_empty() && n.chars().all(|c| c.is_alphanumeric() || c == '_') && !rest.starts_with('=') {
                forced_name = Some(n.to_string());
                text = rest.trim().to_string();
            }
        }
        let stmt = m::parse_statement(&text).map_err(|e| e.message)?;
        let (name, source) = match &stmt {
            Statement::Assign { name, params, .. } => {
                let rhs = text.split_once(":=").or_else(|| text.split_once('=')).map(|(_, r)| r.trim().to_string()).unwrap_or_default();
                let src = if params.is_empty() { rhs } else { format!("{}|{}", params.join(","), rhs) };
                (name.clone(), src)
            }
            Statement::Equation { .. } => (forced_name.clone().unwrap_or_else(|| self.next_name(false)), format!("eq|{text}")),
            Statement::Expr(e) => {
                let upper = matches!(e, Expr::Tuple(items) if items.len() == 3);
                (forced_name.clone().unwrap_or_else(|| self.next_name(upper)), text.clone())
            }
        };
        let color = if source.starts_with('(') { Color32::from_rgb(38, 50, 72) } else { PALETTE[self.color_index % PALETTE.len()] };
        let raw = match &stmt {
            Statement::Assign { .. } => text.clone(),
            _ => format!("{name}: {text}"),
        };
        if let Some(o) = self.objs.iter_mut().find(|o| o.name == name) {
            o.source = source;
            o.raw = raw;
        } else {
            self.color_index += 1;
            self.objs.push(Obj3 { raw, name: name.clone(), source, color, visible: true, kind: None, error: None });
        }
        self.recompute();
        let err = self.objs.iter().find(|o| o.name == name).and_then(|o| o.error.clone());
        if let Some(e) = err {
            self.objs.retain(|o| o.name != name);
            self.recompute();
            return Err(e);
        }
        Ok(())
    }

    fn recompute(&mut self) {
        let mut env = Env::default();
        let mut pts: HashMap<String, P3> = HashMap::new();
        for i in 0..self.objs.len() {
            let src = self.objs[i].source.clone();
            let r = eval_obj(&src, &env, &pts);
            let o = &mut self.objs[i];
            match r {
                Ok(k) => {
                    match &k {
                        K3::Point(p) => {
                            pts.insert(o.name.clone(), *p);
                        }
                        K3::Number(v) => {
                            env.values.insert(o.name.clone(), *v);
                        }
                        _ => {}
                    }
                    if let (Some((params, body)), K3::Surface(_)) = (src.split_once('|'), &k) {
                        if params != "eq" {
                            if let Ok(e) = m::parse_expr(body) {
                                env.functions.insert(o.name.clone(), (params.split(',').map(str::to_string).collect(), e));
                            }
                        }
                    }
                    o.kind = Some(k);
                    o.error = None;
                }
                Err(e) => {
                    o.kind = None;
                    o.error = Some(e);
                }
            }
        }
    }

    fn display(o: &Obj3) -> String {
        match &o.kind {
            Some(K3::Point(p)) => format!("{} = {}", o.name, fmt3(*p)),
            Some(K3::Number(v)) => format!("{} = {}", o.name, m::format_number(*v)),
            Some(K3::Plane { n, d }) => {
                let mut s = String::new();
                for (c, v) in n.iter().zip(["x", "y", "z"]) {
                    if c.abs() < 1e-12 {
                        continue;
                    }
                    let sign = if *c < 0.0 { if s.is_empty() { "-" } else { " - " } } else if s.is_empty() { "" } else { " + " };
                    let a = c.abs();
                    s.push_str(sign);
                    if (a - 1.0).abs() > 1e-12 {
                        s.push_str(&m::format_number(a));
                    }
                    s.push_str(v);
                }
                format!("{}: {} = {}", o.name, s, m::format_number(*d))
            }
            Some(K3::Sphere { c, r }) => format!("{}: Kugel {} , r = {}", o.name, fmt3(*c), m::format_number(*r)),
            Some(K3::Segment(a, b)) => format!("{} = {}", o.name, m::format_number(norm(sub3(*b, *a)))),
            Some(K3::Vector(a, b)) => format!("{} = {}", o.name, fmt3(sub3(*b, *a))),
            _ => {
                let src = o.source.split_once('|').map(|(p, b)| if p == "eq" { b.to_string() } else { format!("({p}) = {b}") }).unwrap_or_else(|| o.source.clone());
                if src.starts_with('(') && o.source.contains('|') {
                    format!("{}{}", o.name, src)
                } else if o.source.starts_with("eq|") {
                    format!("{}: {}", o.name, src)
                } else {
                    format!("{} = {}", o.name, src)
                }
            }
        }
    }

    pub fn side_ui(&mut self, ui: &mut egui::Ui) {
        let mut remove = None;
        let mut toggle = None;
        egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
            ui.spacing_mut().item_spacing.y = 0.0;
            for (i, o) in self.objs.iter().enumerate() {
                let inner = egui::Frame::new().fill(Color32::WHITE).inner_margin(Margin { left: 14, right: 8, top: 10, bottom: 10 }).show(ui, |ui| {
                    ui.set_width(ui.available_width());
                    ui.horizontal(|ui| {
                        let (r, resp) = ui.allocate_exact_size(vec2(22.0, 22.0), Sense::click());
                        if o.visible {
                            ui.painter().circle_filled(r.center(), 8.5, o.color);
                        } else {
                            ui.painter().circle_stroke(r.center(), 7.5, Stroke::new(1.6, o.color));
                        }
                        if resp.clicked() {
                            toggle = Some(i);
                        }
                        ui.add_space(4.0);
                        ui.vertical(|ui| {
                            ui.set_width(ui.available_width() - 28.0);
                            ui.add(egui::Label::new(RichText::new(crate::pretty::pretty(&Self::display(o))).size(16.0)).truncate());
                            if let Some(e) = &o.error {
                                ui.label(RichText::new(e).small().color(theme::ERROR));
                            }
                        });
                        if ui.add(egui::Button::new(RichText::new("×").size(17.0).color(theme::MUTED)).frame(false)).clicked() {
                            remove = Some(i);
                        }
                    });
                });
                let r = inner.response.rect;
                ui.painter().line_segment([r.left_bottom(), r.right_bottom()], Stroke::new(1.0, theme::BORDER));
            }
            let inner = egui::Frame::new().fill(Color32::WHITE).inner_margin(Margin { left: 14, right: 8, top: 10, bottom: 10 }).show(ui, |ui| {
                ui.set_width(ui.available_width());
                ui.horizontal(|ui| {
                    let (r, _) = ui.allocate_exact_size(vec2(22.0, 22.0), Sense::hover());
                    crate::icons::plus(ui.painter(), r, theme::MUTED);
                    ui.add_space(4.0);
                    ui.add(TextEdit::singleline(&mut self.input).hint_text("Eingabe …").font(FontId::proportional(16.0)).frame(false).desired_width(f32::INFINITY))
                })
                .inner
            });
            let r = inner.response.rect;
            ui.painter().line_segment([r.left_bottom(), r.right_bottom()], Stroke::new(1.0, theme::BORDER));
            let resp = inner.inner;
            if self.focus {
                resp.request_focus();
                self.focus = false;
            }
            if resp.lost_focus() && ui.input(|i| i.key_pressed(Key::Enter)) {
                let text = std::mem::take(&mut self.input);
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| self.add_input(&text))) {
                    Ok(Ok(())) => self.error = None,
                    Ok(Err(e)) => {
                        self.error = Some(e);
                        self.input = text;
                    }
                    Err(_) => self.error = Some("Interner Fehler".into()),
                }
                self.focus = true;
            }
            if let Some(e) = &self.error {
                ui.add_space(6.0);
                ui.horizontal(|ui| {
                    ui.add_space(14.0);
                    ui.label(RichText::new(e).small().color(theme::ERROR));
                });
            }
            if self.objs.is_empty() {
                ui.add_space(18.0);
                ui.horizontal(|ui| {
                    ui.add_space(14.0);
                    ui.vertical(|ui| {
                        ui.spacing_mut().item_spacing.y = 4.0;
                        ui.label(RichText::new("Beispiele").small().color(theme::MUTED));
                        for ex in ["f(x, y) = sin(x) cos(y)", "z = x^2/4 - y^2/4", "A = (1, 2, 3)", "2x + y - z = 2", "Kugel((0, 0, 0), 2)", "Gerade(A, (0, 0, 0))", "Kurve(cos(t), sin(t), t/4, t, 0, 12)"] {
                            ui.label(RichText::new(ex).size(13.5).color(theme::MUTED));
                        }
                    });
                });
            }
        });
        if let Some(i) = toggle {
            self.objs[i].visible = !self.objs[i].visible;
        }
        if let Some(i) = remove {
            self.objs.remove(i);
            self.recompute();
        }
    }

    fn project(&self, rect: Rect, p: P3) -> (Pos2, f32) {
        let s = self.range;
        let (x, y, z) = (p[0] / s, p[1] / s, p[2] / s);
        let (sy, cy) = self.yaw.sin_cos();
        let (sp, cp) = self.pitch.sin_cos();
        let x1 = x as f32 * cy - y as f32 * sy;
        let y1 = x as f32 * sy + y as f32 * cy;
        let z1 = z as f32;
        let up = z1 * cp + y1 * sp;
        let depth = y1 * cp - z1 * sp;
        let persp = 4.0 / (4.0 + depth);
        let scale = rect.width().min(rect.height()) * 0.36 * self.zoom;
        (pos2(rect.center().x + x1 * persp * scale, rect.center().y - up * persp * scale), depth)
    }

    fn light(&self, n: P3, base: Color32, alpha: f32) -> Color32 {
        let l = [0.35, -0.45, 0.82];
        let len = norm(n).max(1e-12);
        let k = (dot(n, l) / len).abs() as f32;
        let f = 0.5 + 0.5 * k;
        let [r, g, b, _] = base.to_array();
        Color32::from_rgba_unmultiplied((r as f32 * f) as u8, (g as f32 * f) as u8, (b as f32 * f) as u8, (alpha * 255.0) as u8)
    }

    fn mesh_prims(&self, rect: Rect, out: &mut Vec<Prim>, n: usize, color: Color32, at: &dyn Fn(f64, f64) -> Option<P3>) {
        let mut grid: Vec<Option<P3>> = Vec::with_capacity((n + 1) * (n + 1));
        for j in 0..=n {
            for i in 0..=n {
                grid.push(at(i as f64 / n as f64, j as f64 / n as f64));
            }
        }
        let r = self.range * 1.0001;
        let inside = |p: &P3| p.iter().all(|c| c.abs() <= r && c.is_finite());
        for j in 0..n {
            for i in 0..n {
                let idx = |a: usize, b: usize| grid[b * (n + 1) + a];
                let quad = [idx(i, j), idx(i + 1, j), idx(i + 1, j + 1), idx(i, j + 1)];
                for tri in [[0usize, 1, 2], [0, 2, 3]] {
                    let ps: Option<Vec<P3>> = tri.iter().map(|&k| quad[k]).collect();
                    let Some(ps) = ps else { continue };
                    if !ps.iter().all(inside) {
                        continue;
                    }
                    let nrm = cross(sub3(ps[1], ps[0]), sub3(ps[2], ps[0]));
                    let pr: Vec<(Pos2, f32)> = ps.iter().map(|p| self.project(rect, *p)).collect();
                    let depth = (pr[0].1 + pr[1].1 + pr[2].1) / 3.0;
                    out.push(Prim::Tri([pr[0].0, pr[1].0, pr[2].0], self.light(nrm, color, 0.82), depth));
                }
            }
        }
    }

    fn line_prims(&self, rect: Rect, out: &mut Vec<Prim>, a: P3, b: P3, stroke: Stroke, steps: usize) {
        for k in 0..steps {
            let t0 = k as f64 / steps as f64;
            let t1 = (k + 1) as f64 / steps as f64;
            let p0 = [a[0] + (b[0] - a[0]) * t0, a[1] + (b[1] - a[1]) * t0, a[2] + (b[2] - a[2]) * t0];
            let p1 = [a[0] + (b[0] - a[0]) * t1, a[1] + (b[1] - a[1]) * t1, a[2] + (b[2] - a[2]) * t1];
            let (s0, d0) = self.project(rect, p0);
            let (s1, d1) = self.project(rect, p1);
            out.push(Prim::Line([s0, s1], stroke, (d0 + d1) / 2.0 - 0.001));
        }
    }

    fn clip_line(&self, a: P3, d: P3) -> Option<(P3, P3)> {
        let r = self.range;
        let (mut t0, mut t1) = (f64::NEG_INFINITY, f64::INFINITY);
        for k in 0..3 {
            if d[k].abs() < 1e-14 {
                if a[k].abs() > r {
                    return None;
                }
            } else {
                let ta = (-r - a[k]) / d[k];
                let tb = (r - a[k]) / d[k];
                t0 = t0.max(ta.min(tb));
                t1 = t1.min(ta.max(tb));
            }
        }
        (t0 < t1).then(|| ([a[0] + d[0] * t0, a[1] + d[1] * t0, a[2] + d[2] * t0], [a[0] + d[0] * t1, a[1] + d[1] * t1, a[2] + d[2] * t1]))
    }

    pub fn canvas_ui(&mut self, ui: &mut egui::Ui) {
        let (resp, painter) = ui.allocate_painter(ui.available_size(), Sense::click_and_drag());
        let rect = resp.rect;
        painter.rect_filled(rect, 0.0, Color32::WHITE);
        if resp.dragged() {
            let d = resp.drag_delta();
            self.yaw -= d.x * 0.008;
            self.pitch = (self.pitch + d.y * 0.008).clamp(-1.55, 1.55);
        }
        if resp.hovered() {
            let (scroll, zoom) = ui.input(|i| (i.smooth_scroll_delta.y, i.zoom_delta()));
            if scroll != 0.0 {
                self.zoom = (self.zoom * (scroll * 0.002).exp()).clamp(0.2, 6.0);
            }
            if zoom != 1.0 {
                self.zoom = (self.zoom * zoom).clamp(0.2, 6.0);
            }
        }
        let r = self.range;
        let mut prims: Vec<Prim> = Vec::new();
        let grid = Stroke::new(1.0, Color32::from_rgb(226, 229, 236));
        let steps = 10;
        for k in 0..=steps {
            let t = -r + 2.0 * r * k as f64 / steps as f64;
            self.line_prims(rect, &mut prims, [t, -r, 0.0], [t, r, 0.0], grid, 4);
            self.line_prims(rect, &mut prims, [-r, t, 0.0], [r, t, 0.0], grid, 4);
        }
        let axes = [([1.0, 0.0, 0.0], Color32::from_rgb(198, 40, 40), "x"), ([0.0, 1.0, 0.0], Color32::from_rgb(46, 125, 50), "y"), ([0.0, 0.0, 1.0], Color32::from_rgb(21, 101, 192), "z")];
        for (d, c, _) in axes {
            self.line_prims(rect, &mut prims, [-r * d[0], -r * d[1], -r * d[2]], [r * d[0], r * d[1], r * d[2]], Stroke::new(1.6, c), 8);
        }
        for o in &self.objs {
            if !o.visible {
                continue;
            }
            let Some(k) = &o.kind else { continue };
            let st = Stroke::new(2.6, o.color);
            match k {
                K3::Surface(f) => {
                    self.mesh_prims(rect, &mut prims, 48, o.color, &|u, v| {
                        let (x, y) = (-r + 2.0 * r * u, -r + 2.0 * r * v);
                        let z = f.eval(&[x, y]);
                        z.is_finite().then_some([x, y, z])
                    });
                }
                K3::Plane { n, d } => {
                    let ax = (0..3).max_by(|a, b| n[*a].abs().partial_cmp(&n[*b].abs()).unwrap()).unwrap();
                    let (i, j) = match ax {
                        0 => (1, 2),
                        1 => (0, 2),
                        _ => (0, 1),
                    };
                    let (n, d) = (*n, *d);
                    self.mesh_prims(rect, &mut prims, 12, o.color, &|u, v| {
                        let mut p = [0.0; 3];
                        p[i] = -r + 2.0 * r * u;
                        p[j] = -r + 2.0 * r * v;
                        p[ax] = (d - n[i] * p[i] - n[j] * p[j]) / n[ax];
                        Some(p)
                    });
                }
                K3::Sphere { c, r: rad } => {
                    let (c, rad) = (*c, *rad);
                    self.mesh_prims(rect, &mut prims, 36, o.color, &|u, v| {
                        let th = u * std::f64::consts::TAU;
                        let ph = v * std::f64::consts::PI;
                        Some([c[0] + rad * ph.sin() * th.cos(), c[1] + rad * ph.sin() * th.sin(), c[2] + rad * ph.cos()])
                    });
                }
                K3::Segment(a, b) => self.line_prims(rect, &mut prims, *a, *b, st, 24),
                K3::Vector(a, b) => {
                    self.line_prims(rect, &mut prims, *a, *b, st, 24);
                    let (sb, db) = self.project(rect, *b);
                    let (sa, _) = self.project(rect, *a);
                    let dir = (sb - sa).normalized();
                    let perp = vec2(-dir.y, dir.x) * 5.0;
                    let base = sb - dir * 12.0;
                    prims.push(Prim::Tri([sb, base + perp, base - perp], o.color, db - 0.002));
                }
                K3::Line(a, b) => {
                    if let Some((p, q)) = self.clip_line(*a, sub3(*b, *a)) {
                        self.line_prims(rect, &mut prims, p, q, st, 40);
                    }
                }
                K3::Curve(pts) => {
                    for w in pts.windows(2) {
                        if w.iter().all(|p| p.iter().all(|c| c.is_finite())) {
                            self.line_prims(rect, &mut prims, w[0], w[1], st, 1);
                        }
                    }
                }
                K3::Point(p) => {
                    let (s, dp) = self.project(rect, *p);
                    prims.push(Prim::Dot(s, o.color, dp - 0.003, o.name.clone()));
                }
                K3::Number(_) => {}
            }
        }
        prims.sort_by(|a, b| b.depth().partial_cmp(&a.depth()).unwrap_or(std::cmp::Ordering::Equal));
        let mut mesh = egui::Mesh::default();
        let flush = |mesh: &mut egui::Mesh| {
            if !mesh.is_empty() {
                painter.add(Shape::mesh(std::mem::take(mesh)));
            }
        };
        for p in prims {
            match p {
                Prim::Tri(v, c, _) => {
                    let base = mesh.vertices.len() as u32;
                    for q in v {
                        mesh.colored_vertex(q, c);
                    }
                    mesh.add_triangle(base, base + 1, base + 2);
                }
                Prim::Line(l, s, _) => {
                    flush(&mut mesh);
                    painter.line_segment(l, s);
                }
                Prim::Dot(pos, c, _, label) => {
                    flush(&mut mesh);
                    painter.circle_filled(pos, 6.0, Color32::WHITE);
                    painter.circle_filled(pos, 4.6, c);
                    painter.text(pos + vec2(8.0, -8.0), Align2::LEFT_BOTTOM, label, FontId::proportional(13.0), theme::TEXT);
                }
            }
        }
        flush(&mut mesh);
        for (d, c, name) in axes {
            let (s, _) = self.project(rect, [r * d[0] * 1.08, r * d[1] * 1.08, r * d[2] * 1.08]);
            painter.text(s, Align2::CENTER_CENTER, name, FontId::proportional(15.0), c);
        }
        let hint = Rect::from_min_size(rect.left_top() + vec2(14.0, 12.0), vec2(270.0, 26.0));
        painter.rect_filled(hint, CornerRadius::same(6), Color32::from_white_alpha(230));
        painter.text(hint.left_center() + vec2(8.0, 0.0), Align2::LEFT_CENTER, "Ziehen: drehen   Mausrad: zoomen", FontId::proportional(12.5), theme::MUTED);
    }
}

fn eval_obj(src: &str, env: &Env, pts: &HashMap<String, P3>) -> Result<K3, String> {
    if let Some((params, body)) = src.split_once('|') {
        if params == "eq" {
            return match m::parse_statement(body).map_err(|e| e.message)? {
                Statement::Equation { lhs, rhs } => equation(&lhs, &rhs, env, pts),
                _ => Err("Gleichung erwartet".into()),
            };
        }
        let e = m::parse_expr(body).map_err(|e| e.message)?;
        let ps: Vec<&str> = params.split(',').collect();
        if ps.len() != 2 {
            return Err("Funktion mit zwei Variablen erwartet, z. B. f(x, y)".into());
        }
        return Ok(K3::Surface(m::compile(&e, &[ps[0], ps[1]], env).map_err(|e| e.0)?));
    }
    let e = m::parse_expr(src).map_err(|e| e.message)?;
    expr_obj(&e, env, pts)
}

fn resolve(e: &Expr, pts: &HashMap<String, P3>, env: &Env) -> Result<P3, String> {
    match e {
        Expr::Var(n) if pts.contains_key(n) => Ok(pts[n]),
        Expr::Tuple(items) if items.len() == 3 => {
            let mut p = [0.0; 3];
            for (k, it) in items.iter().enumerate() {
                p[k] = m::eval(it, env).map_err(|e| e.0)?;
            }
            Ok(p)
        }
        _ => Err("Punkt im Raum erwartet, z. B. (1, 2, 3)".into()),
    }
}

fn expr_obj(e: &Expr, env: &Env, pts: &HashMap<String, P3>) -> Result<K3, String> {
    match e {
        Expr::Tuple(items) if items.len() == 3 => return Ok(K3::Point(resolve(e, pts, env)?)),
        Expr::Var(n) if pts.contains_key(n) => return Ok(K3::Point(pts[n])),
        Expr::Call(name, args) => {
            let k = key(name);
            let n = args.len();
            let p = |i: usize| resolve(&args[i], pts, env);
            let num = |i: usize| m::eval(&args[i], env).map_err(|e| e.0);
            match k.as_str() {
                "gerade" | "line" if n == 2 => return Ok(K3::Line(p(0)?, p(1)?)),
                "strecke" | "segment" if n == 2 => return Ok(K3::Segment(p(0)?, p(1)?)),
                "vektor" | "vector" if n == 1 => return Ok(K3::Vector([0.0; 3], p(0)?)),
                "vektor" | "vector" if n == 2 => return Ok(K3::Vector(p(0)?, p(1)?)),
                "kugel" | "sphere" if n == 2 => {
                    let c = p(0)?;
                    let r = match p(1) {
                        Ok(q) => norm(sub3(q, c)),
                        Err(_) => num(1)?,
                    };
                    return Ok(K3::Sphere { c, r });
                }
                "ebene" | "plane" if n == 3 => {
                    let (a, b, c) = (p(0)?, p(1)?, p(2)?);
                    let nrm = cross(sub3(b, a), sub3(c, a));
                    if norm(nrm) < 1e-12 {
                        return Err("Punkte liegen auf einer Geraden".into());
                    }
                    return Ok(K3::Plane { n: nrm, d: dot(nrm, a) });
                }
                "kurve" | "curve" if n == 6 => {
                    let Expr::Var(t) = &args[3] else { return Err("Parameter erwartet".into()) };
                    let (a, b) = (num(4)?, num(5)?);
                    let fx = m::compile(&args[0], &[t.as_str()], env).map_err(|e| e.0)?;
                    let fy = m::compile(&args[1], &[t.as_str()], env).map_err(|e| e.0)?;
                    let fz = m::compile(&args[2], &[t.as_str()], env).map_err(|e| e.0)?;
                    let pts = (0..=600).map(|i| {
                        let t = a + (b - a) * i as f64 / 600.0;
                        [fx.eval(&[t]), fy.eval(&[t]), fz.eval(&[t])]
                    });
                    return Ok(K3::Curve(pts.collect()));
                }
                _ => {}
            }
        }
        _ => {}
    }
    if contains_var(e, "x") || contains_var(e, "y") {
        return Ok(K3::Surface(m::compile(e, &["x", "y"], env).map_err(|e| e.0)?));
    }
    Ok(K3::Number(m::eval(e, env).map_err(|e| e.0)?))
}

fn equation(lhs: &Expr, rhs: &Expr, env: &Env, _pts: &HashMap<String, P3>) -> Result<K3, String> {
    if *lhs == var("z") && !contains_var(rhs, "z") {
        return Ok(K3::Surface(m::compile(rhs, &["x", "y"], env).map_err(|e| e.0)?));
    }
    let f = m::simplify(&sub(lhs.clone(), rhs.clone()));
    let c = m::compile(&f, &["x", "y", "z"], env).map_err(|e| e.0)?;
    let vars = ["x", "y", "z"];
    let d1: Vec<Expr> = vars.iter().map(|v| m::simplify(&m::derivative(&f, v))).collect();
    let has = |e: &Expr| vars.iter().any(|v| contains_var(e, v));
    if d1.iter().all(|d| !has(d)) {
        let n: Vec<f64> = d1.iter().map(|d| m::eval(d, env).unwrap_or(0.0)).collect();
        if n.iter().all(|v| v.abs() < 1e-14) {
            return Err("Keine Ebene".into());
        }
        return Ok(K3::Plane { n: [n[0], n[1], n[2]], d: -c.eval(&[0.0, 0.0, 0.0]) });
    }
    let d2: Vec<Expr> = vars.iter().zip(&d1).map(|(v, d)| m::simplify(&m::derivative(d, v))).collect();
    if d2.iter().all(|d| !has(d)) {
        let k: Vec<f64> = d2.iter().map(|d| m::eval(d, env).unwrap_or(f64::NAN)).collect();
        if k[0].abs() > 1e-12 && (k[0] - k[1]).abs() < 1e-12 && (k[0] - k[2]).abs() < 1e-12 {
            let kk = k[0] / 2.0;
            let g0: Vec<f64> = d1.iter().map(|d| m::compile(d, &vars, env).map(|c| c.eval(&[0.0, 0.0, 0.0])).unwrap_or(f64::NAN)).collect();
            let cen = [-g0[0] / (2.0 * kk), -g0[1] / (2.0 * kk), -g0[2] / (2.0 * kk)];
            let r2 = dot(cen, cen) - c.eval(&[0.0, 0.0, 0.0]) / kk;
            if r2 > 0.0 {
                return Ok(K3::Sphere { c: cen, r: r2.sqrt() });
            }
        }
    }
    Err("Diese Fläche wird im 3D-Rechner noch nicht unterstützt (Ebenen, Kugeln, z = f(x, y))".into())
}
