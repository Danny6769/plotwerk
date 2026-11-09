use eframe::egui::{Color32, Painter, Pos2, Rect, Shape, Stroke, pos2};

fn arc(c: Pos2, r: f32, a0: f32, a1: f32, n: usize) -> Vec<Pos2> {
    (0..=n)
        .map(|i| {
            let t = a0 + (a1 - a0) * i as f32 / n as f32;
            pos2(c.x + r * t.cos(), c.y + r * t.sin())
        })
        .collect()
}

pub fn undo(p: &Painter, r: Rect, color: Color32) {
    let c = r.center();
    let s = r.width().min(r.height()) * 0.28;
    let pts = arc(pos2(c.x + s * 0.15, c.y + s * 0.2), s, -2.6, 1.3, 18);
    let start = pts[0];
    p.add(Shape::line(pts, Stroke::new(1.8, color)));
    p.add(Shape::convex_polygon(
        vec![pos2(start.x - s * 0.55, start.y - s * 0.1), pos2(start.x + s * 0.35, start.y - s * 0.55), pos2(start.x + s * 0.15, start.y + s * 0.45)],
        color,
        Stroke::NONE,
    ));
}

pub fn redo(p: &Painter, r: Rect, color: Color32) {
    let c = r.center();
    let s = r.width().min(r.height()) * 0.28;
    let pts = arc(pos2(c.x - s * 0.15, c.y + s * 0.2), s, std::f32::consts::PI + 2.6, std::f32::consts::PI - 1.3, 18);
    let start = pts[0];
    p.add(Shape::line(pts, Stroke::new(1.8, color)));
    p.add(Shape::convex_polygon(
        vec![pos2(start.x + s * 0.55, start.y - s * 0.1), pos2(start.x - s * 0.15, start.y + s * 0.45), pos2(start.x - s * 0.35, start.y - s * 0.55)],
        color,
        Stroke::NONE,
    ));
}

pub fn menu(p: &Painter, r: Rect, color: Color32) {
    let c = r.center();
    let w = r.width() * 0.3;
    for dy in [-6.0, 0.0, 6.0] {
        p.line_segment([pos2(c.x - w, c.y + dy), pos2(c.x + w, c.y + dy)], Stroke::new(2.0, color));
    }
}

pub fn chevron_down(p: &Painter, c: Pos2, color: Color32) {
    p.add(Shape::line(vec![pos2(c.x - 4.5, c.y - 2.0), pos2(c.x, c.y + 2.5), pos2(c.x + 4.5, c.y - 2.0)], Stroke::new(1.8, color)));
}

pub fn axes(p: &Painter, r: Rect, color: Color32) {
    let c = r.center();
    let s = r.width().min(r.height()) * 0.32;
    let st = Stroke::new(1.7, color);
    p.line_segment([pos2(c.x - s, c.y + s * 0.4), pos2(c.x + s, c.y + s * 0.4)], st);
    p.line_segment([pos2(c.x - s * 0.4, c.y + s), pos2(c.x - s * 0.4, c.y - s)], st);
    p.add(Shape::convex_polygon(vec![pos2(c.x + s + 1.0, c.y + s * 0.4), pos2(c.x + s - 4.0, c.y + s * 0.4 - 3.0), pos2(c.x + s - 4.0, c.y + s * 0.4 + 3.0)], color, Stroke::NONE));
    p.add(Shape::convex_polygon(vec![pos2(c.x - s * 0.4, c.y - s - 1.0), pos2(c.x - s * 0.4 - 3.0, c.y - s + 4.0), pos2(c.x - s * 0.4 + 3.0, c.y - s + 4.0)], color, Stroke::NONE));
}

pub fn grid(p: &Painter, r: Rect, color: Color32) {
    let c = r.center();
    let s = r.width().min(r.height()) * 0.3;
    let st = Stroke::new(1.3, color);
    for i in 0..4 {
        let t = -s + 2.0 * s * i as f32 / 3.0;
        p.line_segment([pos2(c.x + t, c.y - s), pos2(c.x + t, c.y + s)], st);
        p.line_segment([pos2(c.x - s, c.y + t), pos2(c.x + s, c.y + t)], st);
    }
}

pub fn home(p: &Painter, r: Rect, color: Color32) {
    let c = r.center();
    let s = r.width().min(r.height()) * 0.28;
    let st = Stroke::new(1.8, color);
    p.add(Shape::line(vec![pos2(c.x - s * 1.1, c.y - s * 0.05), pos2(c.x, c.y - s), pos2(c.x + s * 1.1, c.y - s * 0.05)], st));
    p.add(Shape::line(
        vec![pos2(c.x - s * 0.75, c.y - s * 0.3), pos2(c.x - s * 0.75, c.y + s * 0.9), pos2(c.x + s * 0.75, c.y + s * 0.9), pos2(c.x + s * 0.75, c.y - s * 0.3)],
        st,
    ));
}

pub fn plus(p: &Painter, r: Rect, color: Color32) {
    let c = r.center();
    let s = r.width().min(r.height()) * 0.24;
    let st = Stroke::new(2.0, color);
    p.line_segment([pos2(c.x - s, c.y), pos2(c.x + s, c.y)], st);
    p.line_segment([pos2(c.x, c.y - s), pos2(c.x, c.y + s)], st);
}

pub fn minus(p: &Painter, r: Rect, color: Color32) {
    let c = r.center();
    let s = r.width().min(r.height()) * 0.24;
    p.line_segment([pos2(c.x - s, c.y), pos2(c.x + s, c.y)], Stroke::new(2.0, color));
}

pub fn app_graph(p: &Painter, r: Rect, color: Color32) {
    let pts: Vec<Pos2> = (0..=20)
        .map(|i| {
            let t = i as f32 / 20.0;
            pos2(r.left() + 3.0 + t * (r.width() - 6.0), r.center().y - ((t - 0.5) * 6.0).sin() * r.height() * 0.3)
        })
        .collect();
    p.add(Shape::line(pts, Stroke::new(2.0, color)));
}

pub fn app_geometry(p: &Painter, r: Rect, color: Color32) {
    let c = r.center();
    let s = r.width() * 0.36;
    let tri = vec![pos2(c.x - s, c.y + s * 0.75), pos2(c.x + s * 0.2, c.y - s * 0.9), pos2(c.x + s, c.y + s * 0.75)];
    p.add(Shape::closed_line(tri.clone(), Stroke::new(1.8, color)));
    for q in tri {
        p.circle_filled(q, 2.6, color);
    }
}

pub fn app_cas(p: &Painter, r: Rect, color: Color32) {
    p.text(r.center(), eframe::egui::Align2::CENTER_CENTER, "x=", eframe::egui::FontId::proportional(r.height() * 0.55), color);
}

pub fn app_probability(p: &Painter, r: Rect, color: Color32) {
    let base = r.bottom() - 4.0;
    let pts: Vec<Pos2> = (0..=24)
        .map(|i| {
            let t = -2.4 + 4.8 * i as f32 / 24.0;
            pos2(r.center().x + t * r.width() * 0.17, base - (-(t * t) / 2.0).exp() * r.height() * 0.62)
        })
        .collect();
    p.line_segment([pos2(r.left() + 3.0, base), pos2(r.right() - 3.0, base)], Stroke::new(1.3, color));
    p.add(Shape::line(pts, Stroke::new(2.0, color)));
}

pub fn app_3d(p: &Painter, r: Rect, color: Color32) {
    let c = r.center();
    let s = r.width() * 0.26;
    let st = Stroke::new(1.6, color);
    let f = [pos2(c.x - s, c.y - s * 0.4), pos2(c.x + s * 0.5, c.y - s * 0.4), pos2(c.x + s * 0.5, c.y + s), pos2(c.x - s, c.y + s)];
    let o = eframe::egui::vec2(s * 0.6, -s * 0.6);
    p.add(Shape::closed_line(f.to_vec(), st));
    p.line_segment([f[0], f[0] + o], st);
    p.line_segment([f[1], f[1] + o], st);
    p.line_segment([f[2], f[2] + o], st);
    p.line_segment([f[0] + o, f[1] + o], st);
    p.line_segment([f[1] + o, f[2] + o], st);
}

pub fn app_table(p: &Painter, r: Rect, color: Color32) {
    let rr = r.shrink(5.0);
    let st = Stroke::new(1.5, color);
    p.rect_stroke(rr, 2.0, st, eframe::egui::StrokeKind::Inside);
    for i in 1..3 {
        let y = rr.top() + rr.height() * i as f32 / 3.0;
        p.line_segment([pos2(rr.left(), y), pos2(rr.right(), y)], st);
    }
    let x = rr.left() + rr.width() * 0.4;
    p.line_segment([pos2(x, rr.top()), pos2(x, rr.bottom())], st);
}
