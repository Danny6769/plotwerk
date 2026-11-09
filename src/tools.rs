use eframe::egui::{Color32, Painter, Pos2, Rect, Shape, Stroke, pos2, vec2};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Tool {
    Move,
    Point,
    Segment,
    Line,
    Circle,
    Polygon,
    Intersect,
    Midpoint,
    Perpendicular,
    Parallel,
    Tangent,
    Roots,
    Extrema,
    Ray,
    Vector,
    Arc,
    Angle,
    Reflect,
    Rotate,
    Translate,
}

pub const GRAPH_GROUPS: &[&[Tool]] = &[
    &[Tool::Move],
    &[Tool::Point, Tool::Intersect, Tool::Roots, Tool::Extrema],
    &[Tool::Line, Tool::Tangent],
    &[Tool::Reflect, Tool::Translate],
];

pub const GEOMETRY_GROUPS: &[&[Tool]] = &[
    &[Tool::Move],
    &[Tool::Point, Tool::Intersect, Tool::Midpoint],
    &[Tool::Segment, Tool::Line, Tool::Ray, Tool::Vector, Tool::Perpendicular, Tool::Parallel],
    &[Tool::Polygon],
    &[Tool::Circle, Tool::Arc],
    &[Tool::Angle],
    &[Tool::Reflect, Tool::Rotate, Tool::Translate],
];

impl Tool {
    pub fn label(self) -> &'static str {
        match self {
            Tool::Move => "Bewegen",
            Tool::Point => "Punkt",
            Tool::Segment => "Strecke",
            Tool::Line => "Gerade",
            Tool::Circle => "Kreis",
            Tool::Polygon => "Vieleck",
            Tool::Intersect => "Schneiden",
            Tool::Midpoint => "Mittelpunkt",
            Tool::Perpendicular => "Senkrechte",
            Tool::Parallel => "Parallele",
            Tool::Tangent => "Tangente",
            Tool::Roots => "Nullstellen",
            Tool::Extrema => "Extrema",
            Tool::Ray => "Halbgerade",
            Tool::Vector => "Vektor",
            Tool::Arc => "Kreisbogen",
            Tool::Angle => "Winkel",
            Tool::Reflect => "Spiegeln",
            Tool::Rotate => "Drehen",
            Tool::Translate => "Verschieben",
        }
    }

    pub fn hint(self) -> &'static str {
        match self {
            Tool::Move => "Punkte ziehen, Hintergrund ziehen zum Verschieben, Mausrad zum Zoomen",
            Tool::Point => "In die Zeichenfläche klicken, um einen Punkt zu setzen",
            Tool::Segment => "Zwei Punkte wählen",
            Tool::Line => "Zwei Punkte wählen",
            Tool::Circle => "Mittelpunkt wählen, dann einen Punkt auf dem Kreis",
            Tool::Polygon => "Ecken nacheinander wählen, zum Schließen erneut den ersten Punkt anklicken",
            Tool::Intersect => "Zwei Objekte wählen",
            Tool::Midpoint => "Zwei Punkte oder eine Strecke wählen",
            Tool::Perpendicular => "Punkt und Gerade wählen",
            Tool::Parallel => "Punkt und Gerade wählen",
            Tool::Tangent => "Auf eine Funktion an der gewünschten Stelle klicken",
            Tool::Roots => "Funktion anklicken",
            Tool::Extrema => "Funktion anklicken",
            Tool::Ray => "Anfangspunkt, dann Richtungspunkt wählen",
            Tool::Vector => "Anfangspunkt, dann Endpunkt wählen",
            Tool::Arc => "Mittelpunkt, Anfangspunkt und Endrichtung wählen",
            Tool::Angle => "Drei Punkte (Scheitel in der Mitte) oder zwei Geraden wählen",
            Tool::Reflect => "Objekt wählen, dann Spiegelpunkt oder Spiegelachse",
            Tool::Rotate => "Objekt wählen, dann Drehzentrum",
            Tool::Translate => "Objekt wählen, dann Vektor",
        }
    }
}

pub fn paint_icon(painter: &Painter, rect: Rect, tool: Tool, color: Color32) {
    let c = rect.center();
    let s = rect.width().min(rect.height()) / 2.0 - 3.0;
    let p = |x: f32, y: f32| pos2(c.x + x * s, c.y + y * s);
    let stroke = Stroke::new(2.0, color);
    let thin = Stroke::new(1.5, color);
    let dot = |pos: Pos2| painter.circle_filled(pos, 3.2, color);
    match tool {
        Tool::Move => {
            let pts = vec![p(-0.55, -0.8), p(-0.55, 0.55), p(-0.2, 0.22), p(0.08, 0.8), p(0.3, 0.7), p(0.02, 0.12), p(0.5, 0.12)];
            painter.add(Shape::closed_line(pts, stroke));
        }
        Tool::Point => {
            painter.circle_filled(c, 4.0, color);
            painter.circle_stroke(c, 7.5, thin);
        }
        Tool::Segment => {
            painter.line_segment([p(-0.7, 0.5), p(0.7, -0.5)], stroke);
            dot(p(-0.7, 0.5));
            dot(p(0.7, -0.5));
        }
        Tool::Line => {
            painter.line_segment([p(-1.0, 0.75), p(1.0, -0.75)], stroke);
            dot(p(-0.4, 0.3));
            dot(p(0.4, -0.3));
        }
        Tool::Circle => {
            painter.circle_stroke(c, s * 0.8, stroke);
            dot(c);
            dot(p(0.8, 0.0));
        }
        Tool::Polygon => {
            let pts = vec![p(-0.75, 0.6), p(0.0, -0.75), p(0.75, 0.6)];
            painter.add(Shape::convex_polygon(pts.clone(), color.gamma_multiply(0.2), Stroke::NONE));
            painter.add(Shape::closed_line(pts.clone(), stroke));
            for q in pts {
                dot(q);
            }
        }
        Tool::Intersect => {
            painter.line_segment([p(-0.8, 0.6), p(0.8, -0.6)], thin);
            painter.line_segment([p(-0.8, -0.6), p(0.8, 0.6)], thin);
            painter.circle_filled(c, 3.6, color);
        }
        Tool::Midpoint => {
            painter.line_segment([p(-0.8, 0.0), p(0.8, 0.0)], thin);
            dot(p(-0.8, 0.0));
            dot(p(0.8, 0.0));
            painter.circle_filled(c, 3.6, color);
            painter.line_segment([p(0.0, -0.35), p(0.0, 0.35)], thin);
        }
        Tool::Perpendicular => {
            painter.line_segment([p(-0.9, 0.6), p(0.9, 0.6)], thin);
            painter.line_segment([p(0.0, 0.6), p(0.0, -0.9)], stroke);
            painter.add(Shape::line(vec![p(0.0, 0.3), p(0.3, 0.3), p(0.3, 0.6)], thin));
            dot(p(0.0, -0.45));
        }
        Tool::Parallel => {
            painter.line_segment([p(-0.9, 0.2), p(0.6, -0.8)], thin);
            painter.line_segment([p(-0.6, 0.8), p(0.9, -0.2)], stroke);
            dot(p(0.15, 0.3));
        }
        Tool::Tangent => {
            let curve: Vec<Pos2> = (0..=20).map(|i| {
                let t = -1.0 + i as f32 / 10.0;
                p(t * 0.9, -0.3 + t * t * 0.9)
            }).collect();
            painter.add(Shape::line(curve, thin));
            painter.line_segment([p(-0.95, -0.3), p(0.95, -0.3)], stroke);
            dot(p(0.0, -0.3) + vec2(0.0, 0.0));
        }
        Tool::Roots => {
            painter.line_segment([p(-1.0, 0.2), p(1.0, 0.2)], Stroke::new(1.0, color.gamma_multiply(0.6)));
            let curve: Vec<Pos2> = (0..=20).map(|i| {
                let t = -1.0 + i as f32 / 10.0;
                p(t * 0.9, 0.2 - (t * t - 0.4) * 1.1)
            }).collect();
            painter.add(Shape::line(curve, thin));
            let r = (0.4f32).sqrt() * 0.9;
            dot(p(-r, 0.2));
            dot(p(r, 0.2));
        }
        Tool::Ray => {
            painter.line_segment([p(-0.8, 0.6), p(1.0, -0.75)], stroke);
            dot(p(-0.8, 0.6));
            dot(p(0.1, -0.08));
        }
        Tool::Vector => {
            let (a, b) = (p(-0.8, 0.6), p(0.8, -0.6));
            painter.line_segment([a, p(0.55, -0.41)], stroke);
            painter.add(Shape::convex_polygon(vec![b, p(0.35, -0.5), p(0.6, -0.15)], color, Stroke::NONE));
            dot(a);
        }
        Tool::Arc => {
            let pts: Vec<Pos2> = (0..=20).map(|i| {
                let t = -0.2 + 1.9 * i as f32 / 20.0;
                p(-0.5 + 1.2 * t.cos(), 0.6 - 1.2 * t.sin())
            }).collect();
            painter.add(Shape::line(pts, stroke));
            dot(p(-0.5, 0.6));
        }
        Tool::Angle => {
            painter.line_segment([p(-0.8, 0.7), p(0.9, 0.7)], thin);
            painter.line_segment([p(-0.8, 0.7), p(0.5, -0.8)], thin);
            let pts: Vec<Pos2> = (0..=12).map(|i| {
                let t = 0.86 * i as f32 / 12.0;
                p(-0.8 + 0.9 * t.cos(), 0.7 - 0.9 * t.sin())
            }).collect();
            painter.add(Shape::line(pts, stroke));
        }
        Tool::Reflect => {
            painter.line_segment([p(0.0, -1.0), p(0.0, 1.0)], Stroke::new(1.3, color.gamma_multiply(0.7)));
            painter.add(Shape::closed_line(vec![p(-0.85, 0.5), p(-0.25, 0.5), p(-0.25, -0.5)], stroke));
            painter.add(Shape::closed_line(vec![p(0.85, 0.5), p(0.25, 0.5), p(0.25, -0.5)], thin));
        }
        Tool::Rotate => {
            let pts: Vec<Pos2> = (0..=16).map(|i| {
                let t = 0.4 + 4.6 * i as f32 / 16.0;
                p(0.75 * t.cos(), 0.75 * t.sin())
            }).collect();
            let end = *pts.last().unwrap();
            painter.add(Shape::line(pts, stroke));
            painter.add(Shape::convex_polygon(vec![end + vec2(4.0, -5.0), end + vec2(5.0, 4.0), end + vec2(-4.0, 1.0)], color, Stroke::NONE));
            dot(c);
        }
        Tool::Translate => {
            painter.add(Shape::closed_line(vec![p(-0.9, 0.7), p(-0.4, 0.7), p(-0.4, 0.2)], thin));
            painter.add(Shape::closed_line(vec![p(0.3, 0.0), p(0.8, 0.0), p(0.8, -0.5)], stroke));
            painter.line_segment([p(-0.5, 0.3), p(0.2, -0.3)], thin);
            painter.add(Shape::convex_polygon(vec![p(0.3, -0.4), p(0.05, -0.33), p(0.2, -0.12)], color, Stroke::NONE));
        }
        Tool::Extrema => {
            let curve: Vec<Pos2> = (0..=20).map(|i| {
                let t = -1.0 + i as f32 / 10.0;
                p(t * 0.9, -0.55 + t * t * 1.2)
            }).collect();
            painter.add(Shape::line(curve, thin));
            dot(p(0.0, -0.55));
        }
    }
}
