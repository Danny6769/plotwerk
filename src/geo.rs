use crate::expr_util::{NoCtx, add, mul, num, pow, sub, substitute_map, var};
use plotwerk_math::{self as m, Compiled, Expr, Solution};
use std::collections::HashMap;
use std::f64::consts::TAU;

pub type Pt = [f64; 2];

#[derive(Clone, Debug)]
pub enum Geo {
    Number(f64),
    Point(Pt),
    Points(Vec<Pt>),
    Function { expr: Expr, f: Compiled, dom: Option<(f64, f64)> },
    Line { a: f64, b: f64, c: f64 },
    Segment(Pt, Pt),
    Ray(Pt, Pt),
    Vector(Pt, Pt),
    Circle { c: Pt, r: f64 },
    Arc { c: Pt, r: f64, a0: f64, sweep: f64, sector: bool },
    Angle { v: Pt, a0: f64, sweep: f64 },
    Polygon(Vec<Pt>),
    Implicit { expr: Expr, f: Compiled },
    Area { f: Compiled, a: f64, b: f64, value: f64 },
    Region { cond: Compiled, bounds: Vec<(Compiled, bool)> },
}

impl Geo {
    pub fn kind_name(&self) -> &'static str {
        match self {
            Geo::Number(_) => "Zahl",
            Geo::Point(_) => "Punkt",
            Geo::Points(_) => "Punkte",
            Geo::Function { .. } => "Funktion",
            Geo::Line { .. } => "Gerade",
            Geo::Segment(..) => "Strecke",
            Geo::Ray(..) => "Halbgerade",
            Geo::Vector(..) => "Vektor",
            Geo::Circle { .. } => "Kreis",
            Geo::Arc { .. } => "Kreisbogen",
            Geo::Angle { .. } => "Winkel",
            Geo::Polygon(_) => "Vieleck",
            Geo::Implicit { .. } => "Kurve",
            Geo::Area { .. } => "Integral",
            Geo::Region { .. } => "Ungleichung",
        }
    }

    pub fn is_point_like(&self) -> bool {
        matches!(self, Geo::Point(_) | Geo::Points(_))
    }

    pub fn is_curve(&self) -> bool {
        !matches!(self, Geo::Number(_) | Geo::Point(_) | Geo::Points(_) | Geo::Angle { .. } | Geo::Area { .. } | Geo::Region { .. })
    }

    pub fn as_line(&self) -> Option<(f64, f64, f64)> {
        match self {
            Geo::Line { a, b, c } => Some((*a, *b, *c)),
            Geo::Segment(p, q) | Geo::Ray(p, q) | Geo::Vector(p, q) => Some(line_through(*p, *q)),
            _ => None,
        }
    }

    pub fn eval_fn(&self, x: f64) -> f64 {
        match self {
            Geo::Function { f, dom, .. } => {
                if let Some((a, b)) = dom {
                    if x < *a || x > *b {
                        return f64::NAN;
                    }
                }
                f.eval(&[x])
            }
            Geo::Area { f, .. } => f.eval(&[x]),
            _ => f64::NAN,
        }
    }
}

pub fn line_through(p: Pt, q: Pt) -> (f64, f64, f64) {
    let a = -(q[1] - p[1]);
    let b = q[0] - p[0];
    (a, b, a * p[0] + b * p[1])
}

pub fn norm_angle(a: f64) -> f64 {
    a.rem_euclid(TAU)
}

pub fn angle_of(c: Pt, p: Pt) -> f64 {
    norm_angle((p[1] - c[1]).atan2(p[0] - c[0]))
}

pub fn solution_value(s: &Solution) -> Option<f64> {
    let v = match s {
        Solution::Exact(e) => m::eval(e, &NoCtx).ok()?,
        Solution::Approx(v) => *v,
    };
    v.is_finite().then_some(v)
}

pub fn solve_zero(expr: &Expr, v: &str) -> Vec<f64> {
    let mut out: Vec<f64> = match m::solve(expr, &num(0.0), v) {
        Ok(sols) if sols.iter().all(|s| !matches!(s, Solution::Exact(e) if crate::expr_util::contains_var(e, "k"))) => {
            sols.iter().filter_map(solution_value).collect()
        }
        _ => match m::compile(expr, &[v], &NoCtx) {
            Ok(c) => m::find_roots(&|t| c.eval(&[t]), -100.0, 100.0),
            Err(_) => Vec::new(),
        },
    };
    out.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    out.dedup_by(|a, b| (*a - *b).abs() < 1e-9);
    out
}

fn implicit_expr(g: &Geo) -> Option<Expr> {
    match g {
        Geo::Line { a, b, c } => Some(sub(add(mul(num(*a), var("x")), mul(num(*b), var("y"))), num(*c))),
        Geo::Circle { c, r } => Some(sub(
            add(pow(sub(var("x"), num(c[0])), num(2.0)), pow(sub(var("y"), num(c[1])), num(2.0))),
            num(r * r),
        )),
        Geo::Implicit { expr, .. } => Some(expr.clone()),
        Geo::Function { expr, .. } => Some(sub(var("y"), expr.clone())),
        _ => None,
    }
}

#[derive(Clone, Copy)]
enum Filter {
    None,
    Seg(Pt, Pt),
    Ray(Pt, Pt),
    Arc(Pt, f64, f64),
    Dom(f64, f64),
}

struct Piece {
    geo: Geo,
    filter: Filter,
}

fn pieces(g: &Geo) -> Vec<Piece> {
    let line = |p: Pt, q: Pt| {
        let (a, b, c) = line_through(p, q);
        Geo::Line { a, b, c }
    };
    match g {
        Geo::Segment(p, q) | Geo::Vector(p, q) => vec![Piece { geo: line(*p, *q), filter: Filter::Seg(*p, *q) }],
        Geo::Ray(p, q) => vec![Piece { geo: line(*p, *q), filter: Filter::Ray(*p, *q) }],
        Geo::Polygon(pts) => (0..pts.len())
            .map(|i| {
                let p = pts[i];
                let q = pts[(i + 1) % pts.len()];
                Piece { geo: line(p, q), filter: Filter::Seg(p, q) }
            })
            .collect(),
        Geo::Arc { c, r, a0, sweep, .. } => vec![Piece { geo: Geo::Circle { c: *c, r: *r }, filter: Filter::Arc(*c, *a0, *sweep) }],
        Geo::Function { expr, f, dom: Some((a, b)) } => {
            vec![Piece { geo: Geo::Function { expr: expr.clone(), f: f.clone(), dom: None }, filter: Filter::Dom(*a, *b) }]
        }
        other => vec![Piece { geo: other.clone(), filter: Filter::None }],
    }
}

fn passes(p: Pt, f: Filter) -> bool {
    let t_of = |a: Pt, b: Pt| {
        let d = [b[0] - a[0], b[1] - a[1]];
        let len2 = d[0] * d[0] + d[1] * d[1];
        if len2 == 0.0 { f64::NAN } else { ((p[0] - a[0]) * d[0] + (p[1] - a[1]) * d[1]) / len2 }
    };
    match f {
        Filter::None => true,
        Filter::Seg(a, b) => (-1e-9..=1.0 + 1e-9).contains(&t_of(a, b)),
        Filter::Ray(a, b) => t_of(a, b) >= -1e-9,
        Filter::Arc(c, a0, sweep) => norm_angle(angle_of(c, p) - a0) <= sweep + 1e-9,
        Filter::Dom(a, b) => p[0] >= a - 1e-9 && p[0] <= b + 1e-9,
    }
}

pub fn intersect(g1: &Geo, g2: &Geo) -> Result<Vec<Pt>, String> {
    let mut out: Vec<Pt> = Vec::new();
    for p1 in pieces(g1) {
        for p2 in pieces(g2) {
            for p in intersect_basic(&p1.geo, &p2.geo)? {
                if passes(p, p1.filter) && passes(p, p2.filter) && p[0].is_finite() && p[1].is_finite() {
                    if !out.iter().any(|q| (q[0] - p[0]).abs() < 1e-9 && (q[1] - p[1]).abs() < 1e-9) {
                        out.push(p);
                    }
                }
            }
        }
    }
    out.sort_by(|a, b| a[0].partial_cmp(&b[0]).unwrap_or(std::cmp::Ordering::Equal));
    Ok(out)
}

fn intersect_basic(g1: &Geo, g2: &Geo) -> Result<Vec<Pt>, String> {
    use Geo::*;
    match (g1, g2) {
        (Line { a: a1, b: b1, c: c1 }, Line { a: a2, b: b2, c: c2 }) => {
            let det = a1 * b2 - a2 * b1;
            if det.abs() < 1e-12 {
                return Ok(vec![]);
            }
            Ok(vec![[(c1 * b2 - c2 * b1) / det, (a1 * c2 - a2 * c1) / det]])
        }
        (Line { a, b, c }, Circle { c: m, r }) | (Circle { c: m, r }, Line { a, b, c }) => {
            let n = (a * a + b * b).sqrt();
            if n == 0.0 {
                return Ok(vec![]);
            }
            let d = (a * m[0] + b * m[1] - c) / n;
            if d.abs() > r + 1e-12 {
                return Ok(vec![]);
            }
            let foot = [m[0] - d * a / n, m[1] - d * b / n];
            let h = (r * r - d * d).max(0.0).sqrt();
            let dir = [-b / n, a / n];
            if h < 1e-12 {
                return Ok(vec![foot]);
            }
            Ok(vec![[foot[0] - h * dir[0], foot[1] - h * dir[1]], [foot[0] + h * dir[0], foot[1] + h * dir[1]]])
        }
        (Circle { c: c1, r: r1 }, Circle { c: c2, r: r2 }) => {
            let dx = c2[0] - c1[0];
            let dy = c2[1] - c1[1];
            let d = (dx * dx + dy * dy).sqrt();
            if d == 0.0 || d > r1 + r2 + 1e-12 || d < (r1 - r2).abs() - 1e-12 {
                return Ok(vec![]);
            }
            let a = (r1 * r1 - r2 * r2 + d * d) / (2.0 * d);
            let h = (r1 * r1 - a * a).max(0.0).sqrt();
            let mx = c1[0] + a * dx / d;
            let my = c1[1] + a * dy / d;
            if h < 1e-12 {
                return Ok(vec![[mx, my]]);
            }
            Ok(vec![[mx + h * dy / d, my - h * dx / d], [mx - h * dy / d, my + h * dx / d]])
        }
        (Function { expr, f, .. }, other) | (other, Function { expr, f, .. }) => {
            let imp = implicit_expr(other).ok_or("Schnitt nicht unterstützt")?;
            let mut map = HashMap::new();
            map.insert("y".to_string(), expr.clone());
            let h = substitute_map(&imp, &map);
            Ok(solve_zero(&h, "x").into_iter().map(|x| [x, f.eval(&[x])]).collect())
        }
        (Line { a, b, c }, Implicit { expr, .. }) | (Implicit { expr, .. }, Line { a, b, c }) => {
            let mut map = HashMap::new();
            if b.abs() > 1e-12 {
                map.insert("y".to_string(), crate::expr_util::div(sub(num(*c), mul(num(*a), var("x"))), num(*b)));
                let h = substitute_map(expr, &map);
                Ok(solve_zero(&h, "x").into_iter().map(|x| [x, (c - a * x) / b]).collect())
            } else {
                let x0 = c / a;
                map.insert("x".to_string(), num(x0));
                let h = substitute_map(expr, &map);
                Ok(solve_zero(&h, "y").into_iter().map(|y| [x0, y]).collect())
            }
        }
        (Circle { c, r }, Implicit { f, .. }) | (Implicit { f, .. }, Circle { c, r }) => {
            let g = |t: f64| f.eval(&[c[0] + r * t.cos(), c[1] + r * t.sin()]);
            let mut ts = m::find_roots(&g, 0.0, TAU);
            ts.dedup_by(|a, b| (*a - *b).abs() < 1e-7 || (*a - *b - TAU).abs() < 1e-7);
            Ok(ts.into_iter().map(|t| [c[0] + r * t.cos(), c[1] + r * t.sin()]).collect())
        }
        _ => Err("Schnitt nicht unterstützt".into()),
    }
}

pub fn polygon_area(pts: &[Pt]) -> f64 {
    let n = pts.len();
    let mut s = 0.0;
    for i in 0..n {
        let p = pts[i];
        let q = pts[(i + 1) % n];
        s += p[0] * q[1] - q[0] * p[1];
    }
    (s / 2.0).abs()
}

pub fn dist(p: Pt, q: Pt) -> f64 {
    ((p[0] - q[0]).powi(2) + (p[1] - q[1]).powi(2)).sqrt()
}

#[derive(Clone, Copy, Debug)]
pub struct Affine {
    pub a: f64,
    pub b: f64,
    pub c: f64,
    pub d: f64,
    pub e: f64,
    pub f: f64,
}

impl Affine {
    pub fn apply(&self, p: Pt) -> Pt {
        [self.a * p[0] + self.b * p[1] + self.e, self.c * p[0] + self.d * p[1] + self.f]
    }

    pub fn det(&self) -> f64 {
        self.a * self.d - self.b * self.c
    }

    pub fn inverse(&self) -> Option<Affine> {
        let det = self.det();
        if det.abs() < 1e-14 {
            return None;
        }
        let (a, b, c, d) = (self.d / det, -self.b / det, -self.c / det, self.a / det);
        Some(Affine { a, b, c, d, e: -(a * self.e + b * self.f), f: -(c * self.e + d * self.f) })
    }

    pub fn translate(v: Pt) -> Affine {
        Affine { a: 1.0, b: 0.0, c: 0.0, d: 1.0, e: v[0], f: v[1] }
    }

    pub fn rotate(angle: f64, center: Pt) -> Affine {
        let (s, c) = angle.sin_cos();
        Affine { a: c, b: -s, c: s, d: c, e: center[0] - c * center[0] + s * center[1], f: center[1] - s * center[0] - c * center[1] }
    }

    pub fn dilate(k: f64, center: Pt) -> Affine {
        Affine { a: k, b: 0.0, c: 0.0, d: k, e: center[0] * (1.0 - k), f: center[1] * (1.0 - k) }
    }

    pub fn reflect_point(p: Pt) -> Affine {
        Affine::dilate(-1.0, p)
    }

    pub fn reflect_line(la: f64, lb: f64, lc: f64) -> Affine {
        let n2 = la * la + lb * lb;
        let (a, b, d) = (1.0 - 2.0 * la * la / n2, -2.0 * la * lb / n2, 1.0 - 2.0 * lb * lb / n2);
        Affine { a, b, c: b, d, e: 2.0 * la * lc / n2, f: 2.0 * lb * lc / n2 }
    }

    pub fn is_translation(&self) -> bool {
        (self.a - 1.0).abs() < 1e-14 && self.b.abs() < 1e-14 && self.c.abs() < 1e-14 && (self.d - 1.0).abs() < 1e-14
    }
}

fn points_on_line(a: f64, b: f64, c: f64) -> (Pt, Pt) {
    let n2 = a * a + b * b;
    let p0 = [a * c / n2, b * c / n2];
    (p0, [p0[0] - b, p0[1] + a])
}

pub fn transform(g: &Geo, t: &Affine) -> Result<Geo, String> {
    let ap = |p: &Pt| t.apply(*p);
    let scale = t.det().abs().sqrt();
    let arc_map = |c: &Pt, r: &f64, a0: &f64, sweep: &f64| {
        let start = [c[0] + r * a0.cos(), c[1] + r * a0.sin()];
        let end = [c[0] + r * (a0 + sweep).cos(), c[1] + r * (a0 + sweep).sin()];
        let nc = ap(c);
        let (ns, ne) = (ap(&start), ap(&end));
        let (from, _) = if t.det() < 0.0 { (ne, ns) } else { (ns, ne) };
        (nc, r * scale, angle_of(nc, from), *sweep)
    };
    Ok(match g {
        Geo::Point(p) => Geo::Point(ap(p)),
        Geo::Points(ps) => Geo::Points(ps.iter().map(ap).collect()),
        Geo::Segment(p, q) => Geo::Segment(ap(p), ap(q)),
        Geo::Ray(p, q) => Geo::Ray(ap(p), ap(q)),
        Geo::Vector(p, q) => Geo::Vector(ap(p), ap(q)),
        Geo::Polygon(ps) => Geo::Polygon(ps.iter().map(ap).collect()),
        Geo::Line { a, b, c } => {
            let (p, q) = points_on_line(*a, *b, *c);
            let (a, b, c) = line_through(ap(&p), ap(&q));
            Geo::Line { a, b, c }
        }
        Geo::Circle { c, r } => Geo::Circle { c: ap(c), r: r * scale },
        Geo::Arc { c, r, a0, sweep, sector } => {
            let (nc, nr, na0, ns) = arc_map(c, r, a0, sweep);
            Geo::Arc { c: nc, r: nr, a0: na0, sweep: ns, sector: *sector }
        }
        Geo::Angle { v, a0, sweep } => {
            let (nc, _, na0, ns) = arc_map(v, &1.0, a0, sweep);
            Geo::Angle { v: nc, a0: na0, sweep: ns }
        }
        Geo::Function { expr, dom, .. } if t.is_translation() => {
            let mut map = HashMap::new();
            map.insert("x".to_string(), sub(var("x"), num(t.e)));
            let e = m::simplify(&add(substitute_map(expr, &map), num(t.f)));
            let f = m::compile(&e, &["x"], &NoCtx).map_err(|e| e.0)?;
            Geo::Function { expr: e, f, dom: dom.map(|(a, b)| (a + t.e, b + t.e)) }
        }
        Geo::Function { .. } | Geo::Implicit { .. } => {
            let imp = match g {
                Geo::Function { expr, .. } => sub(var("y"), expr.clone()),
                Geo::Implicit { expr, .. } => expr.clone(),
                _ => unreachable!(),
            };
            let inv = t.inverse().ok_or("Abbildung nicht umkehrbar")?;
            let mut map = HashMap::new();
            map.insert("x".to_string(), add(add(mul(num(inv.a), var("x")), mul(num(inv.b), var("y"))), num(inv.e)));
            map.insert("y".to_string(), add(add(mul(num(inv.c), var("x")), mul(num(inv.d), var("y"))), num(inv.f)));
            let e = m::simplify(&substitute_map(&imp, &map));
            let f = m::compile(&e, &["x", "y"], &NoCtx).map_err(|e| e.0)?;
            Geo::Implicit { expr: e, f }
        }
        other => return Err(format!("{} kann nicht abgebildet werden", other.kind_name())),
    })
}

pub fn point_on(parent: &Geo, t: f64) -> Option<Pt> {
    match parent {
        Geo::Function { .. } => {
            let y = parent.eval_fn(t);
            y.is_finite().then_some([t, y])
        }
        Geo::Line { a, b, c } => {
            let n = (a * a + b * b).sqrt();
            let (p0, _) = points_on_line(*a, *b, *c);
            Some([p0[0] - b / n * t, p0[1] + a / n * t])
        }
        Geo::Segment(p, q) | Geo::Vector(p, q) => {
            let t = t.clamp(0.0, 1.0);
            Some([p[0] + (q[0] - p[0]) * t, p[1] + (q[1] - p[1]) * t])
        }
        Geo::Ray(p, q) => {
            let t = t.max(0.0);
            Some([p[0] + (q[0] - p[0]) * t, p[1] + (q[1] - p[1]) * t])
        }
        Geo::Circle { c, r } => Some([c[0] + r * t.cos(), c[1] + r * t.sin()]),
        Geo::Arc { c, r, a0, sweep, .. } => {
            let a = a0 + t.clamp(0.0, *sweep);
            Some([c[0] + r * a.cos(), c[1] + r * a.sin()])
        }
        Geo::Polygon(pts) if !pts.is_empty() => {
            let n = pts.len();
            let t = t.rem_euclid(n as f64);
            let i = t.floor() as usize % n;
            let (p, q) = (pts[i], pts[(i + 1) % n]);
            let f = t - t.floor();
            Some([p[0] + (q[0] - p[0]) * f, p[1] + (q[1] - p[1]) * f])
        }
        _ => None,
    }
}

pub fn project_param(parent: &Geo, w: Pt) -> Option<f64> {
    let seg_t = |p: &Pt, q: &Pt| {
        let d = [q[0] - p[0], q[1] - p[1]];
        let len2 = d[0] * d[0] + d[1] * d[1];
        if len2 == 0.0 { 0.0 } else { ((w[0] - p[0]) * d[0] + (w[1] - p[1]) * d[1]) / len2 }
    };
    match parent {
        Geo::Function { dom, .. } => Some(match dom {
            Some((a, b)) => w[0].clamp(*a, *b),
            None => w[0],
        }),
        Geo::Line { a, b, c } => {
            let n = (a * a + b * b).sqrt();
            let (p0, _) = points_on_line(*a, *b, *c);
            Some(((w[0] - p0[0]) * -b + (w[1] - p0[1]) * a) / n)
        }
        Geo::Segment(p, q) | Geo::Vector(p, q) => Some(seg_t(p, q).clamp(0.0, 1.0)),
        Geo::Ray(p, q) => Some(seg_t(p, q).max(0.0)),
        Geo::Circle { c, .. } => Some(angle_of(*c, w)),
        Geo::Arc { c, a0, sweep, .. } => {
            let rel = norm_angle(angle_of(*c, w) - a0);
            Some(if rel <= *sweep { rel } else if rel - sweep < TAU - rel { *sweep } else { 0.0 })
        }
        Geo::Polygon(pts) if !pts.is_empty() => {
            let n = pts.len();
            let mut best = (f64::INFINITY, 0.0);
            for i in 0..n {
                let (p, q) = (pts[i], pts[(i + 1) % n]);
                let t = seg_t(&p, &q).clamp(0.0, 1.0);
                let pt = [p[0] + (q[0] - p[0]) * t, p[1] + (q[1] - p[1]) * t];
                let d = dist(pt, w);
                if d < best.0 {
                    best = (d, i as f64 + t.min(0.999999));
                }
            }
            Some(best.1)
        }
        _ => None,
    }
}
