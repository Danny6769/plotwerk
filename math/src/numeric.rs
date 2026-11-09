#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ExtremumKind {
    Min,
    Max,
}
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Extremum {
    pub x: f64,
    pub y: f64,
    pub kind: ExtremumKind,
}
fn golden(f: &dyn Fn(f64) -> f64, mut a: f64, mut b: f64) -> f64 {
    let r = 0.6180339887498949;
    let mut c = b - r * (b - a);
    let mut d = a + r * (b - a);
    let mut fc = f(c);
    let mut fd = f(d);
    for _ in 0..100 {
        if (b - a).abs() < 2e-13 * (1.0 + a.abs() + b.abs()) {
            break;
        }
        if fc < fd {
            b = d;
            d = c;
            fd = fc;
            c = b - r * (b - a);
            fc = f(c);
        } else {
            a = c;
            c = d;
            fc = fd;
            d = a + r * (b - a);
            fd = f(d);
        }
    }
    (a + b) * 0.5
}
fn bisect(f: &dyn Fn(f64) -> f64, mut a: f64, mut b: f64, mut fa: f64) -> Option<f64> {
    let initial = fa.abs().max(f(b).abs());
    let mut best = (a, fa.abs());
    for _ in 0..100 {
        let m = a + (b - a) * 0.5;
        let fm = f(m);
        if !fm.is_finite() {
            return None;
        }
        if fm.abs() < best.1 {
            best = (m, fm.abs());
        }
        if fm == 0.0 {
            return Some(m);
        }
        if fm.is_sign_positive() == fa.is_sign_positive() {
            a = m;
            fa = fm;
        } else {
            b = m;
        }
        if b - a < 1e-13 * (1.0 + m.abs()) {
            break;
        }
    }
    if best.1 <= initial * 1e-7 {
        Some(best.0)
    } else {
        None
    }
}
pub(crate) fn dedup(v: &mut Vec<f64>) {
    v.retain(|x| x.is_finite());
    v.sort_by(f64::total_cmp);
    v.dedup_by(|a, b| (*a - *b).abs() <= 1e-9 * (1.0 + a.abs().max(b.abs())));
}
pub fn find_roots(f: &dyn Fn(f64) -> f64, a: f64, b: f64) -> Vec<f64> {
    if !a.is_finite() || !b.is_finite() {
        return vec![];
    }
    let (a, b) = (a.min(b), a.max(b));
    if a == b {
        return if f(a) == 0.0 { vec![a] } else { vec![] };
    }
    let n = 2400;
    let h = (b - a) / n as f64;
    let ys: Vec<_> = (0..=n).map(|i| f(a + i as f64 * h)).collect();
    let mut out = Vec::new();
    for i in 0..=n {
        let x = a + i as f64 * h;
        let y = ys[i];
        if !y.is_finite() {
            continue;
        }
        if y == 0.0 {
            out.push(x);
        }
        if i < n
            && ys[i + 1].is_finite()
            && y.signum() != ys[i + 1].signum()
            && y != 0.0
            && ys[i + 1] != 0.0
        {
            if let Some(r) = bisect(f, x, x + h, y) {
                out.push(r);
            }
        }
        if i > 0 && i < n && y.abs() <= ys[i - 1].abs() && y.abs() <= ys[i + 1].abs() {
            let r = golden(&|x| f(x).abs(), x - h, x + h);
            let fr = f(r).abs();
            let scale = ys[i - 1].abs().max(ys[i + 1].abs());
            if fr.is_finite() && fr < 1e-10 * (1.0 + scale.min(1.0)) && fr < scale * 1e-5 {
                out.push(r);
            }
        }
    }
    dedup(&mut out);
    out
}
pub fn find_extrema(f: &dyn Fn(f64) -> f64, a: f64, b: f64) -> Vec<Extremum> {
    if !a.is_finite() || !b.is_finite() || a == b {
        return vec![];
    }
    let (a, b) = (a.min(b), a.max(b));
    let n = 2400;
    let h = (b - a) / n as f64;
    let ys: Vec<_> = (0..=n).map(|i| f(a + i as f64 * h)).collect();
    let mut out: Vec<Extremum> = Vec::new();
    for i in 1..n {
        let (l, y, r) = (ys[i - 1], ys[i], ys[i + 1]);
        if !l.is_finite() || !y.is_finite() || !r.is_finite() {
            continue;
        }
        let kind = if y < l && y <= r {
            ExtremumKind::Min
        } else if y > l && y >= r {
            ExtremumKind::Max
        } else {
            continue;
        };
        let sign = if kind == ExtremumKind::Min { 1.0 } else { -1.0 };
        let x0 = a + i as f64 * h;
        let x = golden(&|x| sign * f(x), x0 - h, x0 + h);
        let y = f(x);
        if x > a
            && x < b
            && y.is_finite()
            && y.abs() < 1e12
            && out
                .last()
                .is_none_or(|e| (e.x - x).abs() > 1e-8 * (1.0 + x.abs()))
        {
            out.push(Extremum { x, y, kind });
        }
    }
    out
}
pub fn integrate_numeric(f: &dyn Fn(f64) -> f64, a: f64, b: f64) -> f64 {
    if a == b {
        return 0.0;
    }
    if !a.is_finite() || !b.is_finite() {
        return f64::NAN;
    }
    if a > b {
        return -integrate_numeric(f, b, a);
    }
    struct Quad<'a> {
        f: &'a dyn Fn(f64) -> f64,
        budget: usize,
    }
    impl Quad<'_> {
        fn recurse(
            &mut self,
            a: f64,
            b: f64,
            ys: [f64; 3],
            whole: f64,
            tol: f64,
            depth: u8,
        ) -> f64 {
            let [fa, fm, fb] = ys;
            let m = a + (b - a) / 2.0;
            let fl = (self.f)(a + (m - a) / 2.0);
            let fr = (self.f)(m + (b - m) / 2.0);
            if !fl.is_finite() || !fr.is_finite() {
                return f64::NAN;
            }
            let l = (m - a) * (fa + 4.0 * fl + fm) / 6.0;
            let r = (b - m) * (fm + 4.0 * fr + fb) / 6.0;
            let delta = l + r - whole;
            if delta.abs() <= 15.0 * tol {
                return l + r + delta / 15.0;
            }
            if depth == 0 || self.budget == 0 {
                return f64::NAN;
            }
            self.budget -= 1;
            self.recurse(a, m, [fa, fl, fm], l, tol / 2.0, depth - 1)
                + self.recurse(m, b, [fm, fr, fb], r, tol / 2.0, depth - 1)
        }
    }
    let mut q = Quad { f, budget: 100_000 };
    let n = 16;
    let mut total = 0.0;
    for i in 0..n {
        let l = a + (b - a) * i as f64 / n as f64;
        let r = a + (b - a) * (i + 1) as f64 / n as f64;
        let ys = [f(l), f((l + r) / 2.0), f(r)];
        if ys.iter().any(|y| !y.is_finite()) {
            return f64::NAN;
        }
        let whole = (r - l) * (ys[0] + 4.0 * ys[1] + ys[2]) / 6.0;
        total += q.recurse(l, r, ys, whole, 1e-10 / n as f64, 24);
    }
    total
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn polynomial_roots() {
        let r = find_roots(&|x| x * x * x - x, -2.0, 2.0);
        assert_eq!(r.len(), 3);
        for (x, y) in r.iter().zip([-1.0, 0.0, 1.0]) {
            assert!((x - y).abs() < 1e-10);
        }
    }
    #[test]
    fn touching() {
        let r = find_roots(&|x| (x - 0.123456789).powi(2), -2.0, 2.0);
        assert_eq!(r.len(), 1);
        assert!((r[0] - 0.123456789).abs() < 1e-9);
    }
    #[test]
    fn poles() {
        assert!(find_roots(&|x| 1.0 / x, -1.0, 1.0).is_empty());
        assert!(find_roots(&|x| 1.0 / (x - 0.12345), -1.0, 1.0).is_empty());
    }
    #[test]
    fn sine() {
        let p = std::f64::consts::PI;
        let r = find_roots(&f64::sin, -4.0, 4.0);
        assert_eq!(r.len(), 3);
        assert!((r[0] + p).abs() < 1e-10);
        let e = find_extrema(&f64::sin, -p, p);
        assert_eq!(e.len(), 2);
        assert!((e[0].x + p / 2.0).abs() < 1e-7);
        assert_eq!(e[0].kind, ExtremumKind::Min);
        assert_eq!(e[1].kind, ExtremumKind::Max);
    }
    #[test]
    fn quadrature() {
        for (a, b, want) in [
            (0.0, std::f64::consts::PI, 2.0),
            (std::f64::consts::PI, 0.0, -2.0),
            (1.0, 1.0, 0.0),
        ] {
            assert!((integrate_numeric(&f64::sin, a, b) - want).abs() < 1e-10);
        }
        assert!(
            (integrate_numeric(&f64::exp, 0.0, 1.0) - (std::f64::consts::E - 1.0)).abs() < 1e-10
        );
        assert!(integrate_numeric(&|x| 1.0 / x, -1.0, 1.0).is_nan());
    }
    #[test]
    fn extrema_polynomial() {
        let e = find_extrema(&|x| x * x * x - x, -2.0, 2.0);
        assert_eq!(e.len(), 2);
        assert!((e[0].x + 1.0 / 3.0f64.sqrt()).abs() < 1e-7);
        assert!(find_extrema(&f64::exp, -1.0, 1.0).is_empty());
    }
    #[test]
    fn scaling_and_boundaries() {
        for scale in [1e-15, 1.0, 1e15] {
            let r = find_roots(&|x| scale * (x - 0.123456789), -1.0, 1.0);
            assert_eq!(r.len(), 1);
            assert!((r[0] - 0.123456789).abs() < 1e-10);
        }
        assert_eq!(find_roots(&|x| x * (x - 1.0), 0.0, 1.0), vec![0.0, 1.0]);
        assert!(find_extrema(&|x| x * x, 0.0, 1.0).is_empty());
    }
}
