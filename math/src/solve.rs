use super::ast::{Constant, Expr, contains};
use super::eval::{Empty, compile, eval};
use super::simplify::{
    add, call, div, expand, int, mul, neg, polynomial, pow, rational_root, simplify, sub,
    synthetic, trim, zero,
};
#[derive(Clone, Debug, PartialEq)]
pub enum Solution {
    Exact(Expr),
    Approx(f64),
}
fn value(e: &Expr) -> Option<f64> {
    eval(e, &Empty).ok().filter(|x| x.is_finite())
}
fn quadratic(p: &[Expr]) -> Vec<Expr> {
    if p.len() == 1 {
        return vec![];
    }
    if p.len() == 2 {
        return vec![div(neg(p[0].clone()), p[1].clone())];
    }
    let (c, b, a) = (p[0].clone(), p[1].clone(), p[2].clone());
    if zero(&b) {
        let square = div(neg(c), a);
        if value(&square).is_some_and(|x| x < 0.0) {
            return vec![];
        }
        let root = call("sqrt", square);
        return if zero(&root) {
            vec![root]
        } else {
            vec![neg(root.clone()), root]
        };
    }
    let disc = sub(pow(b.clone(), int(2)), mul(int(4), mul(a.clone(), c)));
    let d = value(&disc);
    if d.is_some_and(|d| d < 0.0) {
        return vec![];
    }
    let bottom = mul(int(2), a);
    if d == Some(0.0) || zero(&disc) {
        return vec![div(neg(b), bottom)];
    }
    let root = call("sqrt", disc);
    vec![
        div(sub(neg(b.clone()), root.clone()), bottom.clone()),
        div(add(neg(b), root), bottom),
    ]
}
fn isolate(a: &Expr, b: &Expr, v: &str, depth: usize) -> Option<Vec<Expr>> {
    if depth > 64 || contains(b, v) {
        return None;
    }
    if let Expr::Var(n) = a {
        if n == v {
            return Some(vec![simplify(b)]);
        }
    }
    let recurse = |a: &Expr, b: Expr| isolate(a, &simplify(&b), v, depth + 1);
    use Expr::*;
    match a {
        Neg(x) => recurse(x, neg(b.clone())),
        Add(x, y) if contains(x, v) && !contains(y, v) => recurse(x, sub(b.clone(), *y.clone())),
        Add(x, y) if !contains(x, v) && contains(y, v) => recurse(y, sub(b.clone(), *x.clone())),
        Sub(x, y) if contains(x, v) && !contains(y, v) => recurse(x, add(b.clone(), *y.clone())),
        Sub(x, y) if !contains(x, v) && contains(y, v) => recurse(y, sub(*x.clone(), b.clone())),
        Mul(x, y) if contains(x, v) && !contains(y, v) && !zero(y) => {
            recurse(x, div(b.clone(), *y.clone()))
        }
        Mul(x, y) if !contains(x, v) && contains(y, v) && !zero(x) => {
            recurse(y, div(b.clone(), *x.clone()))
        }
        Div(x, y) if contains(x, v) && !contains(y, v) => recurse(x, mul(b.clone(), *y.clone())),
        Div(x, y) if !contains(x, v) && contains(y, v) => {
            if zero(b) {
                Some(vec![])
            } else {
                recurse(y, div(*x.clone(), b.clone()))
            }
        }
        Pow(x, y) if !contains(x, v) && contains(y, v) => {
            let base = value(x)?;
            let target = value(b)?;
            if base <= 0.0 || base == 1.0 {
                return None;
            }
            if target <= 0.0 {
                return Some(vec![]);
            }
            let n = target.ln() / base.ln();
            let rhs = if (n - n.round()).abs() < 1e-12 && n.abs() < i64::MAX as f64 {
                int(n.round() as i64)
            } else if **x == Const(Constant::E) {
                call("ln", b.clone())
            } else {
                div(call("ln", b.clone()), call("ln", *x.clone()))
            };
            recurse(y, rhs)
        }
        Pow(x, y) if contains(x, v) && !contains(y, v) => {
            let n = value(y)?;
            if n == 0.0 {
                return None;
            }
            let t = value(b)?;
            if n.fract() == 0.0 && n as i64 % 2 == 0 {
                if t < 0.0 {
                    return Some(vec![]);
                }
                let r = pow(b.clone(), div(int(1), *y.clone()));
                let mut out = recurse(x, r.clone())?;
                if t != 0.0 {
                    out.extend(recurse(x, neg(r))?);
                }
                Some(out)
            } else if n.fract() == 0.0 && t < 0.0 {
                recurse(x, neg(pow(neg(b.clone()), div(int(1), *y.clone()))))
            } else {
                recurse(x, pow(b.clone(), div(int(1), *y.clone())))
            }
        }
        Call(n, args) if n == "log" && args.len() == 2 => {
            let (base, x) = (&args[0], &args[1]);
            if !contains(base, v) && contains(x, v) {
                if value(base).is_some_and(|x| x <= 0.0 || x == 1.0) {
                    return Some(vec![]);
                }
                recurse(x, pow(base.clone(), b.clone()))
            } else if contains(base, v) && !contains(x, v) {
                if zero(b) {
                    None
                } else {
                    recurse(base, pow(x.clone(), div(int(1), b.clone())))
                }
            } else {
                None
            }
        }
        Call(n, args) if n == "nroot" && args.len() == 2 && !contains(&args[1], v) => {
            if value(&args[1]).is_some_and(|x| x.fract() == 0.0 && x as i64 % 2 == 0)
                && value(b).is_some_and(|x| x < 0.0)
            {
                Some(vec![])
            } else {
                recurse(&args[0], pow(b.clone(), args[1].clone()))
            }
        }
        Call(n, args) if n == "abs" && args.len() == 1 => {
            if value(b).is_some_and(|x| x < 0.0) {
                return Some(vec![]);
            }
            let mut out = recurse(&args[0], b.clone())?;
            if !zero(b) {
                out.extend(recurse(&args[0], neg(b.clone()))?);
            }
            Some(out)
        }
        Call(n, args) if args.len() == 1 => {
            let u = &args[0];
            let target = value(b);
            if matches!(n.as_str(), "sin" | "cos" | "tan") {
                if n != "tan" && target.is_some_and(|x| x.abs() > 1.0) {
                    return Some(vec![]);
                }
                let coefficients = polynomial(u, v)?;
                if coefficients.len() != 2 || zero(&coefficients[1]) {
                    return None;
                }
                let inverse = match n.as_str() {
                    "sin" => "asin",
                    "cos" => "acos",
                    _ => "atan",
                };
                let angle = super::simplify::inverse_trig(inverse, b, true)
                    .unwrap_or_else(|| call(inverse, b.clone()));
                let pi = Const(Constant::Pi);
                let period = mul(
                    mul(int(if n == "tan" { 1 } else { 2 }), Var("k".into())),
                    pi.clone(),
                );
                let mut offsets = vec![angle.clone()];
                if n == "sin" && !target.is_some_and(|x| x.abs() == 1.0) {
                    offsets.push(sub(pi, angle));
                } else if n == "cos" && !target.is_some_and(|x| x.abs() == 1.0) {
                    offsets.push(neg(angle));
                }
                let mut out = Vec::new();
                for offset in offsets {
                    out.extend(recurse(u, add(offset, period.clone()))?);
                }
                return Some(out);
            }
            let rhs = match n.as_str() {
                "sin" | "cos" if target.is_some_and(|x| x.abs() > 1.0) => return Some(vec![]),
                "cosh" if target.is_some_and(|x| x < 1.0) => return Some(vec![]),
                "ln" => pow(Const(Constant::E), b.clone()),
                "log" | "lg" => pow(int(10), b.clone()),
                "ld" => pow(int(2), b.clone()),
                "exp" => {
                    if target.is_some_and(|x| x <= 0.0) {
                        return Some(vec![]);
                    }
                    call("ln", b.clone())
                }
                "sqrt" => {
                    if target.is_some_and(|x| x < 0.0) {
                        return Some(vec![]);
                    }
                    pow(b.clone(), int(2))
                }
                "cbrt" => pow(b.clone(), int(3)),
                "sinh" => call("asinh", b.clone()),
                "asinh" => call("sinh", b.clone()),
                "tanh" => {
                    if target.is_some_and(|x| x.abs() >= 1.0) {
                        return Some(vec![]);
                    }
                    call("atanh", b.clone())
                }
                "atanh" => call("tanh", b.clone()),
                "acosh" => {
                    if target.is_some_and(|x| x < 0.0) {
                        return Some(vec![]);
                    }
                    call("cosh", b.clone())
                }
                "asin" => {
                    if target.is_some_and(|x| x.abs() > std::f64::consts::FRAC_PI_2) {
                        return Some(vec![]);
                    }
                    call("sin", b.clone())
                }
                "acos" => {
                    if target.is_some_and(|x| !(0.0..=std::f64::consts::PI).contains(&x)) {
                        return Some(vec![]);
                    }
                    call("cos", b.clone())
                }
                "atan" => {
                    if target.is_some_and(|x| x.abs() >= std::f64::consts::FRAC_PI_2) {
                        return Some(vec![]);
                    }
                    call("tan", b.clone())
                }
                _ => return None,
            };
            recurse(u, rhs)
        }
        _ => None,
    }
}
pub fn solve(lhs: &Expr, rhs: &Expr, var: &str) -> Result<Vec<Solution>, String> {
    let e = expand(&sub(simplify(lhs), simplify(rhs)));
    if !contains(&e, var) {
        return match value(&e) {
            Some(x) if x == 0.0 => Err("Alle reellen Zahlen sind Lösungen".into()),
            Some(_) => Ok(vec![]),
            None => Err("Keine Lösung gefunden".into()),
        };
    }
    let mut out = Vec::new();
    if let Some(mut p) = polynomial(&e, var) {
        trim(&mut p);
        while p.len() > 3 {
            if let Some(r) = rational_root(&p) {
                p = synthetic(&p, &r).0;
                trim(&mut p);
                out.push(Solution::Exact(r));
            } else {
                break;
            }
        }
        if p.len() <= 3 {
            out.extend(quadratic(&p).into_iter().map(Solution::Exact));
        } else {
            let c =
                compile(&super::simplify::from_poly(&p, var), &[var], &Empty).map_err(|e| e.0)?;
            let lead = value(p.last().unwrap()).unwrap_or(1.0).abs();
            let bound = 1.0
                + p[..p.len() - 1]
                    .iter()
                    .filter_map(value)
                    .map(|c| c.abs() / lead)
                    .fold(0.0, f64::max);
            let bound = bound.min(1e6);
            out.extend(
                super::numeric::find_roots(&|x| c.eval(&[x]), -bound, bound)
                    .into_iter()
                    .map(Solution::Approx),
            );
        }
    } else {
        let (l, r) = (simplify(lhs), simplify(rhs));
        let exact = if !contains(&r, var) {
            isolate(&l, &r, var, 0)
        } else if !contains(&l, var) {
            isolate(&r, &l, var, 0)
        } else {
            None
        };
        if let Some(roots) = exact {
            let c = compile(&e, &[var], &Empty).ok();
            for root in roots {
                let root = if contains(&root, "k") {
                    if let Some(p) = polynomial(&root, "k") {
                        super::simplify::from_poly(&p, "k")
                    } else {
                        root
                    }
                } else {
                    root
                };
                if let Some(x) = value(&root) {
                    if c.as_ref().is_none_or(|c| {
                        let residual = c.eval(&[x]);
                        residual.is_finite() && residual.abs() < 1e-7 * (1.0 + x.abs())
                    }) {
                        out.push(Solution::Exact(simplify(&root)));
                    }
                } else if eval(&root, &Empty).is_err() {
                    out.push(Solution::Exact(simplify(&root)));
                }
            }
        } else {
            let c = compile(&e, &[var], &Empty).map_err(|e| e.0)?;
            let f = |x| c.eval(&[x]);
            let mut r = super::numeric::find_roots(&f, -100.0, 100.0);
            r.extend(super::numeric::find_roots(&f, -10.0, 10.0));
            super::numeric::dedup(&mut r);
            out.extend(r.into_iter().map(Solution::Approx));
            if out.is_empty() {
                return Err("Keine Lösung gefunden".into());
            }
        }
    }
    fn n(s: &Solution) -> f64 {
        match s {
            Solution::Exact(e) => value(e).unwrap_or(f64::NAN),
            Solution::Approx(x) => *x,
        }
    }
    if out
        .iter()
        .filter(|s| matches!(s, Solution::Approx(_)))
        .count()
        > 12
    {
        out.retain(|s| !matches!(s,Solution::Approx(x) if x.abs()>10.0));
        if out
            .iter()
            .filter(|s| matches!(s, Solution::Approx(_)))
            .count()
            > 12
        {
            out.retain(|s| !matches!(s,Solution::Approx(x) if x.abs()>2.0*std::f64::consts::PI));
        }
    }
    fn sorting(s: &Solution) -> f64 {
        match s {
            Solution::Exact(e) => {
                value(&super::ast::substitute(e, "k", &int(0))).unwrap_or(f64::NAN)
            }
            Solution::Approx(x) => *x,
        }
    }
    out.sort_by(|a, b| sorting(a).total_cmp(&sorting(b)));
    out.dedup_by(|a, b| {
        a == b
            || (n(a).is_finite()
                && n(b).is_finite()
                && (n(a) - n(b)).abs() <= 1e-9 * (1.0 + n(a).abs().max(n(b).abs())))
    });
    Ok(out)
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_expr;
    fn roots(s: &str) -> Vec<Solution> {
        let Expr::Call(_, a) = parse_expr(s).unwrap() else {
            panic!()
        };
        solve(&a[0], &a[1], "x").unwrap()
    }
    #[test]
    fn linear_quadratic() {
        for (s, want) in [
            ("2x+1=5", vec![2.0]),
            ("x^2=2", vec![-2.0f64.sqrt(), 2.0f64.sqrt()]),
            ("x^2+1=0", vec![]),
            ("x^2-2x+1=0", vec![1.0]),
            ("x^2=4", vec![-2.0, 2.0]),
        ] {
            let r = roots(s);
            assert_eq!(r.len(), want.len(), "{s}");
            for (r, x) in r.iter().zip(want) {
                let Solution::Exact(e) = r else { panic!() };
                assert!((value(e).unwrap() - x).abs() < 1e-10);
            }
        }
    }
    #[test]
    fn higher() {
        let r = roots("x^3-x=0");
        assert_eq!(
            r,
            vec![
                Solution::Exact(int(-1)),
                Solution::Exact(int(0)),
                Solution::Exact(int(1))
            ]
        );
        assert_eq!(roots("x^5-x-1=0").len(), 1);
    }
    #[test]
    fn inverse_chains() {
        for (s, x) in [
            ("2^x=8", 3.0),
            ("ln(x)=2", 2.0f64.exp()),
            ("sqrt(x+1)=3", 8.0),
            ("e^(2x)=5", 5.0f64.ln() / 2.0),
        ] {
            let r = roots(s);
            assert_eq!(r.len(), 1);
            let Solution::Exact(e) = &r[0] else { panic!() };
            assert!((value(e).unwrap() - x).abs() < 1e-10, "{s}");
        }
        assert!(roots("sqrt(x)=-1").is_empty());
        assert!(roots("exp(x)=-1").is_empty());
    }
    #[test]
    fn constant_equations() {
        assert!(solve(&int(2), &int(3), "x").unwrap().is_empty());
        assert_eq!(
            solve(&int(2), &int(2), "x").unwrap_err(),
            "Alle reellen Zahlen sind Lösungen"
        );
    }
    #[test]
    fn transcendental() {
        let r = roots("cos(x)=x");
        assert_eq!(r.len(), 1);
        let Solution::Approx(x) = r[0] else { panic!() };
        assert!((x - 0.7390851332151607).abs() < 1e-10);
    }
    #[test]
    fn additional_inverses() {
        for (s, want) in [
            ("log(2,x)=3", vec![8.0]),
            ("log(x,8)=3", vec![2.0]),
            ("nroot(x,3)=2", vec![8.0]),
            ("abs(x+1)=2", vec![-3.0, 1.0]),
            ("abs(x)=-2", vec![]),
            ("asin(x)=2", vec![]),
        ] {
            let r = roots(s);
            assert_eq!(r.len(), want.len(), "{s}");
            for (r, w) in r.iter().zip(want) {
                let Solution::Exact(e) = r else { panic!("{s}") };
                assert!((value(e).unwrap() - w).abs() < 1e-10, "{s}");
            }
        }
    }
}

#[cfg(test)]
mod review_tests {
    use super::*;
    use crate::{
        eval::{Empty, compile},
        parser::parse_expr,
    };
    fn expression(s: &str) -> Expr {
        parse_expr(s).unwrap()
    }
    #[test]
    fn symbolic_coefficients() {
        for (input, expected) in [
            ("a*x=2", vec!["2/a"]),
            ("x^2=a", vec!["-sqrt(a)", "sqrt(a)"]),
        ] {
            let Expr::Call(_, eq) = expression(input) else {
                panic!()
            };
            let roots = solve(&eq[0], &eq[1], "x").unwrap();
            assert_eq!(roots.len(), expected.len());
            for (root, want) in roots.iter().zip(expected) {
                assert_eq!(*root, Solution::Exact(simplify(&expression(want))));
            }
        }
        let roots = solve(&expression("a*x^2+b*x+c"), &int(0), "x").unwrap();
        assert_eq!(roots.len(), 2);
        for params in [[1.0, 3.0, -4.0], [2.0, -3.0, -2.0], [0.5, 1.0, -4.0]] {
            for root in &roots {
                let Solution::Exact(e) = root else { panic!() };
                let x = compile(e, &["a", "b", "c"], &Empty).unwrap().eval(&params);
                assert!((params[0] * x * x + params[1] * x + params[2]).abs() < 1e-9);
            }
        }
    }
    #[test]
    fn periodic_families() {
        for (input, count) in [
            ("sin(x)=0.5", 2),
            ("cos(x)=1/2", 2),
            ("tan(x)=1", 1),
            ("sin(2x+1)=sqrt(3)/2", 2),
            ("2sin(3x+1)-1=0", 2),
            ("sin(x)=1", 1),
            ("sin(x)=-1", 1),
            ("cos(x)=1", 1),
            ("cos(x)=-1", 1),
            ("sin(x)=2", 0),
            ("cos(x)=-2", 0),
        ] {
            let Expr::Call(_, eq) = expression(input) else {
                panic!()
            };
            let roots = solve(&eq[0], &eq[1], "x").unwrap();
            assert_eq!(roots.len(), count, "{input}");
            let lhs = compile(&eq[0], &["x"], &Empty).unwrap();
            let rhs = compile(&eq[1], &["x"], &Empty).unwrap();
            for root in roots {
                let Solution::Exact(e) = root else {
                    panic!("{input}")
                };
                assert!(contains(&e, "k"));
                let family = compile(&e, &["k"], &Empty).unwrap();
                for k in [-3.0, -1.0, 0.0, 2.0, 7.0] {
                    let x = family.eval(&[k]);
                    assert!(
                        (lhs.eval(&[x]) - rhs.eval(&[x])).abs() < 1e-9,
                        "{input}: {}",
                        crate::format_expr(&e)
                    );
                }
            }
        }
    }
    #[test]
    fn fallback_window() {
        for input in ["sin(x^2)=0", "sin(x^2)=0.5"] {
            let Expr::Call(_, eq) = expression(input) else {
                panic!()
            };
            let roots = solve(&eq[0], &eq[1], "x").unwrap();
            assert!(!roots.is_empty());
            for root in roots {
                let Solution::Approx(x) = root else { panic!() };
                assert!(x.abs() <= 2.0 * std::f64::consts::PI);
            }
        }
    }
}
