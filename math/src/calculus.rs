use super::ast::{Constant, Expr, contains, rat};
use super::simplify::{add, call, div, int, mul, neg, polynomial, pow, simplify, sub, zero};
use Expr::*;
pub fn derivative(expr: &Expr, var: &str) -> Expr {
    fn d(e: &Expr, v: &str, depth: usize) -> Expr {
        if depth > 128 {
            return Call("differential".into(), vec![e.clone(), Var(v.into())]);
        }
        if !contains(e, v) {
            return int(0);
        }
        let der = |a: &Expr| d(a, v, depth + 1);
        match e {
            Call(n, a) if super::logic::conditional(n) => Call(
                "Wenn".into(),
                a.iter()
                    .enumerate()
                    .map(|(i, e)| {
                        if i % 2 == 1 || i + 1 == a.len() && a.len() % 2 == 1 {
                            der(e)
                        } else {
                            e.clone()
                        }
                    })
                    .collect(),
            ),
            Var(n) => int(if n == v { 1 } else { 0 }),
            Neg(a) => neg(der(a)),
            Add(a, b) => add(der(a), der(b)),
            Sub(a, b) => sub(der(a), der(b)),
            Mul(a, b) => add(mul(der(a), *b.clone()), mul(*a.clone(), der(b))),
            Div(a, b) => div(
                sub(mul(der(a), *b.clone()), mul(*a.clone(), der(b))),
                pow(*b.clone(), int(2)),
            ),
            Pow(a, b) => {
                if !contains(b, v) {
                    mul(
                        mul(*b.clone(), pow(*a.clone(), sub(*b.clone(), int(1)))),
                        der(a),
                    )
                } else {
                    mul(
                        e.clone(),
                        add(
                            mul(der(b), call("ln", *a.clone())),
                            div(mul(*b.clone(), der(a)), *a.clone()),
                        ),
                    )
                }
            }
            Call(n, args)
                if super::eval::arity(n, args.len())
                    && super::stats::builtin_arity(n).is_none() =>
            {
                let u = args[0].clone();
                let du = der(&u);
                let u2 = || pow(u.clone(), int(2));
                let unary = match n.as_str() {
                    "sin" => call("cos", u.clone()),
                    "cos" => neg(call("sin", u.clone())),
                    "tan" => div(int(1), pow(call("cos", u.clone()), int(2))),
                    "cot" => neg(div(int(1), pow(call("sin", u.clone()), int(2)))),
                    "sec" => mul(call("sec", u.clone()), call("tan", u.clone())),
                    "csc" => neg(mul(call("csc", u.clone()), call("cot", u.clone()))),
                    "asin" | "arcsin" => div(int(1), call("sqrt", sub(int(1), u2()))),
                    "acos" | "arccos" => neg(div(int(1), call("sqrt", sub(int(1), u2())))),
                    "atan" | "arctan" => div(int(1), add(int(1), u2())),
                    "sinh" => call("cosh", u.clone()),
                    "cosh" => call("sinh", u.clone()),
                    "tanh" => div(int(1), pow(call("cosh", u.clone()), int(2))),
                    "asinh" | "arcsinh" => div(int(1), call("sqrt", add(u2(), int(1)))),
                    "acosh" | "arccosh" => div(int(1), call("sqrt", sub(u2(), int(1)))),
                    "atanh" | "arctanh" => div(int(1), sub(int(1), u2())),
                    "exp" => call("exp", u.clone()),
                    "ln" => div(int(1), u.clone()),
                    "log" | "lg" if args.len() == 1 => {
                        div(int(1), mul(u.clone(), call("ln", int(10))))
                    }
                    "ld" => div(int(1), mul(u.clone(), call("ln", int(2)))),
                    "sqrt" => div(int(1), mul(int(2), call("sqrt", u.clone()))),
                    "cbrt" => div(int(1), mul(int(3), pow(call("cbrt", u.clone()), int(2)))),
                    "abs" => call("sign", u.clone()),
                    "sgn" | "sign" | "floor" | "ceil" | "round" => int(0),
                    "atan2" => {
                        let w = args[1].clone();
                        return div(
                            sub(mul(w.clone(), du), mul(u.clone(), der(&w))),
                            add(u2(), pow(w, int(2))),
                        );
                    }
                    "log" => return der(&div(call("ln", args[1].clone()), call("ln", u.clone()))),
                    "nroot" => {
                        return der(&Pow(Box::new(u), Box::new(div(int(1), args[1].clone()))));
                    }
                    "min" | "max" => {
                        let direction = if n == "min" { -1 } else { 1 };
                        let mut out = du;
                        let mut previous = u.clone();
                        for (i, w) in args.iter().enumerate().skip(1) {
                            let weight = div(
                                add(
                                    int(1),
                                    mul(int(direction), call("sign", sub(previous, w.clone()))),
                                ),
                                int(2),
                            );
                            out = add(mul(weight.clone(), out), mul(sub(int(1), weight), der(w)));
                            previous = Call(n.clone(), args[..=i].to_vec());
                        }
                        return out;
                    }
                    _ => int(0),
                };
                mul(unary, du)
            }
            Call(n, a) => {
                let mut out = int(0);
                for (i, u) in a.iter().enumerate() {
                    if contains(u, v) {
                        out = add(out, mul(Call(format!("{n}_{}", i + 1), a.clone()), der(u)));
                    }
                }
                out
            }
            Tuple(a) => Tuple(a.iter().map(der).collect()),
            List(a) => List(a.iter().map(der).collect()),
            _ => int(0),
        }
    }
    simplify(&d(expr, var, 0))
}
fn linear(e: &Expr, v: &str) -> Option<Expr> {
    let slope = derivative(e, v);
    if !contains(&slope, v) && !zero(&slope) {
        Some(slope)
    } else {
        None
    }
}
fn poly_integral(p: &[Expr], v: &str) -> Expr {
    let mut out = vec![int(0)];
    out.extend(
        p.iter()
            .enumerate()
            .map(|(i, c)| div(c.clone(), int(i as i64 + 1))),
    );
    super::simplify::from_poly(&out, v)
}
fn primitive_linear(e: &Expr, v: &str) -> Option<Expr> {
    if !contains(e, v) {
        return Some(mul(e.clone(), Var(v.into())));
    }
    match e {
        Neg(a) => Some(neg(primitive_linear(a, v)?)),
        Mul(a, b) if !contains(a, v) => Some(mul(*a.clone(), primitive_linear(b, v)?)),
        Mul(a, b) if !contains(b, v) => Some(mul(*b.clone(), primitive_linear(a, v)?)),
        Div(a, b) if !contains(b, v) => Some(div(primitive_linear(a, v)?, *b.clone())),
        Call(n, a) if a.len() == 1 => {
            let k = linear(&a[0], v)?;
            match n.as_str() {
                "exp" => Some(div(e.clone(), k)),
                "sin" => Some(neg(div(call("cos", a[0].clone()), k))),
                "cos" => Some(div(call("sin", a[0].clone()), k)),
                _ => None,
            }
        }
        Pow(a, b) if !contains(a, v) => {
            let k = linear(b, v)?;
            Some(div(
                e.clone(),
                if **a == Const(Constant::E) {
                    k
                } else {
                    mul(k, call("ln", *a.clone()))
                },
            ))
        }
        _ => None,
    }
}
fn factors(e: &Expr, top: &mut Vec<Expr>, bottom: &mut Vec<Expr>) {
    match e {
        Mul(a, b) => {
            factors(a, top, bottom);
            factors(b, top, bottom)
        }
        Div(a, b) => {
            factors(a, top, bottom);
            factors(b, bottom, top)
        }
        _ => top.push(e.clone()),
    }
}
fn substitution_integral(e: &Expr, v: &str) -> Option<Expr> {
    let mut top = Vec::new();
    let mut bottom = Vec::new();
    factors(e, &mut top, &mut bottom);
    let kernels = top.into_iter().chain(bottom.into_iter().map(|e| match e {
        Pow(a, b) => pow(*a, neg(*b)),
        e => Div(Box::new(int(1)), Box::new(e)),
    }));
    for kernel in kernels.take(128) {
        let identity_derivative = derivative(&kernel, v);
        if contains(&kernel, v) && !zero(&identity_derivative) {
            let coefficient = simplify(&div(div(e.clone(), kernel.clone()), identity_derivative));
            if !contains(&coefficient, v) {
                return Some(mul(coefficient, div(pow(kernel.clone(), int(2)), int(2))));
            }
        }
        let (inner, primitive) = match &kernel {
            Call(n, a) if a.len() == 1 => {
                let g = a[0].clone();
                let primitive = match n.as_str() {
                    "exp" => kernel.clone(),
                    "sin" => neg(call("cos", g.clone())),
                    "cos" => call("sin", g.clone()),
                    "ln" => sub(mul(g.clone(), kernel.clone()), g.clone()),
                    "sqrt" => div(pow(g.clone(), rat(3, 2)), rat(3, 2)),
                    _ => continue,
                };
                (g, primitive)
            }
            Pow(a, b) if !contains(b, v) => {
                let g = *a.clone();
                let primitive = if **b == int(-1) {
                    call("ln", call("abs", g.clone()))
                } else {
                    let exponent = add(*b.clone(), int(1));
                    div(pow(g.clone(), exponent.clone()), exponent)
                };
                (g, primitive)
            }
            Pow(a, b) if !contains(a, v) => {
                let primitive = if **a == Const(Constant::E) {
                    kernel.clone()
                } else {
                    div(kernel.clone(), call("ln", *a.clone()))
                };
                (*b.clone(), primitive)
            }
            Div(a, b) if **a == int(1) => (*b.clone(), call("ln", call("abs", *b.clone()))),
            _ => continue,
        };
        if !contains(&inner, v) {
            continue;
        }
        let d = derivative(&inner, v);
        if zero(&d) {
            continue;
        }
        let remainder = div(e.clone(), kernel);
        let coefficient = simplify(&div(remainder, d));
        if !contains(&coefficient, v) {
            return Some(mul(coefficient, primitive));
        }
    }
    None
}
fn parts_integral(e: &Expr, v: &str) -> Option<Expr> {
    let mut top = Vec::new();
    let mut bottom = Vec::new();
    factors(e, &mut top, &mut bottom);
    if bottom.iter().any(|e| contains(e, v)) {
        return None;
    }
    for i in 0..top.len() {
        let f = &top[i];
        if !matches!(f,Call(n,a) if matches!(n.as_str(),"exp"|"sin"|"cos"|"ln")&&a.len()==1)
            && !matches!(f,Pow(a,b) if !contains(a,v)&&contains(b,v))
        {
            continue;
        }
        let logarithm = matches!(f,Call(n,a) if n=="ln"&&a.len()==1&&linear(&a[0],v).is_some());
        if !logarithm && primitive_linear(f, v).is_none() {
            continue;
        }
        let numerator = top
            .iter()
            .enumerate()
            .filter(|(j, _)| *j != i)
            .fold(int(1), |a, (_, b)| mul(a, b.clone()));
        let denominator = bottom.iter().fold(int(1), |a, b| mul(a, b.clone()));
        let p = polynomial(&div(numerator, denominator), v)?;
        if p.len() > 7 {
            continue;
        }
        if logarithm {
            let Call(_, args) = f else {
                continue;
            };
            let g = &args[0];
            let a = linear(g, v)?;
            let p_int = poly_integral(&p, v);
            let mut numerator = polynomial(&mul(p_int.clone(), a.clone()), v)?;
            let denominator = polynomial(g, v)?;
            if denominator.len() != 2 {
                return None;
            }
            let mut q = vec![int(0); numerator.len().saturating_sub(2) + 1];
            while numerator.len() >= 2 {
                let index = numerator.len() - 2;
                let c = div(numerator.last()?.clone(), denominator[1].clone());
                q[index] = c.clone();
                numerator[index] = sub(numerator[index].clone(), mul(c, denominator[0].clone()));
                numerator.pop();
                super::simplify::trim(&mut numerator);
            }
            let remainder = numerator.first().cloned().unwrap_or(int(0));
            let rest = add(
                poly_integral(&q, v),
                mul(div(remainder, a), call("ln", call("abs", g.clone()))),
            );
            return Some(sub(mul(p_int, f.clone()), rest));
        }
        let mut p = super::simplify::from_poly(&p, v);
        let mut primitive = f.clone();
        let mut out = int(0);
        for order in 0..=6 {
            if zero(&p) {
                return Some(out);
            }
            primitive = primitive_linear(&primitive, v)?;
            let term = mul(p.clone(), primitive.clone());
            out = if order % 2 == 0 {
                add(out, term)
            } else {
                sub(out, term)
            };
            p = derivative(&p, v);
        }
        if zero(&p) {
            return Some(out);
        }
    }
    None
}
pub fn integrate(expr: &Expr, var: &str) -> Option<Expr> {
    fn go(e: &Expr, v: &str, depth: usize) -> Option<Expr> {
        if matches!(e,Call(n,_) if super::logic::conditional(n)) {
            return None;
        }
        if depth > 64 {
            return None;
        }
        let x = Var(v.into());
        if !contains(e, v) {
            return Some(mul(e.clone(), x));
        }
        if let Some(p) = polynomial(e, v) {
            let mut out = int(0);
            for (i, c) in p.into_iter().enumerate() {
                out = add(
                    out,
                    mul(div(c, int(i as i64 + 1)), pow(x.clone(), int(i as i64 + 1))),
                );
            }
            return Some(out);
        }
        let rec = |e: &Expr| go(e, v, depth + 1);
        if let Some(result) = substitution_integral(e, v) {
            return Some(result);
        }
        if let Some(result) = parts_integral(e, v) {
            return Some(result);
        }
        match e {
            Neg(a) => Some(neg(rec(a)?)),
            Add(a, b) => Some(add(rec(a)?, rec(b)?)),
            Sub(a, b) => Some(sub(rec(a)?, rec(b)?)),
            Mul(a, b) if !contains(a, v) => Some(mul(*a.clone(), rec(b)?)),
            Mul(a, b) if !contains(b, v) => Some(mul(*b.clone(), rec(a)?)),
            Mul(..) => {
                let ex = super::simplify::expand(e);
                if ex != *e { rec(&ex) } else { None }
            }
            Div(a, b) if !contains(b, v) => Some(div(rec(a)?, *b.clone())),
            Div(a, b) if !contains(a, v) => {
                if let Some(k) = linear(b, v) {
                    return Some(mul(div(*a.clone(), k), call("ln", call("abs", *b.clone()))));
                }
                if **b == add(int(1), pow(x.clone(), int(2))) {
                    return Some(mul(*a.clone(), call("atan", x)));
                }
                if let Pow(base, n) = &**b {
                    if **n == int(2) {
                        if let Call(name, args) = &**base {
                            if name == "cos" && args.len() == 1 {
                                let k = linear(&args[0], v)?;
                                return Some(mul(div(*a.clone(), k), call("tan", args[0].clone())));
                            }
                        }
                    }
                }
                None
            }
            Pow(a, b) if !contains(b, v) => {
                let k = linear(a, v)?;
                if **b == int(-1) {
                    Some(div(call("ln", call("abs", *a.clone())), k))
                } else {
                    let n = add(*b.clone(), int(1));
                    Some(div(pow(*a.clone(), n.clone()), mul(k, n)))
                }
            }
            Pow(a, b) if !contains(a, v) => {
                let k = linear(b, v)?;
                if **a == Const(Constant::E) {
                    Some(div(e.clone(), k))
                } else {
                    Some(div(e.clone(), mul(k, call("ln", *a.clone()))))
                }
            }
            Call(n, a) if a.len() == 1 => {
                let u = a[0].clone();
                let k = linear(&u, v)?;
                match n.as_str() {
                    "exp" => Some(div(e.clone(), k)),
                    "sin" => Some(neg(div(call("cos", u), k))),
                    "cos" => Some(div(call("sin", u), k)),
                    "sqrt" => Some(div(pow(u, rat(3, 2)), mul(k, rat(3, 2)))),
                    "cbrt" => Some(div(pow(u, rat(4, 3)), mul(k, rat(4, 3)))),
                    _ => None,
                }
            }
            _ => None,
        }
    }
    go(&simplify(expr), var, 0).map(|e| simplify(&e))
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        eval::{Empty, compile},
        parser::parse_expr,
    };
    #[test]
    fn every_builtin() {
        let cases = [
            ("sin(x)", 0.4),
            ("cos(x)", 0.4),
            ("tan(x)", 0.4),
            ("cot(x)", 0.4),
            ("sec(x)", 0.4),
            ("csc(x)", 0.4),
            ("asin(x)", 0.4),
            ("acos(x)", 0.4),
            ("atan(x)", 0.4),
            ("atan2(x,2)", 0.4),
            ("atan2(2,x)", 0.4),
            ("sinh(x)", 0.4),
            ("cosh(x)", 0.4),
            ("tanh(x)", 0.4),
            ("asinh(x)", 0.4),
            ("acosh(x)", 2.0),
            ("atanh(x)", 0.4),
            ("exp(x)", 0.4),
            ("ln(x)", 1.4),
            ("log(x)", 1.4),
            ("log(2,x)", 1.4),
            ("log(x,3)", 1.4),
            ("lg(x)", 1.4),
            ("ld(x)", 1.4),
            ("sqrt(x)", 1.4),
            ("cbrt(x)", 1.4),
            ("nroot(x,3)", 1.4),
            ("nroot(2,x)", 1.4),
            ("abs(x)", 1.4),
            ("sgn(x)", 1.4),
            ("sign(x)", 1.4),
            ("floor(x)", 1.4),
            ("ceil(x)", 1.4),
            ("round(x)", 1.1),
            ("min(x,2)", 1.4),
            ("max(x,2)", 1.4),
            ("x^x", 1.4),
            ("(x+1)/(x+2)", 1.4),
        ];
        for (s, x0) in cases {
            let e = parse_expr(s).unwrap();
            let d = derivative(&e, "x");
            let f = compile(&e, &["x"], &Empty).unwrap();
            let df = compile(&d, &["x"], &Empty).unwrap();
            for offset in [0.0, 0.03, 0.07] {
                let x = x0 + offset;
                let h = 1e-6;
                let want = (f.eval(&[x + h]) - f.eval(&[x - h])) / (2.0 * h);
                let got = df.eval(&[x]);
                assert!(
                    (want - got).abs() < 2e-5 * (1.0 + want.abs()),
                    "{s} @ {x}: {got} != {want}; {}",
                    crate::format_expr(&d)
                );
            }
        }
    }
    #[test]
    fn integration() {
        for s in [
            "3",
            "x^3-2x+1",
            "1/x",
            "1/(2x+3)",
            "exp(2x+1)",
            "e^(2x)",
            "2^x",
            "sin(3x+1)",
            "cos(2x)",
            "1/cos(x)^2",
            "1/(1+x^2)",
            "sqrt(x)",
            "(2x+1)^(2/3)",
            "(x+1)(x-2)",
            "x^(-2)",
        ] {
            let e = parse_expr(s).unwrap();
            let i = integrate(&e, "x").unwrap_or_else(|| panic!("{s}"));
            let d = derivative(&i, "x");
            let f = compile(&e, &["x"], &Empty).unwrap();
            let df = compile(&d, &["x"], &Empty).unwrap();
            for x in [0.3, 0.7, 1.3] {
                let a = f.eval(&[x]);
                let b = df.eval(&[x]);
                assert!(
                    (a - b).abs() < 1e-8 * (1.0 + a.abs()),
                    "{s}: {} -> {a} {b}",
                    crate::format_expr(&i)
                );
            }
        }
    }
    #[test]
    fn unsupported() {
        assert!(integrate(&parse_expr("sin(x^2)").unwrap(), "x").is_none());
        let d = derivative(&parse_expr("foo(x)").unwrap(), "x");
        assert!(!zero(&d));
        assert_eq!(derivative(&parse_expr("foo(y)").unwrap(), "x"), int(0));
    }
}

#[cfg(test)]
mod review_tests {
    use super::*;
    use crate::{
        eval::{Empty, compile},
        parser::parse_expr,
    };
    #[test]
    fn parts_and_substitutions() {
        for input in [
            "x*e^x",
            "2x*e^x",
            "x^2*exp(2x+1)",
            "(x^3+2x+1)*sin(3x+2)",
            "x^6*cos(2x)",
            "x*ln(x)",
            "(x^2+1)*ln(2x+3)",
            "ln(3x+1)",
            "2x*e^(x^2)",
            "3x*exp(2x^2)",
            "x/(x^2+1)",
            "cos(x)*sin(x)^2",
            "2x*cos(x^2)",
            "sin(x)*cos(x)^3",
            "x/(x^2+1)^2",
            "2x*sqrt(x^2+1)",
            "ln(x)/x",
            "exp(sin(x))*cos(x)",
            "(x^6+3x)*ln(3x+2)",
        ] {
            let expr = parse_expr(input).unwrap();
            let anti = integrate(&expr, "x").unwrap_or_else(|| panic!("{input}"));
            let d = derivative(&anti, "x");
            let f = compile(&expr, &["x"], &Empty).unwrap();
            let df = compile(&d, &["x"], &Empty).unwrap();
            for x in [0.25, 0.7, 1.2, 2.1] {
                let want = f.eval(&[x]);
                let got = df.eval(&[x]);
                assert!(
                    (want - got).abs() < 1e-8 * (1.0 + want.abs()),
                    "{input}: {} -> {got} != {want}",
                    crate::format_expr(&anti)
                );
            }
        }
    }
    #[test]
    fn symbolic_integration() {
        let e = parse_expr("k*x").unwrap();
        let anti = integrate(&e, "x").unwrap();
        let c = compile(&derivative(&anti, "x"), &["x", "k"], &Empty).unwrap();
        assert_eq!(c.eval(&[3.0, 7.0]), 21.0);
        for input in ["a*x^2*exp(b*x)", "a*x*ln(b*x+c)"] {
            let expression = parse_expr(input).unwrap();
            let anti = integrate(&expression, "x").unwrap();
            let derivative =
                compile(&derivative(&anti, "x"), &["x", "a", "b", "c"], &Empty).unwrap();
            let expected = compile(&expression, &["x", "a", "b", "c"], &Empty).unwrap();
            for x in [0.3, 0.7, 1.2] {
                let args = [x, 1.5, 2.0, 3.0];
                let want = expected.eval(&args);
                let got = derivative.eval(&args);
                assert!((want - got).abs() < 1e-8 * (1.0 + want.abs()), "{input}");
            }
        }
        assert!(integrate(&parse_expr("x^7*exp(x)").unwrap(), "x").is_none());
    }
}
