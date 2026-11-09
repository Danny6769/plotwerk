use super::ast::{Constant, Expr};
use super::simplify::simplify;
use Expr::*;
pub fn format_number(value: f64) -> String {
    if value.is_nan() {
        return "undefiniert".into();
    }
    if value.is_infinite() {
        return if value < 0.0 { "-∞" } else { "∞" }.into();
    }
    if value.abs() < 0.00005 {
        return "0".into();
    }
    fn strip(s: String) -> String {
        if s.contains('.') {
            s.trim_end_matches('0').trim_end_matches('.').to_string()
        } else {
            s
        }
    }
    if value.abs() >= 1e8 || value.abs() < 1e-4 {
        let s = format!("{value:.4E}");
        let (m, e) = s.split_once('E').unwrap();
        format!("{}E{}", strip(m.into()), e.parse::<i32>().unwrap())
    } else {
        strip(format!("{value:.4}"))
    }
}
fn negative(e: &Expr) -> Option<Expr> {
    match e {
        Rational(n, d) if *n < 0 => Some(super::ast::rat(-(*n as i128), *d as i128)),
        Real(v) if *v < 0.0 => Some(Real(-v)),
        Neg(a) => Some(*a.clone()),
        Mul(a, b) => negative(a).map(|a| super::simplify::mul(a, *b.clone())),
        Div(a, b) => negative(a).map(|a| super::simplify::div(a, *b.clone())),
        _ => None,
    }
}
fn group_after(e: &Expr) -> bool {
    fn precedence(e: &Expr) -> u8 {
        match e {
            Add(..) | Sub(..) => 10,
            Mul(..) | Div(..) | Rational(_, 2..) => 20,
            Neg(_) => 25,
            Pow(..) => 30,
            _ => 50,
        }
    }
    match e {
        Var(n) => matches!(n.as_str(), "x" | "y" | "z" | "t"),
        Mul(_, b) | Div(_, b) => precedence(b) < 21 || group_after(b),
        Pow(_, b) => precedence(b) < 30 || group_after(b),
        Neg(b) => precedence(b) < 25 || group_after(b),
        _ => true,
    }
}
fn render(e: &Expr, parent: u8) -> String {
    let (s, bp) = match e {
        Rational(n, 1) => (n.to_string(), if *n < 0 { 25 } else { 50 }),
        Rational(n, d) => (format!("{n}/{d}"), 20),
        Real(v) => {
            let s = if v.is_infinite() {
                if *v < 0.0 {
                    "-∞".into()
                } else {
                    "∞".into()
                }
            } else {
                v.to_string()
            };
            (s, if *v < 0.0 { 25 } else { 50 })
        }
        Const(c) => (
            match c {
                Constant::Pi => "pi",
                Constant::E => "e",
            }
            .into(),
            50,
        ),
        Var(n) => (n.clone(), 50),
        Neg(a) => (format!("-{}", render(a, 25)), 25),
        Add(a, b) => {
            let s = if let Some(b) = negative(b) {
                format!("{} - {}", render(a, 10), render(&b, 11))
            } else {
                format!("{} + {}", render(a, 10), render(b, 11))
            };
            (s, 10)
        }
        Sub(a, b) => (format!("{} - {}", render(a, 10), render(b, 11)), 10),
        Mul(a, b) if **a == Rational(-1, 1) => (format!("-{}", render(b, 25)), 25),
        Mul(a, b) if matches!(**a,Rational(_,d) if d>1) => {
            let Rational(n, d) = **a else { unreachable!() };
            let numerator = if n == 1 {
                *b.clone()
            } else {
                Mul(Box::new(Rational(n, 1)), b.clone())
            };
            (format!("{}/{}", render(&numerator, 20), d), 20)
        }
        Mul(a, b) => {
            let coeff = matches!(**a, Rational(..) | Real(_));
            let grouped = matches!(**b, Add(..) | Sub(..)) && group_after(a);
            let right = render(b, 21);
            let numeric_power = matches!(**b, Pow(..))
                && right.starts_with(|c: char| c.is_ascii_digit() || c == '.');
            let implicit = grouped
                || coeff
                    && !right.starts_with('e')
                    && !numeric_power
                    && !matches!(**b, Rational(..) | Real(_) | Neg(_) | Div(..));
            let left = if matches!(**a,Rational(_,d) if d!=1) {
                format!("({})", render(a, 0))
            } else {
                render(a, 20)
            };
            (
                if implicit {
                    format!("{left}{right}")
                } else if matches!(**b, Add(..) | Sub(..) | Div(..)) && !group_after(a)
                    || matches!(**a, Rational(..) | Real(_))
                        && matches!(**b, Rational(..) | Real(_))
                    || right.starts_with('-')
                    || coeff && numeric_power
                {
                    format!("{left}*{right}")
                } else {
                    format!("{left} {right}")
                },
                20,
            )
        }
        Div(a, b) => (format!("{}/{}", render(a, 20), render(b, 21)), 20),
        Pow(a, b) => (format!("{}^{}", render(a, 31), render(b, 31)), 30),
        Call(n, a) if super::logic::comparison(n) && a.len() == 2 => {
            let op = match n.as_str() {
                "<=" => "≤",
                ">=" => "≥",
                "!=" => "≠",
                _ => n,
            };
            (
                format!("{} {} {}", render(&a[0], 6), op, render(&a[1], 6)),
                5,
            )
        }
        Call(n, a) if n == "not" && a.len() == 1 => (format!("¬{}", render(&a[0], 6)), 4),
        Call(n, a) if matches!(n.as_str(), "and" | "or") && a.len() == 2 => {
            if n == "and"
                && let (Call(l, b), Call(r, c)) = (&a[0], &a[1])
                && super::logic::comparison(l)
                && super::logic::comparison(r)
                && b.len() == 2
                && c.len() == 2
                && b[1] == c[0]
            {
                let op = |s: &str| match s {
                    "<=" => "≤".to_string(),
                    ">=" => "≥".into(),
                    "!=" => "≠".into(),
                    s => s.to_string(),
                };
                (
                    format!(
                        "{} {} {} {} {}",
                        render(&b[0], 6),
                        op(l),
                        render(&b[1], 6),
                        op(r),
                        render(&c[1], 6)
                    ),
                    5,
                )
            } else {
                let bp = if n == "and" { 3 } else { 2 };
                (
                    format!(
                        "{} {} {}",
                        render(&a[0], bp),
                        if n == "and" { "∧" } else { "∨" },
                        render(&a[1], bp + 1)
                    ),
                    bp,
                )
            }
        }
        Call(n, a) if super::logic::conditional(n) => (
            format!(
                "Wenn({})",
                a.iter()
                    .map(|e| render(e, 0))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            50,
        ),
        Call(n, a) if n == "=" && a.len() == 2 => {
            (format!("{} = {}", render(&a[0], 0), render(&a[1], 0)), 1)
        }
        Call(n, a) => (
            format!(
                "{}({})",
                n,
                a.iter()
                    .map(|e| render(e, 0))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            50,
        ),
        Tuple(a) => (
            format!(
                "({})",
                a.iter()
                    .map(|e| render(e, 0))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            50,
        ),
        List(a) => (
            format!(
                "{{{}}}",
                a.iter()
                    .map(|e| render(e, 0))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            50,
        ),
    };
    if bp < parent { format!("({s})") } else { s }
}
pub fn format_expr(expr: &Expr) -> String {
    render(&simplify(expr), 0)
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        eval::{Empty, compile},
        parser::parse_expr,
    };
    #[test]
    fn numbers() {
        for (n, s) in [
            (2.0, "2"),
            (1.0 / 3.0, "0.3333"),
            (-1.5, "-1.5"),
            (1e-12, "0"),
            (123450000.0, "1.2345E8"),
            (f64::INFINITY, "∞"),
            (f64::NEG_INFINITY, "-∞"),
        ] {
            assert_eq!(format_number(n), s);
        }
        assert_eq!(format_number(f64::NAN), "undefiniert");
    }
    #[test]
    fn readable() {
        for (e, s) in [
            ("x^2+2x-1", "x^2 + 2x - 1"),
            ("3sin(x)", "3sin(x)"),
            ("-x^2", "-x^2"),
            ("(x+1)^2", "(x + 1)^2"),
            ("sqrt(2)", "sqrt(2)"),
            ("1/3", "1/3"),
            ("x/2", "x/2"),
            ("2x/3", "2x/3"),
            ("x-3", "x - 3"),
            ("(1,2)", "(1, 2)"),
            ("{1,2}", "{1, 2}"),
        ] {
            assert_eq!(format_expr(&parse_expr(e).unwrap()), s, "{e}");
        }
    }
    #[test]
    fn roundtrip() {
        for s in [
            "x^2+2x-1",
            "3sin(x)",
            "-x^2",
            "(-x)^2",
            "(x+1)^2",
            "2x/3",
            "x/2",
            "x/(x+1)",
            "x^(x+1)",
            "(x^2)^3",
            "2^(x^2)",
            "x*y*z",
            "a*(x+1)",
            "0.123456789x",
            "2pi*x",
            "x-(x+1)",
            "2^(-x)",
            "sin(x)/cos(x)",
            "(1/3)x",
            "x/2/3",
            "2*a*(x+1)",
            "x^a*(x+1)",
            "(x+1)*a*(x-1)",
            "x^(-a)*(x+1)",
        ] {
            let e = parse_expr(s).unwrap();
            let f = format_expr(&e);
            let q = parse_expr(&f).unwrap();
            let c = compile(&e, &["x", "y", "z", "a"], &Empty).unwrap();
            let d = compile(&q, &["x", "y", "z", "a"], &Empty).unwrap();
            for x in [0.3, 1.2, 2.3] {
                let a = c.eval(&[x, 1.5, 2.0, 3.0]);
                let b = d.eval(&[x, 1.5, 2.0, 3.0]);
                assert!(
                    (a - b).abs() < 1e-9 * (1.0 + a.abs()),
                    "{s} => {f}: {a} {b}"
                );
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
    #[test]
    fn implicit_products_roundtrip() {
        for input in [
            "x^3*cos(x)+3x^2*sin(x)",
            "x^2*e^x+2x*e^x",
            "a*b",
            "a*(x+1)",
            "2*a*(x+1)",
            "x^a*(x+1)",
            "(x+1)*(x-1)",
            "x/(x+1)*cos(x)",
            "1/(x+1)*cos(x)",
            "a*(b/c)",
        ] {
            let e = parse_expr(input).unwrap();
            let formatted = format_expr(&e);
            let r = parse_expr(&formatted).unwrap();
            let l = compile(&e, &["x", "a", "b", "c"], &Empty).unwrap();
            let r = compile(&r, &["x", "a", "b", "c"], &Empty).unwrap();
            for x in [0.3, 0.9, 2.0] {
                let args = [x, 1.4, 2.0, 3.0];
                assert!(
                    (l.eval(&args) - r.eval(&args)).abs() < 1e-9,
                    "{input} -> {formatted}"
                );
            }
            if !input.contains("*(x+1)") && !input.contains("a*(b/c)") {
                assert!(!formatted.contains('*'), "{formatted}");
            }
        }
    }
    #[test]
    fn logarithmic_derivative() {
        let e = parse_expr("ln(x^2+1)").unwrap();
        assert_eq!(format_expr(&crate::derivative(&e, "x")), "2x/(x^2 + 1)");
    }
}

#[cfg(test)]
mod cosmetic_tests {
    use super::*;
    use crate::{
        eval::{Empty, compile},
        parser::parse_expr,
    };
    #[test]
    fn negative_unit_coefficients() {
        for (input, want) in [
            ("x*e^x-e^x", "x e^x - e^x"),
            ("x*ln(x)-x", "x ln(x) - x"),
            ("-b-sqrt(b^2-4*a*c)", "-b - sqrt(b^2 - 4a c)"),
        ] {
            assert_eq!(format_expr(&parse_expr(input).unwrap()), want);
        }
    }
    #[test]
    fn nested_power_and_fraction_roundtrips() {
        for (input, want) in [
            ("e^(x^2)", "e^(x^2)"),
            ("2^(3^x)", "2^(3^x)"),
            ("a/2/b/3/c", "a/(6b c)"),
            ("(-b+sqrt(b^2-4*a*c))/2/a", "(-b + sqrt(b^2 - 4a c))/(2a)"),
        ] {
            let original = parse_expr(input).unwrap();
            let printed = format_expr(&original);
            assert_eq!(printed, want);
            let parsed = parse_expr(&printed).unwrap();
            let a = compile(&original, &["x", "a", "b", "c"], &Empty).unwrap();
            let b = compile(&parsed, &["x", "a", "b", "c"], &Empty).unwrap();
            for x in [0.2, 0.7, 1.2] {
                let args = [x, 2.0, -3.0, -1.0];
                assert!((a.eval(&args) - b.eval(&args)).abs() < 1e-9, "{input}");
            }
        }
    }
}
