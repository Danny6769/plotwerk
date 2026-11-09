use crate::ast::Expr;
use crate::simplify::{int, number};
pub(crate) fn comparison(n: &str) -> bool {
    matches!(n, "<" | ">" | "<=" | ">=" | "==" | "!=")
}
pub(crate) fn conditional(n: &str) -> bool {
    matches!(n.to_lowercase().as_str(), "if" | "wenn")
}
pub(crate) fn builtin(n: &str) -> bool {
    comparison(n) || matches!(n, "and" | "or" | "not") || conditional(n)
}
pub(crate) fn arity(n: &str, len: usize) -> bool {
    if conditional(n) {
        len >= 2
    } else if n == "not" {
        len == 1
    } else {
        len == 2
    }
}
pub(crate) fn operation(n: &str, x: f64, y: f64) -> f64 {
    if x.is_nan() || n != "not" && y.is_nan() {
        return f64::NAN;
    }
    f64::from(match n {
        "<" => x < y,
        ">" => x > y,
        "<=" => x <= y,
        ">=" => x >= y,
        "==" => x == y,
        "!=" => x != y,
        "and" => x != 0.0 && y != 0.0,
        "or" => x != 0.0 || y != 0.0,
        "not" => x == 0.0,
        _ => return f64::NAN,
    })
}
fn value(e: &Expr) -> Option<f64> {
    number(e).or_else(|| {
        if let Expr::Var(n) = e {
            match n.to_lowercase().as_str() {
                "true" | "wahr" => Some(1.0),
                "false" | "falsch" => Some(0.0),
                _ => None,
            }
        } else {
            None
        }
    })
}
pub(crate) fn fold(n: &str, a: Vec<Expr>) -> Expr {
    if !arity(n, a.len()) {
        return Expr::Call(n.into(), a);
    }
    if conditional(n) {
        let mut remaining = Vec::new();
        for pair in a[..a.len() / 2 * 2].chunks_exact(2) {
            if let Some(c) = value(&pair[0]) {
                if c.is_nan() {
                    return Expr::Real(f64::NAN);
                }
                if c != 0.0 {
                    if remaining.is_empty() {
                        return pair[1].clone();
                    }
                    remaining.push(pair[1].clone());
                    return Expr::Call("Wenn".into(), remaining);
                }
            } else {
                remaining.extend_from_slice(pair);
            }
        }
        if a.len() % 2 == 1 {
            if remaining.is_empty() {
                return a.last().unwrap().clone();
            }
            remaining.push(a.last().unwrap().clone());
        }
        if remaining.is_empty() {
            return Expr::Real(f64::NAN);
        }
        return Expr::Call("Wenn".into(), remaining);
    }
    if let Some(x) = value(&a[0]) {
        if n == "and" && x == 0.0 {
            return int(0);
        }
        if n == "or" && x.is_finite() && x != 0.0 {
            return int(1);
        }
        if n == "not" {
            let v = operation(n, x, 0.0);
            return if v.is_nan() {
                Expr::Real(v)
            } else {
                int(v as i64)
            };
        }
        if let Some(y) = value(&a[1]) {
            let v = operation(n, x, y);
            return if v.is_nan() {
                Expr::Real(v)
            } else {
                int(v as i64)
            };
        }
    }
    Expr::Call(n.into(), a)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::Empty;
    use crate::{
        CasEnv, Statement, cas_eval, compile, eval, format_expr, parse_expr, parse_statement,
    };
    #[test]
    fn operators_precedence_chains() {
        for (input, want) in [
            ("1<2", 1.0),
            ("2>=3", 0.0),
            ("2≤3", 1.0),
            ("3≠2", 1.0),
            ("2==2", 1.0),
            ("2!=2", 0.0),
            ("1+2<4&&3>2", 1.0),
            ("!1<2", 0.0),
            ("¬(1>2)∧0==0", 1.0),
            ("0||1&&0", 0.0),
            ("1<2<3", 1.0),
            ("1<2>3", 0.0),
            ("3!>5", 1.0),
            ("!0", 1.0),
            ("!3!", 0.0),
        ] {
            let e = parse_expr(input).unwrap();
            let got = eval(&e, &Empty).unwrap();
            assert_eq!(got, want, "{input}: {}", format_expr(&e));
        }
        for input in ["y<x^2", "x^2+y^2<=9"] {
            assert!(matches!(
                parse_statement(input),
                Ok(Statement::Expr(Expr::Call(_, _)))
            ));
        }
        assert!(matches!(
            parse_statement("y=x^2"),
            Ok(Statement::Equation { .. })
        ));
        let c = compile(&parse_expr("1<x<3").unwrap(), &["x"], &Empty).unwrap();
        for (x, want) in [(0.0, 0.0), (1.0, 0.0), (2.0, 1.0), (3.0, 0.0)] {
            assert_eq!(c.eval(&[x]), want);
        }
        assert!(c.eval(&[f64::NAN]).is_nan());
    }
    #[test]
    fn conditionals_and_calculus() {
        let e = parse_expr("If(x<0,-x,x<2,x^2,10)").unwrap();
        let c = compile(&e, &["x"], &Empty).unwrap();
        for (x, want) in [(-3.0, 3.0), (1.0, 1.0), (3.0, 10.0)] {
            assert_eq!(c.eval(&[x]), want);
        }
        let c = compile(&parse_expr("If(x>0,sqrt(x))").unwrap(), &["x"], &Empty).unwrap();
        assert!(c.eval(&[-1.0]).is_nan());
        assert!(c.eval(&[f64::NAN]).is_nan());
        assert_eq!(
            compile(&parse_expr("If(0,unknown,2)").unwrap(), &[], &Empty)
                .unwrap()
                .eval(&[]),
            2.0
        );
        assert_eq!(
            compile(&parse_expr("If(1,2,unknown)").unwrap(), &[], &Empty)
                .unwrap()
                .eval(&[]),
            2.0
        );
        let d = crate::derivative(&e, "x");
        let d = compile(&d, &["x"], &Empty).unwrap();
        assert_eq!(d.eval(&[-1.0]), -1.0);
        assert_eq!(d.eval(&[1.0]), 2.0);
        assert_eq!(d.eval(&[3.0]), 0.0);
        assert!(crate::integrate(&e, "x").is_none());
        assert!(crate::integrate(&parse_expr("If(wahr,x^2,0)").unwrap(), "x").is_some());
        let mut env = CasEnv::default();
        assert_eq!(
            cas_eval("Wenn(false,Invert({{0}}),3)", &mut env).unwrap(),
            "3"
        );
        assert_eq!(
            cas_eval("If(x<0,-x,x)", &mut env).unwrap(),
            "Wenn(x < 0, -x, x)"
        );
    }
    #[test]
    fn formatting_roundtrips() {
        for input in [
            "0.990426*e^(1.00546x)",
            "2*e^x",
            "2*energy",
            "0.5*exp(x)",
            "x<=3&&x!=0",
            "!(x<0)||x>=2",
            "1<x<3",
            "Wenn(x<0,-x,x)",
            "If(x<0,0,x<1,x^2,2)",
        ] {
            let e = parse_expr(input).unwrap();
            let formatted = format_expr(&e);
            let reparsed = parse_expr(&formatted).unwrap();
            let original = compile(&e, &["x", "energy"], &Empty).unwrap();
            let restored = compile(&reparsed, &["x", "energy"], &Empty).unwrap();
            for x in [-1.2, 0.3, 1.7, 3.0] {
                let a = original.eval(&[x, 4.0]);
                let b = restored.eval(&[x, 4.0]);
                assert!(
                    (a - b).abs() < 1e-10 || a.is_nan() && b.is_nan(),
                    "{input} -> {formatted}"
                );
            }
        }
        assert_eq!(
            format_expr(&parse_expr("0.990426*e^(1.00546x)").unwrap()),
            "0.990426 e^(1.00546x)"
        );
        assert_eq!(format_expr(&parse_expr("2*energy").unwrap()), "2 energy");
    }
}
