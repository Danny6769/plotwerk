#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Constant {
    Pi,
    E,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Rational(i64, i64),
    Real(f64),
    Const(Constant),
    Var(String),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>),
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Expr(Expr),
    Assign {
        name: String,
        params: Vec<String>,
        value: Expr,
    },
    Equation {
        lhs: Expr,
        rhs: Expr,
    },
}

pub(crate) fn rat(n: i128, d: i128) -> Expr {
    if d == 0 {
        return Expr::Real(n as f64 / d as f64);
    }
    let (mut a, mut b) = (n.unsigned_abs(), d.unsigned_abs());
    while b != 0 {
        (a, b) = (b, a % b);
    }
    let g = a.max(1);
    let nn = n.unsigned_abs() / g;
    let dd = d.unsigned_abs() / g;
    let negative = (n < 0) != (d < 0);
    if dd > i64::MAX as u128 || nn > i64::MAX as u128 + u128::from(negative) {
        return Expr::Real(n as f64 / d as f64);
    }
    let nn = if negative { -(nn as i128) } else { nn as i128 };
    Expr::Rational(nn as i64, dd as i64)
}

pub(crate) fn contains(e: &Expr, v: &str) -> bool {
    match e {
        Expr::Var(n) => n == v,
        Expr::Neg(a) => contains(a, v),
        Expr::Add(a, b) | Expr::Sub(a, b) | Expr::Mul(a, b) | Expr::Div(a, b) | Expr::Pow(a, b) => {
            contains(a, v) || contains(b, v)
        }
        Expr::Call(_, a) | Expr::Tuple(a) | Expr::List(a) => a.iter().any(|e| contains(e, v)),
        _ => false,
    }
}

pub(crate) fn substitute(e: &Expr, v: &str, value: &Expr) -> Expr {
    match e {
        Expr::Var(n) if n == v => value.clone(),
        Expr::Neg(a) => Expr::Neg(Box::new(substitute(a, v, value))),
        Expr::Add(a, b) => Expr::Add(
            Box::new(substitute(a, v, value)),
            Box::new(substitute(b, v, value)),
        ),
        Expr::Sub(a, b) => Expr::Sub(
            Box::new(substitute(a, v, value)),
            Box::new(substitute(b, v, value)),
        ),
        Expr::Mul(a, b) => Expr::Mul(
            Box::new(substitute(a, v, value)),
            Box::new(substitute(b, v, value)),
        ),
        Expr::Div(a, b) => Expr::Div(
            Box::new(substitute(a, v, value)),
            Box::new(substitute(b, v, value)),
        ),
        Expr::Pow(a, b) => Expr::Pow(
            Box::new(substitute(a, v, value)),
            Box::new(substitute(b, v, value)),
        ),
        Expr::Call(n, a) => Expr::Call(
            n.clone(),
            a.iter().map(|e| substitute(e, v, value)).collect(),
        ),
        Expr::Tuple(a) => Expr::Tuple(a.iter().map(|e| substitute(e, v, value)).collect()),
        Expr::List(a) => Expr::List(a.iter().map(|e| substitute(e, v, value)).collect()),
        _ => e.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn rationals() {
        assert_eq!(rat(4, -8), Expr::Rational(-1, 2));
        assert_eq!(rat(0, 7), Expr::Rational(0, 1));
        assert!(matches!(rat(i64::MAX as i128 + 1, 1), Expr::Real(_)));
    }
    #[test]
    fn substitution() {
        let e = Expr::Var("x".into());
        assert!(contains(&e, "x"));
        assert_eq!(substitute(&e, "x", &rat(2, 1)), rat(2, 1));
    }
    #[test]
    fn extreme_rationals() {
        assert!(matches!(rat(1, i128::MIN), Expr::Real(_)));
        assert!(matches!(rat(i128::MIN, -1), Expr::Real(_)));
        assert_eq!(rat(i128::MIN, i128::MIN), Expr::Rational(1, 1));
        assert_eq!(rat(i64::MIN as i128, 1), Expr::Rational(i64::MIN, 1));
    }
}
