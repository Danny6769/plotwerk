use super::ast::{Constant, Expr};
use std::collections::HashMap;
#[derive(Clone, Debug, PartialEq)]
pub struct EvalError(pub String);
pub trait Context {
    fn value(&self, name: &str) -> Option<f64>;
    fn function(&self, name: &str) -> Option<(Vec<String>, Expr)>;
}
#[derive(Clone, Debug)]
enum Node {
    C(f64),
    V(usize),
    Neg(Box<Node>),
    Bin(u8, Box<Node>, Box<Node>),
    Fun(String, Vec<Node>),
    Logic(String, Vec<Node>),
    Select(Vec<(Node, Node)>, Option<Box<Node>>),
}
impl Node {
    fn size(&self) -> usize {
        match self {
            Self::Neg(a) => 1 + a.size(),
            Self::Bin(_, a, b) => 1 + a.size() + b.size(),
            Self::Fun(_, a) => 1 + a.iter().map(Self::size).sum::<usize>(),
            Self::Logic(_, a) => 1 + a.iter().map(Self::size).sum::<usize>(),
            Self::Select(a, b) => {
                1 + a.iter().map(|(c, v)| c.size() + v.size()).sum::<usize>()
                    + b.as_ref().map_or(0, |b| b.size())
            }
            _ => 1,
        }
    }
    #[inline]
    fn run(&self, a: &[f64]) -> f64 {
        match self {
            Self::C(v) => *v,
            Self::V(i) => a.get(*i).copied().unwrap_or(f64::NAN),
            Self::Neg(n) => -n.run(a),
            Self::Logic(n, v) => {
                let x = v[0].run(a);
                if n == "and" && x == 0.0 {
                    return 0.0;
                }
                if n == "or" && !x.is_nan() && x != 0.0 {
                    return 1.0;
                }
                super::logic::operation(n, x, v.get(1).map_or(0.0, |n| n.run(a)))
            }
            Self::Select(branches, otherwise) => {
                for (condition, value) in branches {
                    let c = condition.run(a);
                    if c.is_nan() {
                        return f64::NAN;
                    }
                    if c != 0.0 {
                        return value.run(a);
                    }
                }
                otherwise.as_ref().map_or(f64::NAN, |b| b.run(a))
            }
            Self::Bin(op, l, r) => {
                let (x, y) = (l.run(a), r.run(a));
                match op {
                    0 => x + y,
                    1 => x - y,
                    2 => x * y,
                    3 => x / y,
                    _ => x.powf(y),
                }
            }
            Self::Fun(n, v) => {
                if let Some(count) = super::stats::builtin_arity(n) {
                    let mut args = [0.0; 4];
                    for (slot, node) in args.iter_mut().zip(v) {
                        *slot = node.run(a);
                    }
                    return super::stats::builtin(n, &args[..count]);
                }
                let x = v.first().map_or(f64::NAN, |n| n.run(a));
                let y = v.get(1).map_or(f64::NAN, |n| n.run(a));
                if n == "min" {
                    v.iter().map(|n| n.run(a)).fold(f64::INFINITY, |x, y| {
                        if x.is_nan() || y.is_nan() {
                            f64::NAN
                        } else {
                            x.min(y)
                        }
                    })
                } else if n == "max" {
                    v.iter().map(|n| n.run(a)).fold(f64::NEG_INFINITY, |x, y| {
                        if x.is_nan() || y.is_nan() {
                            f64::NAN
                        } else {
                            x.max(y)
                        }
                    })
                } else {
                    function(n, x, y, v.len())
                }
            }
        }
    }
}
pub(crate) fn function(n: &str, x: f64, y: f64, argc: usize) -> f64 {
    if x.is_nan() || (argc == 2 && y.is_nan()) {
        return f64::NAN;
    }
    match n {
        "sin" => x.sin(),
        "cos" => x.cos(),
        "tan" => x.tan(),
        "cot" => 1.0 / x.tan(),
        "sec" => 1.0 / x.cos(),
        "csc" => 1.0 / x.sin(),
        "asin" | "arcsin" => x.asin(),
        "acos" | "arccos" => x.acos(),
        "atan" | "arctan" => x.atan(),
        "atan2" => x.atan2(y),
        "sinh" => x.sinh(),
        "cosh" => x.cosh(),
        "tanh" => x.tanh(),
        "asinh" | "arcsinh" => x.asinh(),
        "acosh" | "arccosh" => x.acosh(),
        "atanh" | "arctanh" => x.atanh(),
        "exp" => x.exp(),
        "ln" => x.ln(),
        "log" if argc == 2 => {
            if x <= 0.0 || x == 1.0 {
                f64::NAN
            } else {
                y.log(x)
            }
        }
        "log" | "lg" => x.log10(),
        "ld" => x.log2(),
        "sqrt" => x.sqrt(),
        "cbrt" => x.cbrt(),
        "nroot" if y == 0.0 => f64::NAN,
        "nroot" => {
            if x < 0.0 && y.fract() == 0.0 && y.abs() % 2.0 == 1.0 {
                -(-x).powf(1.0 / y)
            } else {
                x.powf(1.0 / y)
            }
        }
        "abs" => x.abs(),
        "sgn" | "sign" => {
            if x == 0.0 {
                0.0
            } else {
                x.signum()
            }
        }
        "floor" => x.floor(),
        "ceil" => x.ceil(),
        "round" => x.round(),
        "min" => x.min(y),
        "max" => x.max(y),
        _ => f64::NAN,
    }
}
pub(crate) fn arity(n: &str, len: usize) -> bool {
    if super::logic::builtin(n) {
        return super::logic::arity(n, len);
    }
    if let Some(count) = super::stats::builtin_arity(n) {
        return len == count;
    }
    match n {
        "atan2" | "nroot" => len == 2,
        "log" => len == 1 || len == 2,
        "min" | "max" => len > 0,
        _ => super::parser::builtin(n) && len == 1,
    }
}
#[derive(Clone, Debug)]
pub struct Compiled {
    root: Node,
}
impl Compiled {
    #[inline]
    pub fn eval(&self, args: &[f64]) -> f64 {
        self.root.run(args)
    }
}
fn build(
    e: &Expr,
    vars: &[&str],
    ctx: &dyn Context,
    bindings: &HashMap<String, Node>,
    depth: usize,
    calls: usize,
    budget: &mut usize,
) -> Result<Node, EvalError> {
    if *budget == 0 {
        return Err(EvalError("Ausdruck zu groß".into()));
    }
    *budget -= 1;
    if depth > 128 {
        return Err(EvalError("Ausdruck zu tief verschachtelt".into()));
    }
    let node = match e {
        Expr::Rational(n, d) => Node::C(*n as f64 / *d as f64),
        Expr::Real(v) => Node::C(*v),
        Expr::Const(c) => Node::C(match c {
            Constant::Pi => std::f64::consts::PI,
            Constant::E => std::f64::consts::E,
        }),
        Expr::Var(n) => {
            if matches!(n.as_str(), "inf" | "unendlich") {
                return Ok(Node::C(f64::INFINITY));
            }
            if matches!(
                n.to_lowercase().as_str(),
                "true" | "wahr" | "false" | "falsch"
            ) {
                return Ok(Node::C(
                    if matches!(n.to_lowercase().as_str(), "true" | "wahr") {
                        1.0
                    } else {
                        0.0
                    },
                ));
            }
            if let Some(b) = bindings.get(n) {
                let cost = b.size();
                if cost > *budget {
                    return Err(EvalError("Ausdruck zu groß".into()));
                }
                *budget -= cost;
                b.clone()
            } else if let Some(i) = vars.iter().position(|v| v == n) {
                Node::V(i)
            } else {
                Node::C(
                    ctx.value(n)
                        .ok_or_else(|| EvalError(format!("Unbekannte Variable: {n}")))?,
                )
            }
        }
        Expr::Neg(a) => {
            let a = build(a, vars, ctx, bindings, depth + 1, calls, budget)?;
            if let Node::C(v) = a {
                Node::C(-v)
            } else {
                Node::Neg(Box::new(a))
            }
        }
        Expr::Add(a, b) | Expr::Sub(a, b) | Expr::Mul(a, b) | Expr::Div(a, b) | Expr::Pow(a, b) => {
            let op = match e {
                Expr::Add(..) => 0,
                Expr::Sub(..) => 1,
                Expr::Mul(..) => 2,
                Expr::Div(..) => 3,
                _ => 4,
            };
            let a = build(a, vars, ctx, bindings, depth + 1, calls, budget)?;
            let b = build(b, vars, ctx, bindings, depth + 1, calls, budget)?;
            let n = Node::Bin(op, Box::new(a), Box::new(b));
            if let Node::Bin(_, a, b) = &n {
                if matches!(**a, Node::C(_)) && matches!(**b, Node::C(_)) {
                    Node::C(n.run(&[]))
                } else {
                    n
                }
            } else {
                n
            }
        }
        Expr::Call(n, args) => {
            if matches!(n.as_str(), "and" | "or") && args.len() == 2 {
                let left = build(&args[0], vars, ctx, bindings, depth + 1, calls, budget)?;
                if let Node::C(x) = left {
                    if x.is_nan() {
                        return Ok(Node::C(x));
                    }
                    if n == "and" && x == 0.0 {
                        return Ok(Node::C(0.0));
                    }
                    if n == "or" && x != 0.0 {
                        return Ok(Node::C(1.0));
                    }
                }
                let right = build(&args[1], vars, ctx, bindings, depth + 1, calls, budget)?;
                let constant = matches!(left, Node::C(_)) && matches!(right, Node::C(_));
                let node = Node::Logic(n.clone(), vec![left, right]);
                return Ok(if constant {
                    Node::C(node.run(&[]))
                } else {
                    node
                });
            }
            if super::logic::conditional(n) {
                if args.len() < 2 {
                    return Err(EvalError("Ungültige Argumentanzahl".into()));
                }
                let mut branches = Vec::new();
                let mut otherwise = None;
                for pair in args[..args.len() / 2 * 2].chunks_exact(2) {
                    let c = build(&pair[0], vars, ctx, bindings, depth + 1, calls, budget)?;
                    if let Node::C(v) = c {
                        if v.is_nan() {
                            otherwise = Some(Box::new(Node::C(f64::NAN)));
                            break;
                        }
                        if v == 0.0 {
                            continue;
                        }
                        otherwise = Some(Box::new(build(
                            &pair[1],
                            vars,
                            ctx,
                            bindings,
                            depth + 1,
                            calls,
                            budget,
                        )?));
                        break;
                    }
                    branches.push((
                        c,
                        build(&pair[1], vars, ctx, bindings, depth + 1, calls, budget)?,
                    ));
                }
                if otherwise.is_none() && args.len() % 2 == 1 {
                    otherwise = Some(Box::new(build(
                        args.last().unwrap(),
                        vars,
                        ctx,
                        bindings,
                        depth + 1,
                        calls,
                        budget,
                    )?));
                }
                return Ok(if branches.is_empty() {
                    otherwise.map_or(Node::C(f64::NAN), |b| *b)
                } else {
                    Node::Select(branches, otherwise)
                });
            }
            let a = args
                .iter()
                .map(|e| build(e, vars, ctx, bindings, depth + 1, calls, budget))
                .collect::<Result<Vec<_>, _>>()?;
            if super::logic::builtin(n) {
                if !arity(n, a.len()) {
                    return Err(EvalError("Ungültige Argumentanzahl".into()));
                }
                let constant = a.iter().all(|n| matches!(n, Node::C(_)));
                let node = Node::Logic(n.clone(), a);
                return Ok(if constant {
                    Node::C(node.run(&[]))
                } else {
                    node
                });
            }
            if super::parser::builtin(n) {
                if !arity(n, a.len()) {
                    return Err(EvalError(format!("Ungültige Argumentanzahl: {n}")));
                }
                let constant = a.iter().all(|v| matches!(v, Node::C(_)));
                let node = Node::Fun(n.clone(), a);
                if constant {
                    Node::C(node.run(&[]))
                } else {
                    node
                }
            } else if let Some((params, body)) = ctx.function(n) {
                if calls >= 32 {
                    return Err(EvalError("Rekursionstiefe überschritten".into()));
                }
                if params.len() != a.len() {
                    return Err(EvalError(format!("Ungültige Argumentanzahl: {n}")));
                }
                let b = params.into_iter().zip(a).collect();
                build(&body, vars, ctx, &b, depth + 1, calls + 1, budget)?
            } else {
                return Err(EvalError(format!("Unbekannte Funktion: {n}")));
            }
        }
        _ => return Err(EvalError("Kein numerischer Ausdruck".into())),
    };
    Ok(node)
}
pub fn compile(expr: &Expr, vars: &[&str], ctx: &dyn Context) -> Result<Compiled, EvalError> {
    Ok(Compiled {
        root: build(expr, vars, ctx, &HashMap::new(), 0, 0, &mut 100_000)?,
    })
}
pub fn eval(expr: &Expr, ctx: &dyn Context) -> Result<f64, EvalError> {
    Ok(compile(expr, &[], ctx)?.eval(&[]))
}
pub(crate) struct Empty;
impl Context for Empty {
    fn value(&self, _: &str) -> Option<f64> {
        None
    }
    fn function(&self, _: &str) -> Option<(Vec<String>, Expr)> {
        None
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_expr;
    #[test]
    fn evaluation() {
        for (s, want) in [
            ("2^3^2", 512.0),
            ("-2^2", -4.0),
            ("2^-1", 0.5),
            ("sin(pi/2)", 1.0),
            ("log(2,8)", 3.0),
            ("nroot(-8,3)", -2.0),
        ] {
            assert!((eval(&parse_expr(s).unwrap(), &Empty).unwrap() - want).abs() < 1e-10);
        }
    }
    #[test]
    fn slots_and_domains() {
        let c = compile(&parse_expr("x^2+y").unwrap(), &["x", "y"], &Empty).unwrap();
        assert_eq!(c.eval(&[3.0, 2.0]), 11.0);
        assert!(
            eval(&parse_expr("sqrt(-1)").unwrap(), &Empty)
                .unwrap()
                .is_nan()
        );
        assert_eq!(
            eval(&parse_expr("ln(0)").unwrap(), &Empty).unwrap(),
            f64::NEG_INFINITY
        );
        assert_eq!(
            compile(&Expr::Var("a".into()), &[], &Empty).unwrap_err().0,
            "Unbekannte Variable: a"
        );
    }
    #[test]
    fn propagates_domains() {
        for s in [
            "min(sqrt(-1),2)",
            "max(2,sqrt(-1))",
            "nroot(2,0)",
            "log(1,2)",
        ] {
            assert!(
                eval(&parse_expr(s).unwrap(), &Empty).unwrap().is_nan(),
                "{s}"
            );
        }
        fn thread_safe<T: Send + Sync>() {}
        thread_safe::<Compiled>();
    }
    #[test]
    fn inline_functions() {
        let mut ctx = crate::cas::CasEnv::default();
        ctx.values.insert("a".into(), Expr::Rational(3, 1));
        ctx.functions
            .insert("f".into(), (vec!["t".into()], parse_expr("t^2+a").unwrap()));
        let c = compile(&parse_expr("f(x)").unwrap(), &["x"], &ctx).unwrap();
        assert_eq!(c.eval(&[2.0]), 7.0);
        ctx.functions
            .insert("f".into(), (vec!["t".into()], parse_expr("f(t)").unwrap()));
        assert!(compile(&parse_expr("f(x)").unwrap(), &["x"], &ctx).is_err());
    }
    #[test]
    #[ignore]
    fn throughput() {
        let c = compile(&parse_expr("x^2+2x+1").unwrap(), &["x"], &Empty).unwrap();
        let start = std::time::Instant::now();
        let mut sum = 0.0;
        let n = 20_000_000;
        for i in 0..n {
            sum += std::hint::black_box(&c).eval(&[std::hint::black_box(i as f64 * 1e-6)]);
        }
        let rate = n as f64 / start.elapsed().as_secs_f64();
        eprintln!("{rate:.0} evaluations/s ({sum})");
        assert!(rate >= 20_000_000.0);
    }
}

#[cfg(test)]
mod stats_tests {
    use super::*;
    use crate::parser::parse_expr;
    #[test]
    fn probability_builtins() {
        for (input, want) in [
            ("5!", 120.0),
            ("-3!", -6.0),
            ("2^3!", 64.0),
            ("(2+3)!", 120.0),
            ("nCr(10,3)", 120.0),
            ("binomial(10,3)", 120.0),
            ("binompdf(10,0.3,3)", 0.266827932),
            ("binomcdf(10,0.3,3)", 0.6496107184),
            ("normalcdf(0,1,1.96)", 0.9750021048517795),
            ("normalpdf(0,1,0)", 0.3989422804014327),
            ("invnorm(0,1,0.975)", 1.959963984540054),
            ("poissoncdf(4,2)", 0.2381033055535443),
            ("geometricpdf(0.25,3)", 0.140625),
            ("exponentialcdf(2,1)", 0.8646647167633873),
            ("uniformcdf(1,5,2)", 0.25),
            ("invt(10,0.975)", 2.228138851986274),
            ("invchi2(2,0.95)", 5.991464547107982),
        ] {
            let value = eval(&parse_expr(input).unwrap(), &Empty).unwrap();
            assert!((value - want).abs() < 1e-12, "{input}: {value}");
        }
        let compiled = compile(
            &parse_expr("normalpdf(0,1,x)+binompdf(10,p,3)").unwrap(),
            &["x", "p"],
            &Empty,
        )
        .unwrap();
        assert!((compiled.eval(&[0.0, 0.3]) - 0.6657702124014327).abs() < 1e-12);
        let hyper = compile(
            &parse_expr("hypergeometricpdf(20,7,5,x)").unwrap(),
            &["x"],
            &Empty,
        )
        .unwrap();
        assert!(
            (hyper.eval(&[2.0]) - crate::stats::hypergeometric_pmf(20.0, 7.0, 5.0, 2.0)).abs()
                < 1e-15
        );
        assert!(compile(&parse_expr("normalcdf(0,1)").unwrap(), &[], &Empty).is_err());
        assert!(eval(&parse_expr("{1,2}").unwrap(), &Empty).is_err());
    }
}
