use super::ast::{Expr, Statement, substitute};
use super::eval::{Context, Empty, compile, eval};
use super::format::{format_expr, format_number};
use super::simplify::{int, simplify, sub};
use std::collections::{BTreeSet, HashMap};
#[derive(Clone, Debug, Default)]
pub struct CasEnv {
    pub values: HashMap<String, Expr>,
    pub functions: HashMap<String, (Vec<String>, Expr)>,
}
impl Context for CasEnv {
    fn value(&self, name: &str) -> Option<f64> {
        let e = self.values.get(name)?;
        let e = process(e, self, &HashMap::new(), 0).ok()?;
        eval(&e, &Empty).ok()
    }
    fn function(&self, name: &str) -> Option<(Vec<String>, Expr)> {
        self.functions.get(name).cloned()
    }
}
fn free(e: &Expr, out: &mut BTreeSet<String>) {
    match e {
        Expr::Var(n) => {
            out.insert(n.clone());
        }
        Expr::Neg(a) => free(a, out),
        Expr::Add(a, b) | Expr::Sub(a, b) | Expr::Mul(a, b) | Expr::Div(a, b) | Expr::Pow(a, b) => {
            free(a, out);
            free(b, out)
        }
        Expr::Call(_, a) | Expr::List(a) | Expr::Tuple(a) => {
            for e in a {
                free(e, out)
            }
        }
        _ => {}
    }
}
fn default_var(e: &Expr) -> String {
    let mut s = BTreeSet::new();
    free(e, &mut s);
    if s.len() == 1 {
        s.into_iter().next().unwrap()
    } else {
        "x".into()
    }
}
fn variable(e: &Expr) -> Result<String, String> {
    if let Expr::Var(n) = e {
        Ok(n.clone())
    } else {
        Err("Variable erwartet".into())
    }
}
fn equation(e: &Expr) -> (Expr, Expr) {
    if let Expr::Call(n, a) = e {
        if n == "=" && a.len() == 2 {
            return (a[0].clone(), a[1].clone());
        }
    }
    (e.clone(), int(0))
}
fn command(n: &str) -> Option<&'static str> {
    match n.to_lowercase().as_str() {
        "simplify" | "vereinfache" => Some("simplify"),
        "expand" | "ausmultiplizieren" | "expandiere" => Some("expand"),
        "factor" | "faktorisiere" => Some("factor"),
        "derivative" | "ableitung" => Some("derivative"),
        "integral" => Some("integral"),
        "solve" | "löse" | "loese" => Some("solve"),
        "nsolve" | "nlöse" | "nloese" => Some("nsolve"),
        "numeric" | "numerisch" | "n" => Some("numeric"),
        "substitute" | "ersetze" => Some("substitute"),
        "limit" | "grenzwert" => Some("limit"),
        _ => None,
    }
}
fn symbolic_body(
    e: &Expr,
    env: &CasEnv,
    b: &HashMap<String, Expr>,
) -> (Expr, HashMap<String, Expr>) {
    let mut local = b.clone();
    if let Expr::Var(n) = e {
        if let Some((params, body)) = env.functions.get(n) {
            for p in params {
                local.insert(p.clone(), Expr::Var(p.clone()));
            }
            return (body.clone(), local);
        }
    }
    (e.clone(), local)
}
fn approximate_expr(e: &Expr) -> Expr {
    if let Ok(value) = eval(e, &Empty) {
        return Expr::Real(value);
    }
    use Expr::*;
    simplify(&match e {
        Neg(a) => Neg(Box::new(approximate_expr(a))),
        Add(a, b) => Add(Box::new(approximate_expr(a)), Box::new(approximate_expr(b))),
        Sub(a, b) => Sub(Box::new(approximate_expr(a)), Box::new(approximate_expr(b))),
        Mul(a, b) => Mul(Box::new(approximate_expr(a)), Box::new(approximate_expr(b))),
        Div(a, b) => Div(Box::new(approximate_expr(a)), Box::new(approximate_expr(b))),
        Pow(a, b) => Pow(Box::new(approximate_expr(a)), Box::new(approximate_expr(b))),
        Call(n, a) => Call(n.clone(), a.iter().map(approximate_expr).collect()),
        _ => e.clone(),
    })
}
fn execute(
    cmd: &str,
    a: &[Expr],
    env: &CasEnv,
    b: &HashMap<String, Expr>,
    depth: usize,
) -> Result<Expr, String> {
    let bad = || "Ungültige Argumentanzahl".to_string();
    if a.is_empty() {
        return Err(bad());
    }
    let p = |e: &Expr| process(e, env, b, depth + 1);
    match cmd {
        "simplify" | "expand" | "factor" | "numeric" => {
            if a.len() != 1 {
                return Err(bad());
            }
            let e = p(&a[0])?;
            Ok(match cmd {
                "simplify" => simplify(&e),
                "expand" => super::simplify::expand(&e),
                "factor" => super::simplify::factor(&e, &default_var(&e)),
                _ => approximate_expr(&e),
            })
        }
        "derivative" => {
            if a.len() > 3 {
                return Err(bad());
            }
            let (raw, mut local) = symbolic_body(&a[0], env, b);
            let v = if a.len() > 1 {
                variable(&a[1])?
            } else {
                default_var(&process(&raw, env, &local, depth + 1)?)
            };
            local.insert(v.clone(), Expr::Var(v.clone()));
            let mut e = process(&raw, env, &local, depth + 1)?;
            let order = if a.len() == 3 {
                let n = eval(&p(&a[2])?, &Empty).map_err(|e| e.0)?;
                if !n.is_finite() || n.fract() != 0.0 || !(0.0..=32.0).contains(&n) {
                    return Err("Ungültige Ableitungsordnung".into());
                }
                n as usize
            } else {
                1
            };
            for _ in 0..order {
                e = super::calculus::derivative(&e, &v);
            }
            Ok(e)
        }
        "integral" => {
            if a.len() > 4 {
                return Err(bad());
            }
            let (raw, mut local) = symbolic_body(&a[0], env, b);
            let explicit = a.len() == 2 || a.len() == 4;
            let v = if explicit {
                variable(&a[1])?
            } else {
                default_var(&process(&raw, env, &local, depth + 1)?)
            };
            local.insert(v.clone(), Expr::Var(v.clone()));
            let e = process(&raw, env, &local, depth + 1)?;
            let anti = super::calculus::integrate(&e, &v);
            if a.len() < 3 {
                return anti.ok_or_else(|| "Keine Stammfunktion gefunden".into());
            }
            let i = if explicit { 2 } else { 1 };
            let lo = p(&a[i])?;
            let hi = p(&a[i + 1])?;
            let lon = eval(&lo, &Empty).ok();
            let hin = eval(&hi, &Empty).ok();
            let c = compile(&e, &[&v], &Empty).ok();
            if lo == hi || lon.is_some() && lon == hin {
                return Ok(int(0));
            }
            if lon.zip(hin).zip(c.as_ref()).is_some_and(|((lon, hin), c)| {
                (0..=32).any(|i| !c.eval(&[lon + (hin - lon) * i as f64 / 32.0]).is_finite())
            }) {
                return Err("Integral nicht definiert".into());
            }
            if let Some(f) = anti {
                let upper = simplify(&substitute(&f, &v, &hi));
                let lower = simplify(&substitute(&f, &v, &lo));
                let result = simplify(&sub(upper, lower));
                let mut variables = BTreeSet::new();
                free(&result, &mut variables);
                if !variables.is_empty() || eval(&result, &Empty).is_ok_and(|x| x.is_finite()) {
                    return Ok(result);
                }
            }
            let (Some(lon), Some(hin), Some(c)) = (lon, hin, c) else {
                return Err("Keine Stammfunktion gefunden".into());
            };
            let y = super::numeric::integrate_numeric(&|x| c.eval(&[x]), lon, hin);
            if !y.is_finite() {
                return Err("Integral nicht definiert".into());
            }
            Ok(Expr::Real(y))
        }
        "solve" | "nsolve" => {
            if a.len() > 2 {
                return Err(bad());
            }
            if a.len() == 2 && matches!(a[0], Expr::List(_)) && matches!(a[1], Expr::List(_)) {
                let Expr::List(names) = &a[1] else {
                    unreachable!()
                };
                let mut local = b.clone();
                for name in names {
                    let v = variable(name)?;
                    local.insert(v.clone(), Expr::Var(v));
                }
                let equations = process(&a[0], env, &local, depth + 1)?;
                return super::systems::solve(&equations, &a[1], cmd == "nsolve");
            }
            let (raw, mut local) = symbolic_body(&a[0], env, b);
            let v = if a.len() == 2 {
                variable(&a[1])?
            } else {
                default_var(&process(&raw, env, &local, depth + 1)?)
            };
            local.insert(v.clone(), Expr::Var(v.clone()));
            let e = process(&raw, env, &local, depth + 1)?;
            if matches!(&e,Expr::Call(n,_) if super::logic::comparison(n)||matches!(n.as_str(),"and"|"or"))
            {
                return super::inequalities::solve(&e, &v);
            }
            let (l, r) = equation(&e);
            let roots = super::solve::solve(&l, &r, &v)?;
            let mut out = Vec::new();
            for root in roots {
                let e = match root {
                    super::solve::Solution::Exact(e) if cmd == "solve" => e,
                    super::solve::Solution::Exact(e) => approximate_expr(&e),
                    super::solve::Solution::Approx(x) => Expr::Real(x),
                };
                out.push(Expr::Call("=".into(), vec![Expr::Var(v.clone()), e]));
            }
            Ok(Expr::List(out))
        }
        "substitute" => {
            if a.len() != 3 {
                return Err(bad());
            }
            let v = variable(&a[1])?;
            let mut local = b.clone();
            local.insert(v.clone(), Expr::Var(v.clone()));
            let e = process(&a[0], env, &local, depth + 1)?;
            Ok(simplify(&substitute(&e, &v, &p(&a[2])?)))
        }
        "limit" => {
            if a.len() != 3 {
                return Err(bad());
            }
            let v = variable(&a[1])?;
            let mut local = b.clone();
            local.insert(v.clone(), Expr::Var(v.clone()));
            let e = process(&a[0], env, &local, depth + 1)?;
            let at = eval(&p(&a[2])?, &Empty).map_err(|e| e.0)?;
            let c = compile(&e, &[&v], &Empty).map_err(|e| e.0)?;
            let h = 1e-7 * (1.0 + at.abs());
            let l = c.eval(&[at - h]);
            let r = c.eval(&[at + h]);
            if l.is_finite() && r.is_finite() && (l - r).abs() < 1e-4 * (1.0 + l.abs()) {
                Ok(Expr::Real((l + r) / 2.0))
            } else {
                Err("Grenzwert nicht gefunden".into())
            }
        }
        _ => Err("Unbekannter Befehl".into()),
    }
}
fn process(
    e: &Expr,
    env: &CasEnv,
    b: &HashMap<String, Expr>,
    depth: usize,
) -> Result<Expr, String> {
    if depth > 64 {
        return Err("Rekursionstiefe überschritten".into());
    }
    let p = |e: &Expr| process(e, env, b, depth + 1);
    Ok(match e {
        Expr::Var(n) => {
            if matches!(n.as_str(), "inf" | "unendlich") {
                return Ok(Expr::Real(f64::INFINITY));
            }
            if let Some(e) = b.get(n) {
                e.clone()
            } else if let Some(e) = env.values.get(n) {
                p(e)?
            } else {
                e.clone()
            }
        }
        Expr::Neg(a) => Expr::Neg(Box::new(p(a)?)),
        Expr::Add(a, c) => {
            let (a, c) = (p(a)?, p(c)?);
            if let Some(value) = super::matrices::binary("+", a.clone(), c.clone()) {
                value?
            } else {
                Expr::Add(Box::new(a), Box::new(c))
            }
        }
        Expr::Sub(a, c) => {
            let (a, c) = (p(a)?, p(c)?);
            if let Some(value) = super::matrices::binary("-", a.clone(), c.clone()) {
                value?
            } else {
                Expr::Sub(Box::new(a), Box::new(c))
            }
        }
        Expr::Mul(a, c) => {
            let (a, c) = (p(a)?, p(c)?);
            if let Some(value) = super::matrices::binary("*", a.clone(), c.clone()) {
                value?
            } else {
                Expr::Mul(Box::new(a), Box::new(c))
            }
        }
        Expr::Div(a, c) => Expr::Div(Box::new(p(a)?), Box::new(p(c)?)),
        Expr::Pow(a, c) => {
            let (a, c) = (p(a)?, p(c)?);
            if let Some(value) = super::matrices::binary("^", a.clone(), c.clone()) {
                value?
            } else {
                Expr::Pow(Box::new(a), Box::new(c))
            }
        }
        Expr::Call(n, a) => {
            if super::logic::conditional(n) {
                if a.len() < 2 {
                    return Err("Ungültige Argumentanzahl".into());
                }
                let mut branches = Vec::new();
                for pair in a[..a.len() / 2 * 2].chunks_exact(2) {
                    let condition = simplify(&p(&pair[0])?);
                    if let Ok(value) = eval(&condition, &Empty) {
                        if value.is_nan() {
                            return Ok(Expr::Real(f64::NAN));
                        }
                        if value == 0.0 {
                            continue;
                        }
                        let value = p(&pair[1])?;
                        if branches.is_empty() {
                            return Ok(value);
                        }
                        branches.push(value);
                        return Ok(Expr::Call("Wenn".into(), branches));
                    }
                    branches.push(condition);
                    branches.push(p(&pair[1])?);
                }
                if a.len() % 2 == 1 {
                    let value = p(a.last().unwrap())?;
                    if branches.is_empty() {
                        return Ok(value);
                    }
                    branches.push(value);
                }
                if branches.is_empty() {
                    return Ok(Expr::Real(f64::NAN));
                }
                Expr::Call("Wenn".into(), branches)
            } else if let Some(cmd) = super::matrices::command(n) {
                let args = a.iter().map(p).collect::<Result<Vec<_>, _>>()?;
                super::matrices::execute(cmd, &args)?
            } else if let Some(cmd) = super::advanced::command(n, a.len()) {
                let mut local = b.clone();
                let mut arguments = Vec::new();
                if matches!(cmd, "sum" | "product" | "sequence") && a.len() >= 2 {
                    let v = variable(&a[1])?;
                    local.insert(v.clone(), Expr::Var(v));
                    for (i, e) in a.iter().enumerate() {
                        arguments.push(if i == 1 {
                            e.clone()
                        } else if i == 0 {
                            process(e, env, &local, depth + 1)?
                        } else {
                            p(e)?
                        });
                    }
                } else if matches!(
                    cmd,
                    "taylor"
                        | "limit"
                        | "below"
                        | "above"
                        | "asymptote"
                        | "root"
                        | "extremum"
                        | "inflection"
                ) && !a.is_empty()
                {
                    let (raw, scope) = symbolic_body(&a[0], env, b);
                    local = scope;
                    let v = if matches!(cmd, "limit" | "below" | "above") && a.len() == 3 {
                        variable(&a[1])?
                    } else {
                        default_var(&raw)
                    };
                    local.insert(v.clone(), Expr::Var(v));
                    arguments.push(process(&raw, env, &local, depth + 1)?);
                    for (i, e) in a.iter().enumerate().skip(1) {
                        arguments.push(
                            if i == 1 && matches!(cmd, "limit" | "below" | "above") && a.len() == 3
                            {
                                e.clone()
                            } else {
                                p(e)?
                            },
                        );
                    }
                } else {
                    arguments = a.iter().map(p).collect::<Result<_, _>>()?;
                }
                super::advanced::execute(cmd, &arguments)?
            } else if let Some(cmd) = super::stats_cas::command(n) {
                let arguments = a.iter().map(p).collect::<Result<Vec<_>, _>>()?;
                super::stats_cas::execute(cmd, &arguments)?
            } else if let Some(cmd) = command(n) {
                execute(cmd, a, env, b, depth + 1)?
            } else if let Some((params, body)) = env.functions.get(n) {
                if a.len() != params.len() {
                    return Err(format!("Ungültige Argumentanzahl: {n}"));
                }
                if depth >= 32 {
                    return Err("Rekursionstiefe überschritten".into());
                }
                let args = a.iter().map(p).collect::<Result<Vec<_>, _>>()?;
                let local = params.iter().cloned().zip(args).collect();
                process(body, env, &local, depth + 1)?
            } else {
                if super::parser::builtin(n) && !super::eval::arity(n, a.len()) {
                    return Err(format!("Ungültige Argumentanzahl: {n}"));
                }
                Expr::Call(n.clone(), a.iter().map(p).collect::<Result<_, _>>()?)
            }
        }
        Expr::Tuple(a) => Expr::Tuple(a.iter().map(p).collect::<Result<_, _>>()?),
        Expr::List(a) => Expr::List(a.iter().map(p).collect::<Result<_, _>>()?),
        _ => e.clone(),
    })
}
fn output(e: &Expr) -> String {
    match e {
        Expr::Real(v) => format_number(*v),
        Expr::List(a) => format!(
            "{{{}}}",
            a.iter().map(output).collect::<Vec<_>>().join(", ")
        ),
        Expr::Tuple(a) => format!("({})", a.iter().map(output).collect::<Vec<_>>().join(", ")),
        Expr::Call(n, a) if n == "=" && a.len() == 2 => {
            format!("{} = {}", format_expr(&a[0]), output(&a[1]))
        }
        _ => format_expr(e),
    }
}
pub fn cas_eval(input: &str, env: &mut CasEnv) -> Result<String, String> {
    let statement = super::parser::parse_statement(input)
        .map_err(|e| format!("{} (Position {})", e.message, e.position))?;
    match statement {
        Statement::Assign {
            name,
            params,
            value,
        } => {
            let local = params
                .iter()
                .map(|n| (n.clone(), Expr::Var(n.clone())))
                .collect();
            let e = simplify(&process(&value, env, &local, 0)?);
            if params.is_empty() {
                env.values.insert(name, e.clone());
                Ok(output(&e))
            } else {
                env.functions
                    .insert(name.clone(), (params.clone(), e.clone()));
                Ok(format!(
                    "{}({}) = {}",
                    name,
                    params.join(", "),
                    format_expr(&e)
                ))
            }
        }
        Statement::Expr(e) => {
            let precision =
                matches!(&e,Expr::Call(n,a) if super::advanced::command(n,a.len())==Some("round"));
            let result = simplify(&process(&e, env, &HashMap::new(), 0)?);
            Ok(if precision {
                format_expr(&result)
            } else {
                output(&result)
            })
        }
        Statement::Equation { lhs, rhs } => {
            let l = simplify(&process(&lhs, env, &HashMap::new(), 0)?);
            let r = simplify(&process(&rhs, env, &HashMap::new(), 0)?);
            Ok(format!("{} = {}", output(&l), output(&r)))
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn end_to_end() {
        let mut e = CasEnv::default();
        for (input, want) in [
            ("a:=5", "5"),
            ("a+2", "7"),
            ("f(x):=x^2", "f(x) = x^2"),
            ("f(3)", "9"),
            ("Derivative(f)", "2x"),
            ("Ableitung(x^3,x,2)", "6x"),
            ("Vereinfache(2x+3x)", "5x"),
            ("Ausmultiplizieren((x+1)^2)", "x^2 + 2x + 1"),
            ("Expandiere((x+1)(x-1))", "x^2 - 1"),
            ("Löse(x^2=4,x)", "{x = -2, x = 2}"),
            ("Loese(x^2+1=0)", "{}"),
            ("NSolve(x^2=4)", "{x = -2, x = 2}"),
            ("Numerisch(1/3)", "0.3333"),
            ("N(2)", "2"),
            ("Ersetze(x^2,x,3)", "9"),
            ("Integral(x,0,2)", "2"),
            ("Integral(x,x,0,2)", "2"),
            ("Integral(x)", "x^2/2"),
            ("x^2=4", "x^2 = 4"),
        ] {
            assert_eq!(cas_eval(input, &mut e).unwrap(), want, "{input}");
        }
        assert_eq!(e.value("a"), Some(5.0));
    }
    #[test]
    fn functions_and_shadowing() {
        let mut e = CasEnv::default();
        for input in ["a=7", "f(a)=a+1", "g(x,y)=x-y"] {
            cas_eval(input, &mut e).unwrap();
        }
        assert_eq!(cas_eval("f(2)", &mut e).unwrap(), "3");
        assert_eq!(cas_eval("g(y,x)", &mut e).unwrap(), "-x + y");
        assert!(cas_eval("f(1,2)", &mut e).is_err());
    }
    #[test]
    fn default_variable_after_substitution() {
        let mut e = CasEnv::default();
        cas_eval("a=2", &mut e).unwrap();
        assert_eq!(cas_eval("Derivative(a*t^2)", &mut e).unwrap(), "4t");
        assert_eq!(cas_eval("Solve(a*t=6)", &mut e).unwrap(), "{t = 3}");
        assert_eq!(cas_eval("Integral(a*t)", &mut e).unwrap(), "t^2");
        cas_eval("f(t)=t^2", &mut e).unwrap();
        cas_eval("t=9", &mut e).unwrap();
        assert_eq!(cas_eval("Derivative(f)", &mut e).unwrap(), "2t");
    }
    #[test]
    fn numeric_integral() {
        let mut e = CasEnv::default();
        let s = cas_eval("Integral(exp(-x^2),0,1)", &mut e).unwrap();
        assert_eq!(s, "0.7468");
    }
    #[test]
    fn commands_nested() {
        let mut e = CasEnv::default();
        assert_eq!(
            cas_eval("Derivative(Expand((x+1)^3))", &mut e).unwrap(),
            "3x^2 + 6x + 3"
        );
        assert!(
            cas_eval("Faktorisiere(x^2-1)", &mut e)
                .unwrap()
                .contains("x")
        );
    }
    #[test]
    fn factor_strings_and_exact_integrals() {
        let mut e = CasEnv::default();
        for (s, want) in [
            ("Factor(x^2-1)", "(x - 1)(x + 1)"),
            ("Faktorisiere(2x^2-8)", "2(x - 2)(x + 2)"),
            ("Factor(x^3-x)", "x(x - 1)(x + 1)"),
            ("Integral(sin(x),0,pi)", "2"),
            ("NLöse(x^2=2)", "{x = -1.4142, x = 1.4142}"),
            ("NLoese(x^2=2)", "{x = -1.4142, x = 1.4142}"),
            ("NUMERIC(sqrt(2))", "1.4142"),
        ] {
            assert_eq!(cas_eval(s, &mut e).unwrap(), want, "{s}");
        }
    }
    #[test]
    fn invalid_domains_and_recursion() {
        let mut e = CasEnv::default();
        assert!(cas_eval("Integral(1/x,-1,1)", &mut e).is_err());
        assert_eq!(cas_eval("Integral(x,2,0)", &mut e).unwrap(), "-2");
        e.values.insert("a".into(), Expr::Var("b".into()));
        e.values.insert("b".into(), Expr::Var("a".into()));
        assert_eq!(e.value("a"), None);
        assert!(cas_eval("a", &mut e).is_err());
        for s in [
            "(-2)^(-127)",
            "1/0",
            "0/0",
            "(-9223372036854775807-1)/-1",
            "sqrt(-1)",
            "ln(0)",
        ] {
            let _ = cas_eval(s, &mut e);
        }
    }
    #[test]
    fn weird_strings() {
        let mut env = CasEnv::default();
        let alphabet = [
            'x', '(', ')', '{', '}', '+', '-', '^', '|', 'π', '√', '=', ':', ',', '0', '²', '@',
        ];
        let mut seed = 7u64;
        for _ in 0..4000 {
            let mut s = String::new();
            seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
            for _ in 0..(seed % 30) {
                seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
                s.push(alphabet[(seed >> 32) as usize % alphabet.len()]);
            }
            let _ = cas_eval(&s, &mut env);
        }
        for s in [
            "Solve()",
            "Integral(x,x)",
            "Derivative(x,x,-1)",
            "sin()",
            "a=a+1",
            "a",
            "f(x)=f(x)",
            "f(2)",
        ] {
            let _ = cas_eval(s, &mut env);
        }
    }
}

#[cfg(test)]
mod review_tests {
    use super::*;
    #[test]
    fn reviewer_commands() {
        let mut env = CasEnv::default();
        for (input, want) in [
            ("Löse(sin(x)=0.5)", "{x = pi/6 + 2k pi, x = 5pi/6 + 2k pi}"),
            ("Löse(a x=2,x)", "{x = 2/a}"),
            ("Löse(x^2=a,x)", "{x = -sqrt(a), x = sqrt(a)}"),
            ("Ableitung(a x^2)", "2a x"),
            ("Vereinfache((x^2-1)/(x-1))", "x + 1"),
            ("Ableitung(ln(x^2+1))", "2x/(x^2 + 1)"),
            ("Löse(sin(x)=2)", "{}"),
        ] {
            assert_eq!(cas_eval(input, &mut env).unwrap(), want, "{input}");
        }
        for input in [
            "Integral(x e^x)",
            "Integral(k x)",
            "Integral(k x,0,1)",
            "Integral(k x,x,0,a)",
            "Numeric(a x+pi)",
            "NSolve(a x=2,x)",
            "NSolve(sin(x)=0.5)",
            "Löse(a x^2+b x+c=0,x)",
        ] {
            assert!(cas_eval(input, &mut env).is_ok(), "{input}");
        }
    }
}

#[cfg(test)]
mod cosmetic_tests {
    use super::*;
    #[test]
    fn cosmetic_command_outputs() {
        let mut env = CasEnv::default();
        for (input, want) in [
            ("Integral(x e^x)", "x e^x - e^x"),
            ("Integral(ln(x))", "x ln(x) - x"),
            (
                "Löse(a x^2+b x+c=0,x)",
                "{x = (-b - sqrt(b^2 - 4a c))/(2a), x = (-b + sqrt(b^2 - 4a c))/(2a)}",
            ),
            ("Löse(cos(2x)=0)", "{x = -pi/4 + k pi, x = pi/4 + k pi}"),
            ("Vereinfache(sqrt(8/a))", "2sqrt(2/a)"),
            ("Vereinfache(e^x^2)", "e^(x^2)"),
        ] {
            assert_eq!(cas_eval(input, &mut env).unwrap(), want, "{input}");
        }
    }
}
