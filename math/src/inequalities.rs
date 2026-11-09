use crate::ast::Expr;
use crate::eval::{Empty, compile, eval};
use crate::simplify::{int, polynomial, simplify, sub};
fn leaves<'a>(e: &'a Expr, out: &mut Vec<(&'a str, &'a Expr, &'a Expr)>) -> Result<(), String> {
    if let Expr::Call(n, a) = e {
        if crate::logic::comparison(n) && a.len() == 2 {
            out.push((n, &a[0], &a[1]));
            return Ok(());
        }
        if matches!(n.as_str(), "and" | "or") && a.len() == 2 {
            leaves(&a[0], out)?;
            leaves(&a[1], out)?;
            return Ok(());
        }
    }
    Err("Ungleichung erwartet".into())
}
#[derive(Clone)]
struct Boundary {
    value: f64,
    expr: Expr,
    pole: bool,
}
fn roots(e: &Expr, v: &str) -> Vec<Expr> {
    if polynomial(e, v).is_none() {
        if let Ok(c) = compile(e, &[v], &Empty) {
            return crate::find_roots(&|x| c.eval(&[x]), -100.0, 100.0)
                .into_iter()
                .map(Expr::Real)
                .collect();
        }
    }
    match crate::solve(e, &int(0), v) {
        Ok(roots) => roots
            .into_iter()
            .filter_map(|r| match r {
                crate::Solution::Exact(e) => Some(e),
                crate::Solution::Approx(x) => Some(Expr::Real(x)),
            })
            .collect(),
        Err(_) => Vec::new(),
    }
}
fn truth(e: &Expr, v: &str, x: f64, endpoint: bool) -> bool {
    if let Expr::Call(n, a) = e {
        if matches!(n.as_str(), "and" | "or") && a.len() == 2 {
            return if n == "and" {
                truth(&a[0], v, x, endpoint) && truth(&a[1], v, x, endpoint)
            } else {
                truth(&a[0], v, x, endpoint) || truth(&a[1], v, x, endpoint)
            };
        }
        if crate::logic::comparison(n) && a.len() == 2 {
            let left = compile(&a[0], &[v], &Empty)
                .ok()
                .map(|f| f.eval(&[x]))
                .unwrap_or(f64::NAN);
            let right = compile(&a[1], &[v], &Empty)
                .ok()
                .map(|f| f.eval(&[x]))
                .unwrap_or(f64::NAN);
            if endpoint
                && left.is_finite()
                && right.is_finite()
                && (left - right).abs() < 1e-8 * (1.0 + left.abs() + right.abs())
            {
                return matches!(n.as_str(), "<=" | ">=" | "==");
            }
            return crate::logic::operation(n, left, right) == 1.0;
        }
    }
    false
}
fn condition(
    v: &str,
    lo: Option<&Boundary>,
    hi: Option<&Boundary>,
    left: bool,
    right: bool,
) -> Expr {
    let x = Expr::Var(v.into());
    match (lo, hi) {
        (None, None) => Expr::Call(
            "and".into(),
            vec![
                Expr::Call("<".into(), vec![Expr::Real(f64::NEG_INFINITY), x.clone()]),
                Expr::Call("<".into(), vec![x, Expr::Real(f64::INFINITY)]),
            ],
        ),
        (Some(a), None) => Expr::Call(
            if left { ">=" } else { ">" }.into(),
            vec![x, a.expr.clone()],
        ),
        (None, Some(b)) => Expr::Call(
            if right { "<=" } else { "<" }.into(),
            vec![x, b.expr.clone()],
        ),
        (Some(a), Some(b)) => Expr::Call(
            "and".into(),
            vec![
                Expr::Call(
                    if left { "<=" } else { "<" }.into(),
                    vec![a.expr.clone(), x.clone()],
                ),
                Expr::Call(
                    if right { "<=" } else { "<" }.into(),
                    vec![x, b.expr.clone()],
                ),
            ],
        ),
    }
}
pub(crate) fn solve(e: &Expr, v: &str) -> Result<Expr, String> {
    let mut comparisons = Vec::new();
    leaves(e, &mut comparisons)?;
    compile(e, &[v], &Empty).map_err(|e| e.0)?;
    let mut boundaries = Vec::new();
    let mut nonpolynomial = false;
    for (_, a, b) in &comparisons {
        let difference = simplify(&sub((*a).clone(), (*b).clone()));
        let (n, d) = crate::advanced::fraction_parts(&difference);
        nonpolynomial |= polynomial(&n, v).is_none() || polynomial(&d, v).is_none();
        for (expr, pole) in [(n, false), (d, true)] {
            for root in roots(&expr, v) {
                if let Ok(value) = eval(&root, &Empty) {
                    if value.is_finite() {
                        boundaries.push(Boundary {
                            value,
                            expr: root,
                            pole,
                        });
                    }
                }
            }
        }
    }
    boundaries.sort_by(|a, b| a.value.total_cmp(&b.value));
    let mut unique: Vec<Boundary> = Vec::new();
    for b in boundaries {
        if let Some(last) = unique.last_mut() {
            if (last.value - b.value).abs() < 1e-9 * (1.0 + b.value.abs()) {
                last.pole |= b.pole;
                continue;
            }
        }
        unique.push(b);
    }
    if nonpolynomial {
        unique.retain(|b| b.value > -100.0 && b.value < 100.0);
        unique.insert(
            0,
            Boundary {
                value: -100.0,
                expr: int(-100),
                pole: true,
            },
        );
        unique.push(Boundary {
            value: 100.0,
            expr: int(100),
            pole: true,
        });
    }
    let boundaries = unique;
    let n = boundaries.len();
    let included: Vec<_> = boundaries
        .iter()
        .map(|b| !b.pole && truth(e, v, b.value, true))
        .collect();
    let mut segments = Vec::new();
    for i in 0..=n {
        if nonpolynomial && (i == 0 || i == n) {
            segments.push(false);
            continue;
        }
        let point = if i == 0 {
            boundaries
                .first()
                .map_or(0.0, |b| b.value - (1.0 + b.value.abs()))
        } else if i == n {
            boundaries.last().unwrap().value + 1.0 + boundaries.last().unwrap().value.abs()
        } else {
            boundaries[i - 1].value + (boundaries[i].value - boundaries[i - 1].value) / 2.0
        };
        segments.push(truth(e, v, point, false));
    }
    let mut out = Vec::new();
    let mut i = 0;
    while i <= n {
        if segments[i] {
            let start = i;
            let mut end = i;
            while end < n && included[end] && segments[end + 1] {
                end += 1;
            }
            out.push(condition(
                v,
                if start == 0 {
                    None
                } else {
                    Some(&boundaries[start - 1])
                },
                if end == n {
                    None
                } else {
                    Some(&boundaries[end])
                },
                start > 0 && included[start - 1],
                end < n && included[end],
            ));
            i = end + 1;
        } else {
            if i < n && included[i] && !segments[i + 1] {
                out.push(Expr::Call(
                    "=".into(),
                    vec![Expr::Var(v.into()), boundaries[i].expr.clone()],
                ));
            }
            i += 1;
        }
    }
    Ok(Expr::List(out))
}

#[cfg(test)]
mod tests {
    use crate::{CasEnv, cas_eval};
    #[test]
    fn polynomial_intervals() {
        let mut env = CasEnv::default();
        for (input, want) in [
            ("Löse(x^2>4,x)", "{x < -2, x > 2}"),
            ("Solve(x^2-2x-3<=0,x)", "{-1 ≤ x ≤ 3}"),
            ("Solve(x^2>=0,x)", "{-∞ < x < ∞}"),
            ("Solve(x^2>0,x)", "{x < 0, x > 0}"),
            ("Solve(x^2<=0,x)", "{x = 0}"),
            ("Solve(x^2<0,x)", "{}"),
            ("Solve(1<x<3,x)", "{1 < x < 3}"),
            ("Solve(1/(x-1)>0,x)", "{x > 1}"),
            ("Solve(x!=2,x)", "{x < 2, x > 2}"),
        ] {
            assert_eq!(cas_eval(input, &mut env).unwrap(), want, "{input}");
        }
    }
}

#[cfg(test)]
mod edge_tests {
    use crate::eval::Empty;
    use crate::{compile, eval, parse_expr};
    #[test]
    fn nonpolynomial_sign_changes() {
        let intervals = super::solve(&parse_expr("sin(x)>0").unwrap(), "x").unwrap();
        let crate::Expr::List(intervals) = intervals else {
            panic!()
        };
        assert!(!intervals.is_empty());
        let functions: Vec<_> = intervals
            .iter()
            .map(|e| compile(e, &["x"], &Empty).unwrap())
            .collect();
        for x in [-4.0_f64, -2.0, -0.5, 0.5, 2.0, 4.0] {
            let found = functions.iter().any(|f| f.eval(&[x]) == 1.0);
            assert_eq!(found, x.sin() > 0.0);
        }
        assert_eq!(
            eval(&parse_expr("false&&unknown").unwrap(), &Empty).unwrap(),
            0.0
        );
        assert_eq!(
            eval(&parse_expr("true||unknown").unwrap(), &Empty).unwrap(),
            1.0
        );
    }
}
