use crate::ast::{Expr, contains, substitute};
use crate::eval::{Empty, compile, eval};
use crate::simplify::{add, div, expand, int, mul, neg, polynomial, simplify, sub, zero};
fn equation(e: &Expr) -> Expr {
    if let Expr::Call(n, a) = e {
        if matches!(n.as_str(), "=" | "==") && a.len() == 2 {
            return sub(a[0].clone(), a[1].clone());
        }
    }
    e.clone()
}
fn entries(equations: &[Expr], vars: &[String]) -> Option<Vec<Vec<Expr>>> {
    let mut rows = Vec::new();
    for e in equations {
        let mut constant = e.clone();
        for v in vars {
            constant = substitute(&constant, v, &int(0));
        }
        let constant = simplify(&constant);
        let mut row = Vec::new();
        let mut reconstruction = constant.clone();
        for v in vars {
            let d = crate::derivative(e, v);
            if vars.iter().any(|n| contains(&d, n)) {
                return None;
            }
            reconstruction = add(reconstruction, mul(d.clone(), Expr::Var(v.clone())));
            row.push(d);
        }
        if !zero(&expand(&sub(e.clone(), reconstruction))) {
            return None;
        }
        row.push(neg(constant));
        rows.push(row);
    }
    Some(rows)
}
fn assignments(vars: &[String], values: Vec<Expr>) -> Expr {
    Expr::List(
        vars.iter()
            .zip(values)
            .map(|(v, e)| Expr::Call("=".into(), vec![Expr::Var(v.clone()), simplify(&e)]))
            .collect(),
    )
}
fn linear(eqs: &[Expr], vars: &[String]) -> Option<Result<Expr, String>> {
    let rows = entries(eqs, vars)?;
    Some((|| {
        let (rows, pivots) = crate::matrices::rref(rows, vars.len())?;
        for row in &rows {
            if row[..vars.len()].iter().all(zero) && !zero(&row[vars.len()]) {
                return Ok(Expr::List(vec![]));
            }
        }
        let mut values: Vec<_> = vars.iter().map(|v| Expr::Var(v.clone())).collect();
        for (row, &pivot) in rows.iter().zip(&pivots) {
            let mut value = row[vars.len()].clone();
            for j in 0..vars.len() {
                if j != pivot && !zero(&row[j]) {
                    value = sub(value, mul(row[j].clone(), values[j].clone()));
                }
            }
            values[pivot] = value;
        }
        Ok(assignments(vars, values))
    })())
}
fn exact(eqs: &[Expr], vars: &[String], depth: usize) -> Option<Vec<Vec<Expr>>> {
    if depth > 3 {
        return None;
    }
    if vars.len() == 1 {
        let p = polynomial(eqs.first()?, &vars[0])?;
        if p.len() > 3 {
            return None;
        }
        let roots = crate::solve(&eqs[0], &int(0), &vars[0]).ok()?;
        let mut out = Vec::new();
        for root in roots {
            let crate::Solution::Exact(root) = root else {
                return None;
            };
            if eqs.iter().all(|e| {
                eval(&substitute(e, &vars[0], &root), &Empty)
                    .is_ok_and(|x| x.is_finite() && x.abs() < 1e-8)
            }) {
                out.push(vec![root]);
            }
        }
        return Some(out);
    }
    for (i, e) in eqs.iter().enumerate() {
        for (j, v) in vars.iter().enumerate() {
            let Some(p) = polynomial(e, v) else {
                continue;
            };
            if p.len() != 2 || zero(&p[1]) || vars.iter().any(|v| contains(&p[1], v)) {
                continue;
            }
            let isolated = div(neg(p[0].clone()), p[1].clone());
            let remaining: Vec<_> = vars
                .iter()
                .enumerate()
                .filter(|(k, _)| *k != j)
                .map(|(_, v)| v.clone())
                .collect();
            let reduced: Vec<_> = eqs
                .iter()
                .enumerate()
                .filter(|(k, _)| *k != i)
                .map(|(_, e)| simplify(&substitute(e, v, &isolated)))
                .collect();
            let Some(solutions) = exact(&reduced, &remaining, depth + 1) else {
                continue;
            };
            let mut out = Vec::new();
            for solution in solutions {
                let mut value = isolated.clone();
                for (v, e) in remaining.iter().zip(&solution) {
                    value = substitute(&value, v, e);
                }
                let mut solution = solution;
                solution.insert(j, simplify(&value));
                out.push(solution);
            }
            return Some(out);
        }
    }
    None
}
pub(crate) fn numeric_solve(mut a: Vec<Vec<f64>>, mut b: Vec<f64>) -> Option<Vec<f64>> {
    let n = b.len();
    for i in 0..n {
        let p = (i..n).max_by(|r, s| a[*r][i].abs().total_cmp(&a[*s][i].abs()))?;
        if a[p][i].abs() < 1e-300 || !a[p][i].is_finite() {
            return None;
        }
        a.swap(p, i);
        b.swap(p, i);
        for j in i + 1..n {
            let k = a[j][i] / a[i][i];
            for column in i..n {
                a[j][column] -= k * a[i][column];
            }
            b[j] -= k * b[i];
        }
    }
    let mut out = vec![0.0; n];
    for i in (0..n).rev() {
        out[i] = (b[i] - (i + 1..n).map(|j| a[i][j] * out[j]).sum::<f64>()) / a[i][i];
    }
    out.iter().all(|x| x.is_finite()).then_some(out)
}
fn numeric(eqs: &[Expr], vars: &[String]) -> Result<Vec<Vec<Expr>>, String> {
    let n = vars.len();
    if !(2..=3).contains(&n) || eqs.len() < n {
        return Err("Numerisches System benötigt zwei oder drei Variablen".into());
    }
    let slots: Vec<_> = vars.iter().map(String::as_str).collect();
    let functions: Vec<_> = eqs
        .iter()
        .map(|e| compile(e, &slots, &Empty).map_err(|e| e.0))
        .collect::<Result<_, _>>()?;
    let norm = |x: &[f64]| functions.iter().map(|f| f.eval(x).powi(2)).sum::<f64>();
    let seeds = [-10.0, -3.0, -1.0, 0.0, 1.0, 3.0, 10.0];
    let mut roots: Vec<Vec<f64>> = Vec::new();
    for seed in 0..seeds.len().pow(n as u32) {
        let mut id = seed;
        let mut point: Vec<_> = (0..n)
            .map(|_| {
                let s = seeds[id % seeds.len()];
                id /= seeds.len();
                s
            })
            .collect();
        for _ in 0..80 {
            let error = norm(&point);
            if !error.is_finite() {
                break;
            }
            if error < 1e-60 {
                break;
            }
            let mut lhs = vec![vec![0.0; n]; n];
            let mut rhs = vec![0.0; n];
            for f in &functions {
                let value = f.eval(&point);
                let mut gradient = vec![0.0; n];
                for j in 0..n {
                    let h = 1e-6 * (1.0 + point[j].abs());
                    let mut p = point.clone();
                    p[j] += h;
                    let a = f.eval(&p);
                    p[j] -= 2.0 * h;
                    gradient[j] = (a - f.eval(&p)) / (2.0 * h);
                }
                for i in 0..n {
                    rhs[i] -= gradient[i] * value;
                    for j in 0..n {
                        lhs[i][j] += gradient[i] * gradient[j];
                    }
                }
            }
            for i in 0..n {
                lhs[i][i] += lhs[i][i].max(1e-300) * 1e-14;
            }
            let Some(step) = numeric_solve(lhs, rhs) else {
                break;
            };
            let mut accepted = false;
            let mut scale = 1.0;
            for _ in 0..24 {
                let candidate: Vec<_> = point
                    .iter()
                    .zip(&step)
                    .map(|(x, d)| x + scale * d)
                    .collect();
                if norm(&candidate) < error {
                    point = candidate;
                    accepted = true;
                    break;
                }
                scale *= 0.5;
            }
            if !accepted {
                break;
            }
        }
        if norm(&point) < 1e-18
            && roots.iter().all(|r| {
                r.iter()
                    .zip(&point)
                    .any(|(a, b)| (a - b).abs() > 1e-7 * (1.0 + a.abs().max(b.abs())))
            })
        {
            roots.push(point);
        }
    }
    roots.sort_by(|a, b| {
        a.iter()
            .zip(b)
            .map(|(a, b)| a.total_cmp(b))
            .find(|c| !c.is_eq())
            .unwrap_or(std::cmp::Ordering::Equal)
    });
    Ok(roots
        .into_iter()
        .map(|r| {
            r.into_iter()
                .map(|x| Expr::Real(if x.abs() < 1e-12 { 0.0 } else { x }))
                .collect()
        })
        .collect())
}
pub(crate) fn solve(equations: &Expr, variables: &Expr, approximate: bool) -> Result<Expr, String> {
    let Expr::List(eqs) = equations else {
        return Err("Gleichungsliste erwartet".into());
    };
    let Expr::List(names) = variables else {
        return Err("Variablenliste erwartet".into());
    };
    let mut vars = Vec::new();
    for e in names {
        let Expr::Var(n) = e else {
            return Err("Variable erwartet".into());
        };
        if vars.contains(n) {
            return Err("Doppelte Variable".into());
        }
        vars.push(n.clone());
    }
    if vars.is_empty() || vars.len() > 32 || eqs.len() > 64 {
        return Err("Ungültige Systemgröße".into());
    }
    let eqs: Vec<_> = eqs.iter().map(equation).map(|e| expand(&e)).collect();
    if let Some(solution) = linear(&eqs, &vars) {
        return solution;
    }
    let roots = if !approximate {
        if let Some(roots) = exact(&eqs, &vars, 0) {
            roots
        } else {
            numeric(&eqs, &vars)?
        }
    } else {
        numeric(&eqs, &vars)?
    };
    let mut out: Vec<_> = roots.into_iter().map(|r| assignments(&vars, r)).collect();
    if out.len() == 1 {
        Ok(out.remove(0))
    } else {
        Ok(Expr::List(out))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{CasEnv, cas_eval, parse_expr};
    #[test]
    fn linear_systems() {
        let mut env = CasEnv::default();
        for (input, want) in [
            ("Löse({x+y=2,x-y=0},{x,y})", "{x = 1, y = 1}"),
            ("Solve({2x+y=1,x-y=0},{x,y})", "{x = 1/3, y = 1/3}"),
            ("Solve({x+y=1,x+y=2},{x,y})", "{}"),
            (
                "Solve({x+y+z=6,x-y=0,z=2},{x,y,z})",
                "{x = 2, y = 2, z = 2}",
            ),
        ] {
            assert_eq!(cas_eval(input, &mut env).unwrap(), want, "{input}");
        }
        let e = solve(
            &parse_expr("{x+y=2}").unwrap(),
            &parse_expr("{x,y}").unwrap(),
            false,
        )
        .unwrap();
        let Expr::List(a) = e else { panic!() };
        let Expr::Call(_, rhs) = &a[0] else { panic!() };
        let f = compile(&rhs[1], &["y"], &Empty).unwrap();
        assert_eq!(f.eval(&[3.0]), -1.0);
        assert_eq!(
            a[1],
            Expr::Call(
                "=".into(),
                vec![Expr::Var("y".into()), Expr::Var("y".into())]
            )
        );
    }
    #[test]
    fn nonlinear_exact_substitution() {
        let mut env = CasEnv::default();
        assert_eq!(
            cas_eval("Löse({x^2+y^2=25,y=x+1},{x,y})", &mut env).unwrap(),
            "{{x = -4, y = -3}, {x = 3, y = 4}}"
        );
        assert_eq!(
            cas_eval("Solve({x^2+y^2=25,y=x+1,z=2},{x,y,z})", &mut env).unwrap(),
            "{{x = -4, y = -3, z = 2}, {x = 3, y = 4, z = 2}}"
        );
    }
    #[test]
    fn nonlinear_newton_multiple_roots() {
        for (input, count) in [
            ("{x^2=1,y^2=1}", 4),
            ("{x^2=1,y^2=1,z^2=1}", 8),
            ("{x^2+y^2=25,y=x+1}", 2),
        ] {
            let vars = if input.contains('z') {
                "{x,y,z}"
            } else {
                "{x,y}"
            };
            let e = solve(
                &parse_expr(input).unwrap(),
                &parse_expr(vars).unwrap(),
                true,
            )
            .unwrap();
            let Expr::List(solutions) = e else { panic!() };
            assert_eq!(solutions.len(), count, "{input}");
        }
        let mut env = CasEnv::default();
        let result = cas_eval("NLöse({x^2+y^2=25,y=x+1},{x,y})", &mut env).unwrap();
        assert_eq!(result, "{{x = -4, y = -3}, {x = 3, y = 4}}");
    }
}

#[cfg(test)]
mod edge_tests {
    use crate::{CasEnv, cas_eval};
    #[test]
    fn flat_newton_roots_and_larger_linear_system() {
        let mut env = CasEnv::default();
        assert_eq!(
            cas_eval("NLöse({x^2=0,y^2=0},{x,y})", &mut env).unwrap(),
            "{x = 0, y = 0}"
        );
        assert_eq!(
            cas_eval("Solve({a+b=3,b+c=5,c+d=7,d+t=9,t=5},{a,b,c,d,t})", &mut env).unwrap(),
            "{a = 1, b = 2, c = 3, d = 4, t = 5}"
        );
    }
}
