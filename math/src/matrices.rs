use crate::ast::Expr;
use crate::eval::{Empty, eval};
use crate::simplify::{add, div, int, mul, pow, simplify, sub, zero};
type Matrix = Vec<Vec<Expr>>;
fn matrix(e: &Expr) -> Result<Matrix, String> {
    let Expr::List(rows) = e else {
        return Err("Matrix erwartet".into());
    };
    let mut out = Vec::new();
    let mut width = None;
    for row in rows {
        let Expr::List(values) = row else {
            return Err("Matrix erwartet".into());
        };
        if width.is_some_and(|n| n != values.len()) {
            return Err("Unterschiedliche Zeilenlängen".into());
        }
        if values
            .iter()
            .any(|e| matches!(e, Expr::List(_) | Expr::Tuple(_)))
        {
            return Err("Skalare Matrixeinträge erwartet".into());
        }
        width = Some(values.len());
        out.push(values.iter().map(simplify).collect());
    }
    Ok(out)
}
fn expression(a: Matrix) -> Expr {
    Expr::List(a.into_iter().map(Expr::List).collect())
}
fn vector(e: &Expr) -> Result<Vec<Expr>, String> {
    let Expr::List(a) = e else {
        return Err("Vektor erwartet".into());
    };
    if a.iter().any(|e| matches!(e, Expr::List(_))) {
        return Err("Vektor erwartet".into());
    }
    Ok(a.clone())
}
fn square(a: &Matrix) -> Result<(), String> {
    if a.iter().any(|r| r.len() != a.len()) {
        Err("Quadratische Matrix erwartet".into())
    } else {
        Ok(())
    }
}
pub(crate) fn rref(mut a: Matrix, columns: usize) -> Result<(Matrix, Vec<usize>), String> {
    let mut pivots = Vec::new();
    let mut row = 0;
    for column in 0..columns {
        if row >= a.len() {
            break;
        }
        let pivot = (row..a.len())
            .filter(|r| !zero(&a[*r][column]))
            .max_by(|i, j| {
                let size = |i: usize| eval(&a[i][column], &Empty).unwrap_or(1.0).abs();
                size(*i).total_cmp(&size(*j))
            });
        let Some(pivot) = pivot else {
            continue;
        };
        a.swap(row, pivot);
        let divisor = a[row][column].clone();
        if eval(&divisor, &Empty).is_ok_and(|x| !x.is_finite()) {
            return Err("Ungültiger Matrixeintrag".into());
        }
        for e in &mut a[row] {
            *e = div(e.clone(), divisor.clone());
        }
        for i in 0..a.len() {
            if i == row {
                continue;
            }
            let factor = a[i][column].clone();
            if zero(&factor) {
                continue;
            }
            for j in 0..a[i].len() {
                a[i][j] = sub(a[i][j].clone(), mul(factor.clone(), a[row][j].clone()));
            }
            a[i][column] = int(0);
        }
        pivots.push(column);
        row += 1;
    }
    Ok((a, pivots))
}
fn identity(n: usize) -> Matrix {
    (0..n)
        .map(|i| (0..n).map(|j| int(i64::from(i == j))).collect())
        .collect()
}
fn product(a: &Matrix, b: &Matrix) -> Result<Matrix, String> {
    let width = a.first().map_or(0, Vec::len);
    if width != b.len() {
        return Err("Unverträgliche Matrixdimensionen".into());
    }
    let cols = b.first().map_or(0, Vec::len);
    Ok((0..a.len())
        .map(|i| {
            (0..cols)
                .map(|j| {
                    (0..width).fold(int(0), |s, k| add(s, mul(a[i][k].clone(), b[k][j].clone())))
                })
                .collect()
        })
        .collect())
}
fn inverse(a: Matrix) -> Result<Matrix, String> {
    square(&a)?;
    let n = a.len();
    let id = identity(n);
    let augmented = a
        .into_iter()
        .zip(id)
        .map(|(mut r, b)| {
            r.extend(b);
            r
        })
        .collect();
    let (reduced, pivots) = rref(augmented, n)?;
    if pivots.len() != n {
        return Err("Matrix ist nicht invertierbar".into());
    }
    Ok(reduced.into_iter().map(|r| r[n..].to_vec()).collect())
}
fn scale(a: &Expr, b: &Expr) -> Expr {
    if let Expr::List(v) = b {
        Expr::List(v.iter().map(|e| scale(a, e)).collect())
    } else {
        mul(a.clone(), b.clone())
    }
}
pub(crate) fn binary(op: &str, a: Expr, b: Expr) -> Option<Result<Expr, String>> {
    if !matches!(a, Expr::List(_)) && !matches!(b, Expr::List(_)) {
        return None;
    }
    Some((|| match op {
        "+" | "-" => {
            let (Expr::List(a), Expr::List(b)) = (a, b) else {
                return Err("Unverträgliche Dimensionen".into());
            };
            if a.len() != b.len() {
                return Err("Unverträgliche Dimensionen".into());
            }
            let mut out = Vec::new();
            for (a, b) in a.into_iter().zip(b) {
                out.push(
                    if matches!(a, Expr::List(_)) || matches!(b, Expr::List(_)) {
                        binary(op, a, b).unwrap()?
                    } else if op == "+" {
                        add(a, b)
                    } else {
                        sub(a, b)
                    },
                );
            }
            Ok(Expr::List(out))
        }
        "*" => match (&a, &b) {
            (Expr::List(_), Expr::List(_)) => {
                let left = matrix(&a)?;
                if matches!(&b,Expr::List(v) if v.first().is_none_or(|e|!matches!(e,Expr::List(_))))
                {
                    let v = vector(&b)?;
                    if left.iter().any(|r| r.len() != v.len()) {
                        return Err("Unverträgliche Matrixdimensionen".into());
                    }
                    return Ok(Expr::List(
                        left.iter()
                            .map(|r| {
                                r.iter()
                                    .zip(&v)
                                    .fold(int(0), |s, (a, b)| add(s, mul(a.clone(), b.clone())))
                            })
                            .collect(),
                    ));
                }
                Ok(expression(product(&left, &matrix(&b)?)?))
            }
            (Expr::List(_), _) => Ok(scale(&b, &a)),
            (_, Expr::List(_)) => Ok(scale(&a, &b)),
            _ => unreachable!(),
        },
        "^" => {
            let mut base = matrix(&a)?;
            square(&base)?;
            let n = eval(&b, &Empty).map_err(|_| "Ganzzahliger Exponent erwartet".to_string())?;
            if !n.is_finite() || n.fract() != 0.0 || n.abs() > 1024.0 {
                return Err("Ungültiger Matrixexponent".into());
            }
            if n < 0.0 {
                base = inverse(base)?;
            }
            let mut out = identity(base.len());
            let mut n = n.abs() as u64;
            while n > 0 {
                if n % 2 == 1 {
                    out = product(&out, &base)?;
                }
                n /= 2;
                if n > 0 {
                    base = product(&base, &base)?;
                }
            }
            Ok(expression(out))
        }
        _ => Err("Matrixoperation nicht unterstützt".into()),
    })())
}
pub(crate) fn command(n: &str) -> Option<&'static str> {
    match n.to_lowercase().as_str() {
        "determinante" | "determinant" => Some("det"),
        "invertiere" | "invert" | "inverse" => Some("inverse"),
        "transponiere" | "transpose" => Some("transpose"),
        "einheitsmatrix" | "identity" => Some("identity"),
        "rang" | "rank" => Some("rank"),
        "treppennormalform" | "reducedrowechelonform" => Some("rref"),
        "skalarprodukt" | "dot" => Some("dot"),
        "kreuzprodukt" | "cross" => Some("cross"),
        "länge" | "length" => Some("length"),
        _ => None,
    }
}
pub(crate) fn execute(cmd: &str, a: &[Expr]) -> Result<Expr, String> {
    let required = if matches!(cmd, "dot" | "cross") { 2 } else { 1 };
    if a.len() != required {
        return Err("Ungültige Argumentanzahl".into());
    }
    if cmd == "identity" {
        let n = eval(&a[0], &Empty).map_err(|_| "Ganzzahl erwartet".to_string())?;
        if !n.is_finite() || n < 0.0 || n.fract() != 0.0 || n > 128.0 {
            return Err("Ungültige Matrixgröße".into());
        }
        return Ok(expression(identity(n as usize)));
    }
    if matches!(cmd, "dot" | "cross" | "length") {
        let v = vector(&a[0])?;
        if cmd == "length" {
            return Ok(crate::simplify::call(
                "sqrt",
                v.into_iter().fold(int(0), |s, e| add(s, pow(e, int(2)))),
            ));
        }
        let w = vector(&a[1])?;
        if v.len() != w.len() {
            return Err("Unverträgliche Vektordimensionen".into());
        }
        if cmd == "dot" {
            return Ok(v
                .into_iter()
                .zip(w)
                .fold(int(0), |s, (a, b)| add(s, mul(a, b))));
        }
        if v.len() != 3 {
            return Err("Dreidimensionale Vektoren erwartet".into());
        }
        return Ok(Expr::List(
            (0..3)
                .map(|i| {
                    sub(
                        mul(v[(i + 1) % 3].clone(), w[(i + 2) % 3].clone()),
                        mul(v[(i + 2) % 3].clone(), w[(i + 1) % 3].clone()),
                    )
                })
                .collect(),
        ));
    }
    let mut m = matrix(&a[0])?;
    let n = m.len();
    let cols = m.first().map_or(0, Vec::len);
    match cmd {
        "transpose" => Ok(expression(
            (0..cols)
                .map(|j| (0..n).map(|i| m[i][j].clone()).collect())
                .collect(),
        )),
        "inverse" => Ok(expression(inverse(m)?)),
        "rank" | "rref" => {
            let (m, pivots) = rref(m, cols)?;
            Ok(if cmd == "rank" {
                int(pivots.len() as i64)
            } else {
                expression(m)
            })
        }
        "det" => {
            square(&m)?;
            let mut determinant = int(1);
            for column in 0..n {
                let Some(pivot) = (column..n).find(|i| !zero(&m[*i][column])) else {
                    return Ok(int(0));
                };
                if pivot != column {
                    m.swap(pivot, column);
                    determinant = crate::simplify::neg(determinant);
                }
                let diagonal = m[column][column].clone();
                determinant = mul(determinant, diagonal.clone());
                for i in column + 1..n {
                    let factor = div(m[i][column].clone(), diagonal.clone());
                    for j in column + 1..n {
                        m[i][j] = sub(m[i][j].clone(), mul(factor.clone(), m[column][j].clone()));
                    }
                    m[i][column] = int(0);
                }
            }
            Ok(determinant)
        }
        _ => Err("Unbekannter Matrixbefehl".into()),
    }
}

#[cfg(test)]
mod tests {
    use crate::{CasEnv, cas_eval};
    #[test]
    fn matrix_commands_and_aliases() {
        let mut env = CasEnv::default();
        for (names, args, want) in [
            (vec!["Determinante", "Determinant"], "({{1,2},{3,4}})", "-2"),
            (
                vec!["Invertiere", "Invert", "Inverse"],
                "({{1,2},{3,4}})",
                "{{-2, 1}, {3/2, -1/2}}",
            ),
            (
                vec!["Transponiere", "Transpose"],
                "({{1,2,3},{4,5,6}})",
                "{{1, 4}, {2, 5}, {3, 6}}",
            ),
            (
                vec!["Einheitsmatrix", "Identity"],
                "(2)",
                "{{1, 0}, {0, 1}}",
            ),
            (vec!["Rang", "Rank"], "({{1,2},{2,4}})", "1"),
            (
                vec!["Treppennormalform", "ReducedRowEchelonForm"],
                "({{1,2,3},{2,4,6}})",
                "{{1, 2, 3}, {0, 0, 0}}",
            ),
            (vec!["Skalarprodukt", "Dot"], "({1,2,3},{4,5,6})", "32"),
            (
                vec!["Kreuzprodukt", "Cross"],
                "({1,0,0},{0,1,0})",
                "{0, 0, 1}",
            ),
            (vec!["Länge", "Length"], "({3,4})", "5"),
        ] {
            for name in names {
                let input = format!("{name}{args}");
                assert_eq!(cas_eval(&input, &mut env).unwrap(), want, "{input}");
            }
        }
    }
    #[test]
    fn arithmetic_and_exactness() {
        let mut env = CasEnv::default();
        for (input, want) in [
            ("{{1,2},{3,4}}*{{2,0},{0,2}}", "{{2, 4}, {6, 8}}"),
            ("{{1,2},{3,4}}*{2,3}", "{8, 18}"),
            ("{{1,2}}+{{3,4}}", "{{4, 6}}"),
            ("{{1,2}}-{{3,4}}", "{{-2, -2}}"),
            ("(1/2)*{{1,2},{3,4}}", "{{1/2, 1}, {3/2, 2}}"),
            ("{{1,1},{0,1}}^3", "{{1, 3}, {0, 1}}"),
            ("{{1,1},{0,1}}^-1", "{{1, -1}, {0, 1}}"),
            ("{{1,1},{0,1}}^0", "{{1, 0}, {0, 1}}"),
            ("Determinant({{1/2,1/3},{1/4,1/5}})", "1/60"),
            ("{{},{}}*{}", "{0, 0}"),
        ] {
            assert_eq!(cas_eval(input, &mut env).unwrap(), want, "{input}");
        }
        cas_eval("A={{1,2},{3,4}}", &mut env).unwrap();
        assert_eq!(
            cas_eval("A*Inverse(A)", &mut env).unwrap(),
            "{{1, 0}, {0, 1}}"
        );
    }
    #[test]
    fn invalid_shapes() {
        let mut env = CasEnv::default();
        for input in [
            "Inverse({{1,2},{2,4}})",
            "Determinant({{1,2}})",
            "Rank({{1},{2,3}})",
            "{{1,2}}*{1}",
            "{{1}}+{{1,2}}",
            "Cross({1,2},{3,4})",
            "Identity(-1)",
            "{{1}}^0.5",
        ] {
            assert!(cas_eval(input, &mut env).is_err(), "{input}");
        }
    }
}
