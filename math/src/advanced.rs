use crate::ast::{Expr, rat, substitute};
use crate::eval::{Empty, compile, eval};
use crate::simplify::{
    add, div, expand, from_poly, int, mul, neg, polynomial, polynomial_div_rem, pow, simplify, sub,
    trim, zero,
};
pub(crate) fn command(n: &str, len: usize) -> Option<&'static str> {
    match n.to_lowercase().as_str() {
        "taylorpolynom" | "taylorpolynomial" => Some("taylor"),
        "grenzwert" | "limit" => Some("limit"),
        "linksseitigergrenzwert" | "limitbelow" => Some("below"),
        "rechtsseitigergrenzwert" | "limitabove" => Some("above"),
        "summe" | "sum" if len == 4 => Some("sum"),
        "produkt" | "product" => Some("product"),
        "folge" | "sequence" => Some("sequence"),
        "polynomdivision" | "division" => Some("division"),
        "zähler" | "numerator" => Some("numerator"),
        "nenner" | "denominator" => Some("denominator"),
        "ggt" | "gcd" => Some("gcd"),
        "kgv" | "lcm" => Some("lcm"),
        "primfaktoren" | "primefactors" => Some("primefactors"),
        "istprimzahl" | "isprime" => Some("isprime"),
        "runden" | "round" if len == 2 => Some("round"),
        "asymptote" => Some("asymptote"),
        "wendepunkt" | "inflectionpoint" => Some("inflection"),
        "extremum" => Some("extremum"),
        "nullstelle" | "root" => Some("root"),
        _ => None,
    }
}
pub(crate) fn variable(e: &Expr) -> String {
    fn names(e: &Expr, out: &mut std::collections::BTreeSet<String>) {
        match e {
            Expr::Var(n) if !matches!(n.as_str(), "inf" | "unendlich" | "wahr" | "falsch") => {
                out.insert(n.clone());
            }
            Expr::Neg(a) => names(a, out),
            Expr::Add(a, b)
            | Expr::Sub(a, b)
            | Expr::Mul(a, b)
            | Expr::Div(a, b)
            | Expr::Pow(a, b) => {
                names(a, out);
                names(b, out)
            }
            Expr::Call(_, a) | Expr::List(a) | Expr::Tuple(a) => {
                for e in a {
                    names(e, out)
                }
            }
            _ => {}
        }
    }
    let mut set = std::collections::BTreeSet::new();
    names(e, &mut set);
    if set.len() == 1 {
        set.into_iter().next().unwrap()
    } else {
        "x".into()
    }
}
fn number(e: &Expr) -> Result<f64, String> {
    eval(e, &Empty).map_err(|_| "Numerischer Wert erwartet".into())
}
fn whole(e: &Expr) -> Result<i64, String> {
    if let Expr::Rational(n, 1) = simplify(e) {
        return Ok(n);
    }
    let n = number(e)?;
    if !n.is_finite() || n.fract() != 0.0 || n >= i64::MAX as f64 || n < i64::MIN as f64 {
        return Err("Ganzzahl erwartet".into());
    }
    Ok(n as i64)
}
fn require(a: &[Expr], n: usize) -> Result<(), String> {
    if a.len() == n {
        Ok(())
    } else {
        Err("Ungültige Argumentanzahl".into())
    }
}
fn bounded(e: &Expr, limit: usize) -> bool {
    let mut stack = vec![e];
    let mut count = 0;
    while let Some(e) = stack.pop() {
        count += 1;
        if count > limit {
            return false;
        }
        match e {
            Expr::Neg(a) => stack.push(a),
            Expr::Add(a, b)
            | Expr::Sub(a, b)
            | Expr::Mul(a, b)
            | Expr::Div(a, b)
            | Expr::Pow(a, b) => {
                stack.push(a);
                stack.push(b)
            }
            Expr::Call(_, a) | Expr::List(a) | Expr::Tuple(a) => stack.extend(a),
            _ => {}
        }
    }
    true
}
pub(crate) fn fraction_parts(e: &Expr) -> (Expr, Expr) {
    match e {
        Expr::Rational(n, d) => (int(*n), int(*d)),
        Expr::Neg(a) => {
            let (n, d) = fraction_parts(a);
            (neg(n), d)
        }
        Expr::Mul(a, b) => {
            let (an, ad) = fraction_parts(a);
            let (bn, bd) = fraction_parts(b);
            (mul(an, bn), mul(ad, bd))
        }
        Expr::Div(a, b) => {
            let (an, ad) = fraction_parts(a);
            let (bn, bd) = fraction_parts(b);
            (mul(an, bd), mul(ad, bn))
        }
        Expr::Add(a, b) | Expr::Sub(a, b) => {
            let (an, ad) = fraction_parts(a);
            let (bn, bd) = fraction_parts(b);
            let left = mul(an, bd.clone());
            let right = mul(bn, ad.clone());
            (
                if matches!(e, Expr::Add(..)) {
                    add(left, right)
                } else {
                    sub(left, right)
                },
                mul(ad, bd),
            )
        }
        Expr::Pow(a, b) if matches!(**b,Expr::Rational(n,1) if n.unsigned_abs()<=64) => {
            let Expr::Rational(n, 1) = **b else {
                unreachable!()
            };
            let (an, ad) = fraction_parts(a);
            if n < 0 {
                (pow(ad, int(-n)), pow(an, int(-n)))
            } else {
                (pow(an, int(n)), pow(ad, int(n)))
            }
        }
        _ => (e.clone(), int(1)),
    }
}
fn finite_value(e: &Expr) -> Option<f64> {
    number(e).ok().filter(|x| x.is_finite())
}
fn snapped(x: f64) -> Expr {
    if x.is_finite() && (x - x.round()).abs() < 1e-10 && x.abs() < i64::MAX as f64 {
        int(x.round() as i64)
    } else {
        Expr::Real(x)
    }
}
fn discontinuous(e: &Expr) -> bool {
    match e {
        Expr::Call(n, a) => {
            crate::logic::conditional(n)
                || matches!(n.as_str(), "floor" | "ceil" | "round" | "sgn" | "sign")
                || crate::logic::builtin(n)
                || a.iter().any(discontinuous)
        }
        Expr::Neg(a) => discontinuous(a),
        Expr::Add(a, b) | Expr::Sub(a, b) | Expr::Mul(a, b) | Expr::Div(a, b) | Expr::Pow(a, b) => {
            discontinuous(a) || discontinuous(b)
        }
        _ => false,
    }
}
fn order_at(mut p: Vec<Expr>, v: &str, a: &Expr) -> Option<(usize, Expr)> {
    for order in 0..=64 {
        let value = simplify(&substitute(&from_poly(&p, v), v, a));
        if !zero(&value) {
            return Some((order, value));
        }
        if p.len() <= 1 {
            return None;
        }
        p = p
            .iter()
            .enumerate()
            .skip(1)
            .map(|(i, c)| mul(int(i as i64), c.clone()))
            .collect();
    }
    None
}
fn rational_limit(e: &Expr, v: &str, a: &Expr, side: i8) -> Option<Expr> {
    let (n, d) = fraction_parts(e);
    let p = polynomial(&expand(&n), v)?;
    let q = polynomial(&expand(&d), v)?;
    let at = number(a).ok()?;
    if at.is_infinite() {
        let lead = div(p.last()?.clone(), q.last()?.clone());
        if zero(&lead) {
            return Some(int(0));
        }
        let degree = p.len() as i64 - q.len() as i64;
        return Some(if degree < 0 {
            int(0)
        } else if degree == 0 {
            lead
        } else {
            let mut sign = finite_value(&lead)?.signum();
            if at < 0.0 && degree % 2 != 0 {
                sign = -sign;
            }
            Expr::Real(sign * f64::INFINITY)
        });
    }
    let Some((qn, qv)) = order_at(q, v, a) else {
        return Some(Expr::Real(f64::NAN));
    };
    let Some((pn, pv)) = order_at(p, v, a) else {
        return Some(int(0));
    };
    if pn >= qn {
        return Some(if pn == qn { div(pv, qv) } else { int(0) });
    }
    let degree = qn - pn;
    let coefficient = finite_value(&div(pv, qv))?;
    if side == 0 && degree % 2 == 1 {
        return Some(Expr::Real(f64::NAN));
    }
    let sign = coefficient.signum()
        * if side < 0 && degree % 2 == 1 {
            -1.0
        } else {
            1.0
        };
    Some(Expr::Real(sign * f64::INFINITY))
}
fn extrapolate(e: &Expr, v: &str, a: f64, side: i8) -> f64 {
    let Ok(c) = compile(e, &[v], &Empty) else {
        return f64::NAN;
    };
    let mut previous = Vec::new();
    let mut estimates = Vec::new();
    let mut values = Vec::new();
    for i in 0..30 {
        let h = 0.1 * (1.0 + if a.is_finite() { a.abs() } else { 0.0 }) / 2.0f64.powi(i);
        let at = if a.is_finite() {
            a + side as f64 * h
        } else {
            a.signum() / h
        };
        let value = c.eval(&[at]);
        if value.is_nan() {
            return f64::NAN;
        }
        if value.is_infinite() {
            return value;
        }
        values.push(value);
        let mut row = vec![value];
        for j in 1..=i.min(4) as usize {
            row.push(row[j - 1] + (row[j - 1] - previous[j - 1]) / (2.0f64.powi(j as i32) - 1.0));
        }
        let estimate = *row.last().unwrap();
        estimates.push(estimate);
        previous = row;
        if estimates.len() > 6 {
            let last = &estimates[estimates.len() - 4..];
            if last
                .iter()
                .all(|x| (x - estimate).abs() < 1e-9 * (1.0 + estimate.abs()))
            {
                return estimate;
            }
        }
    }
    let last = *values.last().unwrap();
    if values.len() >= 8 {
        let changes: Vec<_> = values[values.len() - 8..]
            .windows(2)
            .map(|w| w[1] - w[0])
            .collect();
        let change = *changes.last().unwrap();
        if change.abs() > 1e-4
            && changes
                .iter()
                .all(|d| (d - change).abs() < 1e-5 * (1.0 + change.abs()))
        {
            return change.signum() * f64::INFINITY;
        }
    }
    if last.abs() > 1e3
        && last.abs() > values[0].abs().max(1.0) * 1000.0
        && values
            .iter()
            .rev()
            .take(6)
            .all(|x| x.signum() == last.signum())
    {
        return last.signum() * f64::INFINITY;
    }
    let n = estimates.len();
    if (estimates[n - 1] - estimates[n - 2]).abs() < 1e-7 * (1.0 + estimates[n - 1].abs()) {
        estimates[n - 1]
    } else {
        f64::NAN
    }
}
pub(crate) fn limit(e: &Expr, v: &str, a: &Expr, side: i8) -> Expr {
    let e = simplify(e);
    if let Some(value) = rational_limit(&e, v, a, side) {
        return simplify(&value);
    }
    let Ok(at) = number(a) else {
        return Expr::Real(f64::NAN);
    };
    if !discontinuous(&e) {
        let direct = simplify(&substitute(&e, v, a));
        if number(&direct).is_ok_and(|x| x.is_finite() || at.is_infinite() && !x.is_nan()) {
            return direct;
        }
    }
    if side != 0 || at.is_infinite() {
        return snapped(extrapolate(&e, v, at, if side == 0 { 1 } else { side }));
    }
    let left = extrapolate(&e, v, at, -1);
    let right = extrapolate(&e, v, at, 1);
    if left.is_infinite() && left == right {
        Expr::Real(left)
    } else if left.is_finite()
        && right.is_finite()
        && (left - right).abs() < 1e-7 * (1.0 + left.abs())
    {
        snapped((left + right) / 2.0)
    } else {
        Expr::Real(f64::NAN)
    }
}
fn polynomial_pair(a: &Expr, b: &Expr) -> Result<(String, Vec<Expr>, Vec<Expr>), String> {
    let v = variable(&Expr::Tuple(vec![a.clone(), b.clone()]));
    let p = polynomial(&expand(a), &v).ok_or_else(|| "Polynom erwartet".to_string())?;
    let q = polynomial(&expand(b), &v).ok_or_else(|| "Polynom erwartet".to_string())?;
    if !p.iter().chain(&q).all(|e| matches!(e, Expr::Rational(..))) {
        return Err("Rationale Koeffizienten erwartet".into());
    }
    Ok((v, p, q))
}
fn gcd(mut a: u128, mut b: u128) -> u128 {
    while b != 0 {
        (a, b) = (b, a % b);
    }
    a
}
fn polynomial_gcd(mut p: Vec<Expr>, mut q: Vec<Expr>) -> Result<Vec<Expr>, String> {
    trim(&mut p);
    trim(&mut q);
    for _ in 0..130 {
        if q.iter().all(zero) {
            let leading = p.last().cloned().unwrap_or(int(1));
            return Ok(if zero(&leading) {
                vec![int(0)]
            } else {
                p.into_iter().map(|c| div(c, leading.clone())).collect()
            });
        }
        let (_, r) = polynomial_div_rem(&p, &q)
            .ok_or_else(|| "Polynomdivision nicht möglich".to_string())?;
        p = q;
        q = r;
    }
    Err("Polynom zu groß".into())
}
fn modpow(mut a: u64, mut n: u64, m: u64) -> u64 {
    let mut r = 1;
    while n > 0 {
        if n % 2 == 1 {
            r = (r as u128 * a as u128 % m as u128) as u64;
        }
        a = (a as u128 * a as u128 % m as u128) as u64;
        n /= 2;
    }
    r
}
fn is_prime(n: u64) -> bool {
    if n < 2 {
        return false;
    }
    for p in [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37] {
        if n % p == 0 {
            return n == p;
        }
    }
    let s = (n - 1).trailing_zeros();
    let d = (n - 1) >> s;
    'witness: for a in [2, 325, 9375, 28178, 450775, 9780504, 1795265022] {
        let a = a % n;
        if a == 0 {
            continue;
        }
        let mut x = modpow(a, d, n);
        if x == 1 || x == n - 1 {
            continue;
        }
        for _ in 1..s {
            x = (x as u128 * x as u128 % n as u128) as u64;
            if x == n - 1 {
                continue 'witness;
            }
        }
        return false;
    }
    true
}
fn factor_integer(n: u64, out: &mut Vec<u64>) -> Result<(), String> {
    if n == 1 {
        return Ok(());
    }
    if is_prime(n) {
        out.push(n);
        return Ok(());
    }
    if n % 2 == 0 {
        out.push(2);
        return factor_integer(n / 2, out);
    }
    for c in 1..=32u64 {
        let f = |x: u64| ((x as u128 * x as u128 + c as u128) % n as u128) as u64;
        let (mut x, mut y) = (2, 2);
        for _ in 0..100_000 {
            x = f(x);
            y = f(f(y));
            let d = gcd(x.abs_diff(y) as u128, n as u128) as u64;
            if d == n {
                break;
            }
            if d > 1 {
                factor_integer(d, out)?;
                factor_integer(n / d, out)?;
                return Ok(());
            }
        }
    }
    Err("Primfaktorzerlegung nicht möglich".into())
}
fn points(e: &Expr, v: &str, kind: &str, bounds: Option<(f64, f64)>) -> Result<Expr, String> {
    let tested = if kind == "root" {
        e.clone()
    } else if kind == "extremum" {
        crate::derivative(e, v)
    } else {
        crate::derivative(&crate::derivative(e, v), v)
    };
    let c = compile(&tested, &[v], &Empty).map_err(|e| e.0)?;
    let f = compile(e, &[v], &Empty).map_err(|e| e.0)?;
    let roots = if let Some((a, b)) = bounds {
        crate::find_roots(&|x| c.eval(&[x]), a, b)
            .into_iter()
            .map(Expr::Real)
            .collect()
    } else {
        match crate::solve(&tested, &int(0), v) {
            Ok(roots) => roots
                .into_iter()
                .map(|s| match s {
                    crate::Solution::Exact(e) => e,
                    crate::Solution::Approx(x) => Expr::Real(x),
                })
                .collect(),
            Err(_) => Vec::new(),
        }
    };
    let mut out = Vec::new();
    for root in roots {
        if kind != "root" {
            let Some(x) = finite_value(&root) else {
                continue;
            };
            let h = 1e-4 * (1.0 + x.abs());
            let (a, b) = (c.eval(&[x - h]), c.eval(&[x + h]));
            if !a.is_finite() || !b.is_finite() || a.signum() == b.signum() {
                continue;
            }
        }
        let y = if kind == "root" {
            int(0)
        } else {
            simplify(&substitute(e, v, &root))
        };
        if finite_value(&root).is_some_and(|x| !f.eval(&[x]).is_finite()) {
            continue;
        }
        out.push(Expr::Tuple(vec![root, y]));
    }
    Ok(Expr::List(out))
}
pub(crate) fn execute(cmd: &str, a: &[Expr]) -> Result<Expr, String> {
    match cmd {
        "taylor" => {
            require(a, 3)?;
            let v = variable(&a[0]);
            let n = whole(&a[2])?;
            if !(0..=20).contains(&n) {
                return Err("Ungültiger Taylorgrad".into());
            }
            let mut derivative = a[0].clone();
            let mut factorial = 1i128;
            let mut result = int(0);
            for i in 0..=n {
                if !bounded(&derivative, 2048) || !bounded(&result, 20_000) {
                    return Err("Taylor-Ausdruck zu groß".into());
                }
                if i > 0 {
                    factorial *= i as i128;
                }
                let value = simplify(&substitute(&derivative, &v, &a[1]));
                result = add(
                    result,
                    mul(
                        div(value, rat(factorial, 1)),
                        pow(sub(Expr::Var(v.clone()), a[1].clone()), int(i)),
                    ),
                );
                if i < n {
                    derivative = crate::derivative(&derivative, &v);
                }
            }
            Ok(expand(&result))
        }
        "limit" | "below" | "above" => {
            if !(2..=3).contains(&a.len()) {
                return Err("Ungültige Argumentanzahl".into());
            }
            let v = if a.len() == 3 {
                let Expr::Var(v) = &a[1] else {
                    return Err("Variable erwartet".into());
                };
                v.clone()
            } else {
                variable(&a[0])
            };
            Ok(limit(
                &a[0],
                &v,
                a.last().unwrap(),
                if cmd == "below" {
                    -1
                } else if cmd == "above" {
                    1
                } else {
                    0
                },
            ))
        }
        "sum" | "product" | "sequence" => {
            if a.len() != 4 && !(cmd == "sequence" && a.len() == 5) {
                return Err("Ungültige Argumentanzahl".into());
            }
            let Expr::Var(v) = &a[1] else {
                return Err("Variable erwartet".into());
            };
            let start = number(&a[2])?;
            let end = number(&a[3])?;
            let step = if a.len() == 5 { number(&a[4])? } else { 1.0 };
            if !start.is_finite()
                || !end.is_finite()
                || !step.is_finite()
                || step == 0.0
                || cmd != "sequence" && (start.fract() != 0.0 || end.fract() != 0.0)
            {
                return Err("Ungültige Laufgrenzen".into());
            }
            let span = (end - start) / step;
            let count = if (span - span.round()).abs() < 1e-12 * (1.0 + span.abs()) {
                span.round() + 1.0
            } else {
                span.floor() + 1.0
            };
            if count > 10_000.0 {
                return Err("Zu viele Folgenglieder".into());
            }
            let mut result = if cmd == "product" { int(1) } else { int(0) };
            let mut list = Vec::new();
            let mut index = a[2].clone();
            let step_expr = if a.len() == 5 { a[4].clone() } else { int(1) };
            for _ in 0..count.max(0.0) as usize {
                let value = simplify(&substitute(&a[0], v, &index));
                if cmd == "sequence" {
                    list.push(value);
                } else if cmd == "product" {
                    result = mul(result, value);
                } else {
                    result = add(result, value);
                }
                index = add(index, step_expr.clone());
            }
            Ok(if cmd == "sequence" {
                Expr::List(list)
            } else {
                result
            })
        }
        "numerator" | "denominator" => {
            require(a, 1)?;
            let (n, d) = fraction_parts(&simplify(&a[0]));
            Ok(if cmd == "numerator" {
                expand(&n)
            } else {
                expand(&d)
            })
        }
        "division" => {
            require(a, 2)?;
            let (v, p, q) = polynomial_pair(&a[0], &a[1])?;
            let (quotient, remainder) =
                polynomial_div_rem(&p, &q).ok_or_else(|| "Division durch null".to_string())?;
            Ok(Expr::List(vec![
                from_poly(&quotient, &v),
                from_poly(&remainder, &v),
            ]))
        }
        "gcd" | "lcm" => {
            require(a, 2)?;
            if let (Expr::Rational(n, d), Expr::Rational(m, q)) = (simplify(&a[0]), simplify(&a[1]))
            {
                let (n, m, d, q) = (
                    n.unsigned_abs() as u128,
                    m.unsigned_abs() as u128,
                    d as u128,
                    q as u128,
                );
                let numerator = gcd(n, m);
                let denominator = gcd(d, q);
                return Ok(if cmd == "gcd" {
                    rat(numerator as i128, (d / denominator * q) as i128)
                } else if numerator == 0 {
                    int(0)
                } else {
                    rat((n / numerator * m) as i128, denominator as i128)
                });
            }
            if let (Ok(x), Ok(y)) = (whole(&a[0]), whole(&a[1])) {
                let (x, y) = (x.unsigned_abs() as u128, y.unsigned_abs() as u128);
                let g = gcd(x, y);
                let value = if cmd == "gcd" {
                    g
                } else if g == 0 {
                    0
                } else {
                    x / g * y
                };
                return Ok(rat(value as i128, 1));
            }
            let (v, p, q) = polynomial_pair(&a[0], &a[1])?;
            if cmd == "lcm" && (p.iter().all(zero) || q.iter().all(zero)) {
                return Ok(int(0));
            }
            let g = polynomial_gcd(p.clone(), q.clone())?;
            if cmd == "gcd" {
                Ok(from_poly(&g, &v))
            } else {
                if g.iter().all(zero) {
                    return Ok(int(0));
                }
                let product = expand(&mul(from_poly(&p, &v), from_poly(&q, &v)));
                let pp = polynomial(&product, &v).ok_or_else(|| "Polynom erwartet".to_string())?;
                let (quotient, _) = polynomial_div_rem(&pp, &g)
                    .ok_or_else(|| "Polynomdivision nicht möglich".to_string())?;
                let lead = quotient.last().cloned().unwrap_or(int(1));
                Ok(from_poly(
                    &quotient
                        .into_iter()
                        .map(|c| div(c, lead.clone()))
                        .collect::<Vec<_>>(),
                    &v,
                ))
            }
        }
        "primefactors" | "isprime" => {
            require(a, 1)?;
            let n = whole(&a[0])?;
            if cmd == "isprime" {
                return Ok(Expr::Var(
                    if n >= 0 && is_prime(n as u64) {
                        "wahr"
                    } else {
                        "falsch"
                    }
                    .into(),
                ));
            }
            if n == 0 {
                return Err("Null hat keine Primfaktorzerlegung".into());
            }
            let mut factors = Vec::new();
            factor_integer(n.unsigned_abs(), &mut factors)?;
            factors.sort();
            let mut out = Vec::new();
            if n < 0 {
                out.push(int(-1));
            }
            out.extend(factors.into_iter().map(|p| rat(p as i128, 1)));
            Ok(Expr::List(out))
        }
        "round" => {
            require(a, 2)?;
            let value = number(&a[0])?;
            let n = whole(&a[1])?;
            if n.unsigned_abs() > 308 {
                return Err("Ungültige Stellenzahl".into());
            }
            let scale = 10.0f64.powi(n as i32);
            let rounded = value * scale;
            Ok(Expr::Real(if rounded.is_finite() {
                rounded.round() / scale
            } else {
                value
            }))
        }
        "asymptote" => {
            require(a, 1)?;
            let e = simplify(&a[0]);
            let v = variable(&e);
            let (n, d) = fraction_parts(&e);
            let mut out = Vec::new();
            if let Ok(roots) = crate::solve(&d, &int(0), &v) {
                for root in roots {
                    let value = match root {
                        crate::Solution::Exact(e) => e,
                        crate::Solution::Approx(x) => Expr::Real(x),
                    };
                    out.push(Expr::Call("=".into(), vec![Expr::Var(v.clone()), value]));
                }
            }
            let (_, p, q) = polynomial_pair(&n, &d)?;
            let (quotient, _) = polynomial_div_rem(&p, &q)
                .ok_or_else(|| "Rationale Funktion erwartet".to_string())?;
            if quotient.len() <= 2 {
                out.push(Expr::Call(
                    "=".into(),
                    vec![Expr::Var("y".into()), from_poly(&quotient, &v)],
                ));
            }
            Ok(Expr::List(out))
        }
        "root" | "extremum" | "inflection" => {
            if a.len() != 1 && a.len() != 3 {
                return Err("Ungültige Argumentanzahl".into());
            }
            let v = variable(&a[0]);
            let bounds = if a.len() == 3 {
                Some((number(&a[1])?, number(&a[2])?))
            } else {
                None
            };
            points(&a[0], &v, cmd, bounds)
        }
        _ => Err("Unbekannter Befehl".into()),
    }
}

#[cfg(test)]
mod tests {
    use crate::{CasEnv, cas_eval};
    #[test]
    fn taylor_and_scoped_sequences() {
        let mut env = CasEnv::default();
        for (input, want) in [
            ("Taylorpolynom(e^x,0,3)", "x^3/6 + x^2/2 + x + 1"),
            ("TaylorPolynomial(sin(x),0,5)", "x^5/120 - x^3/6 + x"),
            ("Summe(k^2,k,1,4)", "30"),
            ("Sum(1/k,k,1,3)", "11/6"),
            ("Produkt(k,k,1,5)", "120"),
            ("Product(k,k,2,1)", "1"),
            ("Folge(k^2,k,1,4)", "{1, 4, 9, 16}"),
            ("Sequence(k,k,3,1,-1)", "{3, 2, 1}"),
            ("Sequence(k,k,0,0.3,0.1)", "{0, 0.1, 0.2, 0.3}"),
        ] {
            assert_eq!(cas_eval(input, &mut env).unwrap(), want, "{input}");
        }
        cas_eval("k=20", &mut env).unwrap();
        assert_eq!(cas_eval("Sum(k,k,1,3)", &mut env).unwrap(), "6");
        assert_eq!(cas_eval("k", &mut env).unwrap(), "20");
        cas_eval("f(t)=t^2", &mut env).unwrap();
        assert_eq!(
            cas_eval("TaylorPolynomial(f,1,2)", &mut env).unwrap(),
            "t^2"
        );
    }
    #[test]
    fn exact_and_extrapolated_limits() {
        let mut env = CasEnv::default();
        for (input, want) in [
            ("Grenzwert((x^2-1)/(x-1),1)", "2"),
            ("Limit((3x^2+1)/(2x^2-1),x,∞)", "3/2"),
            ("Grenzwert(1/x,inf)", "0"),
            ("Limit(x^2,x,-unendlich)", "∞"),
            ("Limit(x,x,-inf)", "-∞"),
            ("Limit(1/x,0)", "undefiniert"),
            ("LinksseitigerGrenzwert(1/x,0)", "-∞"),
            ("RechtsseitigerGrenzwert(1/x,0)", "∞"),
            ("LimitBelow(1/x,0)", "-∞"),
            ("LimitAbove(1/x,0)", "∞"),
            ("Limit(1/x^2,0)", "∞"),
            ("Limit(sin(x)/x,0)", "1"),
            ("Limit((e^x-1)/x,0)", "1"),
            ("Limit(ln(x),inf)", "∞"),
            ("Limit(If(x<0,-1,1),0)", "undefiniert"),
        ] {
            assert_eq!(cas_eval(input, &mut env).unwrap(), want, "{input}");
        }
    }
    #[test]
    fn polynomial_and_integer_commands() {
        let mut env = CasEnv::default();
        for (input, want) in [
            ("Polynomdivision(x^3-1,x-1)", "{x^2 + x + 1, 0}"),
            ("Division(x^2+1,x+1)", "{x - 1, 2}"),
            ("Zähler((x+1)/(x-1))", "x + 1"),
            ("Nenner((x+1)/(x-1))", "x - 1"),
            ("Numerator(2/3)", "2"),
            ("Denominator(2/3)", "3"),
            ("ggT(18,24)", "6"),
            ("GCD(x^2-1,x^2-2x+1)", "x - 1"),
            ("kgV(6,8)", "24"),
            ("LCM(x-1,x+1)", "x^2 - 1"),
            ("Primfaktoren(12)", "{2, 2, 3}"),
            ("PrimeFactors(-12)", "{-1, 2, 2, 3}"),
            ("IstPrimzahl(97)", "wahr"),
            ("IsPrime(91)", "falsch"),
            ("Runden(pi,6)", "3.141593"),
            ("Round(123,-1)", "120"),
        ] {
            assert_eq!(cas_eval(input, &mut env).unwrap(), want, "{input}");
        }
    }
    #[test]
    fn asymptotes_and_characteristic_points() {
        let mut env = CasEnv::default();
        for (input, want) in [
            ("Asymptote((2x^2-x+1)/(x-1))", "{x = 1, y = 2x + 1}"),
            ("Nullstelle(x^2-1)", "{(-1, 0), (1, 0)}"),
            ("Root(x^2-2)", "{(-sqrt(2), 0), (sqrt(2), 0)}"),
            ("Extremum(x^3-3x)", "{(-1, 2), (1, -2)}"),
            ("Wendepunkt(x^3-3x)", "{(0, 0)}"),
            ("InflectionPoint(x^4)", "{}"),
            ("Extremum(x^3)", "{}"),
        ] {
            assert_eq!(cas_eval(input, &mut env).unwrap(), want, "{input}");
        }
    }
    #[test]
    fn invalid_inputs() {
        let mut env = CasEnv::default();
        for input in [
            "Sum(x,x,1,inf)",
            "Sequence(x,x,0,1,0)",
            "Product(x,x,0,1000000)",
            "TaylorPolynomial(x,0,-1)",
            "Division(x,0)",
            "PrimeFactors(0)",
            "Round(1,-9223372036854775808)",
        ] {
            assert!(cas_eval(input, &mut env).is_err(), "{input}");
        }
    }
}

#[cfg(test)]
mod edge_tests {
    use crate::{CasEnv, cas_eval};
    #[test]
    fn exact_integer_expressions_and_zero_lcm() {
        let mut env = CasEnv::default();
        for (input, want) in [
            (
                "PrimeFactors(1000000007*1000000009)",
                "{1000000007, 1000000009}",
            ),
            ("IsPrime(2305843009213693951)", "wahr"),
            ("LCM(x,0)", "0"),
            ("GCD(1/2,1/3)", "1/6"),
            ("LCM(1/2,1/3)", "1"),
            ("LimitAbove(ln(x),0)", "-∞"),
        ] {
            assert_eq!(cas_eval(input, &mut env).unwrap(), want, "{input}");
        }
    }
}

#[cfg(test)]
mod resource_tests {
    use crate::{CasEnv, cas_eval};
    #[test]
    fn bounded_taylor_growth() {
        let result = cas_eval("TaylorPolynomial(x^x,1,20)", &mut CasEnv::default());
        assert!(result.is_err());
    }
}
