use super::ast::{Constant, Expr, contains, rat};
use Expr::*;
use std::collections::BTreeMap;
pub(crate) fn int(n: i64) -> Expr {
    Rational(n, 1)
}
pub(crate) fn add(a: Expr, b: Expr) -> Expr {
    sum(vec![a, b])
}
pub(crate) fn neg(a: Expr) -> Expr {
    match a {
        Add(a, b) => add(neg(*a), neg(*b)),
        Sub(a, b) => sub(neg(*a), neg(*b)),
        Neg(a) => *a,
        a => mul(int(-1), a),
    }
}
pub(crate) fn sub(a: Expr, b: Expr) -> Expr {
    add(a, neg(b))
}
pub(crate) fn mul(a: Expr, b: Expr) -> Expr {
    product(vec![a, b])
}
pub(crate) fn div(a: Expr, b: Expr) -> Expr {
    simp_div(a, b)
}
pub(crate) fn pow(a: Expr, b: Expr) -> Expr {
    simp_pow(a, b)
}
pub(crate) fn call(n: &str, a: Expr) -> Expr {
    simp_call(n, vec![a])
}
pub(crate) fn number(e: &Expr) -> Option<f64> {
    match e {
        Rational(n, d) => Some(*n as f64 / *d as f64),
        Real(v) => Some(*v),
        _ => None,
    }
}
pub(crate) fn zero(e: &Expr) -> bool {
    number(e) == Some(0.0)
}
pub(crate) fn one(e: &Expr) -> bool {
    number(e) == Some(1.0)
}
fn numeric(a: &Expr, b: &Expr, op: u8) -> Option<Expr> {
    if let (Rational(n, d), Rational(m, q)) = (a, b) {
        let (n, d, m, q) = (*n as i128, *d as i128, *m as i128, *q as i128);
        return Some(match op {
            0 => rat(n * q + m * d, d * q),
            1 => rat(n * m, d * q),
            _ => rat(n * q, d * m),
        });
    }
    Some(Real(match op {
        0 => number(a)? + number(b)?,
        1 => number(a)? * number(b)?,
        _ => number(a)? / number(b)?,
    }))
}
fn key(e: &Expr) -> String {
    format!("{e:?}")
}
fn degree(e: &Expr) -> i64 {
    match e {
        Var(_) => 1,
        Pow(a, b) => {
            if let Rational(n, 1) = **b {
                degree(a).saturating_mul(n)
            } else {
                0
            }
        }
        Mul(a, b) => degree(a).saturating_add(degree(b)),
        Div(a, b) => degree(a).saturating_sub(degree(b)),
        _ => 0,
    }
}
fn largest_power(e: &Expr) -> i64 {
    match e {
        Var(_) => 1,
        Pow(a, b) if matches!(**a, Var(_)) => {
            if let Rational(n, 1) = **b {
                n.max(0)
            } else {
                0
            }
        }
        Mul(a, b) => largest_power(a).max(largest_power(b)),
        _ => 0,
    }
}
fn sum(items: Vec<Expr>) -> Expr {
    fn flatten(e: Expr, out: &mut Vec<Expr>) {
        match e {
            Add(a, b) => {
                flatten(*a, out);
                flatten(*b, out)
            }
            Sub(a, b) => {
                flatten(*a, out);
                flatten(neg(*b), out)
            }
            _ => out.push(e),
        }
    }
    let mut flat = Vec::new();
    for e in items {
        flatten(e, &mut flat);
    }
    let mut terms: BTreeMap<String, (Expr, Expr)> = BTreeMap::new();
    let mut c = int(0);
    for e in flat {
        if number(&e).is_some() {
            c = numeric(&c, &e, 0).unwrap();
            continue;
        }
        let (coef, body) = match e {
            Mul(a, b) if number(&a).is_some() => (*a, *b),
            Neg(a) => (int(-1), *a),
            e => (int(1), e),
        };
        let k = key(&body);
        let entry = terms.entry(k).or_insert((body, int(0)));
        entry.1 = numeric(&entry.1, &coef, 0).unwrap();
    }
    let mut out: Vec<_> = terms.into_values().filter(|(_, c)| !zero(c)).collect();
    fn periodic(e: &Expr) -> bool {
        fn pi(e: &Expr) -> bool {
            match e {
                Const(Constant::Pi) => true,
                Mul(a, b) | Div(a, b) => pi(a) || pi(b),
                _ => false,
            }
        }
        contains(e, "k") && pi(e)
    }
    out.sort_by(|(a, _), (b, _)| {
        periodic(a)
            .cmp(&periodic(b))
            .then_with(|| degree(b).cmp(&degree(a)))
            .then_with(|| largest_power(b).cmp(&largest_power(a)))
            .then_with(|| key(a).cmp(&key(b)))
    });
    let mut out: Vec<Expr> = out
        .into_iter()
        .map(|(a, c)| if one(&c) { a } else { mul(c, a) })
        .collect();
    if !zero(&c) || out.is_empty() {
        out.push(c);
    }
    out.into_iter()
        .reduce(|a, b| Add(Box::new(a), Box::new(b)))
        .unwrap_or(int(0))
}
fn product(items: Vec<Expr>) -> Expr {
    fn flatten(e: Expr, out: &mut Vec<Expr>) {
        match e {
            Mul(a, b) => {
                flatten(*a, out);
                flatten(*b, out)
            }
            Neg(a) => {
                out.push(int(-1));
                flatten(*a, out)
            }
            _ => out.push(e),
        }
    }
    let mut flat = Vec::new();
    for e in items {
        flatten(e, &mut flat);
    }
    if flat.iter().any(|e| matches!(e, Div(..))) {
        fn fraction(e: Expr, top: &mut Vec<Expr>, bottom: &mut Vec<Expr>) {
            match e {
                Mul(a, b) => {
                    fraction(*a, top, bottom);
                    fraction(*b, top, bottom)
                }
                Div(a, b) => {
                    fraction(*a, top, bottom);
                    fraction(*b, bottom, top)
                }
                e => top.push(e),
            }
        }
        let mut top = Vec::new();
        let mut bottom = Vec::new();
        for e in flat {
            fraction(e, &mut top, &mut bottom);
        }
        return div(product(top), product(bottom));
    }
    let mut c = int(1);
    let mut factors: BTreeMap<String, (Expr, Expr)> = BTreeMap::new();
    for e in flat {
        if number(&e).is_some() {
            c = numeric(&c, &e, 1).unwrap();
            continue;
        }
        let (base, exp) = match e {
            Pow(a, b) => (*a, *b),
            e => (e, int(1)),
        };
        let entry = factors.entry(key(&base)).or_insert((base, int(0)));
        entry.1 = add(entry.1.clone(), exp);
    }
    if zero(&c) {
        return c;
    }
    let mut factors: Vec<_> = factors.into_values().collect();
    fn rank(e: &Expr) -> u8 {
        match e {
            Var(_) => 0,
            Const(_) => 1,
            Call(..) => 2,
            _ => 3,
        }
    }
    factors.sort_by(|(a, _), (b, _)| rank(a).cmp(&rank(b)).then_with(|| key(a).cmp(&key(b))));
    let mut out = Vec::new();
    if !one(&c) {
        out.push(c);
    }
    for (base, exp) in factors {
        if !zero(&exp) {
            out.push(if one(&exp) {
                base
            } else {
                Pow(Box::new(base), Box::new(exp))
            });
        }
    }
    out.into_iter()
        .reduce(|a, b| Mul(Box::new(a), Box::new(b)))
        .unwrap_or(int(1))
}
fn simp_div(a: Expr, b: Expr) -> Expr {
    if let Some(v) = numeric(&a, &b, 2) {
        return v;
    }
    if one(&b) {
        return a;
    }
    if zero(&a) {
        return a;
    }
    if a == b {
        return int(1);
    }
    if let Div(x, y) = a {
        return div(*x, mul(*y, b));
    }
    if let Div(x, y) = b {
        return div(mul(a, *y), *x);
    }
    if let Some(reduced) = cancel_polynomials(&a, &b) {
        return reduced;
    }
    if number(&b).is_some() {
        if let Mul(c, d) = &a {
            if number(c).is_some() {
                return mul(numeric(c, &b, 2).unwrap(), *d.clone());
            }
        }
        if let Rational(n, d) = b {
            return mul(rat(d as i128, n as i128), a);
        }
    }
    fn split(e: Expr) -> (Expr, Vec<Expr>) {
        let mut v = Vec::new();
        fn walk(e: Expr, c: &mut Expr, v: &mut Vec<Expr>) {
            match e {
                Mul(a, b) => {
                    walk(*a, c, v);
                    walk(*b, c, v)
                }
                e if number(&e).is_some() => *c = numeric(c, &e, 1).unwrap(),
                e => v.push(e),
            }
        }
        let mut c = int(1);
        walk(e, &mut c, &mut v);
        (c, v)
    }
    let (ac, mut av) = split(a);
    let (bc, mut bv) = split(b);
    let mut i = 0;
    while i < av.len() {
        fn base(e: &Expr) -> (&Expr, Expr) {
            if let Pow(a, b) = e {
                if number(b).is_some() {
                    return (a, *b.clone());
                }
            }
            (e, int(1))
        }
        let (a, an) = base(&av[i]);
        if let Some(j) = bv.iter().position(|e| base(e).0 == a) {
            let bn = base(&bv[j]).1;
            let delta = sub(an, bn);
            let a = a.clone();
            av.remove(i);
            bv.remove(j);
            if let Some(d) = number(&delta) {
                if d > 0.0 {
                    av.insert(i, pow(a, delta));
                } else if d < 0.0 {
                    bv.push(pow(a, neg(delta)));
                }
            }
        } else {
            i += 1;
        }
    }
    let c = numeric(&ac, &bc, 2).unwrap();
    let numerator = product(av);
    let bottom = product(bv);
    if one(&bottom) {
        mul(c, numerator)
    } else {
        if let Rational(n, d) = c {
            Div(
                Box::new(mul(int(n), numerator)),
                Box::new(mul(int(d), bottom)),
            )
        } else {
            Div(Box::new(mul(c, numerator)), Box::new(bottom))
        }
    }
}
fn simp_pow(a: Expr, b: Expr) -> Expr {
    if matches!(a, Real(_)) || matches!(b, Real(_)) {
        if let (Some(x), Some(y)) = (number(&a), number(&b)) {
            return Real(x.powf(y));
        }
    }
    if zero(&b) {
        return int(1);
    }
    if one(&b) {
        return a;
    }
    if one(&a) {
        return a;
    }
    if let (Rational(n, d), Rational(k, 1)) = (&a, &b) {
        if k.unsigned_abs() <= 1024 {
            let p = u32::try_from(k.unsigned_abs()).unwrap();
            if let (Some(n), Some(d)) = ((*n as i128).checked_pow(p), (*d as i128).checked_pow(p)) {
                return if *k < 0 { rat(d, n) } else { rat(n, d) };
            }
        }
        return Real((*n as f64 / *d as f64).powf(*k as f64));
    }
    if let (Some(x), Some(y)) = (number(&a), number(&b)) {
        if matches!(a, Real(_)) || matches!(b, Real(_)) {
            return Real(x.powf(y));
        }
    }
    if let Rational(1, 2) = b {
        return simp_call("sqrt", vec![a]);
    }
    if let Pow(x, y) = &a {
        if matches!(b, Rational(_, 1)) {
            return simp_pow(*x.clone(), mul(*y.clone(), b));
        }
    }
    Pow(Box::new(a), Box::new(b))
}
fn square_parts(n: i64) -> (i64, i64) {
    if n <= 0 {
        return (1, n);
    }
    let root = (n as f64).sqrt() as i64;
    if root as i128 * root as i128 == n as i128 {
        return (root, 1);
    }
    let mut rest = n;
    let mut outside = 1;
    let mut p = 2i64;
    while p <= rest / p && p <= 100_000 {
        let sq = p * p;
        while rest % sq == 0 {
            rest /= sq;
            outside *= p;
        }
        p += 1;
    }
    (outside, rest)
}
fn rational_content(e: &Expr) -> (Expr, Expr) {
    match e {
        Rational(..) => (e.clone(), int(1)),
        Mul(a, b) => {
            let (ac, a) = rational_content(a);
            let (bc, b) = rational_content(b);
            (numeric(&ac, &bc, 1).unwrap(), mul(a, b))
        }
        Div(a, b) => {
            let (ac, a) = rational_content(a);
            let (bc, b) = rational_content(b);
            (numeric(&ac, &bc, 2).unwrap(), div(a, b))
        }
        _ => (int(1), e.clone()),
    }
}
fn simp_call(n: &str, a: Vec<Expr>) -> Expr {
    if super::logic::builtin(n) {
        return super::logic::fold(n, a);
    }
    if let Some(count) = super::stats::builtin_arity(n) {
        if a.len() == count && a.iter().all(|e| number(e).is_some()) {
            let values: Vec<_> = a.iter().filter_map(number).collect();
            if a.iter().all(|e| matches!(e, Rational(_, 1))) {
                if let Some(integer) = super::stats::exact_integer(n, &values) {
                    return int(integer);
                }
            }
            return Real(super::stats::builtin(n, &values));
        }
        return Call(n.into(), a);
    }
    if a.len() == 1 {
        if let Some(angle) = inverse_trig(n, &a[0], false) {
            return angle;
        }
    }
    if super::eval::arity(n, a.len())
        && a.iter().all(|e| number(e).is_some())
        && a.iter().any(|e| matches!(e, Real(_)))
    {
        let x = number(&a[0]).unwrap();
        let y = a.get(1).and_then(number).unwrap_or(f64::NAN);
        let value = if matches!(n, "min" | "max") {
            a.iter().filter_map(number).fold(
                if n == "min" {
                    f64::INFINITY
                } else {
                    f64::NEG_INFINITY
                },
                |x, y| {
                    if x.is_nan() || y.is_nan() {
                        f64::NAN
                    } else if n == "min" {
                        x.min(y)
                    } else {
                        x.max(y)
                    }
                },
            )
        } else {
            super::eval::function(n, x, y, a.len())
        };
        return Real(value);
    }
    if a.len() == 1 && matches!(n, "sin" | "cos" | "tan") {
        let coefficient = match &a[0] {
            Const(Constant::Pi) => Some(int(1)),
            Mul(k, p) if **p == Const(Constant::Pi) => Some(*k.clone()),
            _ => None,
        };
        if let Some(Rational(k, d)) = coefficient {
            let twice = 2i128 * k as i128;
            if twice % d as i128 == 0 {
                let q = (twice / d as i128).rem_euclid(4);
                match n {
                    "sin" => return int([0, 1, 0, -1][q as usize]),
                    "cos" => return int([1, 0, -1, 0][q as usize]),
                    "tan" if q % 2 == 0 => return int(0),
                    _ => {}
                }
            }
        }
    }
    if a.len() == 1 {
        let x = &a[0];
        if n == "sqrt" {
            if zero(x) {
                return int(0);
            }
            if matches!(x, Mul(..) | Div(..)) {
                let (coefficient, rest) = rational_content(x);
                if let Rational(p, q) = coefficient {
                    if p >= 0 && q > 0 {
                        let (u, v) = square_parts(p);
                        let (w, z) = square_parts(q);
                        if u != 1 || w != 1 {
                            return mul(
                                rat(u as i128, w as i128),
                                call("sqrt", mul(rat(v as i128, z as i128), rest)),
                            );
                        }
                    }
                }
            }
            if let Rational(p, q) = x {
                if *p >= 0 && *q > 0 {
                    let (u, v) = square_parts(*p);
                    let (w, z) = square_parts(*q);
                    if v == 1 && z == 1 {
                        return rat(u as i128, w as i128);
                    }
                    if u != 1 || w != 1 {
                        return mul(
                            rat(u as i128, w as i128),
                            Call(n.into(), vec![rat(v as i128, z as i128)]),
                        );
                    }
                }
            }
        }
        if zero(x) {
            match n {
                "sin" | "tan" | "sinh" | "tanh" | "asin" | "atan" | "asinh" | "atanh" | "abs"
                | "sgn" | "sign" | "floor" | "ceil" | "round" | "cbrt" => return int(0),
                "cos" | "cosh" | "exp" | "sec" => return int(1),
                _ => {}
            }
        }
        if n == "ln" && (one(x) || *x == Const(Constant::E)) {
            return int(if one(x) { 0 } else { 1 });
        }
        if n == "abs" {
            if let Rational(p, q) = x {
                return rat((*p as i128).abs(), *q as i128);
            }
        }
    }
    if super::eval::arity(n, a.len()) && a.iter().all(|e| matches!(e, Real(_) | Rational(_, _))) {
        let inexact = a.iter().any(|e| matches!(e, Real(_)));
        let x = number(&a[0]).unwrap();
        let y = a.get(1).and_then(number).unwrap_or(f64::NAN);
        let v = if n == "min" {
            a.iter().filter_map(number).fold(f64::INFINITY, f64::min)
        } else if n == "max" {
            a.iter()
                .filter_map(number)
                .fold(f64::NEG_INFINITY, f64::max)
        } else {
            super::eval::function(n, x, y, a.len())
        };
        if inexact {
            return Real(v);
        }
        if matches!(
            n,
            "floor" | "ceil" | "round" | "sgn" | "sign" | "abs" | "min" | "max"
        ) && v.is_finite()
        {
            if n == "min" || n == "max" {
                return a.iter().find(|e| number(e) == Some(v)).unwrap().clone();
            }
            if v >= i64::MIN as f64 && v < i64::MAX as f64 {
                return int(v as i64);
            }
        }
    }
    Call(n.into(), a)
}
pub fn simplify(expr: &Expr) -> Expr {
    fn go(e: &Expr, depth: usize) -> Expr {
        if depth > 128 {
            return e.clone();
        }
        match e {
            Rational(n, d) => rat(*n as i128, *d as i128),
            Neg(a) => neg(go(a, depth + 1)),
            Add(a, b) => add(go(a, depth + 1), go(b, depth + 1)),
            Sub(a, b) => sub(go(a, depth + 1), go(b, depth + 1)),
            Mul(a, b) => mul(go(a, depth + 1), go(b, depth + 1)),
            Div(a, b) => div(go(a, depth + 1), go(b, depth + 1)),
            Pow(a, b) => pow(go(a, depth + 1), go(b, depth + 1)),
            Call(n, a) => simp_call(n, a.iter().map(|e| go(e, depth + 1)).collect()),
            Tuple(a) => Tuple(a.iter().map(|e| go(e, depth + 1)).collect()),
            List(a) => List(a.iter().map(|e| go(e, depth + 1)).collect()),
            _ => e.clone(),
        }
    }
    go(expr, 0)
}
pub fn expand(expr: &Expr) -> Expr {
    fn first_var(e: &Expr) -> Option<&str> {
        match e {
            Var(v) => Some(v),
            Neg(a) => first_var(a),
            Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) | Pow(a, b) => {
                first_var(a).or_else(|| first_var(b))
            }
            Call(_, a) => a.iter().find_map(first_var),
            _ => None,
        }
    }
    if let Some(v) = first_var(expr) {
        fn only_var(e: &Expr, v: &str) -> bool {
            match e {
                Var(n) => n == v,
                Neg(a) => only_var(a, v),
                Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) | Pow(a, b) => {
                    only_var(a, v) && only_var(b, v)
                }
                Call(_, a) => a.iter().all(|e| only_var(e, v)),
                _ => true,
            }
        }
        if only_var(expr, v)
            && let Some(p) = polynomial(expr, v)
        {
            if p.iter().all(|e| number(e).is_some()) {
                return from_poly(&p, v);
            }
        }
    }
    fn size(e: &Expr) -> usize {
        match e {
            Add(a, b) | Sub(a, b) => size(a).saturating_add(size(b)).min(2049),
            Mul(a, b) => size(a).saturating_mul(size(b)).min(2049),
            Pow(a, b) => {
                if let Rational(n, 1) = **b {
                    if (0..=20).contains(&n) {
                        let k = size(a);
                        let mut total = 1usize;
                        for i in 1..=n as usize {
                            total = total.saturating_mul(k + i - 1) / i;
                            if total > 2048 {
                                return 2049;
                            }
                        }
                        total
                    } else {
                        1
                    }
                } else {
                    1
                }
            }
            _ => 1,
        }
    }
    if size(expr) > 2048 {
        return simplify(expr);
    }
    fn dist(a: Expr, b: Expr, budget: &mut usize) -> Expr {
        if *budget == 0 {
            return mul(a, b);
        }
        *budget -= 1;
        match (a, b) {
            (Add(a, b), c) => add(dist(*a, c.clone(), budget), dist(*b, c, budget)),
            (a, Add(b, c)) => add(dist(a.clone(), *b, budget), dist(a, *c, budget)),
            (a, b) => mul(a, b),
        }
    }
    fn go(e: &Expr, budget: &mut usize, depth: usize) -> Expr {
        if depth > 64 || *budget == 0 {
            return simplify(e);
        }
        match e {
            Add(a, b) => add(go(a, budget, depth + 1), go(b, budget, depth + 1)),
            Sub(a, b) => sub(go(a, budget, depth + 1), go(b, budget, depth + 1)),
            Mul(a, b) => {
                let a = go(a, budget, depth + 1);
                let b = go(b, budget, depth + 1);
                dist(a, b, budget)
            }
            Pow(a, b) => {
                let a = go(a, budget, depth + 1);
                if let Rational(n, 1) = **b {
                    if (0..=20).contains(&n) {
                        let mut out = int(1);
                        for _ in 0..n {
                            out = dist(out, a.clone(), budget);
                            if *budget == 0 {
                                break;
                            }
                        }
                        if *budget > 0 {
                            return out;
                        }
                    }
                }
                pow(a, simplify(b))
            }
            _ => simplify(e),
        }
    }
    go(expr, &mut 4096, 0)
}
pub(crate) fn polynomial(e: &Expr, v: &str) -> Option<Vec<Expr>> {
    if !contains(e, v) {
        return Some(vec![e.clone()]);
    }
    fn plus(mut a: Vec<Expr>, b: Vec<Expr>, negative: bool) -> Vec<Expr> {
        a.resize(a.len().max(b.len()), int(0));
        for (i, c) in b.into_iter().enumerate() {
            a[i] = if negative {
                sub(a[i].clone(), c)
            } else {
                add(a[i].clone(), c)
            };
        }
        trim(&mut a);
        a
    }
    fn times(a: Vec<Expr>, b: Vec<Expr>) -> Option<Vec<Expr>> {
        if a.len() + b.len() > 130 {
            return None;
        }
        let mut out = vec![int(0); a.len() + b.len() - 1];
        for (i, x) in a.iter().enumerate() {
            for (j, y) in b.iter().enumerate() {
                out[i + j] = add(out[i + j].clone(), mul(x.clone(), y.clone()));
            }
        }
        trim(&mut out);
        Some(out)
    }
    match e {
        Rational(..) | Real(_) => Some(vec![e.clone()]),
        Var(n) if n == v => Some(vec![int(0), int(1)]),
        Neg(a) => Some(polynomial(a, v)?.into_iter().map(neg).collect()),
        Add(a, b) => Some(plus(polynomial(a, v)?, polynomial(b, v)?, false)),
        Sub(a, b) => Some(plus(polynomial(a, v)?, polynomial(b, v)?, true)),
        Mul(a, b) => times(polynomial(a, v)?, polynomial(b, v)?),
        Div(a, b) if !contains(b, v) && !zero(b) => Some(
            polynomial(a, v)?
                .into_iter()
                .map(|c| div(c, *b.clone()))
                .collect(),
        ),
        Pow(a, b) => {
            if let Rational(n, 1) = **b {
                if (0..=64).contains(&n) {
                    let p = polynomial(a, v)?;
                    let mut out = vec![int(1)];
                    for _ in 0..n {
                        out = times(out, p.clone())?;
                    }
                    return Some(out);
                }
            }
            None
        }
        _ => None,
    }
}
pub(crate) fn trim(p: &mut Vec<Expr>) {
    while p.len() > 1 && p.last().is_some_and(zero) {
        p.pop();
    }
}
pub(crate) fn polynomial_div_rem(a: &[Expr], b: &[Expr]) -> Option<(Vec<Expr>, Vec<Expr>)> {
    if b.is_empty() || b.iter().all(zero) || !a.iter().chain(b).all(|e| matches!(e, Rational(..))) {
        return None;
    }
    let mut r = a.to_vec();
    trim(&mut r);
    let mut q = vec![int(0); a.len().saturating_sub(b.len()) + 1];
    for _ in 0..130 {
        if r.len() < b.len() || r.iter().all(zero) {
            return Some((q, r));
        }
        let offset = r.len() - b.len();
        let c = numeric(r.last()?, b.last()?, 2)?;
        if !matches!(c, Rational(..)) {
            return None;
        }
        q[offset] = c.clone();
        for (i, x) in b.iter().enumerate() {
            let term = numeric(&c, x, 1)?;
            r[i + offset] = numeric(&r[i + offset], &neg(term), 0)?;
            if !matches!(r[i + offset], Rational(..)) {
                return None;
            }
        }
        trim(&mut r);
    }
    None
}
fn cancel_polynomials(a: &Expr, b: &Expr) -> Option<Expr> {
    fn vars(e: &Expr, out: &mut std::collections::BTreeSet<String>) {
        match e {
            Var(n) => {
                out.insert(n.clone());
            }
            Neg(a) => vars(a, out),
            Add(a, b) | Sub(a, b) | Mul(a, b) | Div(a, b) | Pow(a, b) => {
                vars(a, out);
                vars(b, out)
            }
            Call(_, a) => {
                for e in a {
                    vars(e, out)
                }
            }
            _ => {}
        }
    }
    let mut names = std::collections::BTreeSet::new();
    vars(a, &mut names);
    vars(b, &mut names);
    if names.len() != 1 {
        return None;
    }
    let v = names.first()?;
    let p = polynomial(a, v)?;
    let q = polynomial(b, v)?;
    if p.len() < 2
        || q.len() < 2
        || p.len() > 65
        || q.len() > 65
        || !p.iter().chain(&q).all(|e| matches!(e, Rational(..)))
    {
        return None;
    }
    let (mut x, mut y) = (p.clone(), q.clone());
    for _ in 0..65 {
        if y.iter().all(zero) {
            break;
        }
        let (_, r) = polynomial_div_rem(&x, &y)?;
        x = y;
        y = r;
    }
    if x.len() < 2 || !y.iter().all(zero) {
        return None;
    }
    let leading = x.last()?.clone();
    for c in &mut x {
        *c = numeric(c, &leading, 2)?;
    }
    let (top, rt) = polynomial_div_rem(&p, &x)?;
    let (bottom, rb) = polynomial_div_rem(&q, &x)?;
    if !rt.iter().chain(&rb).all(zero) {
        return None;
    }
    if bottom.len() == 1 {
        return Some(from_poly(
            &top.iter()
                .map(|c| numeric(c, &bottom[0], 2))
                .collect::<Option<Vec<_>>>()?,
            v,
        ));
    }
    Some(div(from_poly(&top, v), from_poly(&bottom, v)))
}
pub(crate) fn inverse_trig(n: &str, x: &Expr, approximate: bool) -> Option<Expr> {
    if !matches!(n, "asin" | "acos" | "atan") {
        return None;
    }
    let pi = Const(Constant::Pi);
    let table = if n == "atan" {
        vec![
            (int(0), rat(0, 1)),
            (int(1), rat(1, 4)),
            (call("sqrt", int(3)), rat(1, 3)),
            (div(call("sqrt", int(3)), int(3)), rat(1, 6)),
        ]
    } else {
        vec![
            (int(0), rat(0, 1)),
            (rat(1, 2), rat(1, 6)),
            (div(call("sqrt", int(2)), int(2)), rat(1, 4)),
            (div(call("sqrt", int(3)), int(2)), rat(1, 3)),
            (int(1), rat(1, 2)),
        ]
    };
    let numeric = if approximate {
        super::eval::eval(x, &super::eval::Empty).ok()
    } else {
        None
    };
    for (input, angle) in table {
        for sign in [1, -1] {
            let candidate = if sign == 1 {
                input.clone()
            } else {
                neg(input.clone())
            };
            let close = numeric
                .zip(super::eval::eval(&candidate, &super::eval::Empty).ok())
                .is_some_and(|(x, y)| x.is_finite() && (x - y).abs() < 1e-14);
            if *x == candidate || close {
                let angle = if sign == 1 {
                    angle.clone()
                } else {
                    neg(angle.clone())
                };
                let angle = if n == "acos" {
                    sub(rat(1, 2), angle)
                } else {
                    angle
                };
                return Some(mul(angle, pi));
            }
        }
    }
    None
}
pub(crate) fn from_poly(p: &[Expr], v: &str) -> Expr {
    let x = Var(v.into());
    let mut e = int(0);
    for (i, c) in p.iter().enumerate() {
        if !zero(c) {
            e = add(
                e,
                if i == 0 {
                    c.clone()
                } else {
                    mul(c.clone(), pow(x.clone(), int(i as i64)))
                },
            );
        }
    }
    e
}
pub(crate) fn synthetic(p: &[Expr], r: &Expr) -> (Vec<Expr>, Expr) {
    if p.len() < 2 {
        return (vec![], p[0].clone());
    }
    let mut q = vec![int(0); p.len() - 1];
    let last = q.len() - 1;
    q[last] = p.last().unwrap().clone();
    for i in (0..last).rev() {
        q[i] = add(p[i + 1].clone(), mul(r.clone(), q[i + 1].clone()));
    }
    let rem = add(p[0].clone(), mul(r.clone(), q[0].clone()));
    (q, rem)
}
pub(crate) fn rational_root(p: &[Expr]) -> Option<Expr> {
    if zero(&p[0]) {
        return Some(int(0));
    }
    if !p.iter().all(|e| matches!(e, Rational(_, _))) {
        return None;
    }
    if p.len() == 2 {
        return Some(div(neg(p[0].clone()), p[1].clone()));
    }
    if p.len() == 3 {
        let disc = sub(
            pow(p[1].clone(), int(2)),
            mul(int(4), mul(p[2].clone(), p[0].clone())),
        );
        let r = call("sqrt", disc);
        return if matches!(r, Rational(..)) {
            Some(div(sub(neg(p[1].clone()), r), mul(int(2), p[2].clone())))
        } else {
            None
        };
    }
    let mut lcm = 1i128;
    for e in p {
        if let Rational(_, d) = e {
            let mut a = lcm;
            let mut b = *d as i128;
            while b != 0 {
                (a, b) = (b, a % b);
            }
            lcm = lcm.checked_mul(*d as i128 / a)?;
        }
    }
    let integer = |e: &Expr| {
        if let Rational(n, d) = e {
            (*n as i128).checked_mul(lcm / *d as i128)
        } else {
            None
        }
    };
    let a = integer(&p[0])?.unsigned_abs();
    let b = integer(p.last()?)?.unsigned_abs();
    fn divisors(n: u128) -> Vec<i128> {
        let mut out = Vec::new();
        let mut i = 1u128;
        while i <= n / i && i <= 100_000 {
            if n % i == 0 {
                out.push(i as i128);
                if i != n / i {
                    out.push((n / i) as i128);
                }
            }
            i += 1;
        }
        out.sort();
        out
    }
    let aa = divisors(a);
    let bb = divisors(b);
    let mut count = 0;
    for n in aa {
        for d in &bb {
            for sign in [-1, 1] {
                count += 1;
                if count > 20_000 {
                    return None;
                }
                let r = rat(n * sign, *d);
                if zero(&synthetic(p, &r).1) {
                    return Some(r);
                }
            }
        }
    }
    None
}
pub fn factor(expr: &Expr, var: &str) -> Expr {
    let s = simplify(expr);
    let Some(mut p) = polynomial(&s, var) else {
        return s;
    };
    if !p.iter().all(|e| matches!(e, Rational(_, _))) || p.len() < 2 {
        return s;
    }
    fn gcd(mut a: i128, mut b: i128) -> i128 {
        while b != 0 {
            (a, b) = (b, a % b);
        }
        a.abs()
    }
    let (mut numerator, mut denominator) = (0i128, 1i128);
    for c in &p {
        if let Rational(n, d) = c {
            numerator = gcd(numerator, *n as i128);
            let Some(lcm) = denominator.checked_mul(*d as i128 / gcd(denominator, *d as i128))
            else {
                return s;
            };
            denominator = lcm;
        }
    }
    if numerator == 0 {
        return int(0);
    }
    if number(p.last().unwrap()).is_some_and(|x| x < 0.0) {
        numerator = -numerator;
    }
    let content = rat(numerator, denominator);
    for c in &mut p {
        *c = div(c.clone(), content.clone());
    }
    let mut factors = vec![content];
    while p.len() > 1 {
        if let Some(r) = rational_root(&p) {
            factors.push(sub(Var(var.into()), r.clone()));
            p = synthetic(&p, &r).0;
            trim(&mut p);
        } else {
            break;
        }
    }
    if p.len() > 1 {
        factors.push(from_poly(&p, var));
    } else {
        factors.insert(0, p[0].clone());
    }
    product(factors)
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{format::format_expr, parser::parse_expr};
    fn s(x: &str) -> Expr {
        simplify(&parse_expr(x).unwrap())
    }
    #[test]
    fn identities() {
        for (a, b) in [
            ("x+0", "x"),
            ("x*1", "x"),
            ("x*0", "0"),
            ("x^1", "x"),
            ("x^0", "1"),
            ("0/x", "0"),
            ("x/1", "x"),
            ("--x", "x"),
            ("x/x", "1"),
            ("2x+3x", "5x"),
            ("x*x", "x^2"),
            ("x^2*x^3", "x^5"),
            ("1/2+1/3", "5/6"),
        ] {
            assert_eq!(s(a), s(b), "{a}");
        }
    }
    #[test]
    fn radicals() {
        for (a, b) in [
            ("sqrt(8)", "2sqrt(2)"),
            ("sqrt(1/4)", "1/2"),
            ("sqrt(72)", "6sqrt(2)"),
            ("sin(0)", "0"),
            ("cos(0)", "1"),
            ("ln(1)", "0"),
            ("ln(e)", "1"),
            ("exp(0)", "1"),
        ] {
            assert_eq!(s(a), s(b), "{a}");
        }
    }
    #[test]
    fn expansion() {
        assert_eq!(expand(&parse_expr("(x+1)^2").unwrap()), s("x^2+2x+1"));
        assert_eq!(expand(&parse_expr("(x+2)(x-2)").unwrap()), s("x^2-4"));
    }
    #[test]
    fn factoring() {
        for a in ["x^2-1", "2x^2-8", "x^3-x", "3x^2+6x+3", "x^4-5x^2+4"] {
            let e = parse_expr(a).unwrap();
            let f = factor(&e, "x");
            assert_eq!(expand(&f), expand(&e), "{}", format_expr(&f));
            assert!(matches!(f, Mul(..) | Pow(..)));
        }
    }
    #[test]
    fn canonical() {
        assert_eq!(s("y+x+3x"), s("4x+y"));
        assert_eq!(s("y*x"), s("x*y"));
        assert!(matches!(s("1/2+0.5"), Real(_)));
        assert!(matches!(s("9223372036854775807+1"), Real(_)));
    }
    #[test]
    fn inexact_constants_remain_inexact() {
        for input in [
            "2.0^0",
            "1^2.0",
            "sin(0.0)",
            "cos(0.0)",
            "ln(1.0)",
            "exp(0.0)",
            "abs(0.0)",
            "1/2+0.5",
            "sqrt(4.0)",
        ] {
            assert!(matches!(s(input), Real(_)), "{input}");
        }
        assert!(matches!(s("min(2.0,sqrt(-1.0))"),Real(x) if x.is_nan()));
    }
    #[test]
    fn bounded() {
        let e = parse_expr("(x+y+z+1)^20").unwrap();
        let _ = expand(&e);
    }
    #[test]
    fn content_and_quotients() {
        for (a, b) in [
            ("x^3/x^2", "x"),
            ("x/x^3", "1/x^2"),
            ("x^2*y/(x*y^2)", "x/y"),
        ] {
            assert_eq!(s(a), s(b), "{a}");
        }
        for (a, b) in [
            ("2x^2+2", "2(x^2+1)"),
            ("2x^3+2x", "2x(x^2+1)"),
            ("x^2/2+1/2", "(x^2+1)/2"),
            ("-3x^2-3", "-3(x^2+1)"),
            ("x^2-2000036x+1000036000099", "(x-1000003)(x-1000033)"),
        ] {
            assert_eq!(factor(&parse_expr(a).unwrap(), "x"), s(b), "{a}");
        }
    }
    #[test]
    fn pi_and_extreme_powers() {
        for (a, b) in [
            ("sin(pi)", "0"),
            ("sin(pi/2)", "1"),
            ("cos(pi)", "-1"),
            ("cos(pi/2)", "0"),
            ("tan(2pi)", "0"),
        ] {
            assert_eq!(s(a), s(b), "{a}");
        }
        assert!(matches!(s("(-2)^(-127)"), Real(_)));
    }
    #[test]
    fn twentieth_power() {
        let e = expand(&parse_expr("(x+1)^20").unwrap());
        let p = polynomial(&e, "x").unwrap();
        assert_eq!(p.len(), 21);
        assert_eq!(p[10], int(184756));
        assert_eq!(p[0], int(1));
        assert_eq!(p[20], int(1));
    }
}

#[cfg(test)]
mod review_tests {
    use super::*;
    use crate::parser::parse_expr;
    fn s(input: &str) -> Expr {
        simplify(&parse_expr(input).unwrap())
    }
    #[test]
    fn polynomial_gcd() {
        for (input, want) in [
            ("(x^2-1)/(x-1)", "x+1"),
            ("(x^4-1)/(x^2-1)", "x^2+1"),
            ("(x^3-x)/(x^2-1)", "x"),
            ("(2x^2-2)/(4x-4)", "x/2+1/2"),
            ("((x^2+1)*(x+2))/((x^2+1)*(x+3))", "(x+2)/(x+3)"),
            ("1/(x^2+1)", "1/(x^2+1)"),
        ] {
            assert_eq!(s(input), s(want), "{input}");
        }
    }
    #[test]
    fn quotient_products() {
        for (input, want) in [
            ("a*(1/b)", "a/b"),
            ("2x*(1/(x^2+1))", "2x/(x^2+1)"),
            ("(a/b)*(c/d)", "(a*c)/(b*d)"),
            ("(a/b)/(c/d)", "(a*d)/(b*c)"),
        ] {
            assert_eq!(s(input), s(want), "{input}");
        }
    }
    #[test]
    fn standard_inverse_values() {
        for (input, want) in [
            ("asin(1/2)", "pi/6"),
            ("asin(-1/2)", "-pi/6"),
            ("asin(sqrt(2)/2)", "pi/4"),
            ("asin(sqrt(3)/2)", "pi/3"),
            ("asin(1)", "pi/2"),
            ("acos(-1/2)", "2pi/3"),
            ("acos(-sqrt(2)/2)", "3pi/4"),
            ("acos(0)", "pi/2"),
            ("acos(-1)", "pi"),
            ("atan(1)", "pi/4"),
            ("atan(-sqrt(3))", "-pi/3"),
            ("atan(sqrt(3)/3)", "pi/6"),
        ] {
            assert_eq!(s(input), s(want), "{input}");
        }
    }
}

#[cfg(test)]
mod cosmetic_tests {
    use super::*;
    use crate::{
        eval::{Empty, compile},
        parser::parse_expr,
    };
    fn s(input: &str) -> Expr {
        simplify(&parse_expr(input).unwrap())
    }
    #[test]
    fn constant_radical_factors() {
        for (input, want) in [
            ("sqrt(8/a)", "2sqrt(2/a)"),
            ("sqrt(8*a)", "2sqrt(2*a)"),
            ("sqrt(9*a/(4*b))", "3sqrt(a/b)/2"),
            ("sqrt(a/8)", "sqrt(a/2)/2"),
            ("sqrt(8)", "2sqrt(2)"),
        ] {
            let simplified = s(input);
            assert_eq!(simplified, s(want), "{input}");
            assert_eq!(simplify(&simplified), simplified, "{input}");
            let original = compile(&parse_expr(input).unwrap(), &["a", "b"], &Empty).unwrap();
            let reduced = compile(&simplified, &["a", "b"], &Empty).unwrap();
            for values in [[0.5, 2.0], [2.0, 3.0], [7.0, 0.3]] {
                assert!(
                    (original.eval(&values) - reduced.eval(&values)).abs() < 1e-12,
                    "{input}"
                );
            }
        }
    }
    #[test]
    fn combined_denominators() {
        for (input, want) in [
            ("a/2/b", "a/(2*b)"),
            ("a/b/c", "a/(b*c)"),
            ("a/2/b/3/c", "a/(6*b*c)"),
            ("(-b+sqrt(b^2-4*a*c))/2/a", "(-b+sqrt(b^2-4*a*c))/(2*a)"),
        ] {
            assert_eq!(s(input), s(want), "{input}");
        }
    }
}

#[cfg(test)]
mod stats_tests {
    use super::*;
    use crate::parser::parse_expr;
    #[test]
    fn integer_and_distribution_folding() {
        for (input, want) in [
            ("5!", Expr::Rational(120, 1)),
            ("nCr(66,33)", Expr::Rational(7219428434016265740, 1)),
            ("binomial(10,3)", Expr::Rational(120, 1)),
        ] {
            assert_eq!(simplify(&parse_expr(input).unwrap()), want);
        }
        assert!(
            matches!(simplify(&parse_expr("normalcdf(0,1,0)").unwrap()),Expr::Real(v) if v==0.5)
        );
        assert!(matches!(
            simplify(&parse_expr("factorial(0.5)").unwrap()),
            Expr::Real(_)
        ));
        assert!(matches!(
            simplify(&parse_expr("normalcdf(0,1,x)").unwrap()),
            Expr::Call(_, _)
        ));
    }
}
