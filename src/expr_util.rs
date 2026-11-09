use plotwerk_math::{Context, Expr};
use std::collections::{HashMap, HashSet};

pub struct NoCtx;

impl Context for NoCtx {
    fn value(&self, _name: &str) -> Option<f64> {
        None
    }

    fn function(&self, _name: &str) -> Option<(Vec<String>, Expr)> {
        None
    }
}

pub fn num(v: f64) -> Expr {
    if v.fract() == 0.0 && v.abs() < 1e15 {
        Expr::Rational(v as i64, 1)
    } else {
        Expr::Real(v)
    }
}

pub fn var(name: &str) -> Expr {
    Expr::Var(name.to_string())
}

pub fn add(a: Expr, b: Expr) -> Expr {
    Expr::Add(Box::new(a), Box::new(b))
}

pub fn sub(a: Expr, b: Expr) -> Expr {
    Expr::Sub(Box::new(a), Box::new(b))
}

pub fn mul(a: Expr, b: Expr) -> Expr {
    Expr::Mul(Box::new(a), Box::new(b))
}

pub fn div(a: Expr, b: Expr) -> Expr {
    Expr::Div(Box::new(a), Box::new(b))
}

pub fn pow(a: Expr, b: Expr) -> Expr {
    Expr::Pow(Box::new(a), Box::new(b))
}

pub fn map_children(e: &Expr, f: &mut dyn FnMut(&Expr) -> Expr) -> Expr {
    match e {
        Expr::Neg(a) => Expr::Neg(Box::new(f(a))),
        Expr::Add(a, b) => Expr::Add(Box::new(f(a)), Box::new(f(b))),
        Expr::Sub(a, b) => Expr::Sub(Box::new(f(a)), Box::new(f(b))),
        Expr::Mul(a, b) => Expr::Mul(Box::new(f(a)), Box::new(f(b))),
        Expr::Div(a, b) => Expr::Div(Box::new(f(a)), Box::new(f(b))),
        Expr::Pow(a, b) => Expr::Pow(Box::new(f(a)), Box::new(f(b))),
        Expr::Call(n, args) => Expr::Call(n.clone(), args.iter().map(|a| f(a)).collect()),
        Expr::Tuple(items) => Expr::Tuple(items.iter().map(|a| f(a)).collect()),
        Expr::List(items) => Expr::List(items.iter().map(|a| f(a)).collect()),
        other => other.clone(),
    }
}

pub fn for_each_child(e: &Expr, f: &mut dyn FnMut(&Expr)) {
    match e {
        Expr::Neg(a) => f(a),
        Expr::Add(a, b) | Expr::Sub(a, b) | Expr::Mul(a, b) | Expr::Div(a, b) | Expr::Pow(a, b) => {
            f(a);
            f(b);
        }
        Expr::Call(_, items) | Expr::Tuple(items) | Expr::List(items) => items.iter().for_each(|a| f(a)),
        _ => {}
    }
}

pub fn substitute_map(e: &Expr, map: &HashMap<String, Expr>) -> Expr {
    match e {
        Expr::Var(n) => map.get(n).cloned().unwrap_or_else(|| e.clone()),
        _ => map_children(e, &mut |c| substitute_map(c, map)),
    }
}

pub fn substitute(e: &Expr, name: &str, with: &Expr) -> Expr {
    let mut map = HashMap::new();
    map.insert(name.to_string(), with.clone());
    substitute_map(e, &map)
}

pub fn contains_var(e: &Expr, name: &str) -> bool {
    if let Expr::Var(n) = e {
        return n == name;
    }
    let mut found = false;
    for_each_child(e, &mut |c| {
        if !found && contains_var(c, name) {
            found = true;
        }
    });
    found
}

pub fn collect_names(e: &Expr, out: &mut HashSet<String>) {
    match e {
        Expr::Var(n) => {
            out.insert(n.clone());
        }
        Expr::Call(n, _) => {
            out.insert(n.clone());
        }
        _ => {}
    }
    for_each_child(e, &mut |c| collect_names(c, out));
}

pub fn collect_vars(e: &Expr, out: &mut HashSet<String>) {
    if let Expr::Var(n) = e {
        out.insert(n.clone());
    }
    for_each_child(e, &mut |c| collect_vars(c, out));
}

pub fn literal_value(e: &Expr) -> Option<f64> {
    match e {
        Expr::Rational(n, d) => Some(*n as f64 / *d as f64),
        Expr::Real(v) => Some(*v),
        Expr::Neg(a) => literal_value(a).map(|v| -v),
        _ => None,
    }
}

pub fn prime_notation(text: &str) -> String {
    let chars: Vec<char> = text.chars().collect();
    let mut out = String::with_capacity(text.len() + 8);
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c == '\'' {
            let mut count = 0;
            while i < chars.len() && chars[i] == '\'' {
                count += 1;
                i += 1;
            }
            let prev_ident = out.chars().last().map(|p| p.is_alphanumeric()).unwrap_or(false);
            if prev_ident {
                out.push_str(&format!("_d{count}"));
            }
            continue;
        }
        out.push(c);
        i += 1;
    }
    out
}

pub fn display_prime(name: &str) -> String {
    if let Some(pos) = name.rfind("_d") {
        if let Ok(n) = name[pos + 2..].parse::<usize>() {
            return format!("{}{}", &name[..pos], "'".repeat(n));
        }
    }
    name.to_string()
}
