use super::ast::{Constant, Expr, Statement};

#[derive(Clone, Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub position: usize,
}
#[derive(Clone, Debug, PartialEq)]
enum T {
    Num(String),
    Id(String),
    Plus,
    Minus,
    Mul,
    Div,
    Pow,
    Lp,
    Rp,
    Lb,
    Rb,
    Comma,
    Eq,
    Bar,
    Sqrt,
    Square,
    Cube,
    Degree,
    Factorial,
    Compare(String),
    And,
    Or,
    Not,
    End,
}
struct Parser {
    tokens: Vec<(T, usize)>,
    i: usize,
    depth: usize,
}
pub(crate) fn builtin(n: &str) -> bool {
    super::logic::builtin(n)
        || super::stats::builtin_arity(n).is_some()
        || matches!(
            n,
            "sin"
                | "cos"
                | "tan"
                | "cot"
                | "sec"
                | "csc"
                | "asin"
                | "acos"
                | "atan"
                | "atan2"
                | "sinh"
                | "cosh"
                | "tanh"
                | "asinh"
                | "acosh"
                | "atanh"
                | "exp"
                | "ln"
                | "log"
                | "lg"
                | "ld"
                | "sqrt"
                | "cbrt"
                | "nroot"
                | "abs"
                | "sgn"
                | "sign"
                | "floor"
                | "ceil"
                | "round"
                | "min"
                | "max"
                | "arcsin"
                | "arccos"
                | "arctan"
                | "arcsinh"
                | "arccosh"
                | "arctanh"
        )
}
fn alias(n: String) -> String {
    match n.as_str() {
        "arcsin" => "asin",
        "arccos" => "acos",
        "arctan" => "atan",
        "arcsinh" => "asinh",
        "arccosh" => "acosh",
        "arctanh" => "atanh",
        _ => return n,
    }
    .into()
}
fn lex(s: &str) -> Result<Vec<(T, usize)>, ParseError> {
    let mut out = Vec::new();
    let mut it = s.char_indices().peekable();
    while let Some((p, c)) = it.next() {
        if c.is_whitespace() {
            continue;
        }
        let t = match c {
            '0'..='9' | '.' => {
                let mut n = c.to_string();
                while let Some(&(_, c)) = it.peek() {
                    if c.is_ascii_digit() || c == '.' {
                        n.push(c);
                        it.next();
                    } else {
                        break;
                    }
                }
                if let Some(&(_, c)) = it.peek() {
                    if c == 'E' || c == 'e' {
                        let mut probe = it.clone();
                        probe.next();
                        if matches!(probe.peek(), Some((_, '+' | '-'))) {
                            probe.next();
                        }
                        if matches!(probe.peek(), Some((_, '0'..='9'))) {
                            n.push(c);
                            it.next();
                            if let Some(&(_, c @ ('+' | '-'))) = it.peek() {
                                n.push(c);
                                it.next();
                            }
                            while let Some(&(_, c)) = it.peek() {
                                if c.is_ascii_digit() {
                                    n.push(c);
                                    it.next();
                                } else {
                                    break;
                                }
                            }
                        }
                    }
                }
                if n.parse::<f64>().is_err() {
                    return Err(ParseError {
                        message: "Ungültige Zahl".into(),
                        position: p,
                    });
                }
                T::Num(n)
            }
            'π' => T::Id("pi".into()),
            'ℯ' => T::Id("e".into()),
            '√' => T::Sqrt,
            '²' => T::Square,
            '³' => T::Cube,
            '°' => T::Degree,
            '+' => T::Plus,
            '-' | '−' => T::Minus,
            '*' | '·' | '×' => T::Mul,
            '/' | '÷' => T::Div,
            '^' => T::Pow,
            '!' if matches!(it.peek(), Some((_, '='))) => {
                it.next();
                T::Compare("!=".into())
            }
            '!' => T::Factorial,
            '¬' => T::Not,
            '≤' => T::Compare("<=".into()),
            '≥' => T::Compare(">=".into()),
            '≠' => T::Compare("!=".into()),
            '<' | '>' => {
                let mut op = c.to_string();
                if matches!(it.peek(), Some((_, '='))) {
                    it.next();
                    op.push('=');
                }
                T::Compare(op)
            }
            '∧' => T::And,
            '∨' => T::Or,
            '&' if matches!(it.peek(), Some((_, '&'))) => {
                it.next();
                T::And
            }
            '|' if matches!(it.peek(), Some((_, '|'))) => {
                it.next();
                T::Or
            }
            '∞' => T::Id("inf".into()),
            '(' => T::Lp,
            ')' => T::Rp,
            '{' => T::Lb,
            '}' => T::Rb,
            ',' => T::Comma,
            '=' if matches!(it.peek(), Some((_, '='))) => {
                it.next();
                T::Compare("==".into())
            }
            '=' => T::Eq,
            '|' => T::Bar,
            ':' if matches!(it.peek(), Some((_, '='))) => {
                it.next();
                T::Eq
            }
            c if c.is_alphabetic() => {
                let mut n = c.to_string();
                while let Some(&(_, c)) = it.peek() {
                    if c.is_alphanumeric() && c != '²' && c != '³' || c == '_' {
                        n.push(c);
                        it.next();
                    } else {
                        break;
                    }
                }
                T::Id(n)
            }
            _ => {
                return Err(ParseError {
                    message: "Unerwartetes Zeichen".into(),
                    position: p,
                });
            }
        };
        out.push((t, p));
        if out.len() > 4096 {
            return Err(ParseError {
                message: "Ausdruck zu groß".into(),
                position: p,
            });
        }
    }
    out.push((T::End, s.len()));
    Ok(out)
}
impl Parser {
    fn t(&self) -> &T {
        &self.tokens[self.i].0
    }
    fn err(&self, msg: &str) -> ParseError {
        ParseError {
            message: msg.into(),
            position: self.tokens[self.i].1,
        }
    }
    fn eat(&mut self, t: T) -> bool {
        if *self.t() == t {
            self.i += 1;
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: T) -> Result<(), ParseError> {
        if self.eat(t) {
            Ok(())
        } else {
            Err(self.err("Klammer nicht geschlossen"))
        }
    }
    fn equation(&mut self) -> Result<Expr, ParseError> {
        let a = self.expr(0)?;
        if self.eat(T::Eq) {
            let b = self.expr(0)?;
            Ok(Expr::Call("=".into(), vec![a, b]))
        } else {
            Ok(a)
        }
    }
    fn expr(&mut self, min: u8) -> Result<Expr, ParseError> {
        self.depth += 1;
        if self.depth > 128 {
            return Err(self.err("Ausdruck zu tief verschachtelt"));
        }
        let t = self.t().clone();
        self.i += 1;
        let mut a = match t {
            T::Num(n) => {
                if n.contains(['.', 'e', 'E']) {
                    Expr::Real(n.parse().unwrap())
                } else {
                    n.parse::<i64>()
                        .map(|n| Expr::Rational(n, 1))
                        .unwrap_or_else(|_| Expr::Real(n.parse().unwrap()))
                }
            }
            T::Id(n) => {
                if *self.t() == T::Lp && !matches!(n.as_str(), "x" | "y" | "z" | "t" | "pi" | "e") {
                    self.i += 1;
                    let args = self.args(T::Rp)?;
                    Expr::Call(alias(n), args)
                } else {
                    match n.as_str() {
                        "pi" => Expr::Const(Constant::Pi),
                        "e" => Expr::Const(Constant::E),
                        _ => Expr::Var(n),
                    }
                }
            }
            T::Minus => Expr::Neg(Box::new(self.expr(25)?)),
            T::Not | T::Factorial => Expr::Call("not".into(), vec![self.expr(4)?]),
            T::Plus => self.expr(25)?,
            T::Sqrt => Expr::Call("sqrt".into(), vec![self.expr(25)?]),
            T::Lp => {
                let a = self.equation()?;
                if self.eat(T::Comma) {
                    let mut v = vec![a];
                    loop {
                        v.push(self.equation()?);
                        if !self.eat(T::Comma) {
                            break;
                        }
                    }
                    self.expect(T::Rp)?;
                    Expr::Tuple(v)
                } else {
                    self.expect(T::Rp)?;
                    a
                }
            }
            T::Lb => Expr::List(self.args(T::Rb)?),
            T::Bar => {
                let a = self.expr(0)?;
                self.expect(T::Bar)?;
                Expr::Call("abs".into(), vec![a])
            }
            _ => {
                self.i = self.i.saturating_sub(1);
                return Err(self.err("Ausdruck erwartet"));
            }
        };
        let mut comparison_rhs: Option<Expr> = None;
        loop {
            if min <= 5
                && let T::Compare(op) = self.t().clone()
            {
                self.i += 1;
                let b = self.expr(6)?;
                let chained = comparison_rhs.is_some();
                let left = comparison_rhs.take().unwrap_or_else(|| a.clone());
                let next = Expr::Call(op, vec![left, b.clone()]);
                a = if chained {
                    Expr::Call("and".into(), vec![a, next])
                } else {
                    next
                };
                comparison_rhs = Some(b);
                continue;
            }
            let logical = match self.t() {
                T::And => Some((3, "and")),
                T::Or => Some((2, "or")),
                _ => None,
            };
            if let Some((bp, name)) = logical {
                if bp < min {
                    break;
                }
                self.i += 1;
                let b = self.expr(bp + 1)?;
                a = Expr::Call(name.into(), vec![a, b]);
                comparison_rhs = None;
                continue;
            }
            if min <= 40 {
                if self.eat(T::Factorial) {
                    a = Expr::Call("factorial".into(), vec![a]);
                    continue;
                }
                let n = match self.t() {
                    T::Square => Some(2),
                    T::Cube => Some(3),
                    _ => None,
                };
                if let Some(n) = n {
                    self.i += 1;
                    a = Expr::Pow(Box::new(a), Box::new(Expr::Rational(n, 1)));
                    continue;
                }
                if self.eat(T::Degree) {
                    a = Expr::Div(
                        Box::new(Expr::Mul(Box::new(a), Box::new(Expr::Const(Constant::Pi)))),
                        Box::new(Expr::Rational(180, 1)),
                    );
                    continue;
                }
            }
            let (bp, op, implicit) = match self.t() {
                T::Plus => (10, 0, false),
                T::Minus => (10, 1, false),
                T::Mul => (20, 2, false),
                T::Div => (20, 3, false),
                T::Pow => (30, 4, false),
                T::Num(_) | T::Id(_) | T::Lp | T::Sqrt => (20, 2, true),
                _ => break,
            };
            if bp < min {
                break;
            }
            if !implicit {
                self.i += 1;
            }
            let b = self.expr(if op == 4 { bp } else { bp + 1 })?;
            a = match op {
                0 => Expr::Add(Box::new(a), Box::new(b)),
                1 => Expr::Sub(Box::new(a), Box::new(b)),
                2 => Expr::Mul(Box::new(a), Box::new(b)),
                3 => Expr::Div(Box::new(a), Box::new(b)),
                _ => Expr::Pow(Box::new(a), Box::new(b)),
            };
        }
        self.depth -= 1;
        Ok(a)
    }
    fn args(&mut self, end: T) -> Result<Vec<Expr>, ParseError> {
        let mut a = Vec::new();
        if self.eat(end.clone()) {
            return Ok(a);
        }
        loop {
            a.push(self.equation()?);
            if !self.eat(T::Comma) {
                break;
            }
        }
        self.expect(end)?;
        Ok(a)
    }
}
pub fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    let mut p = Parser {
        tokens: lex(input)?,
        i: 0,
        depth: 0,
    };
    let e = p.equation()?;
    if *p.t() != T::End {
        return Err(p.err("Unerwartetes Zeichen"));
    }
    Ok(e)
}
pub fn parse_statement(input: &str) -> Result<Statement, ParseError> {
    let tokens = lex(input)?;
    if let Some((T::Id(n), _)) = tokens.first() {
        if !builtin(n) && !matches!(n.as_str(), "pi" | "e") {
            let mut i = 1;
            let mut params = Vec::new();
            let mut valid = true;
            if tokens.get(i).is_some_and(|t| t.0 == T::Lp) {
                i += 1;
                if tokens.get(i).is_some_and(|t| t.0 == T::Rp) {
                    i += 1;
                } else {
                    loop {
                        match tokens.get(i) {
                            Some((T::Id(s), _)) if !params.contains(s) => params.push(s.clone()),
                            _ => {
                                valid = false;
                                break;
                            }
                        }
                        i += 1;
                        if tokens.get(i).is_some_and(|t| t.0 == T::Comma) {
                            i += 1;
                            continue;
                        }
                        if tokens.get(i).is_some_and(|t| t.0 == T::Rp) {
                            i += 1;
                        } else {
                            valid = false;
                        }
                        break;
                    }
                }
            } else if matches!(n.as_str(), "x" | "y" | "z") {
                valid = false;
            }
            if valid && tokens.get(i).is_some_and(|t| t.0 == T::Eq) {
                let mut p = Parser {
                    tokens: tokens.clone(),
                    i: i + 1,
                    depth: 0,
                };
                let value = p.equation()?;
                if *p.t() != T::End {
                    return Err(p.err("Unerwartetes Zeichen"));
                }
                return Ok(Statement::Assign {
                    name: n.clone(),
                    params,
                    value,
                });
            }
        }
    }
    match parse_expr(input)? {
        Expr::Call(n, mut a) if n == "=" && a.len() == 2 => {
            let rhs = a.pop().unwrap();
            let lhs = a.pop().unwrap();
            Ok(Statement::Equation { lhs, rhs })
        }
        e => Ok(Statement::Expr(e)),
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn precedence() {
        assert_eq!(
            parse_expr("-x^2").unwrap(),
            Expr::Neg(Box::new(parse_expr("x^2").unwrap()))
        );
        assert!(parse_expr("2^-1").is_ok());
        assert_eq!(
            parse_expr("2^3^2").unwrap(),
            Expr::Pow(
                Box::new(Expr::Rational(2, 1)),
                Box::new(parse_expr("3^2").unwrap())
            )
        );
    }
    #[test]
    fn implicit() {
        for s in [
            "2x",
            "2 x",
            "3sin(x)",
            "2(x+1)",
            "(x+1)(x-1)",
            "x(x+1)",
            "2pi",
            "(x)y",
            "x y",
        ] {
            assert!(matches!(parse_expr(s).unwrap(), Expr::Mul(_, _)), "{s}");
        }
    }
    #[test]
    fn unicode() {
        for s in [
            "√(x)", "√x", "x²", "x³", "2·x", "2×x", "2÷x", "−x", "30°", "π", "ℯ", "a_1",
        ] {
            assert!(parse_expr(s).is_ok(), "{s}");
        }
        assert_eq!(parse_expr("π").unwrap(), Expr::Const(Constant::Pi));
    }
    #[test]
    fn statements() {
        for s in ["a=3", "f(x)=x^2", "g(x,y):=x+y", "A=(1,2)"] {
            assert!(matches!(parse_statement(s), Ok(Statement::Assign { .. })));
        }
        for s in ["x=3", "y=2x+1", "x^2+y^2=9"] {
            assert!(matches!(parse_statement(s), Ok(Statement::Equation { .. })));
        }
        assert!(parse_expr("Solve(x^2=4,x)").is_ok());
    }
    #[test]
    fn malformed() {
        for s in ["", "(", "foo(", "1..2", "@", "x+", "{{", "|x", "a:", "2,,3"] {
            assert!(parse_expr(s).is_err(), "{s}");
        }
        assert_eq!(parse_expr("π @").unwrap_err().position, 3);
    }
    #[test]
    fn bars_and_decimals() {
        assert_eq!(
            parse_expr("|x|").unwrap(),
            Expr::Call("abs".into(), vec![Expr::Var("x".into())])
        );
        assert!(matches!(parse_expr("0.5").unwrap(), Expr::Real(_)));
        assert!(matches!(parse_expr("1/2").unwrap(), Expr::Div(_, _)));
    }
}

#[cfg(test)]
mod stats_tests {
    use super::*;
    #[test]
    fn postfix_factorial() {
        for input in ["5!", "(n+1)!", "n!^2", "2^n!", "-n!", "2n!"] {
            assert!(parse_expr(input).is_ok(), "{input}");
        }
        assert_eq!(
            parse_expr("n!").unwrap(),
            Expr::Call("factorial".into(), vec![Expr::Var("n".into())])
        );
        assert!(parse_expr("!").is_err());
        assert!(parse_expr("normalpdf(0,1,x)").is_ok());
    }
}
