const SUP: [char; 10] = ['⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹'];

pub fn pretty(s: &str) -> String {
    let chars: Vec<char> = s.chars().collect();
    let mut out = String::with_capacity(s.len());
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c.is_alphabetic() {
            let start = i;
            while i < chars.len() && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let word: String = chars[start..i].iter().collect();
            let next = chars.get(i).copied();
            match word.as_str() {
                "sqrt" if next == Some('(') => out.push('√'),
                "pi" => out.push('π'),
                _ => out.push_str(&word),
            }
            continue;
        }
        if c == '^' {
            let mut j = i + 1;
            while j < chars.len() && chars[j].is_ascii_digit() {
                j += 1;
            }
            let follows_ok = chars.get(j).map(|n| *n != '.' && !n.is_alphanumeric()).unwrap_or(true);
            if j > i + 1 && follows_ok {
                for d in &chars[i + 1..j] {
                    out.push(SUP[d.to_digit(10).unwrap_or(0) as usize]);
                }
                i = j;
                continue;
            }
        }
        if (c == '<' || c == '>' || c == '!') && chars.get(i + 1) == Some(&'=') {
            out.push(match c {
                '<' => '≤',
                '>' => '≥',
                _ => '≠',
            });
            i += 2;
            continue;
        }
        if c == '*' {
            out.push('·');
            i += 1;
            continue;
        }
        out.push(c);
        i += 1;
    }
    out
}
