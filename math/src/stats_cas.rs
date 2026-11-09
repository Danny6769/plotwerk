use crate::ast::Expr;
use crate::eval::{Empty, eval};
use crate::stats;

pub(crate) fn command(name: &str) -> Option<&'static str> {
    match name.to_lowercase().as_str() {
        "binomialkoeffizient" | "binomialcoefficient" | "ncr" => Some("choose"),
        "binomialverteilung" | "binomialdist" => Some("binomialdist"),
        "normal" => Some("normal"),
        "inversnormal" | "inversenormal" => Some("invnormal"),
        "poisson" => Some("poisson"),
        "hypergeometrisch" | "hypergeometric" => Some("hyper"),
        "exponential" => Some("exponential"),
        "inverst" | "inversetdistribution" => Some("invt"),
        "tverteilung" => Some("tcdf"),
        "chiquadrat" => Some("chi2cdf"),
        "inverschiquadrat" => Some("invchi2"),
        "mittelwert" | "mean" => Some("mean"),
        "median" => Some("median"),
        "modalwert" | "mode" => Some("mode"),
        "varianz" | "variance" => Some("variance"),
        "stichprobenvarianz" | "samplevariance" => Some("samplevariance"),
        "standardabweichung" | "sd" | "stdev" | "stdevp" => Some("sd"),
        "stichprobenstandardabweichung" | "samplesd" => Some("samplesd"),
        "summe" | "sum" => Some("sum"),
        "min" => Some("min"),
        "max" => Some("max"),
        "q1" => Some("q1"),
        "q3" => Some("q3"),
        "spannweite" => Some("range"),
        "iqr" => Some("iqr"),
        "kovarianz" | "covariance" => Some("covariance"),
        "korrelationskoeffizient" | "correlationcoefficient" => Some("correlation"),
        "trendlin" | "fitline" => Some("fitline"),
        "trendpoly" | "fitpoly" => Some("fitpoly"),
        "trendexp" | "fitexp" => Some("fitexp"),
        "trendexp2" | "fitexp2" => Some("fitexp2"),
        "trendpot" | "fitpow" => Some("fitpow"),
        "trendlog" | "fitlog" => Some("fitlog"),
        "trendlogistisch" | "fitlogistic" => Some("fitlogistic"),
        "trendsin" | "fitsin" => Some("fitsin"),
        _ => None,
    }
}
fn numeric(e: &Expr) -> Result<f64, String> {
    eval(e, &Empty).map_err(|_| "Numerischer Wert erwartet".into())
}
fn list(e: &Expr) -> Result<Vec<f64>, String> {
    if let Expr::List(values) = e {
        values.iter().map(numeric).collect()
    } else {
        Err("Liste erwartet".into())
    }
}
fn boolean(e: &Expr) -> Option<bool> {
    if let Expr::Var(n) = e {
        match n.to_lowercase().as_str() {
            "true" | "wahr" => Some(true),
            "false" | "falsch" => Some(false),
            _ => None,
        }
    } else {
        None
    }
}
fn points(e: &Expr) -> Result<(Vec<f64>, Vec<f64>), String> {
    let Expr::List(values) = e else {
        return Err("Punktliste erwartet".into());
    };
    let (mut xs, mut ys) = (Vec::new(), Vec::new());
    for point in values {
        let Expr::Tuple(pair) = point else {
            return Err("Punkt mit zwei Koordinaten erwartet".into());
        };
        if pair.len() != 2 {
            return Err("Punkt mit zwei Koordinaten erwartet".into());
        }
        xs.push(numeric(&pair[0])?);
        ys.push(numeric(&pair[1])?);
    }
    Ok((xs, ys))
}
fn arity(args: &[Expr], n: usize) -> Result<(), String> {
    if args.len() == n {
        Ok(())
    } else {
        Err("Ungültige Argumentanzahl".into())
    }
}
pub(crate) fn execute(cmd: &str, args: &[Expr]) -> Result<Expr, String> {
    let number = |i: usize| {
        args.get(i)
            .ok_or_else(|| "Ungültige Argumentanzahl".to_string())
            .and_then(numeric)
    };
    let result = match cmd {
        "choose" => {
            arity(args, 2)?;
            let values = match (number(0), number(1)) {
                (Ok(n), Ok(k)) => [n, k],
                _ if args
                    .iter()
                    .all(|e| !matches!(e, Expr::List(_) | Expr::Tuple(_))) =>
                {
                    return Ok(Expr::Call("nCr".into(), args.to_vec()));
                }
                _ => return Err("Numerischer Wert erwartet".into()),
            };
            if let Some(n) = stats::exact_integer("nCr", &values) {
                return Ok(Expr::Rational(n, 1));
            }
            stats::n_choose_k(values[0], values[1])
        }
        "binomialdist" | "normal" => {
            arity(args, 4)?;
            let (a, b, c) = (number(0)?, number(1)?, number(2)?);
            if let Some(cumulative) = boolean(&args[3]) {
                match (cmd, cumulative) {
                    ("binomialdist", false) => stats::binomial_pmf(a, b, c),
                    ("binomialdist", true) => stats::binomial_cdf(a, b, c),
                    ("normal", false) => stats::normal_pdf(a, b, c),
                    _ => stats::normal_cdf(a, b, c),
                }
            } else {
                let d = number(3)?;
                if cmd == "normal" {
                    stats::normal_range(a, b, c, d)
                } else {
                    stats::binomial_range(a, b, c, d)
                }
            }
        }
        "invnormal" => {
            arity(args, 3)?;
            stats::normal_inverse(number(0)?, number(1)?, number(2)?)
        }
        "poisson" | "exponential" => {
            arity(args, 3)?;
            let (a, b) = (number(0)?, number(1)?);
            let cumulative =
                boolean(&args[2]).ok_or_else(|| "Wahrheitswert erwartet".to_string())?;
            match (cmd, cumulative) {
                ("poisson", false) => stats::poisson_pmf(a, b),
                ("poisson", true) => stats::poisson_cdf(a, b),
                ("exponential", false) => stats::exponential_pdf(a, b),
                _ => stats::exponential_cdf(a, b),
            }
        }
        "hyper" => {
            arity(args, 5)?;
            let (a, b, c, d) = (number(0)?, number(1)?, number(2)?, number(3)?);
            if boolean(&args[4]).ok_or_else(|| "Wahrheitswert erwartet".to_string())? {
                stats::hypergeometric_cdf(a, b, c, d)
            } else {
                stats::hypergeometric_pmf(a, b, c, d)
            }
        }
        "invt" | "tcdf" | "chi2cdf" | "invchi2" => {
            arity(args, 2)?;
            let (a, b) = (number(0)?, number(1)?);
            match cmd {
                "invt" => stats::t_inverse(a, b),
                "tcdf" => stats::t_cdf(a, b),
                "chi2cdf" => stats::chi2_cdf(a, b),
                _ => stats::chi2_inverse(a, b),
            }
        }
        "covariance" | "correlation" => {
            arity(args, 2)?;
            let (a, b) = (list(&args[0])?, list(&args[1])?);
            if cmd == "covariance" {
                stats::covariance(&a, &b)
            } else {
                stats::correlation(&a, &b)
            }
        }
        "fitline" | "fitpoly" | "fitexp" | "fitexp2" | "fitpow" | "fitlog" | "fitlogistic"
        | "fitsin" => {
            let degree = if cmd == "fitpoly" {
                let last = args
                    .last()
                    .ok_or_else(|| "Ungültige Argumentanzahl".to_string())?;
                let degree = numeric(last)?;
                if !degree.is_finite() || degree < 0.0 || degree.fract() != 0.0 || degree > 32.0 {
                    return Err("Ungültiger Polynomgrad".into());
                }
                Some(degree as usize)
            } else {
                None
            };
            let data = if degree.is_some() {
                &args[..args.len() - 1]
            } else {
                args
            };
            let (xs, ys) = match data {
                [points_list] => points(points_list)?,
                [xs, ys] => (list(xs)?, list(ys)?),
                _ => return Err("Ungültige Argumentanzahl".into()),
            };
            let fit = match cmd {
                "fitline" => stats::fit_linear(&xs, &ys),
                "fitpoly" => stats::fit_poly(&xs, &ys, degree.unwrap_or(1)),
                "fitexp" => stats::fit_exp(&xs, &ys),
                "fitexp2" => stats::fit_exp_base(&xs, &ys),
                "fitpow" => stats::fit_power(&xs, &ys),
                "fitlog" => stats::fit_log(&xs, &ys),
                "fitlogistic" => stats::fit_logistic(&xs, &ys),
                _ => stats::fit_sin(&xs, &ys),
            };
            return fit
                .map(|f| f.expr)
                .ok_or_else(|| "Regression nicht möglich".into());
        }
        _ => {
            if matches!(cmd, "min" | "max") && !matches!(args.first(), Some(Expr::List(_))) {
                if args.is_empty() {
                    return Err("Ungültige Argumentanzahl".into());
                }
                return Ok(Expr::Call(cmd.into(), args.to_vec()));
            }
            if matches!(cmd, "mean" | "sd") && args.len() == 2 {
                let (values, freqs) = (list(&args[0])?, list(&args[1])?);
                if cmd == "mean" {
                    stats::mean_weighted(&values, &freqs)
                } else {
                    stats::sd_weighted(&values, &freqs)
                }
            } else {
                arity(args, 1)?;
                let values = list(&args[0])?;
                match cmd {
                    "mean" => stats::mean(&values),
                    "median" => stats::median(&values),
                    "mode" => {
                        return Ok(Expr::List(
                            stats::mode(&values).into_iter().map(Expr::Real).collect(),
                        ));
                    }
                    "variance" => stats::variance(&values),
                    "samplevariance" => stats::sample_variance(&values),
                    "sd" => stats::sd(&values),
                    "samplesd" => stats::sample_sd(&values),
                    "sum" => stats::sum(&values),
                    "min" => stats::min(&values),
                    "max" => stats::max(&values),
                    "q1" => stats::quartile1(&values),
                    "q3" => stats::quartile3(&values),
                    "range" => stats::range(&values),
                    "iqr" => stats::iqr(&values),
                    _ => return Err("Unbekannter Befehl".into()),
                }
            }
        }
    };
    Ok(Expr::Real(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        cas::{CasEnv, cas_eval},
        format::format_number,
        parser::parse_expr,
    };
    #[test]
    fn distribution_commands() {
        let mut env = CasEnv::default();
        for (names, args, want) in [
            (
                vec!["BinomialKoeffizient", "BinomialCoefficient", "nCr"],
                "(10,3)",
                "120",
            ),
            (
                vec!["BinomialVerteilung", "BinomialDist"],
                "(10,0.3,3,false)",
                "0.2668",
            ),
            (
                vec!["BinomialVerteilung", "BinomialDist"],
                "(10,0.3,3,wahr)",
                "0.6496",
            ),
            (
                vec!["BinomialVerteilung", "BinomialDist"],
                "(10,0.3,2,4)",
                "0.7004",
            ),
            (vec!["Normal"], "(0,1,1.96,true)", "0.975"),
            (vec!["Normal"], "(0,1,1.96,falsch)", "0.0584"),
            (vec!["Normal"], "(0,1,-1,1)", "0.6827"),
            (vec!["InversNormal", "InverseNormal"], "(0,1,0.975)", "1.96"),
            (vec!["Poisson"], "(4,2,true)", "0.2381"),
            (vec!["Poisson"], "(4,2,false)", "0.1465"),
            (
                vec!["Hypergeometrisch", "Hypergeometric"],
                "(20,7,5,2,false)",
                "0.3874",
            ),
            (
                vec!["Hypergeometrisch", "Hypergeometric"],
                "(20,7,5,2,true)",
                "0.7932",
            ),
            (vec!["Exponential"], "(2,1,true)", "0.8647"),
            (vec!["Exponential"], "(2,1,false)", "0.2707"),
            (
                vec!["InversT", "InverseTDistribution"],
                "(10,0.975)",
                "2.2281",
            ),
            (vec!["TVerteilung"], "(10,0)", "0.5"),
            (vec!["ChiQuadrat"], "(3,7.815)", "0.95"),
            (vec!["InversChiQuadrat"], "(2,0.95)", "5.9915"),
        ] {
            for name in names {
                let input = format!("{name}{args}");
                assert_eq!(cas_eval(&input, &mut env).unwrap(), want, "{input}");
            }
        }
        assert_eq!(
            cas_eval("BinomialCoefficient(66,33)", &mut env).unwrap(),
            "7219428434016265740"
        );
        assert_eq!(cas_eval("5!", &mut env).unwrap(), "120");
        assert_eq!(cas_eval("factorial(0.5)", &mut env).unwrap(), "0.8862");
        assert_eq!(
            cas_eval("f(x)=nCr(10,x)", &mut env).unwrap(),
            "f(x) = nCr(10, x)"
        );
        assert_eq!(cas_eval("f(3)", &mut env).unwrap(), "120");
        assert_eq!(
            cas_eval("g(x)=normalpdf(0,1,x)", &mut env).unwrap(),
            "g(x) = normalpdf(0, 1, x)"
        );
        assert_eq!(cas_eval("g(0)", &mut env).unwrap(), "0.3989");
    }
    #[test]
    fn descriptive_commands() {
        let mut env = CasEnv::default();
        let data = "({2,4,4,4,5,5,7,9})";
        for (names, want) in [
            (vec!["Mittelwert", "Mean"], "5"),
            (vec!["Median"], "4.5"),
            (vec!["Modalwert", "Mode"], "{4}"),
            (vec!["Varianz", "Variance"], "4"),
            (vec!["Stichprobenvarianz", "SampleVariance"], "4.5714"),
            (vec!["Standardabweichung", "SD", "stdev", "stdevp"], "2"),
            (vec!["Stichprobenstandardabweichung", "SampleSD"], "2.1381"),
            (vec!["Summe", "Sum"], "40"),
            (vec!["Min"], "2"),
            (vec!["Max"], "9"),
            (vec!["Q1"], "4"),
            (vec!["Q3"], "6"),
            (vec!["Spannweite"], "7"),
            (vec!["IQR"], "2"),
        ] {
            for name in names {
                let input = format!("{name}{data}");
                assert_eq!(cas_eval(&input, &mut env).unwrap(), want, "{input}");
                assert_eq!(cas_eval(&input.to_uppercase(), &mut env).unwrap(), want);
            }
        }
        for name in ["Mittelwert", "Mean"] {
            assert_eq!(
                cas_eval(&format!("{name}({{2,4,5,7,9}},{{1,3,2,1,1}})"), &mut env).unwrap(),
                "5"
            );
        }
        assert_eq!(
            cas_eval("SD({2,4,5,7,9},{1,3,2,1,1})", &mut env).unwrap(),
            "2"
        );
        for name in ["Kovarianz", "Covariance"] {
            assert_eq!(
                cas_eval(&format!("{name}({{1,2,3}},{{2,4,6}})"), &mut env).unwrap(),
                "1.3333"
            );
        }
        for name in ["Korrelationskoeffizient", "CorrelationCoefficient"] {
            assert_eq!(
                cas_eval(&format!("{name}({{1,2,3}},{{2,4,6}})"), &mut env).unwrap(),
                "1"
            );
        }
        assert_eq!(cas_eval("Min(1,2)", &mut env).unwrap(), "1");
        assert_eq!(cas_eval("max(x,2)", &mut env).unwrap(), "max(x, 2)");
        assert_eq!(cas_eval("Sum({})", &mut env).unwrap(), "0");
        cas_eval("A={1,2,3}", &mut env).unwrap();
        assert_eq!(cas_eval("Mittelwert(A)", &mut env).unwrap(), "2");
    }
    fn point_list(xs: &[f64], ys: &[f64]) -> String {
        format!(
            "{{{}}}",
            xs.iter()
                .zip(ys)
                .map(|(x, y)| format!("({x},{y})"))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
    #[test]
    fn regression_commands() {
        let mut env = CasEnv::default();
        for name in ["TrendLin", "FitLine"] {
            assert_eq!(
                cas_eval(&format!("{name}({{(1,3),(2,5),(3,7)}})"), &mut env).unwrap(),
                "2x + 1"
            );
            assert_eq!(
                cas_eval(&format!("{name}({{1,2,3}},{{3,5,7}})"), &mut env).unwrap(),
                "2x + 1"
            );
        }
        for name in ["TrendPoly", "FitPoly"] {
            assert_eq!(
                cas_eval(&format!("{name}({{(-1,2),(0,1),(1,2)}},2)"), &mut env).unwrap(),
                "x^2 + 1"
            );
            assert_eq!(
                cas_eval(&format!("{name}({{-1,0,1}},{{2,1,2}},2)"), &mut env).unwrap(),
                "x^2 + 1"
            );
        }
        let xs: Vec<_> = (1..=30).map(|i| i as f64 / 5.0).collect();
        let functions: Vec<(&[&str], Box<dyn Fn(f64) -> f64>)> = vec![
            (&["TrendExp", "FitExp"], Box::new(|x| 2.0 * (0.3 * x).exp())),
            (
                &["TrendExp2", "FitExp2"],
                Box::new(|x| 2.0 * 1.2f64.powf(x)),
            ),
            (&["TrendPot", "FitPow"], Box::new(|x| 1.5 * x.powf(2.3))),
            (&["TrendLog", "FitLog"], Box::new(|x| 0.3 + 1.5 * x.ln())),
            (
                &["TrendLogistisch", "FitLogistic"],
                Box::new(|x| 5.0 / (1.0 + 3.0 * (-1.2 * x).exp())),
            ),
            (
                &["TrendSin", "FitSin"],
                Box::new(|x| 0.5 + 2.0 * (1.3 * x + 0.4).sin()),
            ),
        ];
        for (names, function) in functions {
            let ys: Vec<_> = xs.iter().map(|x| function(*x)).collect();
            for name in names {
                let input = format!("{name}({})", point_list(&xs, &ys));
                let result = cas_eval(&input, &mut env).unwrap();
                let expr = parse_expr(&result).unwrap();
                let compiled = crate::compile(&expr, &["x"], &Empty).unwrap();
                for x in [0.4, 1.3, 3.7, 5.8] {
                    let want = function(x);
                    assert!(
                        (compiled.eval(&[x]) - want).abs() < 3e-5 * (1.0 + want.abs()),
                        "{name}: {result}"
                    );
                }
            }
        }
        assert_eq!(
            cas_eval("TrendExp2({0,1,2},{2,4,8})", &mut env).unwrap(),
            "2*2^x"
        );
    }
    #[test]
    fn invalid_inputs() {
        let mut env = CasEnv::default();
        for input in [
            "Normal()",
            "Mean(1)",
            "Poisson(2,3,vielleicht)",
            "FitLine({1,2},{1})",
            "FitPoly({1,2},{2,3},-1)",
            "FitPoly({},2)",
            "FitLine({(1,2,3)})",
            "Mean({1,2},{1})",
            "Hypergeometric(2,3,1,1,true)",
            "normalpdf(0,0,0)",
        ] {
            let result = cas_eval(input, &mut env);
            assert!(
                result.is_err() || result.as_ref().is_ok_and(|s| s == "undefiniert"),
                "{input}: {result:?}"
            );
        }
        assert_eq!(
            format_number(stats::normal_range(0.0, -1.0, 1.0, 2.0)),
            "undefiniert"
        );
    }
}
