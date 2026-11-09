use crate::ast::{Constant, Expr};
use crate::simplify::{add, call, div, int, mul, pow, simplify};
use std::f64::consts::PI;
fn count(x: f64) -> bool {
    x.is_finite() && x >= 0.0 && x.fract() == 0.0
}
fn probability(x: f64) -> bool {
    x.is_finite() && (0.0..=1.0).contains(&x)
}
fn ln_gamma(z: f64) -> f64 {
    if z <= 0.0 && z.fract() == 0.0 {
        return f64::NAN;
    }
    if z < 0.5 {
        return PI.ln() - (PI * z).sin().abs().ln() - ln_gamma(1.0 - z);
    }
    let c = [
        676.5203681218851,
        -1259.1392167224028,
        771.3234287776531,
        -176.6150291621406,
        12.507343278686905,
        -0.13857109526572012,
        9.984369578019572e-6,
        1.5056327351493116e-7,
    ];
    let z = z - 1.0;
    let mut a = 0.9999999999998099;
    for (i, c) in c.iter().enumerate() {
        a += c / (z + i as f64 + 1.0);
    }
    let t = z + 7.5;
    0.9189385332046727 + (z + 0.5) * t.ln() - t + a.ln()
}
fn gamma_pq(a: f64, x: f64) -> (f64, f64) {
    if !a.is_finite() || a <= 0.0 || x.is_nan() || x < 0.0 {
        return (f64::NAN, f64::NAN);
    }
    if x == 0.0 {
        return (0.0, 1.0);
    }
    if x.is_infinite() {
        return (1.0, 0.0);
    }
    let scale = (a * x.ln() - x - ln_gamma(a)).exp();
    if x < a + 1.0 {
        let mut term = 1.0 / a;
        let mut total = term;
        for i in 1..=100_000 {
            term *= x / (a + i as f64);
            total += term;
            if term.abs() <= total.abs() * 2e-16 {
                break;
            }
        }
        let p = (total * scale).clamp(0.0, 1.0);
        (p, 1.0 - p)
    } else {
        let tiny = 1e-300;
        let mut b = x + 1.0 - a;
        let mut c = 1.0 / tiny;
        let mut d = 1.0 / b.max(tiny);
        let mut h = d;
        for i in 1..=100_000 {
            let i = i as f64;
            let an = -i * (i - a);
            b += 2.0;
            d = an * d + b;
            if d.abs() < tiny {
                d = tiny;
            }
            c = b + an / c;
            if c.abs() < tiny {
                c = tiny;
            }
            d = 1.0 / d;
            let delta = d * c;
            h *= delta;
            if (delta - 1.0).abs() < 3e-15 {
                break;
            }
        }
        let q = (h * scale).clamp(0.0, 1.0);
        (1.0 - q, q)
    }
}
fn beta_fraction(a: f64, b: f64, x: f64) -> f64 {
    let tiny = 1e-300;
    let mut c = 1.0;
    let mut d = 1.0 - (a + b) * x / (a + 1.0);
    if d.abs() < tiny {
        d = tiny;
    }
    d = 1.0 / d;
    let mut h = d;
    for m in 1..=20_000 {
        let m = m as f64;
        for aa in [
            m * (b - m) * x / ((a + 2.0 * m - 1.0) * (a + 2.0 * m)),
            -(a + m) * (a + b + m) * x / ((a + 2.0 * m) * (a + 2.0 * m + 1.0)),
        ] {
            d = 1.0 + aa * d;
            if d.abs() < tiny {
                d = tiny;
            }
            c = 1.0 + aa / c;
            if c.abs() < tiny {
                c = tiny;
            }
            d = 1.0 / d;
            let delta = d * c;
            h *= delta;
            if aa < 0.0 && (delta - 1.0).abs() < 3e-15 {
                return h;
            }
        }
    }
    h
}
fn beta(a: f64, b: f64, x: f64) -> f64 {
    if a <= 0.0 || b <= 0.0 || !a.is_finite() || !b.is_finite() || !probability(x) {
        return f64::NAN;
    }
    if x == 0.0 {
        return 0.0;
    }
    if x == 1.0 {
        return 1.0;
    }
    let front = (ln_gamma(a + b) - ln_gamma(a) - ln_gamma(b) + a * x.ln() + b * (-x).ln_1p()).exp();
    (if x < (a + 1.0) / (a + b + 2.0) {
        front * beta_fraction(a, b, x) / a
    } else {
        1.0 - front * beta_fraction(b, a, 1.0 - x) / b
    })
    .clamp(0.0, 1.0)
}
pub fn factorial(n: f64) -> f64 {
    if !n.is_finite() {
        return if n == f64::INFINITY { n } else { f64::NAN };
    }
    if count(n) && n <= 170.0 {
        return (1..=n as u32).fold(1.0, |a, b| a * b as f64);
    }
    let z = n + 1.0;
    if z <= 0.0 && z.fract() == 0.0 {
        return f64::NAN;
    }
    let sign = if z < 0.0 {
        (PI * z).sin().signum()
    } else {
        1.0
    };
    sign * ln_gamma(z).exp()
}
pub fn n_choose_k(n: f64, k: f64) -> f64 {
    if !count(n) || !count(k) {
        return f64::NAN;
    }
    if k > n {
        return 0.0;
    }
    let k = k.min(n - k);
    if k == 0.0 {
        return 1.0;
    }
    if k <= 100_000.0 {
        let mut value = 1.0;
        for i in 1..=k as u64 {
            value *= (n - k + i as f64) / i as f64;
            if value.is_infinite() {
                return value;
            }
        }
        return value.round();
    }
    (ln_gamma(n + 1.0) - ln_gamma(k + 1.0) - ln_gamma(n - k + 1.0))
        .exp()
        .round()
}
fn log_choose(n: f64, k: f64) -> f64 {
    if k < 0.0 || k > n {
        f64::NEG_INFINITY
    } else if k == 0.0 || k == n {
        0.0
    } else {
        ln_gamma(n + 1.0) - ln_gamma(k + 1.0) - ln_gamma(n - k + 1.0)
    }
}
pub fn binomial_pmf(n: f64, p: f64, k: f64) -> f64 {
    if !count(n) || !probability(p) || !k.is_finite() {
        return f64::NAN;
    }
    if k < 0.0 || k > n || k.fract() != 0.0 {
        return 0.0;
    }
    if p == 0.0 {
        return if k == 0.0 { 1.0 } else { 0.0 };
    }
    if p == 1.0 {
        return if k == n { 1.0 } else { 0.0 };
    }
    (log_choose(n, k) + k * p.ln() + (n - k) * (-p).ln_1p())
        .exp()
        .clamp(0.0, 1.0)
}
pub fn binomial_cdf(n: f64, p: f64, k: f64) -> f64 {
    if !count(n) || !probability(p) || k.is_nan() {
        return f64::NAN;
    }
    if k < 0.0 {
        return 0.0;
    }
    if k >= n || p == 0.0 {
        return 1.0;
    }
    if p == 1.0 {
        return 0.0;
    }
    let k = k.floor();
    beta(n - k, k + 1.0, 1.0 - p)
}
fn binomial_sf(n: f64, p: f64, k: f64) -> f64 {
    if k < 0.0 {
        return 1.0;
    }
    if k >= n {
        return 0.0;
    }
    if p == 0.0 {
        return 0.0;
    }
    if p == 1.0 {
        return 1.0;
    }
    let k = k.floor();
    beta(k + 1.0, n - k, p)
}
pub fn binomial_range(n: f64, p: f64, a: f64, b: f64) -> f64 {
    if !count(n) || !probability(p) || a.is_nan() || b.is_nan() {
        return f64::NAN;
    }
    if a > b {
        return 0.0;
    }
    let lower = binomial_cdf(n, p, a.ceil() - 1.0);
    let upper = binomial_cdf(n, p, b.floor());
    (if lower > 0.5 {
        binomial_sf(n, p, a.ceil() - 1.0) - binomial_sf(n, p, b.floor())
    } else {
        upper - lower
    })
    .clamp(0.0, 1.0)
}
pub fn normal_pdf(mu: f64, sigma: f64, x: f64) -> f64 {
    if !mu.is_finite() || !sigma.is_finite() || sigma <= 0.0 || x.is_nan() {
        return f64::NAN;
    }
    let z = (x - mu) / sigma;
    (-0.5 * z * z).exp() / (sigma * (2.0 * PI).sqrt())
}
pub fn normal_cdf(mu: f64, sigma: f64, x: f64) -> f64 {
    if !mu.is_finite() || !sigma.is_finite() || sigma <= 0.0 || x.is_nan() {
        return f64::NAN;
    }
    let z = (x - mu) / sigma;
    if z == 0.0 {
        return 0.5;
    }
    let tail = 0.5 * gamma_pq(0.5, z * z / 2.0).1;
    if z < 0.0 { tail } else { 1.0 - tail }
}
pub fn normal_range(mu: f64, sigma: f64, a: f64, b: f64) -> f64 {
    if !mu.is_finite() || !sigma.is_finite() || sigma <= 0.0 || a.is_nan() || b.is_nan() {
        return f64::NAN;
    }
    if a > b {
        return if normal_cdf(mu, sigma, a).is_nan() {
            f64::NAN
        } else {
            0.0
        };
    }
    if a >= mu {
        normal_cdf(0.0, 1.0, (mu - a) / sigma) - normal_cdf(0.0, 1.0, (mu - b) / sigma)
    } else {
        normal_cdf(mu, sigma, b) - normal_cdf(mu, sigma, a)
    }
}
pub fn normal_inverse(mu: f64, sigma: f64, p: f64) -> f64 {
    if !mu.is_finite() || !sigma.is_finite() || sigma <= 0.0 || !probability(p) {
        return f64::NAN;
    }
    if p == 0.0 {
        return f64::NEG_INFINITY;
    }
    if p == 1.0 {
        return f64::INFINITY;
    }
    if p == 0.5 {
        return mu;
    }
    let tail = p.min(1.0 - p);
    let (mut lo, mut hi) = (-1.0, 0.0);
    while normal_cdf(0.0, 1.0, lo) > tail {
        lo *= 2.0;
    }
    for _ in 0..120 {
        let mid = (lo + hi) / 2.0;
        if normal_cdf(0.0, 1.0, mid) < tail {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    mu + sigma
        * if p < 0.5 {
            (lo + hi) / 2.0
        } else {
            -(lo + hi) / 2.0
        }
}
pub fn poisson_pmf(lambda: f64, k: f64) -> f64 {
    if !lambda.is_finite() || lambda < 0.0 || !k.is_finite() {
        return f64::NAN;
    }
    if !count(k) {
        return 0.0;
    }
    if lambda == 0.0 {
        return if k == 0.0 { 1.0 } else { 0.0 };
    }
    (-lambda + k * lambda.ln() - ln_gamma(k + 1.0))
        .exp()
        .clamp(0.0, 1.0)
}
pub fn poisson_cdf(lambda: f64, k: f64) -> f64 {
    if !lambda.is_finite() || lambda < 0.0 || k.is_nan() {
        return f64::NAN;
    }
    if k < 0.0 {
        return 0.0;
    }
    if k == f64::INFINITY || lambda == 0.0 {
        return 1.0;
    }
    gamma_pq(k.floor() + 1.0, lambda).1
}
fn hyper_valid(n: f64, k: f64, d: f64) -> bool {
    count(n) && count(k) && count(d) && k <= n && d <= n
}
pub fn hypergeometric_pmf(population: f64, successes: f64, draws: f64, k: f64) -> f64 {
    if !hyper_valid(population, successes, draws) || !k.is_finite() {
        return f64::NAN;
    }
    if !count(k) || k > successes.min(draws) || k < (draws + successes - population).max(0.0) {
        return 0.0;
    }
    (log_choose(successes, k) + log_choose(population - successes, draws - k)
        - log_choose(population, draws))
    .exp()
    .clamp(0.0, 1.0)
}
pub fn hypergeometric_cdf(population: f64, successes: f64, draws: f64, k: f64) -> f64 {
    if !hyper_valid(population, successes, draws) || k.is_nan() {
        return f64::NAN;
    }
    let low = (draws + successes - population).max(0.0);
    let high = draws.min(successes);
    if k < low {
        return 0.0;
    }
    if k >= high {
        return 1.0;
    }
    let k = k.floor();
    let mode = ((draws + 1.0) * (successes + 1.0) / (population + 2.0)).floor();
    let right = k >= mode;
    let mut i = if right { k + 1.0 } else { k };
    let mut term = hypergeometric_pmf(population, successes, draws, i);
    let mut total = term;
    for _ in 0..100_000 {
        if right {
            if i >= high {
                break;
            }
            term *= ((successes - i) * (draws - i))
                / ((i + 1.0) * (population - successes - draws + i + 1.0));
            i += 1.0;
        } else {
            if i <= low {
                break;
            }
            term *= i * (population - successes - draws + i)
                / ((successes - i + 1.0) * (draws - i + 1.0));
            i -= 1.0;
        }
        total += term;
        if term <= total * 1e-16 {
            break;
        }
    }
    if right {
        (1.0 - total).clamp(0.0, 1.0)
    } else {
        total.clamp(0.0, 1.0)
    }
}
pub fn geometric_pmf(p: f64, k: f64) -> f64 {
    if !probability(p) || p == 0.0 || !k.is_finite() {
        return f64::NAN;
    }
    if k < 1.0 || k.fract() != 0.0 {
        return 0.0;
    }
    if p == 1.0 {
        return if k == 1.0 { 1.0 } else { 0.0 };
    }
    p * ((k - 1.0) * (-p).ln_1p()).exp()
}
pub fn geometric_cdf(p: f64, k: f64) -> f64 {
    if !probability(p) || p == 0.0 || k.is_nan() {
        return f64::NAN;
    }
    if k < 1.0 {
        return 0.0;
    }
    if p == 1.0 || k == f64::INFINITY {
        return 1.0;
    }
    -(k.floor() * (-p).ln_1p()).exp_m1()
}
pub fn exponential_pdf(lambda: f64, x: f64) -> f64 {
    if !lambda.is_finite() || lambda <= 0.0 || x.is_nan() {
        return f64::NAN;
    }
    if x < 0.0 {
        0.0
    } else {
        lambda * (-lambda * x).exp()
    }
}
pub fn exponential_cdf(lambda: f64, x: f64) -> f64 {
    if !lambda.is_finite() || lambda <= 0.0 || x.is_nan() {
        return f64::NAN;
    }
    if x <= 0.0 {
        0.0
    } else {
        -(-lambda * x).exp_m1()
    }
}
pub fn uniform_pdf(a: f64, b: f64, x: f64) -> f64 {
    if !a.is_finite() || !b.is_finite() || a >= b || x.is_nan() {
        return f64::NAN;
    }
    if x < a || x > b { 0.0 } else { 1.0 / (b - a) }
}
pub fn uniform_cdf(a: f64, b: f64, x: f64) -> f64 {
    if !a.is_finite() || !b.is_finite() || a >= b || x.is_nan() {
        return f64::NAN;
    }
    ((x - a) / (b - a)).clamp(0.0, 1.0)
}
pub fn t_pdf(df: f64, x: f64) -> f64 {
    if !df.is_finite() || df <= 0.0 || x.is_nan() {
        return f64::NAN;
    }
    (ln_gamma((df + 1.0) / 2.0)
        - ln_gamma(df / 2.0)
        - 0.5 * (df * PI).ln()
        - 0.5 * (df + 1.0) * (x * x / df).ln_1p())
    .exp()
}
pub fn t_cdf(df: f64, x: f64) -> f64 {
    if !df.is_finite() || df <= 0.0 || x.is_nan() {
        return f64::NAN;
    }
    if x == 0.0 {
        return 0.5;
    }
    if x * x < df {
        let central = 0.5 * beta(0.5, df / 2.0, x * x / (df + x * x));
        return if x < 0.0 {
            0.5 - central
        } else {
            0.5 + central
        };
    }
    let tail = 0.5 * beta(df / 2.0, 0.5, df / (df + x * x));
    if x < 0.0 { tail } else { 1.0 - tail }
}
pub fn t_inverse(df: f64, p: f64) -> f64 {
    if !df.is_finite() || df <= 0.0 || !probability(p) {
        return f64::NAN;
    }
    if p == 0.0 {
        return f64::NEG_INFINITY;
    }
    if p == 1.0 {
        return f64::INFINITY;
    }
    if p == 0.5 {
        return 0.0;
    }
    let tail = p.min(1.0 - p);
    let (mut lo, mut hi) = (-1.0, 0.0);
    for _ in 0..1024 {
        if t_cdf(df, lo) <= tail {
            break;
        }
        lo *= 2.0;
        if !lo.is_finite() {
            return if p < 0.5 {
                f64::NEG_INFINITY
            } else {
                f64::INFINITY
            };
        }
    }
    for _ in 0..150 {
        let mid = lo + (hi - lo) / 2.0;
        if t_cdf(df, mid) < tail {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    if p < 0.5 {
        (lo + hi) / 2.0
    } else {
        -(lo + hi) / 2.0
    }
}
pub fn chi2_pdf(df: f64, x: f64) -> f64 {
    if !df.is_finite() || df <= 0.0 || x.is_nan() {
        return f64::NAN;
    }
    if x < 0.0 || x == f64::INFINITY {
        return 0.0;
    }
    if x == 0.0 {
        return if df < 2.0 {
            f64::INFINITY
        } else if df == 2.0 {
            0.5
        } else {
            0.0
        };
    }
    let a = df / 2.0;
    ((a - 1.0) * x.ln() - x / 2.0 - a * 2.0f64.ln() - ln_gamma(a)).exp()
}
pub fn chi2_cdf(df: f64, x: f64) -> f64 {
    if !df.is_finite() || df <= 0.0 || x.is_nan() {
        return f64::NAN;
    }
    if x <= 0.0 {
        0.0
    } else {
        gamma_pq(df / 2.0, x / 2.0).0
    }
}
pub fn chi2_inverse(df: f64, p: f64) -> f64 {
    if !df.is_finite() || df <= 0.0 || !probability(p) {
        return f64::NAN;
    }
    if p == 0.0 {
        return 0.0;
    }
    if p == 1.0 {
        return f64::INFINITY;
    }
    let mut hi = if p < 0.01 {
        2.0 * ((p.ln() + ln_gamma(df / 2.0 + 1.0)) / (df / 2.0)).exp()
    } else {
        df.max(1.0)
    };
    if hi == 0.0 {
        return 0.0;
    }
    let mut lo = 0.0;
    while chi2_cdf(df, hi) < p {
        hi *= 2.0;
        if !hi.is_finite() {
            return hi;
        }
    }
    for _ in 0..180 {
        let mid = lo + (hi - lo) / 2.0;
        let (lower, upper) = gamma_pq(df / 2.0, mid / 2.0);
        if if p > 0.5 { upper > 1.0 - p } else { lower < p } {
            lo = mid;
        } else {
            hi = mid;
        }
    }
    (lo + hi) / 2.0
}
fn finite(xs: &[f64]) -> bool {
    !xs.is_empty() && xs.iter().all(|x| x.is_finite())
}
fn compensated(xs: impl Iterator<Item = f64>) -> f64 {
    let (mut total, mut correction) = (0.0, 0.0);
    for x in xs {
        let y = x - correction;
        let t = total + y;
        correction = (t - total) - y;
        total = t;
    }
    total
}
pub fn sum(xs: &[f64]) -> f64 {
    if xs.iter().any(|x| !x.is_finite()) {
        f64::NAN
    } else {
        compensated(xs.iter().copied())
    }
}
pub fn mean(xs: &[f64]) -> f64 {
    if !finite(xs) {
        f64::NAN
    } else {
        compensated(xs.iter().map(|x| x / xs.len() as f64))
    }
}
fn sorted(xs: &[f64]) -> Option<Vec<f64>> {
    if !finite(xs) {
        return None;
    }
    let mut a = xs.to_vec();
    a.sort_by(f64::total_cmp);
    Some(a)
}
pub fn median(xs: &[f64]) -> f64 {
    let Some(a) = sorted(xs) else {
        return f64::NAN;
    };
    let n = a.len();
    if n % 2 == 1 {
        a[n / 2]
    } else {
        a[n / 2 - 1] / 2.0 + a[n / 2] / 2.0
    }
}
pub fn mode(xs: &[f64]) -> Vec<f64> {
    let Some(a) = sorted(xs) else {
        return vec![];
    };
    let (mut best, mut out, mut i) = (0, Vec::new(), 0);
    while i < a.len() {
        let mut j = i + 1;
        while j < a.len() && a[i] == a[j] {
            j += 1;
        }
        let count = j - i;
        if count > best {
            best = count;
            out.clear();
        }
        if count == best {
            out.push(a[i]);
        }
        i = j;
    }
    out
}
pub fn variance(xs: &[f64]) -> f64 {
    let m = mean(xs);
    if m.is_nan() {
        m
    } else {
        compensated(xs.iter().map(|x| (x - m).powi(2) / xs.len() as f64))
    }
}
pub fn sample_variance(xs: &[f64]) -> f64 {
    if xs.len() < 2 {
        return f64::NAN;
    }
    let m = mean(xs);
    compensated(xs.iter().map(|x| (x - m).powi(2) / (xs.len() - 1) as f64))
}
pub fn sd(xs: &[f64]) -> f64 {
    variance(xs).sqrt()
}
pub fn sample_sd(xs: &[f64]) -> f64 {
    sample_variance(xs).sqrt()
}
pub fn min(xs: &[f64]) -> f64 {
    if !finite(xs) {
        f64::NAN
    } else {
        xs.iter().copied().fold(f64::INFINITY, f64::min)
    }
}
pub fn max(xs: &[f64]) -> f64 {
    if !finite(xs) {
        f64::NAN
    } else {
        xs.iter().copied().fold(f64::NEG_INFINITY, f64::max)
    }
}
pub fn quartile1(xs: &[f64]) -> f64 {
    let Some(a) = sorted(xs) else {
        return f64::NAN;
    };
    median(&a[..a.len() / 2])
}
pub fn quartile3(xs: &[f64]) -> f64 {
    let Some(a) = sorted(xs) else {
        return f64::NAN;
    };
    median(&a[(a.len() + 1) / 2..])
}
pub fn range(xs: &[f64]) -> f64 {
    max(xs) - min(xs)
}
pub fn iqr(xs: &[f64]) -> f64 {
    quartile3(xs) - quartile1(xs)
}
fn weights(values: &[f64], freqs: &[f64]) -> Option<f64> {
    if !finite(values)
        || values.len() != freqs.len()
        || freqs.iter().any(|x| !x.is_finite() || *x < 0.0)
    {
        return None;
    }
    let total = sum(freqs);
    if total > 0.0 && total.is_finite() {
        Some(total)
    } else {
        None
    }
}
pub fn mean_weighted(values: &[f64], freqs: &[f64]) -> f64 {
    let Some(total) = weights(values, freqs) else {
        return f64::NAN;
    };
    compensated(values.iter().zip(freqs).map(|(x, w)| x * (w / total)))
}
pub fn sd_weighted(values: &[f64], freqs: &[f64]) -> f64 {
    let Some(total) = weights(values, freqs) else {
        return f64::NAN;
    };
    let m = mean_weighted(values, freqs);
    compensated(
        values
            .iter()
            .zip(freqs)
            .map(|(x, w)| (x - m).powi(2) * (w / total)),
    )
    .sqrt()
}
fn paired(xs: &[f64], ys: &[f64]) -> bool {
    finite(xs) && finite(ys) && xs.len() == ys.len()
}
pub fn covariance(xs: &[f64], ys: &[f64]) -> f64 {
    if !paired(xs, ys) {
        return f64::NAN;
    }
    let (mx, my) = (mean(xs), mean(ys));
    compensated(
        xs.iter()
            .zip(ys)
            .map(|(x, y)| (x - mx) * (y - my) / xs.len() as f64),
    )
}
pub fn correlation(xs: &[f64], ys: &[f64]) -> f64 {
    if !paired(xs, ys) || xs.len() < 2 {
        return f64::NAN;
    }
    let denominator = sd(xs) * sd(ys);
    if denominator == 0.0 {
        f64::NAN
    } else {
        (covariance(xs, ys) / denominator).clamp(-1.0, 1.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fit {
    pub expr: Expr,
    pub r2: f64,
    pub params: Vec<f64>,
}
fn coefficient(value: f64) -> Expr {
    let rounded = format!("{value:.5e}").parse::<f64>().unwrap_or(value);
    Expr::Real(if rounded == 0.0 { 0.0 } else { rounded })
}
fn x() -> Expr {
    Expr::Var("x".into())
}
fn quality(
    xs: &[f64],
    ys: &[f64],
    params: &[f64],
    model: &dyn Fn(f64, &[f64]) -> f64,
) -> Option<f64> {
    let center = mean(ys);
    let error = compensated(
        xs.iter()
            .zip(ys)
            .map(|(x, y)| (y - model(*x, params)).powi(2)),
    );
    let total = compensated(ys.iter().map(|y| (y - center).powi(2)));
    if !error.is_finite() {
        None
    } else if total == 0.0 {
        Some(
            if error < 1e-20 * (1.0 + center.abs()).powi(2) * ys.len() as f64 {
                1.0
            } else {
                0.0
            },
        )
    } else {
        Some(1.0 - error / total)
    }
}
fn finish(
    xs: &[f64],
    ys: &[f64],
    params: Vec<f64>,
    expr: Expr,
    model: &dyn Fn(f64, &[f64]) -> f64,
) -> Option<Fit> {
    if params.iter().any(|x| !x.is_finite()) {
        return None;
    }
    Some(Fit {
        expr: simplify(&expr),
        r2: quality(xs, ys, &params, model)?,
        params,
    })
}
fn dot(a: &[f64], b: &[f64]) -> f64 {
    compensated(a.iter().zip(b).map(|(x, y)| x * y))
}
fn least_squares(rows: &[Vec<f64>], ys: &[f64]) -> Option<Vec<f64>> {
    let n = rows.len();
    let m = rows.first()?.len();
    if n < m || m == 0 || ys.len() != n {
        return None;
    }
    let mut columns: Vec<Vec<f64>> = (0..m)
        .map(|i| rows.iter().map(|r| r[i]).collect())
        .collect();
    let mut order: Vec<_> = (0..m).collect();
    let mut r = vec![vec![0.0; m]; m];
    let mut rhs = vec![0.0; m];
    let initial = columns.iter().map(|c| dot(c, c).sqrt()).fold(0.0, f64::max);
    for k in 0..m {
        let pivot = (k..m).max_by(|a, b| {
            dot(&columns[*a], &columns[*a]).total_cmp(&dot(&columns[*b], &columns[*b]))
        })?;
        columns.swap(k, pivot);
        order.swap(k, pivot);
        for row in &mut r[..k] {
            row.swap(k, pivot);
        }
        let norm = dot(&columns[k], &columns[k]).sqrt();
        if !norm.is_finite() || norm <= initial * 1e-12 {
            return None;
        }
        r[k][k] = norm;
        for value in &mut columns[k] {
            *value /= norm;
        }
        rhs[k] = dot(&columns[k], ys);
        for j in k + 1..m {
            for _ in 0..2 {
                let projection = dot(&columns[k], &columns[j]);
                r[k][j] += projection;
                for i in 0..n {
                    columns[j][i] -= projection * columns[k][i];
                }
            }
        }
    }
    let mut fitted = vec![0.0; m];
    for i in (0..m).rev() {
        fitted[i] = (rhs[i] - (i + 1..m).map(|j| r[i][j] * fitted[j]).sum::<f64>()) / r[i][i];
    }
    let mut out = vec![0.0; m];
    for i in 0..m {
        out[order[i]] = fitted[i];
    }
    if out.iter().all(|x| x.is_finite()) {
        Some(out)
    } else {
        None
    }
}
fn evaluate_poly(x: f64, p: &[f64]) -> f64 {
    p.iter().rev().fold(0.0, |a, b| a * x + b)
}
pub fn fit_poly(xs: &[f64], ys: &[f64], degree: usize) -> Option<Fit> {
    if !paired(xs, ys) || degree > 32 || xs.len() <= degree {
        return None;
    }
    let center = mean(xs);
    let scale = xs.iter().map(|x| (x - center).abs()).fold(0.0, f64::max);
    if degree > 0 && scale == 0.0 {
        return None;
    }
    let scale = scale.max(f64::MIN_POSITIVE);
    let rows: Vec<Vec<f64>> = xs
        .iter()
        .map(|x| {
            let z = (x - center) / scale;
            let mut row = vec![1.0; degree + 1];
            for i in 1..=degree {
                row[i] = row[i - 1] * z;
            }
            row
        })
        .collect();
    let normalized = least_squares(&rows, ys)?;
    let mut params = vec![0.0; degree + 1];
    for (i, c) in normalized.iter().enumerate() {
        for (j, p) in params.iter_mut().enumerate().take(i + 1) {
            *p += c * n_choose_k(i as f64, j as f64) * (-center).powi((i - j) as i32)
                / scale.powi(i as i32);
        }
    }
    let data_scale = ys.iter().map(|y| y.abs()).fold(0.0, f64::max);
    for (i, value) in params.iter_mut().enumerate() {
        if value.abs() <= 64.0 * f64::EPSILON * data_scale / scale.powi(i as i32) {
            *value = 0.0;
        }
    }
    let expr = params.iter().enumerate().fold(int(0), |a, (i, c)| {
        add(a, mul(coefficient(*c), pow(x(), int(i as i64))))
    });
    finish(xs, ys, params, expr, &evaluate_poly)
}
pub fn fit_linear(xs: &[f64], ys: &[f64]) -> Option<Fit> {
    let fitted = fit_poly(xs, ys, 1)?;
    Some(Fit {
        expr: fitted.expr,
        r2: fitted.r2,
        params: vec![fitted.params[1], fitted.params[0]],
    })
}
fn solve_system(mut a: Vec<Vec<f64>>, mut b: Vec<f64>) -> Option<Vec<f64>> {
    let n = b.len();
    for i in 0..n {
        let pivot = (i..n).max_by(|j, k| a[*j][i].abs().total_cmp(&a[*k][i].abs()))?;
        if a[pivot][i].abs() < 1e-300 || !a[pivot][i].is_finite() {
            return None;
        }
        a.swap(i, pivot);
        b.swap(i, pivot);
        for j in i + 1..n {
            let ratio = a[j][i] / a[i][i];
            for k in i..n {
                a[j][k] -= ratio * a[i][k];
            }
            b[j] -= ratio * b[i];
        }
    }
    let mut out = vec![0.0; n];
    for i in (0..n).rev() {
        out[i] = (b[i] - (i + 1..n).map(|j| a[i][j] * out[j]).sum::<f64>()) / a[i][i];
    }
    if out.iter().all(|x| x.is_finite()) {
        Some(out)
    } else {
        None
    }
}
fn squared_error(xs: &[f64], ys: &[f64], p: &[f64], model: &dyn Fn(f64, &[f64]) -> f64) -> f64 {
    let error = compensated(xs.iter().zip(ys).map(|(x, y)| (y - model(*x, p)).powi(2)));
    if error.is_finite() {
        error
    } else {
        f64::INFINITY
    }
}
fn lm(
    xs: &[f64],
    ys: &[f64],
    mut p: Vec<f64>,
    model: &dyn Fn(f64, &[f64]) -> f64,
    iterations: usize,
) -> Vec<f64> {
    let mut error = squared_error(xs, ys, &p, model);
    let mut lambda = 1e-3;
    let m = p.len();
    for _ in 0..iterations {
        let mut matrix = vec![vec![0.0; m]; m];
        let mut rhs = vec![0.0; m];
        let mut trial = p.clone();
        for (&x, &y) in xs.iter().zip(ys) {
            let predicted = model(x, &p);
            let residual = y - predicted;
            let mut gradient = vec![0.0; m];
            for j in 0..m {
                let h = 1e-6 * (1.0 + p[j].abs());
                trial[j] = p[j] + h;
                let upper = model(x, &trial);
                trial[j] = p[j] - h;
                let lower = model(x, &trial);
                trial[j] = p[j];
                gradient[j] = (upper - lower) / (2.0 * h);
            }
            for i in 0..m {
                rhs[i] += gradient[i] * residual;
                for j in 0..m {
                    matrix[i][j] += gradient[i] * gradient[j];
                }
            }
        }
        for i in 0..m {
            matrix[i][i] += lambda * matrix[i][i].max(1e-12);
        }
        let Some(delta) = solve_system(matrix, rhs) else {
            lambda *= 10.0;
            if lambda > 1e16 {
                break;
            }
            continue;
        };
        let candidate: Vec<_> = p.iter().zip(&delta).map(|(p, d)| p + d).collect();
        let next = squared_error(xs, ys, &candidate, model);
        if next < error {
            let improvement = error - next;
            p = candidate;
            error = next;
            lambda = (lambda * 0.2).max(1e-15);
            if delta
                .iter()
                .zip(&p)
                .all(|(d, p)| d.abs() < 1e-11 * (1.0 + p.abs()))
                || improvement < 1e-24 * (1.0 + error)
            {
                break;
            }
        } else {
            lambda *= 10.0;
            if lambda > 1e16 {
                break;
            }
        }
        if error < 1e-26 {
            break;
        }
    }
    p
}
pub fn fit_exp(xs: &[f64], ys: &[f64]) -> Option<Fit> {
    if !paired(xs, ys) || ys.iter().any(|y| *y <= 0.0) {
        return None;
    }
    let logs: Vec<_> = ys.iter().map(|y| y.ln()).collect();
    let line = fit_linear(xs, &logs)?;
    let model = |x: f64, p: &[f64]| p[0] * (p[1] * x).exp();
    let params = lm(
        xs,
        ys,
        vec![line.params[1].exp(), line.params[0]],
        &model,
        100,
    );
    let expr = mul(
        coefficient(params[0]),
        pow(Expr::Const(Constant::E), mul(coefficient(params[1]), x())),
    );
    finish(xs, ys, params, expr, &model)
}
pub fn fit_exp_base(xs: &[f64], ys: &[f64]) -> Option<Fit> {
    let exp = fit_exp(xs, ys)?;
    let params = vec![exp.params[0], exp.params[1].exp()];
    let expr = mul(coefficient(params[0]), pow(coefficient(params[1]), x()));
    finish(xs, ys, params, expr, &|x, p| p[0] * p[1].powf(x))
}
pub fn fit_power(xs: &[f64], ys: &[f64]) -> Option<Fit> {
    if !paired(xs, ys) || xs.iter().chain(ys).any(|x| *x <= 0.0) {
        return None;
    }
    let logsx: Vec<_> = xs.iter().map(|x| x.ln()).collect();
    let logsy: Vec<_> = ys.iter().map(|x| x.ln()).collect();
    let line = fit_linear(&logsx, &logsy)?;
    let model = |x: f64, p: &[f64]| p[0] * x.powf(p[1]);
    let params = lm(
        xs,
        ys,
        vec![line.params[1].exp(), line.params[0]],
        &model,
        100,
    );
    let expr = mul(coefficient(params[0]), pow(x(), coefficient(params[1])));
    finish(xs, ys, params, expr, &model)
}
pub fn fit_log(xs: &[f64], ys: &[f64]) -> Option<Fit> {
    if !paired(xs, ys) || xs.iter().any(|x| *x <= 0.0) {
        return None;
    }
    let transformed: Vec<_> = xs.iter().map(|x| x.ln()).collect();
    let line = fit_linear(&transformed, ys)?;
    let params = vec![line.params[1], line.params[0]];
    let expr = add(
        coefficient(params[0]),
        mul(coefficient(params[1]), call("ln", x())),
    );
    finish(xs, ys, params, expr, &|x, p| p[0] + p[1] * x.ln())
}
fn sigmoid(x: f64) -> f64 {
    if x >= 0.0 {
        1.0 / (1.0 + (-x).exp())
    } else {
        let e = x.exp();
        e / (1.0 + e)
    }
}
pub fn fit_logistic(xs: &[f64], ys: &[f64]) -> Option<Fit> {
    if !paired(xs, ys)
        || xs.len() < 4
        || ys.iter().any(|x| *x <= 0.0)
        || range(xs) == 0.0
        || range(ys) == 0.0
    {
        return None;
    }
    let center = mean(xs);
    let scale = range(xs) / 2.0;
    let z: Vec<_> = xs.iter().map(|x| (x - center) / scale).collect();
    let model = |x: f64, p: &[f64]| {
        if p[2] > 0.0 {
            p[2] * sigmoid(p[1] * x - p[0])
        } else {
            f64::NAN
        }
    };
    let mut best: Option<(f64, Vec<f64>)> = None;
    for multiplier in [1.001, 1.05, 1.2, 1.5, 2.0, 4.0, 10.0] {
        let capacity = max(ys) * multiplier;
        let transformed: Vec<_> = ys.iter().map(|y| (capacity / y - 1.0).ln()).collect();
        let Some(line) = fit_linear(&z, &transformed) else {
            continue;
        };
        let p = lm(
            &z,
            ys,
            vec![line.params[1], -line.params[0], capacity],
            &model,
            250,
        );
        let error = squared_error(&z, ys, &p, &model);
        if best.as_ref().is_none_or(|(e, _)| error < *e) {
            best = Some((error, p));
        }
    }
    let (_, p) = best?;
    let b = p[1] / scale;
    let params = vec![(p[0] + b * center).exp(), b, p[2]];
    if params[0] <= 0.0 {
        return None;
    }
    let expr = div(
        coefficient(params[2]),
        add(
            int(1),
            mul(
                coefficient(params[0]),
                pow(Expr::Const(Constant::E), mul(coefficient(-params[1]), x())),
            ),
        ),
    );
    finish(xs, ys, params, expr, &|x, p| {
        p[2] * sigmoid(p[1] * x - p[0].ln())
    })
}
pub fn fit_sin(xs: &[f64], ys: &[f64]) -> Option<Fit> {
    if !paired(xs, ys) || xs.len() < 4 || range(xs) == 0.0 || range(ys) == 0.0 {
        return None;
    }
    let center = mean(xs);
    let span = range(xs);
    let z: Vec<_> = xs.iter().map(|x| (x - center) / span).collect();
    let mut sorted: Vec<_> = z.iter().copied().zip(ys.iter().copied()).collect();
    sorted.sort_by(|a, b| a.0.total_cmp(&b.0));
    let gaps: Vec<_> = sorted
        .windows(2)
        .map(|p| p[1].0 - p[0].0)
        .filter(|x| *x > 0.0)
        .collect();
    if gaps.is_empty() {
        return None;
    }
    let upper = (PI / median(&gaps)).min(512.0 * PI);
    let mut frequencies: Vec<f64> = (0..=768)
        .map(|i| 0.1 + (upper - 0.1) * i as f64 / 768.0)
        .collect();
    let baseline = mean(ys);
    let mut crossings = Vec::new();
    for pair in sorted.windows(2) {
        let (a, b) = (pair[0].1 - baseline, pair[1].1 - baseline);
        if a * b < 0.0 {
            crossings.push(pair[0].0 + (pair[1].0 - pair[0].0) * a / (a - b));
        }
    }
    if crossings.len() >= 2 {
        let gaps: Vec<_> = crossings.windows(2).map(|w| w[1] - w[0]).collect();
        let estimate = PI / median(&gaps);
        frequencies.extend([estimate / 2.0, estimate, estimate * 2.0]);
    }
    let model = |x: f64, p: &[f64]| p[0] + p[1] * (p[2] * x + p[3]).sin();
    let mut candidates: Vec<(f64, Vec<f64>)> = Vec::new();
    for frequency in frequencies {
        let rows: Vec<_> = z
            .iter()
            .map(|x| vec![1.0, (frequency * x).sin(), (frequency * x).cos()])
            .collect();
        let Some(p) = least_squares(&rows, ys) else {
            continue;
        };
        let params = vec![p[0], p[1].hypot(p[2]), frequency, p[2].atan2(p[1])];
        let error = squared_error(&z, ys, &params, &model);
        candidates.push((error, params));
    }
    candidates.sort_by(|a, b| a.0.total_cmp(&b.0));
    let mut best: Option<(f64, Vec<f64>)> = None;
    for (_, initial) in candidates.into_iter().take(8) {
        let p = lm(&z, ys, initial, &model, 250);
        let error = squared_error(&z, ys, &p, &model);
        if best.as_ref().is_none_or(|(e, _)| error < *e) {
            best = Some((error, p));
        }
    }
    let (_, p) = best?;
    let mut b = p[1];
    let mut c = p[2] / span;
    let mut d = p[3] - c * center;
    if b < 0.0 {
        b = -b;
        d += PI;
    }
    if c < 0.0 {
        c = -c;
        d = PI - d;
    }
    d = (d + PI).rem_euclid(2.0 * PI) - PI;
    let params = vec![p[0], b, c, d];
    let expr = add(
        coefficient(params[0]),
        mul(
            coefficient(params[1]),
            call(
                "sin",
                add(mul(coefficient(params[2]), x()), coefficient(params[3])),
            ),
        ),
    );
    finish(xs, ys, params, expr, &model)
}

pub(crate) fn builtin_arity(name: &str) -> Option<usize> {
    match name {
        "factorial" => Some(1),
        "nCr" | "binomial" => Some(2),
        "binompdf" | "binomcdf" | "normalpdf" | "normalcdf" | "invnorm" | "uniformpdf"
        | "uniformcdf" => Some(3),
        "hypergeometricpdf" | "hypergeometriccdf" => Some(4),
        "poissonpdf" | "poissoncdf" | "geometricpdf" | "geometriccdf" | "exponentialpdf"
        | "exponentialcdf" | "tpdf" | "tcdf" | "invt" | "chi2pdf" | "chi2cdf" | "invchi2" => {
            Some(2)
        }
        _ => None,
    }
}
pub(crate) fn builtin(name: &str, a: &[f64]) -> f64 {
    if builtin_arity(name) != Some(a.len()) {
        return f64::NAN;
    }
    match name {
        "factorial" => factorial(a[0]),
        "nCr" | "binomial" => n_choose_k(a[0], a[1]),
        "binompdf" => binomial_pmf(a[0], a[1], a[2]),
        "binomcdf" => binomial_cdf(a[0], a[1], a[2]),
        "normalpdf" => normal_pdf(a[0], a[1], a[2]),
        "normalcdf" => normal_cdf(a[0], a[1], a[2]),
        "invnorm" => normal_inverse(a[0], a[1], a[2]),
        "poissonpdf" => poisson_pmf(a[0], a[1]),
        "poissoncdf" => poisson_cdf(a[0], a[1]),
        "hypergeometricpdf" => hypergeometric_pmf(a[0], a[1], a[2], a[3]),
        "hypergeometriccdf" => hypergeometric_cdf(a[0], a[1], a[2], a[3]),
        "geometricpdf" => geometric_pmf(a[0], a[1]),
        "geometriccdf" => geometric_cdf(a[0], a[1]),
        "exponentialpdf" => exponential_pdf(a[0], a[1]),
        "exponentialcdf" => exponential_cdf(a[0], a[1]),
        "uniformpdf" => uniform_pdf(a[0], a[1], a[2]),
        "uniformcdf" => uniform_cdf(a[0], a[1], a[2]),
        "tpdf" => t_pdf(a[0], a[1]),
        "tcdf" => t_cdf(a[0], a[1]),
        "invt" => t_inverse(a[0], a[1]),
        "chi2pdf" => chi2_pdf(a[0], a[1]),
        "chi2cdf" => chi2_cdf(a[0], a[1]),
        "invchi2" => chi2_inverse(a[0], a[1]),
        _ => f64::NAN,
    }
}
pub(crate) fn exact_integer(name: &str, a: &[f64]) -> Option<i64> {
    if name == "factorial" {
        let n = *a.first()?;
        if count(n) && n <= 20.0 {
            return Some((1..=n as i64).product());
        }
        return None;
    }
    if !matches!(name, "nCr" | "binomial") {
        return None;
    }
    let (n, k) = (*a.first()?, *a.get(1)?);
    if !count(n) || !count(k) || n >= u64::MAX as f64 || k >= u64::MAX as f64 {
        return None;
    }
    if k > n {
        return Some(0);
    }
    let n = n as u64;
    let k = (k as u64).min(n - k as u64);
    let mut value = 1u128;
    for i in 1..=k {
        value = value.checked_mul((n - k + i) as u128)? / i as u128;
        if value > i64::MAX as u128 {
            return None;
        }
    }
    Some(value as i64)
}

#[cfg(test)]
mod tests {
    use super::*;
    fn close(actual: f64, expected: f64, tolerance: f64) {
        assert!(
            (actual - expected).abs() <= tolerance,
            "{actual:.16e} != {expected:.16e}"
        );
    }
    #[test]
    fn combinations_and_gamma() {
        close(n_choose_k(10.0, 3.0), 120.0, 0.0);
        close(n_choose_k(1000.0, 2.0), 499500.0, 0.0);
        close(n_choose_k(5.0, 7.0), 0.0, 0.0);
        close(factorial(0.0), 1.0, 0.0);
        close(factorial(5.0), 120.0, 0.0);
        close(factorial(0.5), PI.sqrt() / 2.0, 2e-15);
        close(factorial(-0.5), PI.sqrt(), 2e-15);
        assert!(factorial(-1.0).is_nan());
        assert!(n_choose_k(4.2, 2.0).is_nan());
        assert_eq!(
            exact_integer("nCr", &[66.0, 33.0]),
            Some(7219428434016265740)
        );
    }
    #[test]
    fn binomial_values() {
        close(binomial_pmf(10.0, 0.3, 3.0), 0.266827932, 2e-15);
        close(binomial_cdf(10.0, 0.3, 3.0), 0.6496107184, 4e-15);
        let total: f64 = (0..=10).map(|k| binomial_pmf(10.0, 0.3, k as f64)).sum();
        close(total, 1.0, 1e-14);
        close(
            binomial_range(10.0, 0.3, 2.0, 4.0),
            (2..=4).map(|k| binomial_pmf(10.0, 0.3, k as f64)).sum(),
            1e-14,
        );
        close(
            binomial_range(10.0, 0.3, 2.1, 4.9),
            binomial_range(10.0, 0.3, 3.0, 4.0),
            1e-14,
        );
        close(binomial_pmf(10.0, 0.0, 0.0), 1.0, 0.0);
        close(binomial_cdf(10.0, 1.0, 9.0), 0.0, 0.0);
        close(binomial_pmf(1000.0, 0.5, 500.0), 0.0252250181783608, 3e-14);
        assert!(binomial_cdf(-1.0, 0.2, 2.0).is_nan());
    }
    #[test]
    fn normal_reference_and_tails() {
        close(normal_cdf(0.0, 1.0, 1.96), 0.9750021048517795, 2e-15);
        close(normal_inverse(0.0, 1.0, 0.975), 1.959963984540054, 1e-12);
        close(normal_pdf(0.0, 1.0, 0.0), 1.0 / (2.0 * PI).sqrt(), 1e-15);
        close(normal_cdf(0.0, 1.0, -8.0), 6.220960574271784e-16, 2e-29);
        for p in [
            1e-15,
            1e-10,
            0.01,
            0.1,
            0.5 - 1e-12,
            0.5,
            0.975,
            1.0 - 1e-12,
        ] {
            let q = normal_inverse(2.0, 3.0, p);
            close(normal_cdf(2.0, 3.0, q), p, 2e-15);
        }
        close(normal_range(0.0, 1.0, -1.0, 1.0), 0.6826894921370859, 2e-15);
        assert!(normal_range(0.0, 1.0, 8.0, 9.0) > 6e-16);
        assert!(normal_cdf(0.0, -1.0, 0.0).is_nan());
        assert_eq!(normal_inverse(0.0, 1.0, 0.0), f64::NEG_INFINITY);
        assert_eq!(normal_inverse(0.0, 1.0, 1.0), f64::INFINITY);
    }
    #[test]
    fn poisson_and_hypergeometric() {
        close(poisson_cdf(4.0, 2.0), 0.2381033056, 5e-11);
        close(poisson_pmf(4.0, 2.0), 8.0 * (-4.0f64).exp(), 1e-15);
        close(poisson_cdf(0.0, 0.0), 1.0, 0.0);
        close(poisson_pmf(0.0, 0.0), 1.0, 0.0);
        for k in 0..=5 {
            let expected = n_choose_k(7.0, k as f64) * n_choose_k(13.0, 5.0 - k as f64)
                / n_choose_k(20.0, 5.0);
            close(
                hypergeometric_pmf(20.0, 7.0, 5.0, k as f64),
                expected,
                3e-15,
            );
            let cdf: f64 = (0..=k)
                .map(|i| hypergeometric_pmf(20.0, 7.0, 5.0, i as f64))
                .sum();
            close(hypergeometric_cdf(20.0, 7.0, 5.0, k as f64), cdf, 1e-14);
        }
        close(hypergeometric_pmf(0.0, 0.0, 0.0, 0.0), 1.0, 0.0);
        assert!(hypergeometric_cdf(5.0, 6.0, 3.0, 1.0).is_nan());
    }
    #[test]
    fn elementary_distributions() {
        close(geometric_pmf(0.25, 3.0), 0.140625, 1e-15);
        close(geometric_cdf(0.25, 3.0), 0.578125, 1e-15);
        close(geometric_pmf(1.0, 1.0), 1.0, 0.0);
        close(exponential_pdf(2.0, 1.0), 2.0 * (-2.0f64).exp(), 1e-15);
        close(exponential_cdf(2.0, 1.0), 1.0 - (-2.0f64).exp(), 1e-15);
        close(exponential_cdf(2.0, -1.0), 0.0, 0.0);
        close(uniform_pdf(1.0, 5.0, 2.0), 0.25, 0.0);
        close(uniform_cdf(1.0, 5.0, 2.0), 0.25, 0.0);
        close(uniform_cdf(1.0, 5.0, 8.0), 1.0, 0.0);
        assert!(uniform_pdf(1.0, 1.0, 1.0).is_nan());
    }
    #[test]
    fn student_and_chi_squared() {
        close(t_inverse(10.0, 0.975), 2.228138851986274, 1e-12);
        close(t_pdf(1.0, 0.0), 1.0 / PI, 1e-15);
        close(t_cdf(1.0, 1.0), 0.75, 1e-15);
        close(chi2_cdf(3.0, 7.815), 0.95, 1e-4);
        close(chi2_inverse(2.0, 0.95), -2.0 * 0.05f64.ln(), 1e-12);
        close(chi2_pdf(2.0, 2.0), 0.5 * (-1.0f64).exp(), 1e-15);
        for df in [0.5, 1.0, 3.0, 10.0, 100.0] {
            for p in [1e-10, 0.001, 0.1, 0.5 - 1e-12, 0.5, 0.975, 1.0 - 1e-10] {
                close(t_cdf(df, t_inverse(df, p)), p, 1e-13);
                close(chi2_cdf(df, chi2_inverse(df, p)), p, 1e-13);
            }
        }
        close(chi2_cdf(0.2, chi2_inverse(0.2, 1e-12)), 1e-12, 1e-25);
        assert!(t_inverse(0.0, 0.5).is_nan());
        assert_eq!(chi2_pdf(1.0, 0.0), f64::INFINITY);
        close(chi2_pdf(2.0, 0.0), 0.5, 0.0);
        close(chi2_pdf(3.0, 0.0), 0.0, 0.0);
    }
    #[test]
    fn descriptive_statistics() {
        let a = [2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0];
        close(mean(&a), 5.0, 1e-15);
        close(median(&a), 4.5, 0.0);
        assert_eq!(mode(&a), vec![4.0]);
        close(sd(&a), 2.0, 1e-15);
        close(sample_sd(&a), 2.138089935, 1e-9);
        close(variance(&a), 4.0, 1e-15);
        close(sample_variance(&a), 32.0 / 7.0, 1e-15);
        close(sum(&a), 40.0, 0.0);
        close(min(&a), 2.0, 0.0);
        close(max(&a), 9.0, 0.0);
        close(range(&a), 7.0, 0.0);
        close(quartile1(&a), 4.0, 0.0);
        close(quartile3(&a), 6.0, 0.0);
        close(iqr(&a), 2.0, 0.0);
        let odd = [1.0, 2.0, 3.0, 4.0, 5.0];
        close(quartile1(&odd), 1.5, 0.0);
        close(quartile3(&odd), 4.5, 0.0);
        assert_eq!(mode(&[1.0, 1.0, 2.0, 2.0, 3.0]), vec![1.0, 2.0]);
        assert!(mean(&[]).is_nan());
        assert!(sample_sd(&[1.0]).is_nan());
        assert!(median(&[f64::NAN]).is_nan());
        close(sum(&[]), 0.0, 0.0);
        let offset = [1e12 + 1.0, 1e12 + 2.0, 1e12 + 3.0];
        close(variance(&offset), 2.0 / 3.0, 1e-15);
    }
    #[test]
    fn weighted_and_correlation() {
        let values = [2.0, 4.0, 5.0, 7.0, 9.0];
        let frequencies = [1.0, 3.0, 2.0, 1.0, 1.0];
        close(mean_weighted(&values, &frequencies), 5.0, 1e-15);
        close(sd_weighted(&values, &frequencies), 2.0, 1e-15);
        close(
            covariance(&[1.0, 2.0, 3.0], &[2.0, 4.0, 6.0]),
            4.0 / 3.0,
            1e-15,
        );
        close(correlation(&[1.0, 2.0, 3.0], &[2.0, 4.0, 6.0]), 1.0, 1e-15);
        close(correlation(&[1.0, 2.0, 3.0], &[6.0, 4.0, 2.0]), -1.0, 1e-15);
        assert!(mean_weighted(&[1.0], &[-1.0]).is_nan());
        assert!(mean_weighted(&[1.0], &[0.0]).is_nan());
        assert!(correlation(&[1.0, 1.0], &[2.0, 3.0]).is_nan());
        assert!(covariance(&[1.0], &[]).is_nan());
    }
    fn check_fit(fit: Option<Fit>, xs: &[f64], ys: &[f64]) -> Fit {
        let fit = fit.expect("fit");
        assert!(fit.r2 > 1.0 - 1e-10, "r2={} {:?}", fit.r2, fit.params);
        let compiled = crate::compile(&fit.expr, &["x"], &crate::eval::Empty).unwrap();
        for (x, y) in xs.iter().zip(ys) {
            let got = compiled.eval(&[*x]);
            assert!(
                (got - y).abs() < 2e-5 * (1.0 + y.abs()),
                "{} @ {x}: {got} != {y}",
                crate::format_expr(&fit.expr)
            );
        }
        fit
    }
    #[test]
    fn linear_and_polynomial_fits() {
        let xs: Vec<_> = (-12..=12).map(|i| i as f64 / 3.0).collect();
        let ys: Vec<_> = xs.iter().map(|x| 1.5 * x + 0.3).collect();
        let line = check_fit(fit_linear(&xs, &ys), &xs, &ys);
        close(line.params[0], 1.5, 1e-12);
        close(line.params[1], 0.3, 1e-12);
        assert_eq!(crate::format_expr(&line.expr), "1.5x + 0.3");
        for degree in 0..=6 {
            let ys: Vec<_> = xs
                .iter()
                .map(|x| {
                    (0..=degree)
                        .map(|i| (i as f64 + 0.2) * x.powi(i as i32))
                        .sum()
                })
                .collect();
            let fit = check_fit(fit_poly(&xs, &ys, degree), &xs, &ys);
            for (i, p) in fit.params.iter().enumerate() {
                close(*p, i as f64 + 0.2, 1e-8);
            }
        }
        let ys: Vec<_> = xs.iter().map(|x| 2.0 * x).collect();
        assert_eq!(
            crate::format_expr(&fit_linear(&xs, &ys).unwrap().expr),
            "2x"
        );
        assert!(fit_linear(&[1.0, 1.0], &[2.0, 3.0]).is_none());
        assert!(fit_poly(&[1.0, 2.0], &[2.0, 3.0], 2).is_none());
    }
    #[test]
    fn exponential_power_log_fits() {
        let xs: Vec<_> = (1..=30).map(|i| i as f64 / 8.0).collect();
        let ys: Vec<_> = xs.iter().map(|x| 2.5 * (0.7 * x).exp()).collect();
        let fit = check_fit(fit_exp(&xs, &ys), &xs, &ys);
        close(fit.params[0], 2.5, 1e-8);
        close(fit.params[1], 0.7, 1e-8);
        let fit = check_fit(fit_exp_base(&xs, &ys), &xs, &ys);
        close(fit.params[1], 0.7f64.exp(), 1e-8);
        let ys: Vec<_> = xs.iter().map(|x| 1.7 * x.powf(2.3)).collect();
        let fit = check_fit(fit_power(&xs, &ys), &xs, &ys);
        close(fit.params[0], 1.7, 1e-8);
        close(fit.params[1], 2.3, 1e-8);
        let ys: Vec<_> = xs.iter().map(|x| 0.3 + 1.5 * x.ln()).collect();
        check_fit(fit_log(&xs, &ys), &xs, &ys);
        assert!(fit_exp(&[1.0, 2.0], &[-1.0, 2.0]).is_none());
        assert!(fit_log(&[0.0, 1.0], &[1.0, 2.0]).is_none());
    }
    #[test]
    fn logistic_fits() {
        let xs: Vec<_> = (-20..=40).map(|i| i as f64 / 5.0).collect();
        for (a, b, c) in [(3.0, 0.8, 10.0), (0.4, -1.2, 4.0)] {
            let ys: Vec<_> = xs.iter().map(|x| c / (1.0 + a * (-b * x).exp())).collect();
            let fit = check_fit(fit_logistic(&xs, &ys), &xs, &ys);
            for (p, want) in fit.params.iter().zip([a, b, c]) {
                close(*p, want, 1e-6);
            }
        }
    }
    #[test]
    fn sinusoidal_fits() {
        let xs: Vec<_> = (0..100).map(|i| i as f64 * 0.13).collect();
        for (a, b, c, d) in [(0.5, 2.0, 1.3, 0.4), (-2.0, 0.7, 3.2, -1.2)] {
            let ys: Vec<_> = xs.iter().map(|x| a + b * (c * x + d).sin()).collect();
            let fit = check_fit(fit_sin(&xs, &ys), &xs, &ys);
            for (p, want) in fit.params.iter().zip([a, b, c, d]) {
                close(*p, want, 1e-6);
            }
        }
    }
    #[test]
    fn invalid_inputs_never_panic() {
        for value in [
            f64::NAN,
            f64::INFINITY,
            f64::NEG_INFINITY,
            -1.0,
            0.0,
            0.5,
            2.0,
        ] {
            let _ = factorial(value);
            let _ = normal_inverse(0.0, 1.0, value);
            let _ = t_inverse(10.0, value);
            let _ = chi2_inverse(3.0, value);
            let _ = binomial_cdf(value, value, value);
            let _ = hypergeometric_cdf(value, value, value, value);
        }
        assert!(fit_sin(&[], &[]).is_none());
        assert!(fit_logistic(&[1.0, 2.0], &[1.0, 2.0]).is_none());
        assert!(fit_linear(&[1.0, 2.0], &[1.0, f64::NAN]).is_none());
    }
}
