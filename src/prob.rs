use crate::theme;
use eframe::egui::{self, Align2, Color32, CornerRadius, FontId, Margin, Painter, Pos2, Rect, RichText, Sense, Shape, Stroke, pos2, vec2};
use plotwerk_math::{format_number, stats as st};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Dist {
    Binomial,
    Normal,
    Poisson,
    Hypergeometric,
    Geometric,
    Exponential,
    Uniform,
    StudentT,
    ChiSquared,
}

impl Dist {
    pub const ALL: [Dist; 9] = [
        Dist::Binomial,
        Dist::Normal,
        Dist::Poisson,
        Dist::Hypergeometric,
        Dist::Geometric,
        Dist::Exponential,
        Dist::Uniform,
        Dist::StudentT,
        Dist::ChiSquared,
    ];

    pub fn label(self) -> &'static str {
        match self {
            Dist::Binomial => "Binomialverteilung",
            Dist::Normal => "Normalverteilung",
            Dist::Poisson => "Poissonverteilung",
            Dist::Hypergeometric => "Hypergeometrische Verteilung",
            Dist::Geometric => "Geometrische Verteilung",
            Dist::Exponential => "Exponentialverteilung",
            Dist::Uniform => "Stetige Gleichverteilung",
            Dist::StudentT => "t-Verteilung",
            Dist::ChiSquared => "Chi-Quadrat-Verteilung",
        }
    }

    pub fn discrete(self) -> bool {
        matches!(self, Dist::Binomial | Dist::Poisson | Dist::Hypergeometric | Dist::Geometric)
    }

    fn params(self) -> &'static [(&'static str, f64, f64, f64)] {
        match self {
            Dist::Binomial => &[("n", 10.0, 1.0, 1.0), ("p", 0.3, 0.0, 0.01)],
            Dist::Normal => &[("μ", 0.0, f64::NEG_INFINITY, 0.1), ("σ", 1.0, 1e-9, 0.1)],
            Dist::Poisson => &[("λ", 4.0, 1e-9, 0.1)],
            Dist::Hypergeometric => &[("N", 50.0, 1.0, 1.0), ("K", 20.0, 0.0, 1.0), ("n", 10.0, 1.0, 1.0)],
            Dist::Geometric => &[("p", 0.2, 1e-6, 0.01)],
            Dist::Exponential => &[("λ", 0.5, 1e-9, 0.05)],
            Dist::Uniform => &[("a", 0.0, f64::NEG_INFINITY, 0.1), ("b", 5.0, f64::NEG_INFINITY, 0.1)],
            Dist::StudentT => &[("df", 10.0, 1.0, 1.0)],
            Dist::ChiSquared => &[("df", 4.0, 1.0, 1.0)],
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Interval {
    Left,
    Between,
    Right,
}

pub struct ProbPanel {
    pub dist: Dist,
    pub params: [f64; 3],
    pub interval: Interval,
    pub a: f64,
    pub b: f64,
    pub target: f64,
}

impl Default for ProbPanel {
    fn default() -> Self {
        let mut p = Self { dist: Dist::Binomial, params: [0.0; 3], interval: Interval::Left, a: 2.0, b: 3.0, target: 0.95 };
        p.reset_params();
        p
    }
}

impl ProbPanel {
    fn reset_params(&mut self) {
        for (i, (_, d, _, _)) in self.dist.params().iter().enumerate() {
            self.params[i] = *d;
        }
        let (m, s) = self.mean_sd();
        if self.dist.discrete() {
            self.a = (m - s).round().max(0.0);
            self.b = m.round();
        } else {
            self.a = m - s;
            self.b = m + s;
        }
    }

    fn sanitized(&self) -> [f64; 3] {
        let mut p = self.params;
        match self.dist {
            Dist::Binomial => {
                p[0] = p[0].round().max(1.0);
                p[1] = p[1].clamp(0.0, 1.0);
            }
            Dist::Hypergeometric => {
                p[0] = p[0].round().max(1.0);
                p[1] = p[1].round().clamp(0.0, p[0]);
                p[2] = p[2].round().clamp(1.0, p[0]);
            }
            Dist::Geometric => p[0] = p[0].clamp(1e-6, 1.0),
            Dist::StudentT | Dist::ChiSquared => p[0] = p[0].max(1.0),
            _ => {}
        }
        p
    }

    pub fn pmf(&self, k: f64) -> f64 {
        let p = self.sanitized();
        match self.dist {
            Dist::Binomial => st::binomial_pmf(p[0], p[1], k),
            Dist::Poisson => st::poisson_pmf(p[0], k),
            Dist::Hypergeometric => st::hypergeometric_pmf(p[0], p[1], p[2], k),
            Dist::Geometric => st::geometric_pmf(p[0], k),
            Dist::Normal => st::normal_pdf(p[0], p[1], k),
            Dist::Exponential => st::exponential_pdf(p[0], k),
            Dist::Uniform => st::uniform_pdf(p[0], p[1], k),
            Dist::StudentT => st::t_pdf(p[0], k),
            Dist::ChiSquared => st::chi2_pdf(p[0], k),
        }
    }

    pub fn cdf(&self, x: f64) -> f64 {
        let p = self.sanitized();
        if self.dist.discrete() && x < self.support_min() {
            return 0.0;
        }
        match self.dist {
            Dist::Binomial => st::binomial_cdf(p[0], p[1], x.floor()),
            Dist::Poisson => st::poisson_cdf(p[0], x.floor()),
            Dist::Hypergeometric => st::hypergeometric_cdf(p[0], p[1], p[2], x.floor()),
            Dist::Geometric => st::geometric_cdf(p[0], x.floor()),
            Dist::Normal => st::normal_cdf(p[0], p[1], x),
            Dist::Exponential => st::exponential_cdf(p[0], x),
            Dist::Uniform => st::uniform_cdf(p[0], p[1], x),
            Dist::StudentT => st::t_cdf(p[0], x),
            Dist::ChiSquared => st::chi2_cdf(p[0], x),
        }
    }

    fn support_min(&self) -> f64 {
        match self.dist {
            Dist::Geometric => 1.0,
            Dist::Hypergeometric => {
                let p = self.sanitized();
                (p[2] - (p[0] - p[1])).max(0.0)
            }
            _ => 0.0,
        }
    }

    pub fn mean_sd(&self) -> (f64, f64) {
        let p = self.sanitized();
        match self.dist {
            Dist::Binomial => (p[0] * p[1], (p[0] * p[1] * (1.0 - p[1])).sqrt()),
            Dist::Normal => (p[0], p[1]),
            Dist::Poisson => (p[0], p[0].sqrt()),
            Dist::Hypergeometric => {
                let (nn, k, n) = (p[0], p[1], p[2]);
                let m = n * k / nn;
                let v = if nn > 1.0 { n * (k / nn) * ((nn - k) / nn) * ((nn - n) / (nn - 1.0)) } else { 0.0 };
                (m, v.max(0.0).sqrt())
            }
            Dist::Geometric => (1.0 / p[0], (1.0 - p[0]).sqrt() / p[0]),
            Dist::Exponential => (1.0 / p[0], 1.0 / p[0]),
            Dist::Uniform => ((p[0] + p[1]) / 2.0, (p[1] - p[0]).abs() / 12f64.sqrt()),
            Dist::StudentT => (0.0, if p[0] > 2.0 { (p[0] / (p[0] - 2.0)).sqrt() } else { f64::INFINITY }),
            Dist::ChiSquared => (p[0], (2.0 * p[0]).sqrt()),
        }
    }

    pub fn probability(&self) -> f64 {
        let v = if self.dist.discrete() {
            let (a, b) = (self.a.round(), self.b.round());
            match self.interval {
                Interval::Left => self.cdf(b),
                Interval::Right => 1.0 - self.cdf(a - 1.0),
                Interval::Between => {
                    if b < a {
                        0.0
                    } else {
                        self.cdf(b) - self.cdf(a - 1.0)
                    }
                }
            }
        } else {
            match self.interval {
                Interval::Left => self.cdf(self.b),
                Interval::Right => 1.0 - self.cdf(self.a),
                Interval::Between => (self.cdf(self.b) - self.cdf(self.a)).max(0.0),
            }
        };
        v.clamp(0.0, 1.0)
    }

    fn label(&self) -> String {
        let f = |v: f64| format_number(if self.dist.discrete() { v.round() } else { v });
        match self.interval {
            Interval::Left => format!("P(X ≤ {})", f(self.b)),
            Interval::Right => format!("P(X ≥ {})", f(self.a)),
            Interval::Between => format!("P({} ≤ X ≤ {})", f(self.a), f(self.b)),
        }
    }

    fn in_interval(&self, x: f64) -> bool {
        match self.interval {
            Interval::Left => x <= self.b + 1e-9,
            Interval::Right => x >= self.a - 1e-9,
            Interval::Between => x >= self.a - 1e-9 && x <= self.b + 1e-9,
        }
    }

    fn range(&self) -> (f64, f64) {
        let (m, s) = self.mean_sd();
        let p = self.sanitized();
        let s = if s.is_finite() { s } else { 3.0 };
        match self.dist {
            Dist::Binomial => ((m - 4.5 * s).floor().max(0.0), (m + 4.5 * s).ceil().min(p[0]).max(1.0)),
            Dist::Hypergeometric => ((m - 4.5 * s).floor().max(self.support_min()), (m + 4.5 * s).ceil().min(p[2].min(p[1])).max(1.0)),
            Dist::Poisson => (0.0, (m + 4.5 * s).ceil().max(5.0)),
            Dist::Geometric => (1.0, (m + 4.0 * s).ceil().max(6.0)),
            Dist::Exponential => (0.0, m + 5.0 * s),
            Dist::ChiSquared => (0.0, m + 4.5 * s),
            Dist::Uniform => {
                let w = (p[1] - p[0]).abs().max(1e-9);
                (p[0].min(p[1]) - 0.25 * w, p[0].max(p[1]) + 0.25 * w)
            }
            _ => (m - 4.0 * s, m + 4.0 * s),
        }
    }

    fn inverse(&self, target: f64) -> f64 {
        let target = target.clamp(1e-12, 1.0 - 1e-12);
        let (lo0, hi0) = self.range();
        if self.dist.discrete() {
            let mut k = self.support_min();
            while k <= hi0 * 4.0 + 50.0 {
                if self.cdf(k) >= target - 1e-12 {
                    return k;
                }
                k += 1.0;
            }
            return f64::NAN;
        }
        let (mut lo, mut hi) = (lo0 - (hi0 - lo0) * 10.0, hi0 + (hi0 - lo0) * 10.0);
        if matches!(self.dist, Dist::Exponential | Dist::ChiSquared) {
            lo = 0.0;
        }
        for _ in 0..200 {
            let mid = 0.5 * (lo + hi);
            if self.cdf(mid) < target {
                lo = mid;
            } else {
                hi = mid;
            }
        }
        0.5 * (lo + hi)
    }

    pub fn side_ui(&mut self, ui: &mut egui::Ui) {
        egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
            egui::Frame::new().inner_margin(Margin::symmetric(16, 14)).show(ui, |ui| {
                ui.set_width(ui.available_width());
                ui.label(RichText::new("Verteilung").small().color(theme::MUTED));
                let before = self.dist;
                egui::ComboBox::from_id_salt("dist")
                    .width(ui.available_width())
                    .selected_text(RichText::new(self.dist.label()).size(15.5))
                    .show_ui(ui, |ui| {
                        for d in Dist::ALL {
                            ui.selectable_value(&mut self.dist, d, d.label());
                        }
                    });
                if before != self.dist {
                    self.reset_params();
                }
                ui.add_space(10.0);
                ui.horizontal_wrapped(|ui| {
                    for (i, (name, _, min, step)) in self.dist.params().iter().enumerate() {
                        ui.label(RichText::new(*name).size(15.5).strong());
                        let mut dv = egui::DragValue::new(&mut self.params[i]).speed(*step).min_decimals(if *step < 1.0 { 2 } else { 0 });
                        if min.is_finite() {
                            dv = dv.range(*min..=f64::INFINITY);
                        }
                        if *name == "p" {
                            dv = dv.range(0.0..=1.0);
                        }
                        ui.add_sized(vec2(78.0, 28.0), dv);
                        ui.add_space(8.0);
                    }
                });
                ui.add_space(14.0);
                ui.label(RichText::new("Bereich").small().color(theme::MUTED));
                ui.horizontal(|ui| {
                    for (iv, text) in [(Interval::Left, "P(X ≤ b)"), (Interval::Between, "P(a ≤ X ≤ b)"), (Interval::Right, "P(X ≥ a)")] {
                        let on = self.interval == iv;
                        let btn = egui::Button::new(RichText::new(text).color(if on { theme::ACCENT } else { theme::TEXT }))
                            .fill(if on { theme::ACCENT_SOFT } else { theme::HOVER })
                            .stroke(Stroke::NONE);
                        if ui.add(btn).clicked() {
                            self.interval = iv;
                        }
                    }
                });
                ui.add_space(6.0);
                ui.horizontal(|ui| {
                    let speed = if self.dist.discrete() { 1.0 } else { 0.05 };
                    if self.interval != Interval::Left {
                        ui.label(RichText::new("a").strong());
                        ui.add_sized(vec2(90.0, 28.0), egui::DragValue::new(&mut self.a).speed(speed).max_decimals(4));
                    }
                    if self.interval != Interval::Right {
                        ui.label(RichText::new("b").strong());
                        ui.add_sized(vec2(90.0, 28.0), egui::DragValue::new(&mut self.b).speed(speed).max_decimals(4));
                    }
                });
                ui.add_space(14.0);
                egui::Frame::new().fill(theme::ACCENT_SOFT).corner_radius(CornerRadius::same(8)).inner_margin(Margin::symmetric(14, 10)).show(ui, |ui| {
                    ui.set_width(ui.available_width());
                    ui.label(RichText::new(format!("{} = {}", self.label(), format_number(self.probability()))).size(19.0).color(theme::ACCENT).strong());
                    let (m, s) = self.mean_sd();
                    ui.label(RichText::new(format!("μ = {}     σ = {}", format_number(m), format_number(s))).color(theme::TEXT));
                });
                ui.add_space(14.0);
                ui.label(RichText::new("Umkehrung: Grenze b zu P(X ≤ b)").small().color(theme::MUTED));
                ui.horizontal(|ui| {
                    ui.label("P =");
                    ui.add_sized(vec2(90.0, 28.0), egui::DragValue::new(&mut self.target).speed(0.005).range(0.0..=1.0).max_decimals(4));
                    let b = self.inverse(self.target);
                    let text = if self.dist.discrete() { format!("kleinstes b = {}", format_number(b)) } else { format!("b = {}", format_number(b)) };
                    ui.label(RichText::new(text).strong());
                });
                if self.dist.discrete() {
                    ui.add_space(16.0);
                    ui.label(RichText::new("Wahrscheinlichkeiten").small().color(theme::MUTED));
                    let (lo, hi) = self.range();
                    egui::Grid::new("prob_table").num_columns(3).striped(true).spacing(vec2(18.0, 4.0)).show(ui, |ui| {
                        ui.label(RichText::new("k").strong());
                        ui.label(RichText::new("P(X = k)").strong());
                        ui.label(RichText::new("P(X ≤ k)").strong());
                        ui.end_row();
                        let mut k = lo;
                        while k <= hi {
                            let hl = self.in_interval(k);
                            let c = if hl { theme::ACCENT } else { theme::TEXT };
                            ui.label(RichText::new(format_number(k)).color(c));
                            ui.label(RichText::new(format_number(self.pmf(k))).color(c));
                            ui.label(RichText::new(format_number(self.cdf(k))).color(theme::MUTED));
                            ui.end_row();
                            k += 1.0;
                        }
                    });
                }
            });
        });
    }

    pub fn chart_ui(&mut self, ui: &mut egui::Ui) {
        let (resp, painter) = ui.allocate_painter(ui.available_size(), Sense::hover());
        let full = resp.rect;
        painter.rect_filled(full, 0.0, Color32::WHITE);
        let rect = Rect::from_min_max(full.min + vec2(70.0, 60.0), full.max - vec2(40.0, 56.0));
        let (lo, hi) = self.range();
        let discrete = self.dist.discrete();
        let (x0, x1) = if discrete { (lo - 0.8, hi + 0.8) } else { (lo, hi) };
        let mut ymax: f64 = 0.0;
        if discrete {
            let mut k = lo;
            while k <= hi {
                ymax = ymax.max(self.pmf(k));
                k += 1.0;
            }
        } else {
            for i in 0..=400 {
                let x = x0 + (x1 - x0) * i as f64 / 400.0;
                let y = self.pmf(x);
                if y.is_finite() {
                    ymax = ymax.max(y);
                }
            }
        }
        if !(ymax > 0.0) || !ymax.is_finite() {
            ymax = 1.0;
        }
        ymax *= 1.12;
        let sx = |x: f64| rect.left() + ((x - x0) / (x1 - x0)) as f32 * rect.width();
        let sy = |y: f64| rect.bottom() - (y / ymax) as f32 * rect.height();
        draw_axes(&painter, rect, x0, x1, 0.0, ymax, discrete);
        let hl = theme::ACCENT;
        let base = Color32::from_rgb(196, 202, 214);
        if discrete {
            let mut k = lo;
            let w = (rect.width() / (x1 - x0) as f32 * 0.72).clamp(2.0, 60.0);
            while k <= hi {
                let p = self.pmf(k);
                let r = Rect::from_min_max(pos2(sx(k) - w / 2.0, sy(p)), pos2(sx(k) + w / 2.0, rect.bottom()));
                painter.rect_filled(r, CornerRadius::same(2), if self.in_interval(k) { hl } else { base });
                k += 1.0;
            }
        } else {
            let n = 600;
            let mut mesh = egui::Mesh::default();
            let fill = hl.gamma_multiply(0.35);
            let mut count = 0u32;
            for i in 0..=n {
                let x = x0 + (x1 - x0) * i as f64 / n as f64;
                if !self.in_interval(x) {
                    continue;
                }
                let y = self.pmf(x);
                if !y.is_finite() {
                    continue;
                }
                mesh.colored_vertex(pos2(sx(x), rect.bottom()), fill);
                mesh.colored_vertex(pos2(sx(x), sy(y).max(rect.top())), fill);
                if count > 0 {
                    let k = count * 2;
                    mesh.add_triangle(k - 2, k - 1, k);
                    mesh.add_triangle(k - 1, k, k + 1);
                }
                count += 1;
            }
            painter.add(Shape::mesh(mesh));
            let pts: Vec<Pos2> = (0..=n)
                .filter_map(|i| {
                    let x = x0 + (x1 - x0) * i as f64 / n as f64;
                    let y = self.pmf(x);
                    y.is_finite().then(|| pos2(sx(x), sy(y).max(rect.top() - 4.0)))
                })
                .collect();
            painter.add(Shape::line(pts, Stroke::new(2.6, hl)));
        }
        painter.text(
            pos2(rect.center().x, full.top() + 26.0),
            Align2::CENTER_CENTER,
            format!("{} = {}", self.label(), format_number(self.probability())),
            FontId::proportional(20.0),
            theme::TEXT,
        );
    }
}

fn nice(raw: f64) -> f64 {
    let p = 10f64.powf(raw.log10().floor());
    let m = raw / p;
    (if m < 1.5 { 1.0 } else if m < 3.5 { 2.0 } else if m < 7.5 { 5.0 } else { 10.0 }) * p
}

pub fn draw_axes(painter: &Painter, rect: Rect, x0: f64, x1: f64, y0: f64, y1: f64, integer_x: bool) {
    let grid = Color32::from_rgb(236, 238, 242);
    let axis = Stroke::new(1.3, Color32::from_rgb(70, 76, 90));
    let font = FontId::proportional(12.0);
    let ystep = nice((y1 - y0) / 6.0);
    let mut y = (y0 / ystep).ceil() * ystep;
    while y <= y1 + 1e-12 {
        let py = rect.bottom() - ((y - y0) / (y1 - y0)) as f32 * rect.height();
        painter.line_segment([pos2(rect.left(), py), pos2(rect.right(), py)], Stroke::new(1.0, grid));
        painter.text(pos2(rect.left() - 8.0, py), Align2::RIGHT_CENTER, format_number(y), font.clone(), theme::MUTED);
        y += ystep;
    }
    let mut xstep = nice((x1 - x0) / 10.0);
    if integer_x {
        xstep = xstep.max(1.0).round();
    }
    let mut x = (x0 / xstep).ceil() * xstep;
    while x <= x1 + 1e-12 {
        let px = rect.left() + ((x - x0) / (x1 - x0)) as f32 * rect.width();
        painter.line_segment([pos2(px, rect.bottom()), pos2(px, rect.bottom() + 4.0)], axis);
        painter.text(pos2(px, rect.bottom() + 7.0), Align2::CENTER_TOP, format_number(x), font.clone(), theme::MUTED);
        x += xstep;
    }
    painter.line_segment([rect.left_bottom(), rect.right_bottom()], axis);
    painter.line_segment([rect.left_bottom(), rect.left_top()], axis);
}
