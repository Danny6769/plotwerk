use crate::expr_util::NoCtx;
use crate::prob::draw_axes;
use crate::theme;
use eframe::egui::{self, Align2, Color32, CornerRadius, FontId, Margin, Pos2, Rect, RichText, Sense, Shape, Stroke, TextEdit, pos2, vec2};
use plotwerk_math::{self as m, format_number, stats as st};

pub const COLS: usize = 6;
pub const ROWS: usize = 60;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Model {
    Linear,
    Poly2,
    Poly3,
    Exp,
    ExpBase,
    Power,
    Log,
    Logistic,
    Sin,
}

impl Model {
    pub const ALL: [Model; 9] = [Model::Linear, Model::Poly2, Model::Poly3, Model::Exp, Model::ExpBase, Model::Power, Model::Log, Model::Logistic, Model::Sin];

    pub fn label(self) -> &'static str {
        match self {
            Model::Linear => "Linear  a·x + b",
            Model::Poly2 => "Quadratisch",
            Model::Poly3 => "Kubisch",
            Model::Exp => "Exponentiell  a·e^(b·x)",
            Model::ExpBase => "Exponentiell  a·b^x",
            Model::Power => "Potenz  a·x^b",
            Model::Log => "Logarithmisch  a + b·ln(x)",
            Model::Logistic => "Logistisch",
            Model::Sin => "Sinus",
        }
    }

    fn fit(self, xs: &[f64], ys: &[f64]) -> Option<st::Fit> {
        match self {
            Model::Linear => st::fit_linear(xs, ys),
            Model::Poly2 => st::fit_poly(xs, ys, 2),
            Model::Poly3 => st::fit_poly(xs, ys, 3),
            Model::Exp => st::fit_exp(xs, ys),
            Model::ExpBase => st::fit_exp_base(xs, ys),
            Model::Power => st::fit_power(xs, ys),
            Model::Log => st::fit_log(xs, ys),
            Model::Logistic => st::fit_logistic(xs, ys),
            Model::Sin => st::fit_sin(xs, ys),
        }
    }
}

pub enum TableAction {
    None,
    ToGraph { points: String, function: Option<String> },
}

pub struct TablePanel {
    cells: Vec<Vec<String>>,
    pub x_col: usize,
    pub y_col: usize,
    pub model: Model,
}

impl Default for TablePanel {
    fn default() -> Self {
        let mut cells = vec![vec![String::new(); COLS]; ROWS];
        let sample = [(1.0, 2.1), (2.0, 3.9), (3.0, 6.2), (4.0, 7.8), (5.0, 10.1), (6.0, 12.2)];
        for (i, (x, y)) in sample.iter().enumerate() {
            cells[i][0] = format_number(*x);
            cells[i][1] = format_number(*y);
        }
        Self { cells, x_col: 0, y_col: 1, model: Model::Linear }
    }
}

fn col_name(c: usize) -> String {
    ((b'A' + c as u8) as char).to_string()
}

fn cell_value(s: &str) -> Option<f64> {
    let t = s.trim().replace(',', ".");
    if t.is_empty() {
        return None;
    }
    if let Ok(v) = t.parse::<f64>() {
        return Some(v);
    }
    m::parse_expr(&t).ok().and_then(|e| m::eval(&e, &NoCtx).ok()).filter(|v| v.is_finite())
}

impl TablePanel {
    pub fn clear(&mut self) {
        for row in &mut self.cells {
            for c in row {
                c.clear();
            }
        }
    }

    pub fn cells_text(&self) -> Vec<String> {
        let mut out = Vec::new();
        for (r, row) in self.cells.iter().enumerate() {
            for (c, v) in row.iter().enumerate() {
                if !v.trim().is_empty() {
                    out.push(format!("{}{}={}", col_name(c), r + 1, v.trim()));
                }
            }
        }
        out
    }

    pub fn set_cell_text(&mut self, line: &str) {
        let Some((addr, val)) = line.split_once('=') else { return };
        let mut chars = addr.chars();
        let Some(c) = chars.next() else { return };
        let col = (c as u8).wrapping_sub(b'A') as usize;
        let Ok(row) = chars.as_str().parse::<usize>() else { return };
        if col < COLS && row >= 1 && row <= ROWS {
            self.cells[row - 1][col] = val.to_string();
        }
    }

    fn column(&self, c: usize) -> Vec<f64> {
        self.cells.iter().filter_map(|r| cell_value(&r[c])).collect()
    }

    fn pairs(&self) -> (Vec<f64>, Vec<f64>) {
        let mut xs = Vec::new();
        let mut ys = Vec::new();
        for r in &self.cells {
            if let (Some(x), Some(y)) = (cell_value(&r[self.x_col]), cell_value(&r[self.y_col])) {
                xs.push(x);
                ys.push(y);
            }
        }
        (xs, ys)
    }

    pub fn grid_ui(&mut self, ui: &mut egui::Ui) {
        let cell_w = 96.0;
        let head_w = 44.0;
        let row_h = 28.0;
        egui::ScrollArea::both().auto_shrink([false, false]).show(ui, |ui| {
            ui.spacing_mut().item_spacing = vec2(0.0, 0.0);
            ui.horizontal(|ui| {
                let (r, _) = ui.allocate_exact_size(vec2(head_w, row_h), Sense::hover());
                ui.painter().rect_filled(r, 0.0, theme::GUTTER);
                for c in 0..COLS {
                    let (r, _) = ui.allocate_exact_size(vec2(cell_w, row_h), Sense::hover());
                    let used = c == self.x_col || c == self.y_col;
                    ui.painter().rect_filled(r, 0.0, if used { theme::ACCENT_SOFT } else { theme::GUTTER });
                    ui.painter().rect_stroke(r, 0.0, Stroke::new(1.0, theme::BORDER), egui::StrokeKind::Inside);
                    ui.painter().text(r.center(), Align2::CENTER_CENTER, col_name(c), FontId::proportional(13.0), if used { theme::ACCENT } else { theme::MUTED });
                }
            });
            for row in 0..ROWS {
                ui.horizontal(|ui| {
                    let (r, _) = ui.allocate_exact_size(vec2(head_w, row_h), Sense::hover());
                    ui.painter().rect_filled(r, 0.0, theme::GUTTER);
                    ui.painter().rect_stroke(r, 0.0, Stroke::new(1.0, theme::BORDER), egui::StrokeKind::Inside);
                    ui.painter().text(r.center(), Align2::CENTER_CENTER, (row + 1).to_string(), FontId::proportional(12.5), theme::MUTED);
                    for c in 0..COLS {
                        let text = &mut self.cells[row][c];
                        let bad = !text.trim().is_empty() && cell_value(text).is_none();
                        let resp = ui.add_sized(
                            vec2(cell_w, row_h),
                            TextEdit::singleline(text)
                                .frame(false)
                                .margin(Margin::symmetric(6, 4))
                                .horizontal_align(egui::Align::Max)
                                .text_color(if bad { theme::ERROR } else { theme::TEXT }),
                        );
                        let r = resp.rect;
                        let stroke = if resp.has_focus() { Stroke::new(2.0, theme::ACCENT) } else { Stroke::new(1.0, theme::BORDER) };
                        ui.painter().rect_stroke(r, 0.0, stroke, egui::StrokeKind::Inside);
                    }
                });
            }
        });
    }

    pub fn side_ui(&mut self, ui: &mut egui::Ui) -> TableAction {
        let mut action = TableAction::None;
        egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
            egui::Frame::new().inner_margin(Margin::symmetric(16, 14)).show(ui, |ui| {
                ui.set_width(ui.available_width());
                ui.horizontal(|ui| {
                    ui.label(RichText::new("x-Werte").small().color(theme::MUTED));
                    egui::ComboBox::from_id_salt("xcol").width(60.0).selected_text(col_name(self.x_col)).show_ui(ui, |ui| {
                        for c in 0..COLS {
                            ui.selectable_value(&mut self.x_col, c, col_name(c));
                        }
                    });
                    ui.add_space(10.0);
                    ui.label(RichText::new("y-Werte").small().color(theme::MUTED));
                    egui::ComboBox::from_id_salt("ycol").width(60.0).selected_text(col_name(self.y_col)).show_ui(ui, |ui| {
                        for c in 0..COLS {
                            ui.selectable_value(&mut self.y_col, c, col_name(c));
                        }
                    });
                });
                ui.add_space(12.0);
                let xs = self.column(self.x_col);
                ui.label(RichText::new(format!("Statistik Spalte {}", col_name(self.x_col))).strong());
                if xs.is_empty() {
                    ui.label(RichText::new("Keine Zahlen in dieser Spalte").color(theme::MUTED));
                } else {
                    let rows: Vec<(&str, f64)> = vec![
                        ("n", xs.len() as f64),
                        ("Mittelwert x̄", st::mean(&xs)),
                        ("Standardabweichung σ", st::sd(&xs)),
                        ("Stichproben-Stdabw. s", if xs.len() > 1 { st::sample_sd(&xs) } else { f64::NAN }),
                        ("Varianz σ²", st::variance(&xs)),
                        ("Summe", st::sum(&xs)),
                        ("Minimum", st::min(&xs)),
                        ("1. Quartil", st::quartile1(&xs)),
                        ("Median", st::median(&xs)),
                        ("3. Quartil", st::quartile3(&xs)),
                        ("Maximum", st::max(&xs)),
                        ("Spannweite", st::range(&xs)),
                    ];
                    egui::Grid::new("stats_grid").num_columns(2).striped(true).spacing(vec2(20.0, 4.0)).show(ui, |ui| {
                        for (k, v) in rows {
                            ui.label(RichText::new(k).color(theme::MUTED));
                            ui.label(RichText::new(format_number(v)).strong());
                            ui.end_row();
                        }
                    });
                }
                ui.add_space(16.0);
                ui.label(RichText::new("Regression").strong());
                egui::ComboBox::from_id_salt("model").width(ui.available_width()).selected_text(self.model.label()).show_ui(ui, |ui| {
                    for mdl in Model::ALL {
                        ui.selectable_value(&mut self.model, mdl, mdl.label());
                    }
                });
                let (xs, ys) = self.pairs();
                ui.add_space(6.0);
                if xs.len() >= 2 {
                    let r = st::correlation(&xs, &ys);
                    let fit = self.model.fit(&xs, &ys);
                    egui::Frame::new().fill(theme::ACCENT_SOFT).corner_radius(CornerRadius::same(8)).inner_margin(Margin::symmetric(12, 10)).show(ui, |ui| {
                        ui.set_width(ui.available_width());
                        match &fit {
                            Some(f) => {
                                ui.label(RichText::new(format!("y = {}", crate::pretty::pretty(&m::format_expr(&f.expr)))).size(17.0).color(theme::ACCENT).strong());
                                ui.label(format!("R² = {}", format_number(f.r2)));
                            }
                            None => {
                                ui.label(RichText::new("Modell passt nicht zu den Daten").color(theme::ERROR));
                            }
                        }
                        ui.label(format!("Korrelationskoeffizient r = {}", format_number(r)));
                    });
                    ui.add_space(8.0);
                    if ui.add(egui::Button::new(RichText::new("In Grafikrechner übernehmen").color(Color32::WHITE).strong()).fill(theme::ACCENT)).clicked() {
                        let pts: Vec<String> = xs.iter().zip(&ys).map(|(x, y)| format!("({}, {})", format_number(*x), format_number(*y))).collect();
                        action = TableAction::ToGraph {
                            points: format!("{{{}}}", pts.join(", ")),
                            function: fit.map(|f| format!("y = {}", m::format_expr(&f.expr))),
                        };
                    }
                } else {
                    ui.label(RichText::new("Mindestens zwei Wertepaare nötig").color(theme::MUTED));
                }
            });
        });
        action
    }

    pub fn chart_ui(&mut self, ui: &mut egui::Ui) {
        let (resp, painter) = ui.allocate_painter(ui.available_size(), Sense::hover());
        let full = resp.rect;
        painter.rect_filled(full, 0.0, Color32::WHITE);
        painter.line_segment([full.left_top(), full.right_top()], Stroke::new(1.0, theme::BORDER));
        let (xs, ys) = self.pairs();
        if xs.is_empty() {
            painter.text(full.center(), Align2::CENTER_CENTER, "Werte in die Tabelle eintragen", FontId::proportional(15.0), theme::MUTED);
            return;
        }
        let rect = Rect::from_min_max(full.min + vec2(64.0, 30.0), full.max - vec2(24.0, 40.0));
        let (mut x0, mut x1) = (st::min(&xs), st::max(&xs));
        let (mut y0, mut y1) = (st::min(&ys), st::max(&ys));
        let pad = |a: &mut f64, b: &mut f64| {
            let w = (*b - *a).abs().max(1.0);
            *a -= w * 0.1;
            *b += w * 0.1;
        };
        pad(&mut x0, &mut x1);
        pad(&mut y0, &mut y1);
        y0 = y0.min(0.0);
        let sx = |x: f64| rect.left() + ((x - x0) / (x1 - x0)) as f32 * rect.width();
        let sy = |y: f64| rect.bottom() - ((y - y0) / (y1 - y0)) as f32 * rect.height();
        draw_axes(&painter, rect, x0, x1, y0, y1, false);
        if let Some(fit) = self.model.fit(&xs, &ys) {
            if let Ok(f) = m::compile(&fit.expr, &["x"], &NoCtx) {
                let pts: Vec<Pos2> = (0..=400)
                    .filter_map(|i| {
                        let x = x0 + (x1 - x0) * i as f64 / 400.0;
                        let y = f.eval(&[x]);
                        (y.is_finite() && y > y0 - (y1 - y0) && y < y1 + (y1 - y0)).then(|| pos2(sx(x), sy(y).clamp(rect.top() - 2.0, rect.bottom() + 2.0)))
                    })
                    .collect();
                painter.add(Shape::line(pts, Stroke::new(2.4, theme::ACCENT)));
            }
        }
        for (x, y) in xs.iter().zip(&ys) {
            let p = pos2(sx(*x), sy(*y));
            painter.circle_filled(p, 5.5, Color32::WHITE);
            painter.circle_filled(p, 4.2, Color32::from_rgb(38, 50, 72));
        }
    }
}
