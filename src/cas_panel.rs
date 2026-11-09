use crate::theme;
use eframe::egui::{self, Align2, Color32, FontId, Margin, Rect, RichText, Sense, Stroke, TextEdit, pos2, vec2};
use plotwerk_math::{CasEnv, cas_eval};

pub struct CasRow {
    pub input: String,
    pub output: Result<String, String>,
    pub plotted: bool,
}

#[derive(Default)]
pub struct CasPanel {
    pub rows: Vec<CasRow>,
    env: CasEnv,
    new_input: String,
    focus_new: bool,
}

pub enum CasAction {
    None,
    Plot(String),
}

fn safe_eval(input: &str, env: &mut CasEnv) -> Result<String, String> {
    let input = input.to_string();
    let mut local = env.clone();
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let r = cas_eval(&input, &mut local);
        (r, local)
    }));
    match result {
        Ok((r, new_env)) => {
            *env = new_env;
            r
        }
        Err(_) => Err("Interner Fehler bei der Berechnung".into()),
    }
}

const GUTTER_W: f32 = 40.0;

fn paint_gutter(ui: &egui::Ui, rect: Rect, number: usize) -> Rect {
    let gutter = Rect::from_min_max(rect.min, pos2(rect.left() + GUTTER_W, rect.bottom()));
    let painter = ui.painter();
    painter.rect_filled(gutter, 0.0, theme::GUTTER);
    painter.line_segment([gutter.right_top(), gutter.right_bottom()], Stroke::new(1.0, theme::BORDER));
    painter.text(pos2(gutter.center().x, rect.top() + 19.0), Align2::CENTER_CENTER, number.to_string(), FontId::proportional(13.0), theme::MUTED);
    painter.line_segment([rect.left_bottom(), rect.right_bottom()], Stroke::new(1.0, theme::BORDER));
    gutter
}

fn paint_arrow(ui: &mut egui::Ui, color: Color32) {
    let (r, _) = ui.allocate_exact_size(vec2(18.0, 18.0), Sense::hover());
    let y = r.center().y;
    let stroke = Stroke::new(1.6, color);
    ui.painter().line_segment([pos2(r.left() + 1.0, y), pos2(r.right() - 3.0, y)], stroke);
    ui.painter().line_segment([pos2(r.right() - 8.0, y - 4.5), pos2(r.right() - 3.0, y)], stroke);
    ui.painter().line_segment([pos2(r.right() - 8.0, y + 4.5), pos2(r.right() - 3.0, y)], stroke);
}

impl CasPanel {
    pub fn insert_text(&mut self, text: &str) {
        if text == "\u{8}" {
            self.new_input.pop();
        } else {
            self.new_input.push_str(text);
        }
        self.focus_new = true;
    }

    pub fn submit(&mut self) {
        if !self.new_input.trim().is_empty() {
            let input = std::mem::take(&mut self.new_input);
            let output = safe_eval(&input, &mut self.env);
            self.rows.push(CasRow { input, output, plotted: false });
        }
        self.focus_new = true;
    }

    pub fn clear(&mut self) {
        self.rows.clear();
        self.env = CasEnv::default();
        self.new_input.clear();
    }

    pub fn push_input(&mut self, input: &str) {
        let output = safe_eval(input, &mut self.env);
        self.rows.push(CasRow { input: input.to_string(), output, plotted: false });
    }

    fn recompute(&mut self) {
        self.env = CasEnv::default();
        for row in &mut self.rows {
            row.output = safe_eval(&row.input, &mut self.env);
        }
    }

    pub fn ui(&mut self, ui: &mut egui::Ui) -> CasAction {
        let mut action = CasAction::None;
        let mut remove: Option<usize> = None;
        let mut changed = false;
        let row_margin = Margin { left: GUTTER_W as i8 + 12, right: 8, top: 8, bottom: 8 };
        egui::ScrollArea::vertical().auto_shrink([false, false]).stick_to_bottom(true).show(ui, |ui| {
            ui.spacing_mut().item_spacing.y = 0.0;
            for (i, row) in self.rows.iter_mut().enumerate() {
                let inner = egui::Frame::new().fill(Color32::WHITE).inner_margin(row_margin).show(ui, |ui| {
                    ui.set_width(ui.available_width());
                    ui.spacing_mut().item_spacing.y = 4.0;
                    ui.horizontal(|ui| {
                        let edit = ui.add(
                            TextEdit::singleline(&mut row.input)
                                .font(FontId::proportional(16.0))
                                .frame(false)
                                .desired_width(ui.available_width() - 26.0),
                        );
                        if edit.lost_focus() {
                            changed = true;
                        }
                        if ui.add(egui::Button::new(RichText::new("×").size(17.0).color(theme::MUTED)).frame(false)).on_hover_text("Zeile löschen").clicked() {
                            remove = Some(i);
                        }
                    });
                    ui.horizontal(|ui| match &row.output {
                        Ok(out) => {
                            paint_arrow(ui, theme::MUTED);
                            ui.label(RichText::new(crate::pretty::pretty(out)).size(16.0).color(theme::TEXT));
                        }
                        Err(e) => {
                            ui.label(RichText::new(e).color(theme::ERROR).small());
                        }
                    });
                });
                let rect = inner.response.rect;
                let gutter = paint_gutter(ui, rect, i + 1);
                if let Ok(out) = &row.output {
                    if out.contains('x') && !out.starts_with('{') {
                        let c = pos2(gutter.center().x, rect.top() + 44.0);
                        let marble = Rect::from_center_size(c, vec2(20.0, 20.0));
                        let resp = ui.interact(marble, ui.id().with(("cas_marble", i)), Sense::click());
                        if row.plotted {
                            ui.painter().circle_filled(c, 7.0, theme::ACCENT);
                        } else {
                            ui.painter().circle_filled(c, 7.0, Color32::WHITE);
                            ui.painter().circle_stroke(c, 6.5, Stroke::new(1.6, if resp.hovered() { theme::ACCENT } else { theme::MUTED }));
                        }
                        if resp.on_hover_text("In der Grafik darstellen").clicked() && !row.plotted {
                            row.plotted = true;
                            action = CasAction::Plot(out.clone());
                        }
                    }
                }
            }
            let n = self.rows.len();
            let inner = egui::Frame::new().fill(Color32::WHITE).inner_margin(row_margin).show(ui, |ui| {
                ui.set_width(ui.available_width());
                ui.add(
                    TextEdit::singleline(&mut self.new_input)
                        .hint_text("Eingabe …")
                        .font(FontId::proportional(16.0))
                        .frame(false)
                        .desired_width(f32::INFINITY),
                )
            });
            paint_gutter(ui, inner.response.rect, n + 1);
            let resp = inner.inner;
            if self.focus_new {
                resp.request_focus();
                self.focus_new = false;
            }
            if resp.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)) && !self.new_input.trim().is_empty() {
                let input = std::mem::take(&mut self.new_input);
                let output = safe_eval(&input, &mut self.env);
                self.rows.push(CasRow { input, output, plotted: false });
                self.focus_new = true;
            }
            ui.add_space(12.0);
            ui.horizontal(|ui| {
                ui.add_space(GUTTER_W + 12.0);
                ui.label(RichText::new("Löse · NLöse · Ableitung · Integral · Faktorisiere · Ausmultiplizieren · Vereinfache · Ersetze").small().color(theme::MUTED));
            });
        });
        if let Some(i) = remove {
            self.rows.remove(i);
            changed = true;
        }
        if changed {
            self.recompute();
        }
        action
    }
}
