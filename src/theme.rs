use eframe::egui::{self, Color32, CornerRadius, FontData, FontDefinitions, FontFamily, FontId, Stroke, TextStyle, Visuals, vec2};
use std::sync::Arc;

pub const ACCENT: Color32 = Color32::from_rgb(52, 92, 214);
pub const ACCENT_SOFT: Color32 = Color32::from_rgb(232, 238, 253);
pub const TEXT: Color32 = Color32::from_rgb(28, 30, 38);
pub const MUTED: Color32 = Color32::from_rgb(112, 117, 130);
pub const ICON: Color32 = Color32::from_rgb(60, 64, 76);
pub const HOVER: Color32 = Color32::from_rgb(241, 243, 247);
pub const BORDER: Color32 = Color32::from_rgb(222, 225, 231);
pub const GUTTER: Color32 = Color32::from_rgb(244, 245, 248);
pub const PANEL: Color32 = Color32::WHITE;
pub const ERROR: Color32 = Color32::from_rgb(198, 40, 40);
pub const EXAM_OK: Color32 = Color32::from_rgb(27, 125, 62);
pub const EXAM_ALERT: Color32 = Color32::from_rgb(190, 32, 38);

fn load_fonts(ctx: &egui::Context) {
    let mut fonts = FontDefinitions::default();
    let dir = std::env::var("WINDIR").unwrap_or_else(|_| "C:\\Windows".into());
    let mut loaded = Vec::new();
    for (name, file) in [("ui", "segoeui.ttf"), ("ui_symbols", "seguisym.ttf"), ("ui_math", "cambria.ttc")] {
        if let Ok(bytes) = std::fs::read(format!("{dir}\\Fonts\\{file}")) {
            fonts.font_data.insert(name.to_string(), Arc::new(FontData::from_owned(bytes)));
            loaded.push(name);
        }
    }
    if loaded.contains(&"ui") {
        fonts.families.entry(FontFamily::Proportional).or_default().insert(0, "ui".into());
    }
    for fallback in ["ui_symbols", "ui_math"] {
        if loaded.contains(&fallback) {
            fonts.families.entry(FontFamily::Proportional).or_default().push(fallback.into());
            fonts.families.entry(FontFamily::Monospace).or_default().push(fallback.into());
        }
    }
    ctx.set_fonts(fonts);
}

pub fn apply(ctx: &egui::Context) {
    load_fonts(ctx);
    ctx.set_theme(egui::Theme::Light);
    let mut v = Visuals::light();
    v.panel_fill = PANEL;
    v.window_fill = Color32::WHITE;
    v.extreme_bg_color = Color32::WHITE;
    v.faint_bg_color = HOVER;
    v.override_text_color = Some(TEXT);
    v.selection.bg_fill = ACCENT_SOFT;
    v.selection.stroke = Stroke::new(1.0, ACCENT);
    v.hyperlink_color = ACCENT;
    v.window_corner_radius = CornerRadius::same(8);
    v.window_stroke = Stroke::new(1.0, BORDER);
    v.widgets.noninteractive.bg_stroke = Stroke::new(1.0, BORDER);
    for w in [&mut v.widgets.inactive, &mut v.widgets.hovered, &mut v.widgets.active, &mut v.widgets.open] {
        w.corner_radius = CornerRadius::same(6);
    }
    v.widgets.inactive.weak_bg_fill = HOVER;
    v.widgets.inactive.bg_fill = Color32::from_rgb(214, 220, 236);
    v.widgets.hovered.weak_bg_fill = Color32::from_rgb(232, 236, 246);
    v.widgets.hovered.bg_stroke = Stroke::new(1.0, ACCENT.gamma_multiply(0.6));
    v.widgets.active.weak_bg_fill = ACCENT_SOFT;
    v.widgets.active.bg_stroke = Stroke::new(1.0, ACCENT);
    v.slider_trailing_fill = true;
    ctx.set_visuals_of(egui::Theme::Light, v);
    ctx.all_styles_mut(|s| {
        s.spacing.item_spacing = vec2(8.0, 6.0);
        s.spacing.button_padding = vec2(12.0, 6.0);
        s.spacing.interact_size.y = 28.0;
        s.text_styles.insert(TextStyle::Body, FontId::new(15.0, FontFamily::Proportional));
        s.text_styles.insert(TextStyle::Button, FontId::new(14.5, FontFamily::Proportional));
        s.text_styles.insert(TextStyle::Monospace, FontId::new(14.5, FontFamily::Monospace));
        s.text_styles.insert(TextStyle::Small, FontId::new(12.5, FontFamily::Proportional));
        s.text_styles.insert(TextStyle::Heading, FontId::new(19.0, FontFamily::Proportional));
    });
}
