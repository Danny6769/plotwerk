use crate::cas_panel::{CasAction, CasPanel};
use crate::doc::{Document, ObjDef};
use crate::exam::{Exam, Preset, Session, fmt_duration};
use crate::geo::{Geo, Pt};
use crate::theme;
use crate::icons;
use crate::tools::{GEOMETRY_GROUPS, GRAPH_GROUPS, Tool, paint_icon};
use crate::view::View;
use eframe::egui::{
    self, Align, Align2, Color32, CornerRadius, FontId, Key, Layout, Margin, PointerButton, Pos2, Rect, RichText, Sense,
    Stroke, TextEdit, ViewportCommand, WindowLevel, pos2, vec2,
};
use std::panic::{AssertUnwindSafe, catch_unwind};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Graph,
    Geometry,
    Cas,
    Probability,
    Table,
    Space3D,
}

impl Mode {
    pub const ALL: [Mode; 6] = [Mode::Graph, Mode::Geometry, Mode::Space3D, Mode::Cas, Mode::Probability, Mode::Table];

    pub fn title(self) -> &'static str {
        match self {
            Mode::Graph => "Grafikrechner",
            Mode::Geometry => "Geometrie",
            Mode::Cas => "CAS",
            Mode::Probability => "Wahrscheinlichkeit",
            Mode::Table => "Tabelle & Statistik",
            Mode::Space3D => "3D-Rechner",
        }
    }

    pub fn from_key(key: &str) -> Mode {
        match key {
            "geometrie" | "geometry" => Mode::Geometry,
            "cas" => Mode::Cas,
            "wahrscheinlichkeit" => Mode::Probability,
            "tabelle" => Mode::Table,
            "3d" => Mode::Space3D,
            _ => Mode::Graph,
        }
    }

    pub fn available(self, cas_ok: bool) -> bool {
        self != Mode::Cas || cas_ok
    }

    pub fn paint_icon(self, p: &egui::Painter, r: Rect, color: Color32) {
        match self {
            Mode::Graph => icons::app_graph(p, r, color),
            Mode::Geometry => icons::app_geometry(p, r, color),
            Mode::Cas => icons::app_cas(p, r, color),
            Mode::Probability => icons::app_probability(p, r, color),
            Mode::Table => icons::app_table(p, r, color),
            Mode::Space3D => icons::app_3d(p, r, color),
        }
    }
}

enum Drag {
    Point(String),
    OnObject(String),
    Pan,
}

enum RowAction {
    Toggle(usize),
    Select(String),
    Edit(usize),
    Commit(usize, String),
    Cancel,
    Delete(String),
    SliderStart,
    Slider(String, f64),
    Props(String),
}

pub struct PlotwerkApp {
    mode: Mode,
    doc: Document,
    view: View,
    tool: Tool,
    pending: Vec<String>,
    selected: Option<String>,
    input: String,
    input_error: Option<String>,
    editing: Option<(usize, String)>,
    drag: Option<Drag>,
    cas: CasPanel,
    prob: crate::prob::ProbPanel,
    table: crate::table::TablePanel,
    space: crate::space3d::Space3D,
    exam: Exam,
    backup: Option<Vec<ObjDef>>,
    status: Option<String>,
    cursor: Option<Pt>,
    focus_input: bool,
    app_menu: bool,
    keyboard: bool,
    export_pending: bool,
    canvas_rect: Rect,
    props: Option<String>,
    rotate_dialog: Option<(String, String, String)>,
    algebra_tab: u8,
    vt_start: f64,
    vt_step: f64,
    file_menu: bool,
    path: Option<std::path::PathBuf>,
}

fn guarded<T>(f: impl FnOnce() -> Result<T, String>) -> Result<T, String> {
    match catch_unwind(AssertUnwindSafe(f)) {
        Ok(r) => r,
        Err(_) => Err("Interner Fehler bei der Berechnung".into()),
    }
}

impl PlotwerkApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        theme::apply(&cc.egui_ctx);
        let mut app = Self {
            mode: Mode::Graph,
            doc: Document::default(),
            view: View::default(),
            tool: Tool::Move,
            pending: Vec::new(),
            selected: None,
            input: String::new(),
            input_error: None,
            editing: None,
            drag: None,
            cas: CasPanel::default(),
            prob: crate::prob::ProbPanel::default(),
            table: crate::table::TablePanel::default(),
            space: crate::space3d::Space3D::default(),
            exam: Exam::Off,
            backup: None,
            status: None,
            cursor: None,
            focus_input: true,
            app_menu: false,
            keyboard: false,
            export_pending: false,
            canvas_rect: Rect::NOTHING,
            props: None,
            rotate_dialog: None,
            algebra_tab: 0,
            vt_start: -3.0,
            vt_step: 1.0,
            file_menu: false,
            path: None,
        };
        let args: Vec<String> = std::env::args().collect();
        if let Some(pos) = args.iter().position(|a| a == "--load") {
            if let Some(path) = args.get(pos + 1) {
                app.load_path(std::path::PathBuf::from(path), &cc.egui_ctx, false);
            }
        } else if let Some(path) = args.iter().skip(1).find(|a| !a.starts_with("--")) {
            app.load_path(std::path::PathBuf::from(path), &cc.egui_ctx, true);
        }
        app
    }

    fn mode_key(&self) -> &'static str {
        match self.mode {
            Mode::Graph => "graph",
            Mode::Geometry => "geometrie",
            Mode::Cas => "cas",
            Mode::Probability => "wahrscheinlichkeit",
            Mode::Table => "tabelle",
            Mode::Space3D => "3d",
        }
    }

    fn update_title(&self, ctx: &egui::Context) {
        let name = self
            .path
            .as_ref()
            .and_then(|p| p.file_name())
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| "Unbenannt".into());
        ctx.send_viewport_cmd(ViewportCommand::Title(format!("Plotwerk – {name}")));
    }

    fn new_file(&mut self, ctx: &egui::Context) {
        self.doc.clear_all();
        self.cas.clear();
        self.space.clear();
        self.view.reset();
        self.selected = None;
        self.pending.clear();
        self.path = None;
        self.update_title(ctx);
    }

    fn load_path(&mut self, path: std::path::PathBuf, ctx: &egui::Context, remember: bool) {
        let text = match std::fs::read_to_string(&path) {
            Ok(t) => t,
            Err(e) => {
                self.status = Some(format!("Datei konnte nicht gelesen werden: {e}"));
                return;
            }
        };
        let saved = match crate::file::from_text(&text) {
            Ok(s) => s,
            Err(e) => {
                self.status = Some(e);
                return;
            }
        };
        self.doc.clear_all();
        self.cas.clear();
        self.selected = None;
        self.pending.clear();
        for line in &saved.plain {
            if let Some(rest) = line.strip_prefix("cas:") {
                self.cas.push_input(rest.trim());
            } else if let Some(mode) = line.strip_prefix("mode:") {
                self.set_mode(Mode::from_key(mode.trim()));
            } else if let Some(rest) = line.strip_prefix("3d:") {
                let _ = self.space.add_input(rest.trim());
            } else if let Some(zoom) = line.strip_prefix("zoom:") {
                if let Ok(s) = zoom.trim().parse::<f64>() {
                    self.view.scale = s;
                }
            } else {
                let doc = &mut self.doc;
                let _ = guarded(|| doc.add_input(line));
            }
        }
        if saved.plain.is_empty() {
            self.set_mode(Mode::from_key(&saved.mode));
            let doc = &mut self.doc;
            let defs = saved.defs;
            let _ = guarded(move || {
                doc.set_defs(defs);
                Ok(())
            });
            for c in &saved.cas {
                self.cas.push_input(c);
            }
            self.space.clear();
            for s3 in &saved.space {
                let _ = self.space.add_input(s3);
            }
            if !saved.cells.is_empty() {
                self.table.clear();
                for c in &saved.cells {
                    self.table.set_cell_text(c);
                }
            }
            if let Some((cx, cy, scale, axes, grid)) = saved.view {
                self.view.cx = cx;
                self.view.cy = cy;
                self.view.scale = scale;
                self.view.show_axes = axes;
                self.view.show_grid = grid;
            }
        }
        self.doc.recompute();
        if remember {
            self.path = Some(path);
            self.update_title(ctx);
        }
        self.status = None;
    }

    fn open_dialog(&mut self, ctx: &egui::Context) {
        if let Some(path) = rfd::FileDialog::new().add_filter("Plotwerk", &["pwk"]).add_filter("Text", &["txt"]).pick_file() {
            self.load_path(path, ctx, true);
        }
    }

    fn save(&mut self, ctx: &egui::Context, ask: bool) {
        let path = match (&self.path, ask) {
            (Some(p), false) => Some(p.clone()),
            _ => rfd::FileDialog::new().add_filter("Plotwerk", &["pwk"]).set_file_name("zeichnung.pwk").save_file(),
        };
        let Some(mut path) = path else { return };
        if path.extension().is_none() {
            path.set_extension("pwk");
        }
        let cas: Vec<String> = self.cas.rows.iter().map(|r| r.input.clone()).collect();
        let text = crate::file::to_text(
            self.mode_key(),
            (self.view.cx, self.view.cy, self.view.scale, self.view.show_axes, self.view.show_grid),
            &self.doc.defs(),
            &cas,
            &self.table.cells_text(),
            &self.space.sources(),
        );
        match std::fs::write(&path, text) {
            Ok(()) => {
                self.status = None;
                self.path = Some(path);
                self.update_title(ctx);
            }
            Err(e) => self.status = Some(format!("Speichern fehlgeschlagen: {e}")),
        }
    }

    fn exam_running(&self) -> bool {
        matches!(self.exam, Exam::Running(_) | Exam::ConfirmEnd(_))
    }

    fn cas_allowed(&self) -> bool {
        match &self.exam {
            Exam::Running(s) | Exam::ConfirmEnd(s) => s.preset.cas_allowed(),
            _ => true,
        }
    }

    fn set_mode(&mut self, mode: Mode) {
        if mode == self.mode {
            return;
        }
        self.mode = mode;
        self.pending.clear();
        match mode {
            Mode::Geometry => {
                self.view.show_axes = false;
                self.view.show_grid = false;
            }
            _ => {
                self.view.show_axes = true;
                self.view.show_grid = true;
            }
        }
        let groups: &[&[Tool]] = if mode == Mode::Geometry { GEOMETRY_GROUPS } else { GRAPH_GROUPS };
        if !groups.iter().any(|g| g.contains(&self.tool)) {
            self.tool = Tool::Move;
        }
    }

    fn start_exam(&mut self, ctx: &egui::Context, preset: Preset) {
        self.backup = Some(self.doc.defs());
        self.doc.clear_all();
        self.cas.clear();
        self.space.clear();
        self.selected = None;
        self.pending.clear();
        self.editing = None;
        self.input.clear();
        self.view.reset();
        if !preset.cas_allowed() && self.mode == Mode::Cas {
            self.set_mode(Mode::Graph);
        }
        ctx.send_viewport_cmd(ViewportCommand::Fullscreen(true));
        ctx.send_viewport_cmd(ViewportCommand::WindowLevel(WindowLevel::AlwaysOnTop));
        self.exam = Exam::Running(Session::new(preset));
    }

    fn close_report(&mut self) {
        self.exam = Exam::Off;
        self.doc.clear_all();
        self.cas.clear();
        if let Some(defs) = self.backup.take() {
            self.doc.set_defs(defs);
        }
    }

    fn submit_input(&mut self) {
        let text = self.input.trim().to_string();
        if text.is_empty() {
            return;
        }
        let doc = &mut self.doc;
        match guarded(|| doc.add_input(&text)) {
            Ok(name) => {
                self.input.clear();
                self.input_error = None;
                self.selected = Some(name);
            }
            Err(e) => self.input_error = Some(e),
        }
        self.focus_input = true;
    }

    fn run_command(&mut self, source: String, point_like: bool) {
        let doc = &mut self.doc;
        match guarded(|| doc.add_command(&source, point_like)) {
            Ok(name) => {
                self.selected = Some(name);
                self.status = None;
            }
            Err(e) => self.status = Some(e),
        }
    }

    fn pick_point(&mut self, rect: Rect, pos: Pos2) -> String {
        if let Some(i) = self.view.hit(rect, &self.doc, pos, &|g| matches!(g, Geo::Point(_))) {
            return self.doc.objects[i].def.name.clone();
        }
        let w = self.view.snap(self.view.to_world(rect, pos));
        self.doc.add_point(w)
    }

    fn tool_click(&mut self, rect: Rect, pos: Pos2) {
        let curve = |g: &Geo| !matches!(g, Geo::Number(_) | Geo::Point(_) | Geo::Points(_));
        let function = |g: &Geo| matches!(g, Geo::Function { .. });
        let line = |g: &Geo| matches!(g, Geo::Line { .. } | Geo::Segment(..));
        match self.tool {
            Tool::Move => {
                self.selected = self.view.hit(rect, &self.doc, pos, &|_| true).map(|i| self.doc.objects[i].def.name.clone());
            }
            Tool::Point => {
                if self.view.hit(rect, &self.doc, pos, &|g| matches!(g, Geo::Point(_))).is_some() {
                    return;
                }
                if let Some(i) = self.view.hit(rect, &self.doc, pos, &|g| g.is_curve() && !matches!(g, Geo::Implicit { .. })) {
                    let parent = self.doc.objects[i].def.name.clone();
                    let w = self.view.to_world(rect, pos);
                    if let Some(t) = self.doc.objects[i].geo.as_ref().and_then(|g| crate::geo::project_param(g, w)) {
                        self.run_command(format!("Punkt({parent}, {})", crate::doc::fmt_param(t)), true);
                        return;
                    }
                }
                let w = self.view.snap(self.view.to_world(rect, pos));
                let name = self.doc.add_point(w);
                self.selected = Some(name);
            }
            Tool::Segment | Tool::Line | Tool::Circle | Tool::Midpoint | Tool::Ray | Tool::Vector => {
                if self.pending.is_empty() && self.tool == Tool::Midpoint {
                    if let Some(i) = self.view.hit(rect, &self.doc, pos, &|g| matches!(g, Geo::Segment(..) | Geo::Circle { .. })) {
                        let name = self.doc.objects[i].def.name.clone();
                        self.run_command(format!("Mittelpunkt({name})"), true);
                        return;
                    }
                }
                let p = self.pick_point(rect, pos);
                if self.pending.last() == Some(&p) {
                    return;
                }
                self.pending.push(p);
                if self.pending.len() == 2 {
                    let (a, b) = (self.pending[0].clone(), self.pending[1].clone());
                    self.pending.clear();
                    let (cmd, point_like) = match self.tool {
                        Tool::Segment => ("Strecke", false),
                        Tool::Line => ("Gerade", false),
                        Tool::Circle => ("Kreis", false),
                        Tool::Ray => ("Halbgerade", false),
                        Tool::Vector => ("Vektor", false),
                        _ => ("Mittelpunkt", true),
                    };
                    self.run_command(format!("{cmd}({a}, {b})"), point_like);
                }
            }
            Tool::Arc => {
                let p = self.pick_point(rect, pos);
                if self.pending.last() == Some(&p) {
                    return;
                }
                self.pending.push(p);
                if self.pending.len() == 3 {
                    let cmd = format!("Kreisbogen({}, {}, {})", self.pending[0], self.pending[1], self.pending[2]);
                    self.pending.clear();
                    self.run_command(cmd, false);
                }
            }
            Tool::Angle => {
                let line_hit = self.view.hit(rect, &self.doc, pos, &|g| g.as_line().is_some());
                let point_hit = self.view.hit(rect, &self.doc, pos, &|g| matches!(g, Geo::Point(_)));
                let pending_is_line = self
                    .pending
                    .first()
                    .and_then(|n| self.doc.find(n))
                    .and_then(|i| self.doc.objects[i].geo.as_ref())
                    .map(|g| g.as_line().is_some())
                    .unwrap_or(false);
                if point_hit.is_none() && (self.pending.is_empty() || pending_is_line) {
                    if let Some(i) = line_hit {
                        let name = self.doc.objects[i].def.name.clone();
                        if !self.pending.contains(&name) {
                            self.pending.push(name);
                        }
                        if self.pending.len() == 2 {
                            let cmd = format!("Winkel({}, {})", self.pending[0], self.pending[1]);
                            self.pending.clear();
                            self.run_command(cmd, false);
                        }
                        return;
                    }
                }
                if pending_is_line {
                    self.pending.clear();
                }
                let p = self.pick_point(rect, pos);
                if self.pending.last() == Some(&p) {
                    return;
                }
                self.pending.push(p);
                if self.pending.len() == 3 {
                    let cmd = format!("Winkel({}, {}, {})", self.pending[0], self.pending[1], self.pending[2]);
                    self.pending.clear();
                    self.run_command(cmd, false);
                }
            }
            Tool::Reflect | Tool::Rotate | Tool::Translate => {
                if self.pending.is_empty() {
                    if let Some(i) = self.view.hit(rect, &self.doc, pos, &|g| !matches!(g, Geo::Number(_) | Geo::Area { .. })) {
                        self.pending.push(self.doc.objects[i].def.name.clone());
                    }
                    return;
                }
                let obj = self.pending[0].clone();
                match self.tool {
                    Tool::Reflect => {
                        if let Some(i) = self.view.hit(rect, &self.doc, pos, &|g| matches!(g, Geo::Point(_)) || g.as_line().is_some()) {
                            let m = self.doc.objects[i].def.name.clone();
                            if m != obj {
                                self.pending.clear();
                                self.run_command(format!("Spiegle({obj}, {m})"), false);
                            }
                        }
                    }
                    Tool::Rotate => {
                        let center = self.pick_point(rect, pos);
                        self.pending.clear();
                        self.rotate_dialog = Some((obj, center, "90".into()));
                    }
                    _ => {
                        if let Some(i) = self.view.hit(rect, &self.doc, pos, &|g| matches!(g, Geo::Vector(..))) {
                            let v = self.doc.objects[i].def.name.clone();
                            self.pending.clear();
                            self.run_command(format!("Verschiebe({obj}, {v})"), false);
                        }
                    }
                }
            }
            Tool::Polygon => {
                let p = self.pick_point(rect, pos);
                if self.pending.len() >= 3 && self.pending[0] == p {
                    let list = self.pending.join(", ");
                    self.pending.clear();
                    self.run_command(format!("Vieleck({list})"), false);
                } else if !self.pending.contains(&p) {
                    self.pending.push(p);
                }
            }
            Tool::Intersect => {
                if let Some(i) = self.view.hit(rect, &self.doc, pos, &curve) {
                    let name = self.doc.objects[i].def.name.clone();
                    if !self.pending.contains(&name) {
                        self.pending.push(name);
                    }
                    if self.pending.len() == 2 {
                        let cmd = format!("Schneide({}, {})", self.pending[0], self.pending[1]);
                        self.pending.clear();
                        self.run_command(cmd, true);
                    }
                }
            }
            Tool::Perpendicular | Tool::Parallel => {
                if let Some(i) = self.view.hit(rect, &self.doc, pos, &|g| matches!(g, Geo::Point(_)) || line(g)) {
                    let name = self.doc.objects[i].def.name.clone();
                    if !self.pending.contains(&name) {
                        self.pending.push(name);
                    }
                }
                if self.pending.len() == 2 {
                    let is_point = |n: &String| self.doc.find(n).and_then(|i| self.doc.objects[i].geo.as_ref()).map(|g| matches!(g, Geo::Point(_))).unwrap_or(false);
                    let (pt, ln) = if is_point(&self.pending[0]) { (self.pending[0].clone(), self.pending[1].clone()) } else { (self.pending[1].clone(), self.pending[0].clone()) };
                    self.pending.clear();
                    let cmd = if self.tool == Tool::Perpendicular { "Senkrechte" } else { "Parallele" };
                    self.run_command(format!("{cmd}({pt}, {ln})"), false);
                }
            }
            Tool::Tangent => {
                if let Some(i) = self.view.hit(rect, &self.doc, pos, &function) {
                    let name = crate::expr_util::display_prime(&self.doc.objects[i].def.name);
                    let x0 = self.view.to_world(rect, pos)[0];
                    let x0 = crate::doc::fmt_coord(x0);
                    self.run_command(format!("Tangente({x0}, {name})"), false);
                }
            }
            Tool::Roots | Tool::Extrema => {
                if let Some(i) = self.view.hit(rect, &self.doc, pos, &function) {
                    let name = self.doc.objects[i].def.name.clone();
                    let cmd = if self.tool == Tool::Roots { "Nullstelle" } else { "Extremum" };
                    self.run_command(format!("{cmd}({name})"), true);
                }
            }
        }
    }

    fn top_bar(&mut self, ctx: &egui::Context) {
        let (fill, on_color) = match &self.exam {
            Exam::Running(s) | Exam::ConfirmEnd(s) => (if s.violated { theme::EXAM_ALERT } else { theme::EXAM_OK }, true),
            _ => (Color32::WHITE, false),
        };
        let text_color = if on_color { Color32::WHITE } else { theme::TEXT };
        let icon_color = if on_color { Color32::WHITE } else { theme::ICON };
        let mut title_rect = Rect::NOTHING;
        let mut menu_rect = Rect::NOTHING;
        egui::TopBottomPanel::top("top")
            .exact_height(56.0)
            .frame(egui::Frame::new().fill(fill).inner_margin(Margin::symmetric(12, 0)).stroke(Stroke::new(1.0, theme::BORDER)))
            .show(ctx, |ui| {
                ui.horizontal_centered(|ui| {
                    let exam_on = on_color;
                    let (r, resp) = ui.allocate_exact_size(vec2(36.0, 36.0), if exam_on { Sense::hover() } else { Sense::click() });
                    if (resp.hovered() || self.file_menu) && !exam_on {
                        ui.painter().rect_filled(r, CornerRadius::same(18), theme::HOVER);
                    }
                    icons::menu(ui.painter(), r, if exam_on { Color32::from_white_alpha(90) } else { icon_color });
                    if !exam_on && resp.on_hover_text("Menü").clicked() {
                        self.file_menu = !self.file_menu;
                    }
                    menu_rect = r;
                    ui.add_space(4.0);
                    let (r, _) = ui.allocate_exact_size(vec2(28.0, 28.0), Sense::hover());
                    paint_logo(ui.painter(), r, if on_color { Color32::WHITE } else { theme::ACCENT });
                    ui.add_space(4.0);
                    let galley = ui.painter().layout_no_wrap(self.mode.title().to_string(), FontId::proportional(19.0), text_color);
                    let (r, resp) = ui.allocate_exact_size(galley.size() + vec2(34.0, 12.0), Sense::click());
                    if resp.hovered() || self.app_menu {
                        ui.painter().rect_filled(r, CornerRadius::same(6), if on_color { Color32::from_white_alpha(40) } else { theme::HOVER });
                    }
                    ui.painter().galley(r.left_center() + vec2(8.0, -galley.size().y / 2.0), galley, text_color);
                    icons::chevron_down(ui.painter(), r.right_center() - vec2(14.0, 0.0), icon_color);
                    if resp.on_hover_text("App wechseln").clicked() {
                        self.app_menu = !self.app_menu;
                    }
                    title_rect = r;
                    ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                        match &self.exam {
                            Exam::Running(s) | Exam::ConfirmEnd(s) => {
                                let end = ui.add(
                                    egui::Button::new(RichText::new("Prüfung beenden").color(fill).strong())
                                        .fill(Color32::WHITE)
                                        .min_size(vec2(140.0, 32.0)),
                                );
                                let elapsed = fmt_duration(s.elapsed());
                                let status = if s.violated { "Auffälligkeit protokolliert" } else { "Keine Auffälligkeiten" };
                                ui.add_space(10.0);
                                ui.label(RichText::new(status).color(Color32::WHITE));
                                ui.add_space(10.0);
                                ui.label(RichText::new(elapsed).monospace().size(18.0).strong().color(Color32::WHITE));
                                ui.label(RichText::new("PRÜFUNGSMODUS").strong().color(Color32::WHITE));
                                if end.clicked() {
                                    if let Exam::Running(s) = std::mem::replace(&mut self.exam, Exam::Off) {
                                        self.exam = Exam::ConfirmEnd(s);
                                    }
                                }
                            }
                            _ => {
                                let exam_btn = egui::Button::new(RichText::new("Prüfungsmodus").color(theme::ACCENT).strong())
                                    .fill(theme::ACCENT_SOFT)
                                    .stroke(Stroke::NONE)
                                    .min_size(vec2(130.0, 34.0));
                                if ui.add(exam_btn).clicked() {
                                    self.exam = Exam::Setup(Preset::Full);
                                }
                                ui.add_space(8.0);
                                let (r, resp) = ui.allocate_exact_size(vec2(36.0, 36.0), Sense::click());
                                let can = self.doc.can_redo();
                                if resp.hovered() && can {
                                    ui.painter().rect_filled(r, CornerRadius::same(18), theme::HOVER);
                                }
                                icons::redo(ui.painter(), r, if can { theme::ICON } else { theme::BORDER });
                                if resp.on_hover_text("Wiederholen (Strg+Y)").clicked() && can {
                                    self.doc.redo();
                                }
                                let (r, resp) = ui.allocate_exact_size(vec2(36.0, 36.0), Sense::click());
                                let can = self.doc.can_undo();
                                if resp.hovered() && can {
                                    ui.painter().rect_filled(r, CornerRadius::same(18), theme::HOVER);
                                }
                                icons::undo(ui.painter(), r, if can { theme::ICON } else { theme::BORDER });
                                if resp.on_hover_text("Rückgängig (Strg+Z)").clicked() && can {
                                    self.doc.undo();
                                }
                            }
                        }
                    });
                });
            });
        if self.file_menu {
            let mut action: Option<u8> = None;
            let entries = [
                ("Neu", "Strg+N"),
                ("Öffnen …", "Strg+O"),
                ("Speichern", "Strg+S"),
                ("Speichern unter …", "Strg+Umschalt+S"),
                ("Als Bild exportieren (PNG) …", ""),
                ("Bildschirmtastatur", "Strg+K"),
            ];
            let area = egui::Area::new(egui::Id::new("file_menu"))
                .order(egui::Order::Foreground)
                .fixed_pos(menu_rect.left_bottom() + vec2(0.0, 6.0))
                .show(ctx, |ui| {
                    egui::Frame::popup(ui.style()).fill(Color32::WHITE).corner_radius(CornerRadius::same(8)).inner_margin(Margin::same(6)).show(ui, |ui| {
                        ui.set_width(250.0);
                        for (i, (label, key)) in entries.into_iter().enumerate() {
                            let (r, resp) = ui.allocate_exact_size(vec2(250.0, 36.0), Sense::click());
                            if resp.hovered() {
                                ui.painter().rect_filled(r, CornerRadius::same(6), theme::HOVER);
                            }
                            ui.painter().text(r.left_center() + vec2(12.0, 0.0), Align2::LEFT_CENTER, label, FontId::proportional(15.0), theme::TEXT);
                            ui.painter().text(r.right_center() - vec2(12.0, 0.0), Align2::RIGHT_CENTER, key, FontId::proportional(12.5), theme::MUTED);
                            if resp.clicked() {
                                action = Some(i as u8);
                            }
                        }
                    });
                });
            let clicked_outside = ctx.input(|i| i.pointer.any_pressed())
                && ctx.pointer_interact_pos().map(|p| !area.response.rect.contains(p) && !menu_rect.contains(p)).unwrap_or(false);
            if clicked_outside || action.is_some() {
                self.file_menu = false;
            }
            match action {
                Some(0) => self.new_file(ctx),
                Some(1) => self.open_dialog(ctx),
                Some(2) => self.save(ctx, false),
                Some(3) => self.save(ctx, true),
                Some(4) => {
                    if self.mode == Mode::Graph || self.mode == Mode::Geometry || self.mode == Mode::Cas {
                        self.export_pending = true;
                        ctx.send_viewport_cmd(ViewportCommand::Screenshot(egui::UserData::default()));
                    } else {
                        self.status = Some("Export ist im Grafikrechner, in der Geometrie und im CAS möglich".into());
                    }
                }
                Some(5) => self.keyboard = !self.keyboard,
                _ => {}
            }
        }
        if self.app_menu {
            let cas_ok = self.cas_allowed();
            let area = egui::Area::new(egui::Id::new("app_menu"))
                .order(egui::Order::Foreground)
                .fixed_pos(title_rect.left_bottom() + vec2(0.0, 6.0))
                .show(ctx, |ui| {
                    egui::Frame::popup(ui.style()).fill(Color32::WHITE).corner_radius(CornerRadius::same(8)).inner_margin(Margin::same(6)).show(ui, |ui| {
                        ui.set_width(250.0);
                        ui.label(RichText::new("Apps").small().color(theme::MUTED));
                        for m in Mode::ALL {
                            let enabled = m.available(cas_ok);
                            let (r, resp) = ui.allocate_exact_size(vec2(250.0, 44.0), if enabled { Sense::click() } else { Sense::hover() });
                            let active = self.mode == m;
                            if active {
                                ui.painter().rect_filled(r, CornerRadius::same(6), theme::ACCENT_SOFT);
                            } else if resp.hovered() && enabled {
                                ui.painter().rect_filled(r, CornerRadius::same(6), theme::HOVER);
                            }
                            let color = if !enabled { theme::BORDER } else if active { theme::ACCENT } else { theme::ICON };
                            let icon_rect = Rect::from_center_size(r.left_center() + vec2(22.0, 0.0), vec2(26.0, 26.0));
                            m.paint_icon(ui.painter(), icon_rect, color);
                            ui.painter().text(
                                r.left_center() + vec2(46.0, 0.0),
                                Align2::LEFT_CENTER,
                                m.title(),
                                FontId::proportional(15.5),
                                if enabled { theme::TEXT } else { theme::MUTED },
                            );
                            if resp.clicked() && enabled {
                                self.set_mode(m);
                                self.app_menu = false;
                            }
                        }
                    });
                });
            let clicked_outside = ctx.input(|i| i.pointer.any_pressed())
                && ctx.pointer_interact_pos().map(|p| !area.response.rect.contains(p) && !title_rect.contains(p)).unwrap_or(false);
            if clicked_outside {
                self.app_menu = false;
            }
        }
    }

    fn algebra_panel(&mut self, ui: &mut egui::Ui) {
        ui.horizontal(|ui| {
            ui.spacing_mut().item_spacing.x = 0.0;
            let w = ui.available_width() / 2.0;
            for (i, label) in ["Algebra", "Wertetabelle"].into_iter().enumerate() {
                let (r, resp) = ui.allocate_exact_size(vec2(w, 40.0), Sense::click());
                let active = self.algebra_tab == i as u8;
                ui.painter().text(r.center(), Align2::CENTER_CENTER, label, FontId::proportional(14.5), if active { theme::ACCENT } else { theme::MUTED });
                ui.painter().rect_filled(Rect::from_min_max(pos2(r.left(), r.bottom() - if active { 2.5 } else { 1.0 }), r.right_bottom()), 0.0, if active { theme::ACCENT } else { theme::BORDER });
                if resp.clicked() {
                    self.algebra_tab = i as u8;
                }
            }
        });
        if self.algebra_tab == 1 {
            self.value_table(ui);
            return;
        }
        let mut actions: Vec<RowAction> = Vec::new();
        let rows: Vec<_> = self
            .doc
            .objects
            .iter()
            .map(|o| {
                let (main, sub) = self.doc.display(o);
                let slider = if o.free_number { o.def.slider.clone().zip(match o.geo { Some(Geo::Number(v)) => Some(v), _ => None }) } else { None };
                (
                    o.def.name.clone(),
                    main,
                    sub,
                    o.error.clone(),
                    o.def.visible,
                    o.def.color,
                    slider,
                    o.geo.as_ref().map(|g| matches!(g, Geo::Number(_))).unwrap_or(false),
                )
            })
            .collect();
        let row_margin = Margin { left: 14, right: 8, top: 10, bottom: 10 };
        egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
            ui.spacing_mut().item_spacing.y = 0.0;
            for (idx, (name, main, sub, error, visible, color, slider, is_number)) in rows.iter().enumerate() {
                let selected = self.selected.as_deref() == Some(name.as_str());
                let inner = egui::Frame::new()
                    .fill(if selected { theme::ACCENT_SOFT } else { Color32::WHITE })
                    .inner_margin(row_margin)
                    .show(ui, |ui| {
                        ui.set_width(ui.available_width());
                        ui.spacing_mut().item_spacing.y = 3.0;
                        ui.horizontal(|ui| {
                            let (r, vis) = ui.allocate_exact_size(vec2(22.0, 22.0), Sense::click());
                            let c = r.center();
                            if *is_number && slider.is_none() {
                                ui.painter().circle_stroke(c, 7.5, Stroke::new(1.4, theme::BORDER));
                            } else if *visible {
                                ui.painter().circle_filled(c, 8.5, *color);
                                ui.painter().circle_stroke(c, 8.5, Stroke::new(1.0, color.gamma_multiply(0.7)));
                            } else {
                                ui.painter().circle_filled(c, 8.0, Color32::WHITE);
                                ui.painter().circle_stroke(c, 7.5, Stroke::new(1.6, *color));
                            }
                            if vis.on_hover_text("Ein-/Ausblenden").clicked() {
                                actions.push(RowAction::Toggle(idx));
                            }
                            ui.add_space(4.0);
                            ui.vertical(|ui| {
                                ui.set_width(ui.available_width() - 28.0);
                                let editing_this = matches!(&self.editing, Some((i, _)) if *i == idx);
                                if editing_this {
                                    if let Some((_, buf)) = &mut self.editing {
                                        let te = ui.add(TextEdit::singleline(buf).font(FontId::proportional(16.0)).desired_width(f32::INFINITY));
                                        te.request_focus();
                                        if ui.input(|i| i.key_pressed(Key::Escape)) {
                                            actions.push(RowAction::Cancel);
                                        } else if te.lost_focus() {
                                            actions.push(RowAction::Commit(idx, buf.clone()));
                                        }
                                    }
                                } else {
                                    if let Some(sub) = sub {
                                        ui.label(RichText::new(crate::pretty::pretty(sub)).size(13.0).color(theme::MUTED));
                                    }
                                    let text_color = if error.is_some() { theme::ERROR } else { theme::TEXT };
                                    let label = ui.add(
                                        egui::Label::new(RichText::new(crate::pretty::pretty(main)).size(16.0).color(text_color))
                                            .sense(Sense::click())
                                            .truncate(),
                                    );
                                    if label.double_clicked() {
                                        actions.push(RowAction::Edit(idx));
                                    } else if label.clicked() {
                                        actions.push(RowAction::Select(name.clone()));
                                    }
                                    if label.secondary_clicked() {
                                        actions.push(RowAction::Props(name.clone()));
                                    }
                                    label.on_hover_text("Doppelklick zum Bearbeiten, Rechtsklick für Eigenschaften");
                                }
                                if let Some(e) = error {
                                    ui.label(RichText::new(e).small().color(theme::ERROR));
                                }
                                if let Some((range, value)) = slider {
                                    let mut v = *value;
                                    ui.spacing_mut().slider_width = ui.available_width();
                                    let s = ui.add(egui::Slider::new(&mut v, range.min..=range.max).step_by(range.step).show_value(false));
                                    if s.drag_started() {
                                        actions.push(RowAction::SliderStart);
                                    }
                                    if s.changed() {
                                        actions.push(RowAction::Slider(name.clone(), v));
                                    }
                                }
                            });
                            let del = egui::Button::new(RichText::new("×").size(17.0).color(theme::MUTED)).frame(false);
                            if ui.add(del).on_hover_text("Löschen").clicked() {
                                actions.push(RowAction::Delete(name.clone()));
                            }
                        });
                    });
                let r = inner.response.rect;
                if selected {
                    ui.painter().rect_filled(Rect::from_min_size(r.min, vec2(3.0, r.height())), 0.0, theme::ACCENT);
                }
                ui.painter().line_segment([r.left_bottom(), r.right_bottom()], Stroke::new(1.0, theme::BORDER));
            }
            let inner = egui::Frame::new().fill(Color32::WHITE).inner_margin(row_margin).show(ui, |ui| {
                ui.set_width(ui.available_width());
                ui.horizontal(|ui| {
                    let (r, _) = ui.allocate_exact_size(vec2(22.0, 22.0), Sense::hover());
                    icons::plus(ui.painter(), r, theme::MUTED);
                    ui.add_space(4.0);
                    ui.add(
                        TextEdit::singleline(&mut self.input)
                            .hint_text("Eingabe …")
                            .font(FontId::proportional(16.0))
                            .frame(false)
                            .desired_width(f32::INFINITY),
                    )
                })
                .inner
            });
            let r = inner.response.rect;
            ui.painter().line_segment([r.left_bottom(), r.right_bottom()], Stroke::new(1.0, theme::BORDER));
            let resp = inner.inner;
            if self.focus_input {
                resp.request_focus();
                self.focus_input = false;
            }
            if resp.changed() {
                self.input_error = None;
            }
            if resp.lost_focus() && ui.input(|i| i.key_pressed(Key::Enter)) {
                self.submit_input();
            }
            if let Some(e) = &self.input_error {
                ui.add_space(6.0);
                ui.horizontal(|ui| {
                    ui.add_space(14.0);
                    ui.label(RichText::new(e).color(theme::ERROR).small());
                });
            }
            if rows.is_empty() {
                ui.add_space(18.0);
                ui.horizontal(|ui| {
                    ui.add_space(14.0);
                    ui.vertical(|ui| {
                        ui.spacing_mut().item_spacing.y = 4.0;
                        ui.label(RichText::new("Beispiele").small().color(theme::MUTED));
                        for ex in ["f(x) = sin(x) + x/2", "A = (2, 1)", "x^2 + y^2 = 9", "Nullstelle(f)", "Integral(f, 0, 3)"] {
                            ui.label(RichText::new(ex).size(13.5).color(theme::MUTED));
                        }
                    });
                });
            }
        });
        for a in actions {
            match a {
                RowAction::Toggle(i) => {
                    let v = &mut self.doc.objects[i].def.visible;
                    *v = !*v;
                }
                RowAction::Select(n) => self.selected = Some(n),
                RowAction::Edit(i) => {
                    let text = self.doc.definition_text(&self.doc.objects[i]);
                    self.editing = Some((i, text));
                }
                RowAction::Commit(i, text) => {
                    self.editing = None;
                    let doc = &mut self.doc;
                    if let Err(e) = guarded(|| doc.redefine(i, &text)) {
                        self.status = Some(e);
                    }
                }
                RowAction::Cancel => self.editing = None,
                RowAction::Delete(n) => {
                    self.doc.remove(&n);
                    if self.selected.as_deref() == Some(n.as_str()) {
                        self.selected = None;
                    }
                }
                RowAction::SliderStart => self.doc.snapshot(),
                RowAction::Slider(n, v) => self.doc.set_number(&n, v),
                RowAction::Props(n) => {
                    self.selected = Some(n.clone());
                    self.props = Some(n);
                }
            }
        }
    }

    fn toolbar(&mut self, ui: &mut egui::Ui) {
        let groups: &[&[Tool]] = if self.mode == Mode::Geometry { GEOMETRY_GROUPS } else { GRAPH_GROUPS };
        let frame = egui::Frame::new().fill(Color32::WHITE).inner_margin(Margin::symmetric(8, 6)).show(ui, |ui| {
            ui.set_width(ui.available_width());
            ui.horizontal(|ui| {
                ui.spacing_mut().item_spacing.x = 2.0;
                for (gi, group) in groups.iter().enumerate() {
                    if gi > 0 {
                        ui.add_space(6.0);
                        let (r, _) = ui.allocate_exact_size(vec2(1.0, 34.0), Sense::hover());
                        ui.painter().rect_filled(r, 0.0, theme::BORDER);
                        ui.add_space(6.0);
                    }
                    for &t in group.iter() {
                        let active = self.tool == t;
                        let (rect, resp) = ui.allocate_exact_size(vec2(46.0, 46.0), Sense::click());
                        let fill = if active { theme::ACCENT_SOFT } else if resp.hovered() { theme::HOVER } else { Color32::TRANSPARENT };
                        ui.painter().rect_filled(rect, CornerRadius::same(8), fill);
                        let icon_rect = Rect::from_center_size(rect.center(), vec2(32.0, 32.0));
                        paint_icon(ui.painter(), icon_rect, t, if active { theme::ACCENT } else { theme::ICON });
                        if resp.on_hover_text(t.label()).clicked() {
                            self.tool = t;
                            self.pending.clear();
                        }
                    }
                }
                ui.add_space(18.0);
                ui.vertical(|ui| {
                    ui.spacing_mut().item_spacing.y = 1.0;
                    ui.add_space(4.0);
                    ui.label(RichText::new(self.tool.label()).size(15.0).strong());
                    let mut hint = self.tool.hint().to_string();
                    if !self.pending.is_empty() {
                        hint.push_str(&format!("  (gewählt: {})", self.pending.join(", ")));
                    }
                    ui.label(RichText::new(hint).size(12.5).color(theme::MUTED));
                });
            });
        });
        let r = frame.response.rect;
        ui.painter().line_segment([r.left_bottom(), r.right_bottom()], Stroke::new(1.0, theme::BORDER));
    }

    fn canvas(&mut self, ui: &mut egui::Ui) {
        let (resp, painter) = ui.allocate_painter(ui.available_size(), Sense::click_and_drag());
        let rect = resp.rect;
        self.canvas_rect = rect;
        if resp.hovered() {
            let (scroll, zoom) = ui.input(|i| (i.smooth_scroll_delta.y, i.zoom_delta()));
            if let Some(pos) = resp.hover_pos() {
                if scroll != 0.0 {
                    self.view.zoom_at(rect, pos, (scroll as f64 * 0.0022).exp());
                }
                if zoom != 1.0 {
                    self.view.zoom_at(rect, pos, zoom as f64);
                }
            }
        }
        let hover = resp.hover_pos();
        self.cursor = hover.map(|p| self.view.to_world(rect, p));
        if resp.drag_started_by(PointerButton::Primary) {
            let start = resp.interact_pointer_pos().unwrap_or(rect.center());
            let hit = if self.tool == Tool::Move {
                self.view.hit(rect, &self.doc, start, &|g| matches!(g, Geo::Point(_)))
            } else {
                None
            };
            match hit {
                Some(i) if self.doc.objects[i].on_object.is_some() => {
                    self.doc.snapshot();
                    let name = self.doc.objects[i].def.name.clone();
                    self.selected = Some(name.clone());
                    self.drag = Some(Drag::OnObject(name));
                }
                Some(i) if self.doc.objects[i].free_point => {
                    self.doc.snapshot();
                    let name = self.doc.objects[i].def.name.clone();
                    self.selected = Some(name.clone());
                    self.drag = Some(Drag::Point(name));
                }
                _ => self.drag = Some(Drag::Pan),
            }
        }
        if resp.drag_started_by(PointerButton::Secondary) || resp.drag_started_by(PointerButton::Middle) {
            self.drag = Some(Drag::Pan);
        }
        if resp.dragged() {
            match &self.drag {
                Some(Drag::Point(name)) => {
                    if let Some(p) = resp.interact_pointer_pos() {
                        let w = self.view.snap(self.view.to_world(rect, p));
                        let name = name.clone();
                        self.doc.set_point(&name, w);
                    }
                }
                Some(Drag::OnObject(name)) => {
                    if let Some(p) = resp.interact_pointer_pos() {
                        let w = self.view.to_world(rect, p);
                        let name = name.clone();
                        let parent = self.doc.find(&name).and_then(|i| self.doc.objects[i].on_object.clone());
                        let t = parent
                            .and_then(|pn| self.doc.find(&pn))
                            .and_then(|i| self.doc.objects[i].geo.as_ref())
                            .and_then(|g| crate::geo::project_param(g, w));
                        if let Some(t) = t {
                            self.doc.set_point_param(&name, t);
                        }
                    }
                }
                Some(Drag::Pan) => self.view.pan_pixels(resp.drag_delta()),
                None => {}
            }
        }
        if resp.drag_stopped() {
            self.drag = None;
        }
        if resp.clicked() {
            if let Some(pos) = resp.interact_pointer_pos() {
                self.tool_click(rect, pos);
            }
        }
        if resp.secondary_clicked() {
            if let Some(pos) = resp.interact_pointer_pos() {
                if let Some(i) = self.view.hit(rect, &self.doc, pos, &|_| true) {
                    let name = self.doc.objects[i].def.name.clone();
                    self.selected = Some(name.clone());
                    self.props = Some(name);
                }
            }
        }
        let hovered = if self.drag.is_none() {
            hover.and_then(|p| self.view.hit(rect, &self.doc, p, &|_| true))
        } else {
            None
        };
        let painter = painter.with_clip_rect(rect);
        self.view.draw_background(&painter, rect);
        let order = |g: &Geo| match g {
            Geo::Area { .. } | Geo::Polygon(_) => 0,
            Geo::Point(_) | Geo::Points(_) => 2,
            _ => 1,
        };
        for layer in 0..3 {
            for (i, obj) in self.doc.objects.iter().enumerate() {
                if obj.geo.as_ref().map(order) != Some(layer) {
                    continue;
                }
                let hl = hovered == Some(i)
                    || self.selected.as_deref() == Some(obj.def.name.as_str())
                    || self.pending.contains(&obj.def.name);
                self.view.draw_object(&painter, rect, obj, hl);
            }
        }
        if let (Some(p), Some(first)) = (hover, self.pending.first()) {
            if matches!(self.tool, Tool::Segment | Tool::Line | Tool::Circle | Tool::Polygon | Tool::Midpoint | Tool::Ray | Tool::Vector | Tool::Arc) {
                let anchor = self.doc.find(first).and_then(|i| match self.doc.objects[i].geo {
                    Some(Geo::Point(pt)) => Some(pt),
                    _ => None,
                });
                let last = self.pending.last().and_then(|n| self.doc.find(n)).and_then(|i| match self.doc.objects[i].geo {
                    Some(Geo::Point(pt)) => Some(pt),
                    _ => None,
                });
                let preview = Stroke::new(1.5, theme::ACCENT.gamma_multiply(0.6));
                if let (Some(a), Some(l)) = (anchor, last) {
                    let sa = self.view.to_screen(rect, a);
                    let sl = self.view.to_screen(rect, l);
                    if self.tool == Tool::Circle {
                        painter.circle_stroke(sa, sa.distance(p), preview);
                    } else {
                        painter.line_segment([sl, p], preview);
                        if self.tool == Tool::Polygon && self.pending.len() >= 2 {
                            painter.line_segment([p, sa], Stroke::new(1.0, theme::ACCENT.gamma_multiply(0.3)));
                        }
                    }
                }
            }
        }
        if let (Some(i), Some(p)) = (hovered, hover) {
            let obj = &self.doc.objects[i];
            let (main, _) = self.doc.display(obj);
            let main = crate::pretty::pretty(&main);
            let font = FontId::proportional(13.0);
            let galley = painter.layout_no_wrap(main, font, theme::TEXT);
            let mut box_rect = Rect::from_min_size(p + vec2(14.0, 14.0), galley.size() + vec2(14.0, 8.0));
            if box_rect.right() > rect.right() - 4.0 {
                box_rect = box_rect.translate(vec2(-(box_rect.width() + 28.0), 0.0));
            }
            if box_rect.bottom() > rect.bottom() - 4.0 {
                box_rect = box_rect.translate(vec2(0.0, -(box_rect.height() + 28.0)));
            }
            painter.rect(box_rect, CornerRadius::same(6), Color32::from_white_alpha(245), Stroke::new(1.0, theme::BORDER), egui::StrokeKind::Inside);
            painter.galley(box_rect.min + vec2(7.0, 4.0), galley, theme::TEXT);
        }
        let shadow = egui::epaint::Shadow { offset: [0, 2], blur: 10, spread: 0, color: Color32::from_black_alpha(36) };
        let fg = ui.painter().clone();
        let round_btn = |ui: &mut egui::Ui, center: Pos2, id: &str, tip: &str, paint: fn(&egui::Painter, Rect, Color32)| {
            let r = Rect::from_center_size(center, vec2(40.0, 40.0));
            fg.add(shadow.as_shape(r, CornerRadius::same(20)));
            let resp = ui.interact(r, ui.id().with(id), Sense::click());
            fg.circle_filled(r.center(), 20.0, if resp.hovered() { theme::HOVER } else { Color32::WHITE });
            paint(&fg, r, theme::ICON);
            resp.on_hover_text(tip).clicked()
        };
        let base = rect.right_bottom() - vec2(36.0, 36.0);
        if round_btn(ui, base - vec2(0.0, 104.0), "zoom_in", "Vergrößern", icons::plus) {
            self.view.zoom_at(rect, rect.center(), 1.25);
        }
        if round_btn(ui, base - vec2(0.0, 52.0), "zoom_out", "Verkleinern", icons::minus) {
            self.view.zoom_at(rect, rect.center(), 0.8);
        }
        if round_btn(ui, base, "zoom_home", "Standardansicht", icons::home) {
            self.view.reset();
        }
        let bar = Rect::from_min_size(rect.right_top() + vec2(-104.0, 12.0), vec2(92.0, 44.0));
        fg.add(shadow.as_shape(bar, CornerRadius::same(8)));
        fg.rect_filled(bar, CornerRadius::same(8), Color32::WHITE);
        let toggles: [(&str, &str, bool, fn(&egui::Painter, Rect, Color32)); 2] = [
            ("t_axes", "Achsen ein-/ausblenden", self.view.show_axes, icons::axes),
            ("t_grid", "Gitter ein-/ausblenden", self.view.show_grid, icons::grid),
        ];
        for (k, (id, tip, on, paint)) in toggles.into_iter().enumerate() {
            let r = Rect::from_min_size(bar.min + vec2(6.0 + k as f32 * 42.0, 5.0), vec2(36.0, 34.0));
            let resp = ui.interact(r, ui.id().with(id), Sense::click());
            let fill = if on { theme::ACCENT_SOFT } else if resp.hovered() { theme::HOVER } else { Color32::TRANSPARENT };
            fg.rect_filled(r, CornerRadius::same(6), fill);
            paint(&fg, r, if on { theme::ACCENT } else { theme::ICON });
            if resp.on_hover_text(tip).clicked() {
                if k == 0 {
                    self.view.show_axes = !self.view.show_axes;
                } else {
                    self.view.show_grid = !self.view.show_grid;
                }
            }
        }
    }

    fn status_bar(&mut self, ctx: &egui::Context) {
        egui::TopBottomPanel::bottom("status")
            .exact_height(26.0)
            .frame(egui::Frame::new().fill(theme::GUTTER).inner_margin(Margin::symmetric(14, 0)).stroke(Stroke::new(1.0, theme::BORDER)))
            .show(ctx, |ui| {
                ui.horizontal_centered(|ui| {
                    if let Some(s) = &self.status {
                        ui.label(RichText::new(s).small().color(theme::ERROR));
                    }
                    ui.with_layout(Layout::right_to_left(Align::Center), |ui| {
                        if let Some(c) = self.cursor {
                            ui.label(RichText::new(format!("x = {:.3}    y = {:.3}", c[0], c[1])).small().color(theme::MUTED));
                        }
                    });
                });
            });
    }

    fn value_table(&mut self, ui: &mut egui::Ui) {
        let funcs: Vec<(String, Color32, usize)> = self
            .doc
            .objects
            .iter()
            .enumerate()
            .filter(|(_, o)| o.def.visible && matches!(o.geo, Some(Geo::Function { .. })))
            .map(|(i, o)| (crate::expr_util::display_prime(&o.def.name), o.def.color, i))
            .collect();
        egui::Frame::new().inner_margin(Margin::symmetric(14, 10)).show(ui, |ui| {
            ui.horizontal(|ui| {
                ui.label(RichText::new("Start").color(theme::MUTED));
                ui.add_sized(vec2(70.0, 26.0), egui::DragValue::new(&mut self.vt_start).speed(0.1).max_decimals(3));
                ui.add_space(8.0);
                ui.label(RichText::new("Schritt").color(theme::MUTED));
                ui.add_sized(vec2(70.0, 26.0), egui::DragValue::new(&mut self.vt_step).speed(0.05).range(0.001..=1000.0).max_decimals(3));
            });
        });
        if funcs.is_empty() {
            ui.horizontal(|ui| {
                ui.add_space(14.0);
                ui.label(RichText::new("Keine sichtbaren Funktionen").color(theme::MUTED));
            });
            return;
        }
        egui::ScrollArea::both().auto_shrink([false, false]).show(ui, |ui| {
            egui::Frame::new().inner_margin(Margin::symmetric(14, 0)).show(ui, |ui| {
                egui::Grid::new("value_table").striped(true).spacing(vec2(22.0, 5.0)).show(ui, |ui| {
                    ui.label(RichText::new("x").strong());
                    for (name, color, _) in &funcs {
                        ui.label(RichText::new(format!("{name}(x)")).strong().color(*color));
                    }
                    ui.end_row();
                    for k in 0..41 {
                        let x = self.vt_start + self.vt_step * k as f64;
                        ui.label(plotwerk_math::format_number(x));
                        for (_, _, i) in &funcs {
                            let y = self.doc.objects[*i].geo.as_ref().map(|g| g.eval_fn(x)).unwrap_or(f64::NAN);
                            ui.label(plotwerk_math::format_number(y));
                        }
                        ui.end_row();
                    }
                });
            });
        });
    }

    fn handle_export(&mut self, ctx: &egui::Context) {
        if !self.export_pending {
            return;
        }
        let image = ctx.input(|i| {
            i.raw.events.iter().find_map(|e| match e {
                egui::Event::Screenshot { image, .. } => Some(image.clone()),
                _ => None,
            })
        });
        let Some(image) = image else { return };
        self.export_pending = false;
        let ppp = ctx.pixels_per_point();
        let r = self.canvas_rect;
        let x0 = ((r.left() * ppp).round().max(0.0) as usize).min(image.size[0]);
        let y0 = ((r.top() * ppp).round().max(0.0) as usize).min(image.size[1]);
        let x1 = ((r.right() * ppp).round() as usize).min(image.size[0]);
        let y1 = ((r.bottom() * ppp).round() as usize).min(image.size[1]);
        if x1 <= x0 || y1 <= y0 {
            self.status = Some("Bild konnte nicht erstellt werden".into());
            return;
        }
        let (w, h) = (x1 - x0, y1 - y0);
        let mut data = Vec::with_capacity(w * h * 4);
        for y in y0..y1 {
            for x in x0..x1 {
                let c = image.pixels[y * image.size[0] + x];
                data.extend_from_slice(&[c.r(), c.g(), c.b(), 255]);
            }
        }
        let Some(mut path) = rfd::FileDialog::new().add_filter("PNG-Bild", &["png"]).set_file_name("grafik.png").save_file() else { return };
        if path.extension().is_none() {
            path.set_extension("png");
        }
        let result = (|| -> Result<(), String> {
            let file = std::fs::File::create(&path).map_err(|e| e.to_string())?;
            let mut enc = png::Encoder::new(std::io::BufWriter::new(file), w as u32, h as u32);
            enc.set_color(png::ColorType::Rgba);
            enc.set_depth(png::BitDepth::Eight);
            let mut writer = enc.write_header().map_err(|e| e.to_string())?;
            writer.write_image_data(&data).map_err(|e| e.to_string())?;
            Ok(())
        })();
        self.status = match result {
            Ok(()) => None,
            Err(e) => Some(format!("Export fehlgeschlagen: {e}")),
        };
    }

    fn keyboard_insert(&mut self, text: &str) {
        if self.mode == Mode::Cas {
            if text == "\n" {
                self.cas.submit();
            } else {
                self.cas.insert_text(text);
            }
            return;
        }
        match text {
            "\n" => self.submit_input(),
            "\u{8}" => {
                self.input.pop();
            }
            t => self.input.push_str(t),
        }
        self.algebra_tab = 0;
        self.focus_input = true;
    }

    fn keyboard_panel(&mut self, ctx: &egui::Context) {
        let mut pressed: Option<&'static str> = None;
        let rows: [&[(&str, &'static str)]; 4] = [
            &[("x", "x"), ("y", "y"), ("π", "pi"), ("e", "e"), ("x²", "^2"), ("xⁿ", "^"), ("√", "sqrt("), ("|x|", "abs("), ("7", "7"), ("8", "8"), ("9", "9"), ("÷", "/")],
            &[("(", "("), (")", ")"), ("<", "<"), (">", ">"), ("≤", "<="), ("≥", ">="), ("°", "°"), ("!", "!"), ("4", "4"), ("5", "5"), ("6", "6"), ("×", "*")],
            &[("sin", "sin("), ("cos", "cos("), ("tan", "tan("), ("ln", "ln("), ("log", "log("), ("eˣ", "e^("), ("'", "'"), (",", ", "), ("1", "1"), ("2", "2"), ("3", "3"), ("−", "-")],
            &[("=", "="), ("{", "{"), ("}", "}"), ("∞", "inf"), ("Löse", "Löse("), ("Ableitung", "Ableitung("), ("Integral", "Integral("), ("Entf", "\u{8}"), ("0", "0"), (".", "."), ("Enter", "\n"), ("+", "+")],
        ];
        egui::TopBottomPanel::bottom("keyboard")
            .frame(egui::Frame::new().fill(theme::GUTTER).inner_margin(Margin::symmetric(12, 10)).stroke(Stroke::new(1.0, theme::BORDER)))
            .show(ctx, |ui| {
                ui.spacing_mut().item_spacing = vec2(6.0, 6.0);
                let width = ((ui.available_width() - 6.0 * 11.0) / 12.0).clamp(40.0, 110.0);
                for row in rows {
                    ui.horizontal(|ui| {
                        for (label, text) in row {
                            let digit = label.chars().all(|c| c.is_ascii_digit() || c == '.');
                            let special = matches!(*text, "\n" | "\u{8}");
                            let fill = if special { theme::ACCENT_SOFT } else if digit { Color32::WHITE } else { Color32::from_rgb(234, 237, 243) };
                            let btn = egui::Button::new(RichText::new(*label).size(15.5).color(if special { theme::ACCENT } else { theme::TEXT }))
                                .fill(fill)
                                .stroke(Stroke::new(1.0, theme::BORDER))
                                .min_size(vec2(width, 40.0));
                            if ui.add(btn).clicked() {
                                pressed = Some(text);
                            }
                        }
                    });
                }
            });
        if let Some(t) = pressed {
            self.keyboard_insert(t);
        }
    }

    fn props_window(&mut self, ctx: &egui::Context) {
        let Some(name) = self.props.clone() else { return };
        let Some(idx) = self.doc.find(&name) else {
            self.props = None;
            return;
        };
        let mut open = true;
        let mut rename_to: Option<String> = None;
        let mut delete = false;
        let id = egui::Id::new("props_rename");
        let mut buf: String = ctx.data_mut(|d| d.get_temp::<(String, String)>(id)).filter(|(n, _)| *n == name).map(|(_, b)| b).unwrap_or_else(|| crate::expr_util::display_prime(&name));
        let kind = self.doc.objects[idx].geo.as_ref().map(|g| g.kind_name()).unwrap_or("Objekt");
        egui::Window::new(RichText::new(format!("Eigenschaften: {kind}")).strong())
            .id(egui::Id::new("props_window"))
            .collapsible(false)
            .resizable(false)
            .default_pos(ctx.screen_rect().right_top() + vec2(-360.0, 120.0))
            .open(&mut open)
            .show(ctx, |ui| {
                ui.set_width(300.0);
                ui.horizontal(|ui| {
                    ui.label(RichText::new("Name").color(theme::MUTED));
                    let te = ui.add(TextEdit::singleline(&mut buf).desired_width(140.0));
                    if te.lost_focus() && ui.input(|i| i.key_pressed(Key::Enter)) {
                        rename_to = Some(buf.clone());
                    }
                    if ui.button("Umbenennen").clicked() {
                        rename_to = Some(buf.clone());
                    }
                });
                ui.add_space(8.0);
                let def = &mut self.doc.objects[idx].def;
                ui.label(RichText::new("Farbe").color(theme::MUTED));
                ui.horizontal_wrapped(|ui| {
                    let mut colors: Vec<Color32> = crate::doc::PALETTE.to_vec();
                    colors.push(crate::doc::POINT_COLOR);
                    colors.push(Color32::from_rgb(120, 120, 120));
                    for c in colors {
                        let (r, resp) = ui.allocate_exact_size(vec2(26.0, 26.0), Sense::click());
                        ui.painter().circle_filled(r.center(), 10.0, c);
                        if def.color == c {
                            ui.painter().circle_stroke(r.center(), 12.5, Stroke::new(2.0, theme::ACCENT));
                        }
                        if resp.clicked() {
                            def.color = c;
                        }
                    }
                    egui::color_picker::color_edit_button_srgba(ui, &mut def.color, egui::color_picker::Alpha::Opaque);
                });
                ui.add_space(8.0);
                ui.horizontal(|ui| {
                    ui.label(RichText::new("Linienstärke").color(theme::MUTED));
                    ui.add(egui::Slider::new(&mut def.width, 1.0..=8.0).step_by(0.5));
                });
                ui.horizontal(|ui| {
                    ui.label(RichText::new("Linienstil").color(theme::MUTED));
                    for (v, label) in [(0u8, "durchgezogen"), (1, "gestrichelt"), (2, "gepunktet")] {
                        ui.selectable_value(&mut def.dash, v, label);
                    }
                });
                ui.add_space(4.0);
                ui.checkbox(&mut def.label, "Beschriftung anzeigen");
                ui.checkbox(&mut def.visible, "Sichtbar");
                ui.add_space(8.0);
                if ui.add(egui::Button::new(RichText::new("Objekt löschen").color(theme::ERROR))).clicked() {
                    delete = true;
                }
            });
        ctx.data_mut(|d| d.insert_temp(id, (name.clone(), buf)));
        if let Some(new) = rename_to {
            let new = crate::expr_util::prime_notation(&new);
            match self.doc.rename(&name, &new) {
                Ok(()) => {
                    if self.selected.as_deref() == Some(name.as_str()) {
                        self.selected = Some(new.clone());
                    }
                    self.props = Some(new);
                    self.status = None;
                }
                Err(e) => self.status = Some(e),
            }
        }
        if delete {
            self.doc.remove(&name);
            self.props = None;
            self.selected = None;
        } else if !open {
            self.props = None;
        }
    }

    fn rotate_window(&mut self, ctx: &egui::Context) {
        let Some((obj, center, mut angle)) = self.rotate_dialog.take() else { return };
        let mut done = None;
        let mut cancel = false;
        egui::Modal::new(egui::Id::new("rotate_dialog")).show(ctx, |ui| {
            ui.set_width(300.0);
            ui.label(RichText::new(format!("{obj} um {center} drehen")).strong().size(16.0));
            ui.add_space(8.0);
            ui.horizontal(|ui| {
                ui.label("Winkel");
                let te = ui.add(TextEdit::singleline(&mut angle).desired_width(80.0));
                te.request_focus();
                ui.label("°");
                if te.lost_focus() && ui.input(|i| i.key_pressed(Key::Enter)) {
                    done = Some(angle.clone());
                }
            });
            ui.add_space(10.0);
            ui.horizontal(|ui| {
                if ui.add(egui::Button::new(RichText::new("Drehen").color(Color32::WHITE)).fill(theme::ACCENT)).clicked() {
                    done = Some(angle.clone());
                }
                if ui.button("Abbrechen").clicked() {
                    cancel = true;
                }
            });
        });
        if let Some(a) = done {
            let a = a.trim().trim_end_matches('°').replace(',', ".");
            self.run_command(format!("Drehe({obj}, {a}°, {center})"), false);
        } else if !cancel {
            self.rotate_dialog = Some((obj, center, angle));
        }
    }

    fn exam_dialogs(&mut self, ctx: &egui::Context) {
        match &mut self.exam {
            Exam::Setup(preset) => {
                let mut start = None;
                let mut cancel = false;
                let modal = egui::Modal::new(egui::Id::new("exam_setup")).show(ctx, |ui| {
                    ui.set_width(460.0);
                    ui.label(RichText::new("Prüfungsmodus starten").size(19.0).strong());
                    ui.add_space(8.0);
                    ui.label("Beim Start wird die aktuelle Arbeit ausgeblendet, das Fenster wechselt in den Vollbildmodus und bleibt im Vordergrund. Jedes Verlassen des Programms wird mit Uhrzeit protokolliert und färbt die Kopfzeile rot.");
                    ui.add_space(12.0);
                    ui.label(RichText::new("Konfiguration").strong());
                    for p in [Preset::Full, Preset::NoCas] {
                        ui.radio_value(preset, p, p.label());
                    }
                    ui.add_space(14.0);
                    ui.horizontal(|ui| {
                        if ui.add(egui::Button::new(RichText::new("Prüfung starten").color(Color32::WHITE).strong()).fill(theme::EXAM_OK).min_size(vec2(150.0, 32.0))).clicked() {
                            start = Some(*preset);
                        }
                        if ui.add(egui::Button::new("Abbrechen").min_size(vec2(100.0, 32.0))).clicked() {
                            cancel = true;
                        }
                    });
                });
                if let Some(p) = start {
                    self.start_exam(ctx, p);
                } else if cancel || modal.should_close() {
                    self.exam = Exam::Off;
                }
            }
            Exam::ConfirmEnd(_) => {
                let mut end = false;
                let mut back = false;
                egui::Modal::new(egui::Id::new("exam_confirm")).show(ctx, |ui| {
                    ui.set_width(380.0);
                    ui.label(RichText::new("Prüfung beenden?").size(18.0).strong());
                    ui.add_space(6.0);
                    ui.label("Nach dem Beenden wird das Prüfungsprotokoll angezeigt. Zeige es der Aufsicht.");
                    ui.add_space(12.0);
                    ui.horizontal(|ui| {
                        if ui.add(egui::Button::new(RichText::new("Beenden").color(Color32::WHITE).strong()).fill(theme::EXAM_ALERT).min_size(vec2(120.0, 32.0))).clicked() {
                            end = true;
                        }
                        if ui.add(egui::Button::new("Weiterarbeiten").min_size(vec2(120.0, 32.0))).clicked() {
                            back = true;
                        }
                    });
                });
                if end || back {
                    if let Exam::ConfirmEnd(s) = std::mem::replace(&mut self.exam, Exam::Off) {
                        if end {
                            ctx.send_viewport_cmd(ViewportCommand::WindowLevel(WindowLevel::Normal));
                            ctx.send_viewport_cmd(ViewportCommand::Fullscreen(false));
                            self.exam = Exam::Finished(s.finish());
                        } else {
                            self.exam = Exam::Running(s);
                        }
                    }
                }
            }
            Exam::Finished(report) => {
                let mut close = false;
                egui::Modal::new(egui::Id::new("exam_report")).show(ctx, |ui| {
                    ui.set_width(520.0);
                    let (color, status) = if report.violated { (theme::EXAM_ALERT, "Auffälligkeiten protokolliert") } else { (theme::EXAM_OK, "Keine Auffälligkeiten") };
                    egui::Frame::new().fill(color).corner_radius(CornerRadius::same(8)).inner_margin(Margin::symmetric(14, 10)).show(ui, |ui| {
                        ui.set_width(ui.available_width());
                        ui.label(RichText::new("Prüfungsprotokoll").size(19.0).strong().color(Color32::WHITE));
                        ui.label(RichText::new(status).color(Color32::WHITE));
                    });
                    ui.add_space(10.0);
                    egui::Grid::new("exam_grid").num_columns(2).spacing(vec2(24.0, 6.0)).show(ui, |ui| {
                        ui.label(RichText::new("Datum").color(theme::MUTED));
                        ui.label(report.start.format("%d.%m.%Y").to_string());
                        ui.end_row();
                        ui.label(RichText::new("Beginn").color(theme::MUTED));
                        ui.label(report.start.format("%H:%M:%S").to_string());
                        ui.end_row();
                        ui.label(RichText::new("Ende").color(theme::MUTED));
                        ui.label(report.end.format("%H:%M:%S").to_string());
                        ui.end_row();
                        ui.label(RichText::new("Dauer").color(theme::MUTED));
                        ui.label(fmt_duration(report.duration));
                        ui.end_row();
                        ui.label(RichText::new("Konfiguration").color(theme::MUTED));
                        ui.label(report.preset.label());
                        ui.end_row();
                        ui.label(RichText::new("Prüfcode").color(theme::MUTED));
                        ui.label(RichText::new(&report.code).monospace().strong());
                        ui.end_row();
                    });
                    ui.add_space(10.0);
                    ui.label(RichText::new("Ereignisse").strong());
                    egui::ScrollArea::vertical().max_height(220.0).show(ui, |ui| {
                        for e in &report.events {
                            ui.horizontal(|ui| {
                                ui.label(RichText::new(e.time.format("%H:%M:%S").to_string()).monospace().color(theme::MUTED));
                                ui.label(RichText::new(&e.text).color(if e.violation { theme::EXAM_ALERT } else { theme::TEXT }));
                            });
                        }
                    });
                    ui.add_space(12.0);
                    if ui.add(egui::Button::new("Schließen").min_size(vec2(110.0, 32.0))).clicked() {
                        close = true;
                    }
                });
                if close {
                    self.close_report();
                }
            }
            _ => {}
        }
    }

    fn shortcuts(&mut self, ctx: &egui::Context) {
        if ctx.wants_keyboard_input() {
            return;
        }
        let (undo, redo, del, esc) = ctx.input_mut(|i| {
            let undo = i.consume_key(egui::Modifiers::COMMAND, Key::Z);
            let redo = i.consume_key(egui::Modifiers::COMMAND, Key::Y) || i.consume_key(egui::Modifiers::COMMAND | egui::Modifiers::SHIFT, Key::Z);
            (undo, redo, i.key_pressed(Key::Delete), i.key_pressed(Key::Escape))
        });
        if !self.exam_running() {
            let (save, save_as, open, new) = ctx.input_mut(|i| {
                let save_as = i.consume_key(egui::Modifiers::COMMAND | egui::Modifiers::SHIFT, Key::S);
                let save = i.consume_key(egui::Modifiers::COMMAND, Key::S);
                let open = i.consume_key(egui::Modifiers::COMMAND, Key::O);
                let new = i.consume_key(egui::Modifiers::COMMAND, Key::N);
                (save, save_as, open, new)
            });
            if save_as {
                self.save(ctx, true);
            } else if save {
                self.save(ctx, false);
            }
            if open {
                self.open_dialog(ctx);
            }
            if new {
                self.new_file(ctx);
            }
        }
        if undo {
            self.doc.undo();
        }
        if redo {
            self.doc.redo();
        }
        if del {
            if let Some(n) = self.selected.take() {
                self.doc.remove(&n);
            }
        }
        if esc {
            self.pending.clear();
            self.tool = Tool::Move;
            self.status = None;
        }
    }
}

fn paint_logo(painter: &egui::Painter, r: Rect, color: Color32) {
    painter.rect_stroke(r.shrink(1.0), CornerRadius::same(6), Stroke::new(1.6, color), egui::StrokeKind::Inside);
    let pts: Vec<Pos2> = (0..=16)
        .map(|i| {
            let t = i as f32 / 16.0;
            let x = r.left() + 4.0 + t * (r.width() - 8.0);
            let y = r.center().y + 1.0 - (t * std::f32::consts::TAU).sin() * (r.height() * 0.28);
            pos2(x, y)
        })
        .collect();
    painter.add(egui::Shape::line(pts, Stroke::new(2.0, color)));
}

impl eframe::App for PlotwerkApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if let Exam::Running(s) | Exam::ConfirmEnd(s) = &mut self.exam {
            s.monitor(ctx);
        }
        self.shortcuts(ctx);
        if ctx.input_mut(|i| i.consume_key(egui::Modifiers::COMMAND, Key::K)) {
            self.keyboard = !self.keyboard;
        }
        self.top_bar(ctx);
        self.status_bar(ctx);
        if self.keyboard && matches!(self.mode, Mode::Graph | Mode::Geometry | Mode::Cas) {
            self.keyboard_panel(ctx);
        }
        let side_width = match self.mode {
            Mode::Cas => 440.0,
            Mode::Probability | Mode::Table => 380.0,
            _ => 340.0,
        };
        let panel_id = match self.mode {
            Mode::Cas => "cas",
            Mode::Probability => "prob",
            Mode::Space3D => "space3d",
            Mode::Table => "table",
            _ => "algebra",
        };
        egui::SidePanel::left(panel_id)
            .resizable(true)
            .default_width(side_width)
            .width_range(260.0..=720.0)
            .frame(egui::Frame::new().fill(theme::PANEL).inner_margin(Margin::ZERO).stroke(Stroke::new(1.0, theme::BORDER)))
            .show(ctx, |ui| match self.mode {
                Mode::Cas => {
                    if let CasAction::Plot(text) = self.cas.ui(ui) {
                        let doc = &mut self.doc;
                        match guarded(|| doc.add_input(&text)) {
                            Ok(n) => {
                                self.selected = Some(n);
                                self.status = None;
                            }
                            Err(e) => self.status = Some(e),
                        }
                    }
                }
                Mode::Probability => self.prob.side_ui(ui),
                Mode::Space3D => self.space.side_ui(ui),
                Mode::Table => {
                    if let crate::table::TableAction::ToGraph { points, function } = self.table.side_ui(ui) {
                        let doc = &mut self.doc;
                        let mut err = guarded(|| doc.add_input(&points)).err();
                        if let Some(f) = function {
                            let doc = &mut self.doc;
                            if let Err(e) = guarded(|| doc.add_input(&f)) {
                                err = Some(e);
                            }
                        }
                        self.status = err;
                        self.set_mode(Mode::Graph);
                    }
                }
                _ => self.algebra_panel(ui),
            });
        egui::CentralPanel::default().frame(egui::Frame::new().fill(Color32::WHITE)).show(ctx, |ui| match self.mode {
            Mode::Probability => self.prob.chart_ui(ui),
            Mode::Space3D => self.space.canvas_ui(ui),
            Mode::Table => {
                let h = (ui.available_height() * 0.55).max(200.0);
                let w = ui.available_width();
                ui.allocate_ui(vec2(w, h), |ui| {
                    ui.set_min_size(vec2(w, h));
                    self.table.grid_ui(ui);
                });
                self.table.chart_ui(ui);
            }
            _ => {
                if self.mode != Mode::Cas {
                    self.toolbar(ui);
                }
                self.canvas(ui);
            }
        });
        self.handle_export(ctx);
        self.props_window(ctx);
        self.rotate_window(ctx);
        self.exam_dialogs(ctx);
    }
}
