use crate::expr_util::{
    NoCtx, collect_names, collect_vars, contains_var, display_prime, literal_value, map_children, num, prime_notation,
    substitute, substitute_map, var,
};
use crate::geo::{self, Affine, Geo, Pt, angle_of, dist, intersect, line_through, norm_angle, polygon_area, solve_zero};
use eframe::egui::Color32;
use plotwerk_math::{self as m, Context, Expr, Statement};
use std::collections::{HashMap, HashSet};

pub const PALETTE: [Color32; 8] = [
    Color32::from_rgb(21, 101, 192),
    Color32::from_rgb(198, 40, 40),
    Color32::from_rgb(46, 125, 50),
    Color32::from_rgb(239, 108, 0),
    Color32::from_rgb(106, 27, 154),
    Color32::from_rgb(0, 131, 143),
    Color32::from_rgb(93, 64, 55),
    Color32::from_rgb(173, 20, 87),
];

pub const POINT_COLOR: Color32 = Color32::from_rgb(38, 50, 72);

#[derive(Clone, Debug, PartialEq)]
pub struct SliderRange {
    pub min: f64,
    pub max: f64,
    pub step: f64,
}

#[derive(Clone, Debug)]
pub struct ObjDef {
    pub name: String,
    pub params: Vec<String>,
    pub source: String,
    pub equation: bool,
    pub color: Color32,
    pub visible: bool,
    pub slider: Option<SliderRange>,
    pub width: f32,
    pub dash: u8,
    pub label: bool,
}

impl Default for ObjDef {
    fn default() -> Self {
        Self {
            name: String::new(),
            params: vec![],
            source: String::new(),
            equation: false,
            color: Color32::BLACK,
            visible: true,
            slider: None,
            width: 2.4,
            dash: 0,
            label: false,
        }
    }
}

pub struct Object {
    pub def: ObjDef,
    pub geo: Option<Geo>,
    pub error: Option<String>,
    pub free_point: bool,
    pub free_number: bool,
    pub derived: bool,
    pub on_object: Option<String>,
}

#[derive(Default)]
pub struct Env {
    pub values: HashMap<String, f64>,
    pub functions: HashMap<String, (Vec<String>, Expr)>,
}

impl Context for Env {
    fn value(&self, name: &str) -> Option<f64> {
        self.values.get(name).copied()
    }

    fn function(&self, name: &str) -> Option<(Vec<String>, Expr)> {
        self.functions.get(name).cloned()
    }
}

#[derive(Default)]
pub struct Document {
    pub objects: Vec<Object>,
    pub revision: u64,
    undo: Vec<Vec<ObjDef>>,
    redo: Vec<Vec<ObjDef>>,
    color_index: usize,
}

const RESERVED: [&str; 6] = ["x", "y", "z", "e", "pi", "t"];
const LOWER_NAMES: &str = "fghpqrsuvwklmnabcdij";
const NUMBER_NAMES: &str = "abcdkmnpqrsuvw";
const VECTOR_NAMES: &str = "uvwrst";
const ANGLE_NAMES: &str = "αβγδεζηθκλμνξρστφχψω";

fn command_key(name: &str) -> String {
    name.to_lowercase().replace('ä', "ae").replace('ö', "oe").replace('ü', "ue")
}

pub fn is_relation(name: &str) -> bool {
    matches!(name, "<" | ">" | "<=" | ">=" | "and" | "or" | "not" | "!=" | "==")
}

fn is_command(name: &str) -> bool {
    matches!(
        command_key(name).as_str(),
        "point" | "punkt" | "line" | "gerade" | "segment" | "strecke" | "circle" | "kreis" | "polygon" | "vieleck"
            | "midpoint" | "mittelpunkt" | "distance" | "abstand" | "intersect" | "schneide" | "root" | "nullstelle"
            | "roots" | "nullstellen" | "extremum" | "turningpoint" | "wendepunkt" | "derivative" | "ableitung"
            | "tangent" | "tangente" | "integral" | "perpendicular" | "senkrechte" | "orthogonalgerade" | "parallel"
            | "parallele" | "area" | "flaeche" | "slope" | "steigung" | "angle" | "winkel" | "length" | "laenge"
            | "ray" | "halbgerade" | "vector" | "vektor" | "circulararc" | "kreisbogen" | "circularsector" | "kreissektor"
            | "reflect" | "spiegle" | "rotate" | "drehe" | "translate" | "verschiebe" | "dilate" | "streckung" | "function"
            | "funktion" | "sequence" | "folge"
    )
}

impl Document {
    pub fn defs(&self) -> Vec<ObjDef> {
        self.objects.iter().map(|o| o.def.clone()).collect()
    }

    pub fn snapshot(&mut self) {
        let defs = self.defs();
        self.undo.push(defs);
        if self.undo.len() > 200 {
            self.undo.remove(0);
        }
        self.redo.clear();
    }

    pub fn can_undo(&self) -> bool {
        !self.undo.is_empty()
    }

    pub fn can_redo(&self) -> bool {
        !self.redo.is_empty()
    }

    pub fn undo(&mut self) {
        if let Some(defs) = self.undo.pop() {
            self.redo.push(self.defs());
            self.set_defs(defs);
        }
    }

    pub fn redo(&mut self) {
        if let Some(defs) = self.redo.pop() {
            self.undo.push(self.defs());
            self.set_defs(defs);
        }
    }

    pub fn clear_all(&mut self) {
        self.objects.clear();
        self.undo.clear();
        self.redo.clear();
        self.color_index = 0;
        self.revision += 1;
    }

    pub fn set_defs(&mut self, defs: Vec<ObjDef>) {
        self.objects = defs
            .into_iter()
            .map(|def| Object { def, geo: None, error: None, free_point: false, free_number: false, derived: false, on_object: None })
            .collect();
        self.recompute();
    }

    pub fn find(&self, name: &str) -> Option<usize> {
        self.objects.iter().position(|o| o.def.name == name)
    }

    fn next_color(&mut self) -> Color32 {
        let c = PALETTE[self.color_index % PALETTE.len()];
        self.color_index += 1;
        c
    }

    fn name_taken(&self, name: &str) -> bool {
        self.find(name).is_some() || RESERVED.contains(&name)
    }

    pub fn next_point_name(&self) -> String {
        for round in 0.. {
            for c in 'A'..='Z' {
                let name = if round == 0 { c.to_string() } else { format!("{c}_{round}") };
                if !self.name_taken(&name) {
                    return name;
                }
            }
        }
        unreachable!()
    }

    fn next_lower_name(&self, pool: &str) -> String {
        for round in 0.. {
            for c in pool.chars() {
                let name = if round == 0 { c.to_string() } else { format!("{c}_{round}") };
                if !self.name_taken(&name) {
                    return name;
                }
            }
        }
        unreachable!()
    }

    pub fn recompute(&mut self) {
        let mut env = Env::default();
        let mut geos: HashMap<String, Geo> = HashMap::new();
        for i in 0..self.objects.len() {
            let def = self.objects[i].def.clone();
            let result = Evaluator { env: &env, geos: &geos }.evaluate(&def);
            let obj = &mut self.objects[i];
            obj.free_point = false;
            obj.free_number = false;
            obj.derived = false;
            obj.on_object = None;
            match result {
                Ok(out) => {
                    obj.free_point = out.free_point;
                    obj.free_number = out.free_number;
                    obj.derived = out.derived;
                    obj.on_object = out.on_object.clone();
                    obj.error = None;
                    register(&mut env, &def.name, &out.geo);
                    geos.insert(def.name.clone(), out.geo.clone());
                    obj.geo = Some(out.geo);
                }
                Err(e) => {
                    obj.geo = None;
                    obj.error = Some(e);
                }
            }
        }
        for obj in &mut self.objects {
            if obj.free_number && obj.def.slider.is_none() {
                if let Some(Geo::Number(v)) = obj.geo {
                    let r = v.abs().max(5.0).ceil();
                    obj.def.slider = Some(SliderRange { min: -r, max: r, step: 0.1 });
                }
            }
        }
        self.revision += 1;
    }

    fn defined_names(&self) -> HashSet<String> {
        self.objects.iter().map(|o| o.def.name.clone()).collect()
    }

    fn auto_sliders(&mut self, expr_names: &HashSet<String>, exclude: &[String]) {
        let defined = self.defined_names();
        let mut missing: Vec<String> = expr_names
            .iter()
            .filter(|n| {
                !defined.contains(*n)
                    && !RESERVED.contains(&n.as_str())
                    && !exclude.contains(n)
                    && n.chars().count() == 1
                    && n.chars().all(|c| c.is_ascii_lowercase())
            })
            .cloned()
            .collect();
        missing.sort();
        for name in missing {
            let color = self.next_color();
            self.objects.insert(
                0,
                Object {
                    def: ObjDef {
                        name,
                        params: vec![],
                        source: "1".into(),
                        equation: false,
                        color,
                        visible: true,
                        slider: Some(SliderRange { min: -5.0, max: 5.0, step: 0.1 }),
                        ..ObjDef::default()
                    },
                    geo: None,
                    error: None,
                    free_point: false,
                    free_number: false,
                    derived: false,
                    on_object: None,
                },
            );
        }
    }

    pub fn add_input(&mut self, raw: &str) -> Result<String, String> {
        let text = prime_notation(raw.trim());
        if text.is_empty() {
            return Err("Leere Eingabe".into());
        }
        let stmt = m::parse_statement(&text).map_err(|e| e.message)?;
        let before = self.defs();
        let before_color = self.color_index;
        let (mut def, names) = match stmt {
            Statement::Assign { name, params, value } => {
                let rhs = split_rhs(&text);
                let mut names = HashSet::new();
                collect_vars(&value, &mut names);
                let mut params = params;
                if params.is_empty() && contains_var(&value, "x") && !matches!(value, Expr::Call(ref n, _) if is_command(n)) {
                    params = vec!["x".into()];
                }
                if let Some(i) = self.find(&name) {
                    self.snapshot();
                    self.objects[i].def.source = rhs;
                    self.objects[i].def.params = params;
                    self.objects[i].def.equation = false;
                    self.recompute();
                    return self.check_or_rollback(&name, before, before_color);
                }
                (
                    ObjDef { name, params: params.clone(), source: rhs, equation: false, color: Color32::BLACK, visible: true, slider: None, ..ObjDef::default() },
                    names.into_iter().filter(|n| !params.contains(n)).collect::<HashSet<_>>(),
                )
            }
            Statement::Equation { lhs, rhs } => {
                let mut names = HashSet::new();
                collect_vars(&lhs, &mut names);
                collect_vars(&rhs, &mut names);
                let name = self.next_lower_name(LOWER_NAMES);
                (ObjDef { name, params: vec![], source: text.clone(), equation: true, color: Color32::BLACK, visible: true, slider: None, ..ObjDef::default() }, names)
            }
            Statement::Expr(e) => {
                let mut names = HashSet::new();
                collect_vars(&e, &mut names);
                let is_cmd = matches!(&e, Expr::Call(n, _) if is_command(n) || is_relation(n));
                let params = if contains_var(&e, "x") && !is_cmd { vec!["x".to_string()] } else { vec![] };
                (
                    ObjDef { name: String::new(), params, source: text.clone(), equation: false, color: Color32::BLACK, visible: true, slider: None, ..ObjDef::default() },
                    names,
                )
            }
        };
        self.snapshot();
        let exclude: Vec<String> = def.params.clone();
        self.auto_sliders(&names, &exclude);
        if def.name.is_empty() {
            def.name = "__pending".into();
        }
        let pending = def.name == "__pending";
        def.color = self.next_color();
        self.objects.push(Object { def, geo: None, error: None, free_point: false, free_number: false, derived: false, on_object: None });
        self.recompute();
        if pending {
            let idx = self.objects.len() - 1;
            let name = match &self.objects[idx].geo {
                Some(g) => self.auto_name(g),
                None => self.next_lower_name(LOWER_NAMES),
            };
            self.objects[idx].def.name = name;
            self.recompute();
        }
        let name = self.objects.last().map(|o| o.def.name.clone()).unwrap_or_default();
        if let Some(o) = self.objects.last_mut() {
            if matches!(o.geo, Some(Geo::Point(_)) | Some(Geo::Points(_))) {
                o.def.color = POINT_COLOR;
                o.def.label = true;
                self.color_index = self.color_index.saturating_sub(1);
            }
        }
        self.check_or_rollback(&name, before, before_color)
    }

    fn check_or_rollback(&mut self, name: &str, before: Vec<ObjDef>, before_color: usize) -> Result<String, String> {
        let err = self.find(name).and_then(|i| self.objects[i].error.clone());
        if let Some(e) = err {
            self.undo.pop();
            self.set_defs(before);
            self.color_index = before_color;
            return Err(e);
        }
        Ok(name.to_string())
    }

    pub fn redefine(&mut self, idx: usize, text: &str) -> Result<String, String> {
        let name = self.objects[idx].def.name.clone();
        let text = text.trim();
        let full = if text.contains('=') || text.contains(":=") { text.to_string() } else { format!("{name} = {text}") };
        let stmt = m::parse_statement(&prime_notation(&full)).map_err(|e| e.message)?;
        if let Statement::Equation { .. } = stmt {
            let before = self.defs();
            let before_color = self.color_index;
            self.snapshot();
            self.objects[idx].def.source = prime_notation(&full);
            self.objects[idx].def.equation = true;
            self.objects[idx].def.params.clear();
            self.recompute();
            return self.check_or_rollback(&name, before, before_color);
        }
        let stmt_name = match &stmt {
            Statement::Assign { name, .. } => name.clone(),
            _ => name.clone(),
        };
        if stmt_name != name {
            return Err(format!("Name muss {name} bleiben"));
        }
        self.add_input(&full)
    }

    pub fn dependents(&self, name: &str) -> Vec<String> {
        let mut doomed: HashSet<String> = HashSet::new();
        doomed.insert(name.to_string());
        for obj in &self.objects {
            if doomed.contains(&obj.def.name) {
                continue;
            }
            let mut names = HashSet::new();
            if let Some(e) = parse_source(&obj.def) {
                collect_names(&e, &mut names);
            }
            let derived_refs: HashSet<String> = names.iter().map(|n| n.split("_d").next().unwrap_or(n).to_string()).collect();
            if derived_refs.iter().any(|n| doomed.contains(n)) {
                doomed.insert(obj.def.name.clone());
            }
        }
        doomed.into_iter().collect()
    }

    pub fn remove(&mut self, name: &str) {
        self.snapshot();
        let doomed = self.dependents(name);
        self.objects.retain(|o| !doomed.contains(&o.def.name));
        self.recompute();
    }

    pub fn set_point(&mut self, name: &str, p: Pt) {
        if let Some(i) = self.find(name) {
            self.objects[i].def.source = format!("({}, {})", fmt_coord(p[0]), fmt_coord(p[1]));
            self.recompute();
        }
    }

    pub fn set_number(&mut self, name: &str, v: f64) {
        if let Some(i) = self.find(name) {
            self.objects[i].def.source = fmt_coord(v);
            self.recompute();
        }
    }

    pub fn add_point(&mut self, p: Pt) -> String {
        let name = self.next_point_name();
        let n = self.add_named(&name, &format!("({}, {})", fmt_coord(p[0]), fmt_coord(p[1])), POINT_COLOR);
        if let Some(i) = self.find(&n) {
            self.objects[i].def.label = true;
        }
        n
    }

    pub fn auto_name(&self, g: &Geo) -> String {
        match g {
            Geo::Point(_) | Geo::Points(_) => self.next_point_name(),
            Geo::Number(_) => self.next_lower_name(NUMBER_NAMES),
            Geo::Vector(..) => self.next_lower_name(VECTOR_NAMES),
            Geo::Angle { .. } => self.next_lower_name(ANGLE_NAMES),
            _ => self.next_lower_name(LOWER_NAMES),
        }
    }

    pub fn add_command(&mut self, source: &str, point_like: bool) -> Result<String, String> {
        let before = self.defs();
        let before_color = self.color_index;
        let color = if point_like { POINT_COLOR } else { self.next_color() };
        self.add_named("__pending", source, color);
        let idx = self.objects.len() - 1;
        if let Some(g) = self.objects[idx].geo.clone() {
            let name = self.auto_name(&g);
            let o = &mut self.objects[idx];
            o.def.name = name;
            if g.is_point_like() {
                o.def.color = POINT_COLOR;
                o.def.label = true;
            }
            self.recompute();
        }
        let name = self.objects[idx].def.name.clone();
        self.check_or_rollback(&name, before, before_color)
    }

    pub fn set_point_param(&mut self, name: &str, t: f64) {
        if let Some(i) = self.find(name) {
            if let Some(parent) = self.objects[i].on_object.clone() {
                self.objects[i].def.source = format!("Punkt({parent}, {})", fmt_param(t));
                self.recompute();
            }
        }
    }

    pub fn rename(&mut self, old: &str, new: &str) -> Result<(), String> {
        let new = new.trim();
        if new.is_empty() || !new.chars().next().map(|c| c.is_alphabetic()).unwrap_or(false) || !new.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Err("Ungültiger Name".into());
        }
        if new == old {
            return Ok(());
        }
        if self.name_taken(new) {
            return Err(format!("{new} ist bereits vergeben"));
        }
        self.snapshot();
        for o in &mut self.objects {
            if o.def.name == old {
                o.def.name = new.to_string();
            }
            o.def.source = rename_ident(&o.def.source, old, new);
        }
        self.recompute();
        Ok(())
    }

    fn add_named(&mut self, name: &str, source: &str, color: Color32) -> String {
        self.snapshot();
        self.objects.push(Object {
            def: ObjDef { name: name.into(), params: vec![], source: source.into(), equation: false, color, visible: true, slider: None, ..ObjDef::default() },
            geo: None,
            error: None,
            free_point: false,
            free_number: false,
            derived: false,
            on_object: None,
        });
        self.recompute();
        name.to_string()
    }

    pub fn definition_text(&self, obj: &Object) -> String {
        let name = display_prime(&obj.def.name);
        if obj.def.equation {
            return obj.def.source.clone();
        }
        if obj.def.params.is_empty() {
            format!("{name} = {}", obj.def.source)
        } else {
            format!("{name}({}) = {}", obj.def.params.join(", "), obj.def.source)
        }
    }

    pub fn display(&self, obj: &Object) -> (String, Option<String>) {
        let name = display_prime(&obj.def.name);
        let Some(geo) = &obj.geo else {
            return (self.definition_text(obj), obj.error.clone());
        };
        let source_display = display_prime_in(&obj.def.source);
        let main = match geo {
            Geo::Number(v) => format!("{name} = {}", m::format_number(*v)),
            Geo::Point(p) => format!("{name} = {}", fmt_pt(*p)),
            Geo::Points(ps) => {
                if ps.is_empty() {
                    format!("{name} = {{}}")
                } else {
                    format!("{name} = {{{}}}", ps.iter().map(|p| fmt_pt(*p)).collect::<Vec<_>>().join(", "))
                }
            }
            Geo::Function { expr, dom, .. } => {
                let p = obj.def.params.first().cloned().unwrap_or_else(|| "x".into());
                let s = if obj.derived {
                    format!("{name}({p}) = {}", m::format_expr(&m::simplify(expr)))
                } else if obj.def.equation {
                    source_display.clone()
                } else {
                    format!("{name}({p}) = {source_display}")
                };
                match dom {
                    Some((a, b)) => format!("{s},  ({} ≤ x ≤ {})", m::format_number(*a), m::format_number(*b)),
                    None => s,
                }
            }
            Geo::Ray(p, q) => {
                let (a, b, c) = line_through(*p, *q);
                format!("{name}: {}", fmt_line(a, b, c))
            }
            Geo::Vector(p, q) => format!("{name} = {}", fmt_pt([q[0] - p[0], q[1] - p[1]])),
            Geo::Arc { r, sweep, sector, .. } => {
                format!("{name} = {}", m::format_number(if *sector { 0.5 * r * r * sweep } else { r * sweep }))
            }
            Geo::Angle { sweep, .. } => format!("{name} = {}°", fmt_deg(*sweep)),
            Geo::Line { a, b, c } => format!("{name}: {}", fmt_line(*a, *b, *c)),
            Geo::Segment(p, q) => format!("{name} = {}", m::format_number(dist(*p, *q))),
            Geo::Circle { c, r } => format!("{name}: {}", fmt_circle(*c, *r)),
            Geo::Polygon(pts) => format!("{name} = {}", m::format_number(polygon_area(pts))),
            Geo::Implicit { .. } | Geo::Region { .. } => format!("{name}: {source_display}"),
            Geo::Area { value, .. } => format!("{name} = {}", m::format_number(*value)),
        };
        let sub = if obj.derived && !matches!(geo, Geo::Function { .. }) {
            Some(source_display)
        } else if obj.derived {
            Some(source_display)
        } else {
            None
        };
        (main, sub)
    }
}

fn display_prime_in(source: &str) -> String {
    let mut out = String::new();
    let mut ident = String::new();
    let flush = |ident: &mut String, out: &mut String| {
        if !ident.is_empty() {
            out.push_str(&display_prime(ident));
            ident.clear();
        }
    };
    for c in source.chars() {
        if c.is_alphanumeric() || c == '_' {
            ident.push(c);
        } else {
            flush(&mut ident, &mut out);
            out.push(c);
        }
    }
    flush(&mut ident, &mut out);
    out
}

fn split_rhs(text: &str) -> String {
    if let Some((_, r)) = text.split_once(":=") {
        return r.trim().to_string();
    }
    text.split_once('=').map(|(_, r)| r.trim().to_string()).unwrap_or_else(|| text.to_string())
}

fn parse_source(def: &ObjDef) -> Option<Expr> {
    if def.equation {
        match m::parse_statement(&def.source).ok()? {
            Statement::Equation { lhs, rhs } => Some(Expr::Tuple(vec![lhs, rhs])),
            Statement::Expr(e) => Some(e),
            Statement::Assign { value, .. } => Some(value),
        }
    } else {
        m::parse_expr(&def.source).ok()
    }
}

fn register(env: &mut Env, name: &str, geo: &Geo) {
    match geo {
        Geo::Number(v) => {
            env.values.insert(name.to_string(), *v);
        }
        Geo::Segment(p, q) => {
            env.values.insert(name.to_string(), dist(*p, *q));
        }
        Geo::Polygon(pts) => {
            env.values.insert(name.to_string(), polygon_area(pts));
        }
        Geo::Area { value, .. } => {
            env.values.insert(name.to_string(), *value);
        }
        Geo::Angle { sweep, .. } => {
            env.values.insert(name.to_string(), *sweep);
        }
        Geo::Arc { r, sweep, sector, .. } => {
            env.values.insert(name.to_string(), if *sector { 0.5 * r * r * sweep } else { r * sweep });
        }
        Geo::Vector(p, q) => {
            env.values.insert(name.to_string(), dist(*p, *q));
        }
        Geo::Function { expr, .. } => {
            env.functions.insert(name.to_string(), (vec!["x".into()], expr.clone()));
            let d1 = m::simplify(&m::derivative(expr, "x"));
            let d2 = m::simplify(&m::derivative(&d1, "x"));
            let d3 = m::simplify(&m::derivative(&d2, "x"));
            env.functions.insert(format!("{name}_d1"), (vec!["x".into()], d1));
            env.functions.insert(format!("{name}_d2"), (vec!["x".into()], d2));
            env.functions.insert(format!("{name}_d3"), (vec!["x".into()], d3));
        }
        _ => {}
    }
}

pub fn fmt_coord(v: f64) -> String {
    let r = (v * 100.0).round() / 100.0;
    m::format_number(if r == 0.0 { 0.0 } else { r })
}

pub fn fmt_pt(p: Pt) -> String {
    format!("({}, {})", m::format_number(p[0]), m::format_number(p[1]))
}

fn signed_term(coef: f64, sym: &str, first: bool) -> String {
    if coef.abs() < 1e-12 {
        return String::new();
    }
    let sign = if coef < 0.0 { if first { "-" } else { " - " } } else if first { "" } else { " + " };
    let a = coef.abs();
    let body = if sym.is_empty() {
        m::format_number(a)
    } else if (a - 1.0).abs() < 1e-12 {
        sym.to_string()
    } else {
        format!("{}{sym}", m::format_number(a))
    };
    format!("{sign}{body}")
}

pub fn fmt_line(a: f64, b: f64, c: f64) -> String {
    if b.abs() > 1e-12 {
        let slope = -a / b;
        let icpt = c / b;
        let mut s = signed_term(slope, "x", true);
        let first = s.is_empty();
        s.push_str(&signed_term(icpt, "", first));
        if s.is_empty() {
            s = "0".into();
        }
        format!("y = {s}")
    } else {
        format!("x = {}", m::format_number(c / a))
    }
}

pub fn fmt_circle(c: Pt, r: f64) -> String {
    let part = |sym: &str, v: f64| {
        if v.abs() < 1e-12 {
            format!("{sym}²")
        } else if v > 0.0 {
            format!("({sym} - {})²", m::format_number(v))
        } else {
            format!("({sym} + {})²", m::format_number(-v))
        }
    };
    format!("{} + {} = {}", part("x", c[0]), part("y", c[1]), m::format_number(r * r))
}

struct EvalOut {
    geo: Geo,
    free_point: bool,
    free_number: bool,
    derived: bool,
    on_object: Option<String>,
}

struct Evaluator<'a> {
    env: &'a Env,
    geos: &'a HashMap<String, Geo>,
}

impl<'a> Evaluator<'a> {
    fn evaluate(&self, def: &ObjDef) -> Result<EvalOut, String> {
        if def.equation {
            let stmt = m::parse_statement(&def.source).map_err(|e| e.message)?;
            let geo = match stmt {
                Statement::Equation { lhs, rhs } => self.equation(&lhs, &rhs)?,
                Statement::Assign { value, .. } => self.equation(&var("y"), &value)?,
                Statement::Expr(e) => self.geo_of(&e, "x")?,
            };
            return Ok(EvalOut { geo, free_point: false, free_number: false, derived: false, on_object: None });
        }
        let expr = m::parse_expr(&def.source).map_err(|e| e.message)?;
        let param = def.params.first().map(|s| s.as_str()).unwrap_or("x");
        if !def.params.is_empty() {
            return Ok(EvalOut { geo: self.function(&expr, param)?, free_point: false, free_number: false, derived: false, on_object: None });
        }
        let free_number = literal_value(&expr).is_some();
        let free_point = matches!(&expr, Expr::Tuple(items) if items.len() == 2 && items.iter().all(|i| literal_value(i).is_some()));
        let derived = matches!(&expr, Expr::Call(n, _) if is_command(n));
        let geo = self.geo_of(&expr, param)?;
        let on_object = match &expr {
            Expr::Call(n, args) if matches!(command_key(n).as_str(), "point" | "punkt") && args.len() == 2 => match &args[0] {
                Expr::Var(parent) if self.geos.get(parent).map(|g| g.is_curve()).unwrap_or(false) => Some(parent.clone()),
                _ => None,
            },
            _ => None,
        };
        Ok(EvalOut { geo, free_point, free_number, derived: derived && on_object.is_none(), on_object })
    }

    fn inline(&self, e: &Expr, depth: usize) -> Result<Expr, String> {
        if depth > 32 {
            return Err("Zirkuläre Definition".into());
        }
        match e {
            Expr::Var(n) => {
                if let Some(v) = self.env.values.get(n) {
                    return Ok(num(*v));
                }
                if let Some(Geo::Point(p)) = self.geos.get(n) {
                    return Ok(Expr::Tuple(vec![num(p[0]), num(p[1])]));
                }
                Ok(e.clone())
            }
            Expr::Mul(a, b) => {
                if let (Expr::Var(axis), Expr::Var(p)) = (a.as_ref(), b.as_ref()) {
                    if let Some(Geo::Point(pt)) = self.geos.get(p) {
                        if axis == "x" {
                            return Ok(num(pt[0]));
                        }
                        if axis == "y" {
                            return Ok(num(pt[1]));
                        }
                    }
                }
                Ok(Expr::Mul(Box::new(self.inline(a, depth)?), Box::new(self.inline(b, depth)?)))
            }
            Expr::Call(name, args) => {
                let args: Vec<Expr> = args.iter().map(|a| self.inline(a, depth)).collect::<Result<_, _>>()?;
                if let Some((params, body)) = self.env.functions.get(name) {
                    if params.len() != args.len() {
                        return Err(format!("{} erwartet {} Argument(e)", display_prime(name), params.len()));
                    }
                    let map: HashMap<String, Expr> = params.iter().cloned().zip(args).collect();
                    return Ok(substitute_map(body, &map));
                }
                if (name == "x" || name == "y") && args.len() == 1 {
                    if let Expr::Tuple(items) = &args[0] {
                        if items.len() == 2 {
                            return Ok(items[if name == "x" { 0 } else { 1 }].clone());
                        }
                    }
                }
                Ok(Expr::Call(name.clone(), args))
            }
            _ => {
                let mut err = None;
                let out = map_children(e, &mut |c| match self.inline(c, depth) {
                    Ok(v) => v,
                    Err(x) => {
                        err = Some(x);
                        c.clone()
                    }
                });
                match err {
                    Some(x) => Err(x),
                    None => Ok(out),
                }
            }
        }
    }

    fn function(&self, expr: &Expr, param: &str) -> Result<Geo, String> {
        let inlined = self.inline(expr, 0)?;
        let body = if param == "x" { inlined } else { substitute(&inlined, param, &var("x")) };
        let f = m::compile(&body, &["x"], self.env).map_err(|e| e.0)?;
        Ok(Geo::Function { expr: body, f, dom: None })
    }

    fn number(&self, e: &Expr) -> Result<f64, String> {
        match self.geo_of(e, "x")? {
            Geo::Number(v) => Ok(v),
            other => Err(format!("Zahl erwartet, {} gefunden", other.kind_name())),
        }
    }

    fn point(&self, e: &Expr) -> Result<Pt, String> {
        match self.geo_of(e, "x")? {
            Geo::Point(p) => Ok(p),
            Geo::Points(ps) if ps.len() == 1 => Ok(ps[0]),
            other => Err(format!("Punkt erwartet, {} gefunden", other.kind_name())),
        }
    }

    fn func(&self, e: &Expr) -> Result<(Expr, plotwerk_math::Compiled), String> {
        match self.geo_of(e, "x")? {
            Geo::Function { expr, f, .. } => Ok((expr, f)),
            Geo::Line { a, b, c } if b.abs() > 1e-12 => {
                let expr = m::simplify(&crate::expr_util::sub(num(c / b), crate::expr_util::mul(num(a / b), var("x"))));
                let f = m::compile(&expr, &["x"], &NoCtx).map_err(|e| e.0)?;
                Ok((expr, f))
            }
            other => Err(format!("Funktion erwartet, {} gefunden", other.kind_name())),
        }
    }

    fn geo_of(&self, e: &Expr, param: &str) -> Result<Geo, String> {
        match e {
            Expr::Var(n) => {
                if let Some(g) = self.geos.get(n) {
                    return Ok(g.clone());
                }
                if self.env.functions.contains_key(n) {
                    return self.function(&Expr::Call(n.clone(), vec![var("x")]), "x");
                }
            }
            Expr::Call(name, args) if is_command(name) => return self.command(name, args),
            Expr::Call(name, _) if is_relation(name) => return self.region(e),
            Expr::Tuple(items) if items.len() == 2 => {
                return Ok(Geo::Point([self.number(&items[0])?, self.number(&items[1])?]));
            }
            Expr::Tuple(_) => return Err("Nur 2D-Punkte werden unterstützt".into()),
            Expr::List(items) => {
                let pts: Vec<Pt> = items.iter().map(|i| self.point(i)).collect::<Result<_, _>>()?;
                return Ok(Geo::Points(pts));
            }
            _ => {}
        }
        let inlined = self.inline(e, 0)?;
        if contains_var(&inlined, param) {
            return self.function(e, param);
        }
        if contains_var(&inlined, "y") {
            return Err("Gleichung erwartet (z. B. y = ...)".into());
        }
        let v = m::eval(&inlined, self.env).map_err(|e| e.0)?;
        Ok(Geo::Number(v))
    }

    fn region(&self, e: &Expr) -> Result<Geo, String> {
        let inlined = self.inline(e, 0)?;
        let cond = m::compile(&inlined, &["x", "y"], self.env).map_err(|e| e.0)?;
        let mut bounds = Vec::new();
        fn leaves(e: &Expr, out: &mut Vec<(Expr, bool)>) {
            if let Expr::Call(op, args) = e {
                match op.as_str() {
                    "<" | ">" | "<=" | ">=" if args.len() == 2 => {
                        out.push((crate::expr_util::sub(args[0].clone(), args[1].clone()), op == "<" || op == ">"));
                    }
                    "and" | "or" | "not" => args.iter().for_each(|a| leaves(a, out)),
                    _ => {}
                }
            }
        }
        let mut ls = Vec::new();
        leaves(&inlined, &mut ls);
        for (b, strict) in ls {
            if let Ok(c) = m::compile(&b, &["x", "y"], self.env) {
                bounds.push((c, strict));
            }
        }
        Ok(Geo::Region { cond, bounds })
    }

    fn equation(&self, lhs: &Expr, rhs: &Expr) -> Result<Geo, String> {
        let l = self.inline(lhs, 0)?;
        let r = self.inline(rhs, 0)?;
        if l == var("y") && !contains_var(&r, "y") {
            return self.function(&r, "x");
        }
        if r == var("y") && !contains_var(&l, "y") {
            return self.function(&l, "x");
        }
        let f = m::simplify(&crate::expr_util::sub(l, r));
        let has_xy = |e: &Expr| contains_var(e, "x") || contains_var(e, "y");
        let fx = m::simplify(&m::derivative(&f, "x"));
        let fy = m::simplify(&m::derivative(&f, "y"));
        let compiled = m::compile(&f, &["x", "y"], self.env).map_err(|e| e.0)?;
        if !has_xy(&fx) && !has_xy(&fy) {
            let a = m::eval(&fx, self.env).map_err(|e| e.0)?;
            let b = m::eval(&fy, self.env).map_err(|e| e.0)?;
            if a.abs() < 1e-14 && b.abs() < 1e-14 {
                return Err("Keine Gerade".into());
            }
            return Ok(Geo::Line { a, b, c: -compiled.eval(&[0.0, 0.0]) });
        }
        let fxx = m::simplify(&m::derivative(&fx, "x"));
        let fyy = m::simplify(&m::derivative(&fy, "y"));
        let fxy = m::simplify(&m::derivative(&fx, "y"));
        if !has_xy(&fxx) && !has_xy(&fyy) && !has_xy(&fxy) {
            let (kxx, kyy, kxy) = (
                m::eval(&fxx, self.env).unwrap_or(f64::NAN),
                m::eval(&fyy, self.env).unwrap_or(f64::NAN),
                m::eval(&fxy, self.env).unwrap_or(f64::NAN),
            );
            if kxx.abs() > 1e-12 && (kxx - kyy).abs() < 1e-12 && kxy.abs() < 1e-12 {
                let k = kxx / 2.0;
                let fx0 = m::compile(&fx, &["x", "y"], self.env).map_err(|e| e.0)?.eval(&[0.0, 0.0]);
                let fy0 = m::compile(&fy, &["x", "y"], self.env).map_err(|e| e.0)?.eval(&[0.0, 0.0]);
                let c = [-fx0 / (2.0 * k), -fy0 / (2.0 * k)];
                let r2 = c[0] * c[0] + c[1] * c[1] - compiled.eval(&[0.0, 0.0]) / k;
                if r2 > 0.0 {
                    return Ok(Geo::Circle { c, r: r2.sqrt() });
                }
            }
        }
        Ok(Geo::Implicit { expr: f, f: compiled })
    }

    fn command(&self, name: &str, args: &[Expr]) -> Result<Geo, String> {
        let key = command_key(name);
        let n = args.len();
        let bad = || Err(format!("Falsche Argumente für {name}"));
        match key.as_str() {
            "point" | "punkt" if n == 2 => {
                if let Expr::Var(parent) = &args[0] {
                    if let Some(g) = self.geos.get(parent) {
                        if g.is_curve() {
                            let t = self.number(&args[1])?;
                            return geo::point_on(g, t).map(Geo::Point).ok_or_else(|| "Punkt liegt außerhalb des Objekts".to_string());
                        }
                    }
                }
                Ok(Geo::Point([self.number(&args[0])?, self.number(&args[1])?]))
            }
            "line" | "gerade" if n == 2 => {
                let p = self.point(&args[0])?;
                if let Ok(q) = self.point(&args[1]) {
                    if dist(p, q) < 1e-14 {
                        return Err("Punkte sind identisch".into());
                    }
                    let (a, b, c) = line_through(p, q);
                    return Ok(Geo::Line { a, b, c });
                }
                let (a, b, _) = self.geo_of(&args[1], "x")?.as_line().ok_or("Gerade erwartet")?;
                Ok(Geo::Line { a, b, c: a * p[0] + b * p[1] })
            }
            "segment" | "strecke" if n == 2 => Ok(Geo::Segment(self.point(&args[0])?, self.point(&args[1])?)),
            "circle" | "kreis" if n == 2 => {
                let c = self.point(&args[0])?;
                let r = match self.point(&args[1]) {
                    Ok(p) => dist(c, p),
                    Err(_) => self.number(&args[1])?,
                };
                if r <= 0.0 {
                    return Err("Radius muss positiv sein".into());
                }
                Ok(Geo::Circle { c, r })
            }
            "circle" | "kreis" if n == 3 => {
                let (a, b, c) = (self.point(&args[0])?, self.point(&args[1])?, self.point(&args[2])?);
                let d = 2.0 * (a[0] * (b[1] - c[1]) + b[0] * (c[1] - a[1]) + c[0] * (a[1] - b[1]));
                if d.abs() < 1e-14 {
                    return Err("Punkte liegen auf einer Geraden".into());
                }
                let s = |p: Pt| p[0] * p[0] + p[1] * p[1];
                let ux = (s(a) * (b[1] - c[1]) + s(b) * (c[1] - a[1]) + s(c) * (a[1] - b[1])) / d;
                let uy = (s(a) * (c[0] - b[0]) + s(b) * (a[0] - c[0]) + s(c) * (b[0] - a[0])) / d;
                Ok(Geo::Circle { c: [ux, uy], r: dist([ux, uy], a) })
            }
            "polygon" | "vieleck" if n >= 3 => {
                Ok(Geo::Polygon(args.iter().map(|a| self.point(a)).collect::<Result<_, _>>()?))
            }
            "midpoint" | "mittelpunkt" => match n {
                2 => {
                    let (p, q) = (self.point(&args[0])?, self.point(&args[1])?);
                    Ok(Geo::Point([(p[0] + q[0]) / 2.0, (p[1] + q[1]) / 2.0]))
                }
                1 => match self.geo_of(&args[0], "x")? {
                    Geo::Segment(p, q) => Ok(Geo::Point([(p[0] + q[0]) / 2.0, (p[1] + q[1]) / 2.0])),
                    Geo::Circle { c, .. } => Ok(Geo::Point(c)),
                    _ => bad(),
                },
                _ => bad(),
            },
            "distance" | "abstand" if n == 2 => {
                let p = self.point(&args[0])?;
                if let Ok(q) = self.point(&args[1]) {
                    return Ok(Geo::Number(dist(p, q)));
                }
                let (a, b, c) = self.geo_of(&args[1], "x")?.as_line().ok_or("Punkt oder Gerade erwartet")?;
                Ok(Geo::Number((a * p[0] + b * p[1] - c).abs() / (a * a + b * b).sqrt()))
            }
            "length" | "laenge" if n == 1 => match self.geo_of(&args[0], "x")? {
                Geo::Segment(p, q) => Ok(Geo::Number(dist(p, q))),
                Geo::Circle { r, .. } => Ok(Geo::Number(std::f64::consts::TAU * r)),
                Geo::Polygon(pts) => Ok(Geo::Number((0..pts.len()).map(|i| dist(pts[i], pts[(i + 1) % pts.len()])).sum())),
                Geo::Point(p) => Ok(Geo::Number(dist(p, [0.0, 0.0]))),
                _ => bad(),
            },
            "area" | "flaeche" if n == 1 => match self.geo_of(&args[0], "x")? {
                Geo::Polygon(pts) => Ok(Geo::Number(polygon_area(&pts))),
                Geo::Circle { r, .. } => Ok(Geo::Number(std::f64::consts::PI * r * r)),
                _ => bad(),
            },
            "slope" | "steigung" if n == 1 => {
                let (a, b, _) = self.geo_of(&args[0], "x")?.as_line().ok_or("Gerade erwartet")?;
                if b.abs() < 1e-14 {
                    return Err("Senkrechte Gerade hat keine Steigung".into());
                }
                Ok(Geo::Number(-a / b))
            }
            "angle" | "winkel" if n == 3 => {
                let (a, b, c) = (self.point(&args[0])?, self.point(&args[1])?, self.point(&args[2])?);
                if dist(a, b) < 1e-14 || dist(c, b) < 1e-14 {
                    return Err("Winkel nicht definiert".into());
                }
                let a0 = angle_of(b, a);
                Ok(Geo::Angle { v: b, a0, sweep: norm_angle(angle_of(b, c) - a0) })
            }
            "angle" | "winkel" if n == 2 => {
                let g1 = self.geo_of(&args[0], "x")?;
                let g2 = self.geo_of(&args[1], "x")?;
                let (a1, b1, _) = g1.as_line().ok_or("Gerade erwartet")?;
                let (a2, b2, _) = g2.as_line().ok_or("Gerade erwartet")?;
                let v = intersect(&g1, &g2)?.first().copied().ok_or("Geraden sind parallel")?;
                let d1 = (a1).atan2(-b1);
                let d2 = (a2).atan2(-b2);
                let a0 = norm_angle(d1);
                let sweep = norm_angle(d2 - d1) % std::f64::consts::PI;
                Ok(Geo::Angle { v, a0, sweep })
            }
            "ray" | "halbgerade" if n == 2 => {
                let p = self.point(&args[0])?;
                let q = match self.point(&args[1]) {
                    Ok(q) => q,
                    Err(_) => {
                        let (a, b, _) = self.geo_of(&args[1], "x")?.as_line().ok_or("Punkt oder Gerade erwartet")?;
                        [p[0] - b, p[1] + a]
                    }
                };
                if dist(p, q) < 1e-14 {
                    return Err("Punkte sind identisch".into());
                }
                Ok(Geo::Ray(p, q))
            }
            "vector" | "vektor" => match n {
                1 => match self.geo_of(&args[0], "x")? {
                    Geo::Point(q) => Ok(Geo::Vector([0.0, 0.0], q)),
                    Geo::Segment(p, q) | Geo::Vector(p, q) => Ok(Geo::Vector(p, q)),
                    _ => bad(),
                },
                2 => Ok(Geo::Vector(self.point(&args[0])?, self.point(&args[1])?)),
                _ => bad(),
            },
            "circulararc" | "kreisbogen" | "circularsector" | "kreissektor" if n == 3 => {
                let c = self.point(&args[0])?;
                let a = self.point(&args[1])?;
                let b = self.point(&args[2])?;
                let r = dist(c, a);
                if r < 1e-14 {
                    return Err("Radius ist 0".into());
                }
                let a0 = angle_of(c, a);
                let sweep = norm_angle(angle_of(c, b) - a0);
                Ok(Geo::Arc { c, r, a0, sweep, sector: key.contains("sector") || key.contains("sektor") })
            }
            "reflect" | "spiegle" if n == 2 => {
                let g = self.geo_of(&args[0], "x")?;
                let t = match self.geo_of(&args[1], "x")? {
                    Geo::Point(p) => Affine::reflect_point(p),
                    other => {
                        let (a, b, c) = other.as_line().ok_or("Punkt oder Gerade erwartet")?;
                        Affine::reflect_line(a, b, c)
                    }
                };
                geo::transform(&g, &t)
            }
            "rotate" | "drehe" if n == 2 || n == 3 => {
                let g = self.geo_of(&args[0], "x")?;
                let ang = self.number(&args[1])?;
                let c = if n == 3 { self.point(&args[2])? } else { [0.0, 0.0] };
                geo::transform(&g, &Affine::rotate(ang, c))
            }
            "translate" | "verschiebe" if n == 2 => {
                let g = self.geo_of(&args[0], "x")?;
                let v = match self.geo_of(&args[1], "x")? {
                    Geo::Vector(p, q) => [q[0] - p[0], q[1] - p[1]],
                    Geo::Point(p) => p,
                    _ => return Err("Vektor erwartet".into()),
                };
                geo::transform(&g, &Affine::translate(v))
            }
            "dilate" | "streckung" if n == 2 || n == 3 => {
                let g = self.geo_of(&args[0], "x")?;
                let k = self.number(&args[1])?;
                let c = if n == 3 { self.point(&args[2])? } else { [0.0, 0.0] };
                geo::transform(&g, &Affine::dilate(k, c))
            }
            "function" | "funktion" if n == 3 => {
                let (expr, f) = self.func(&args[0])?;
                let a = self.number(&args[1])?;
                let b = self.number(&args[2])?;
                Ok(Geo::Function { expr, f, dom: Some((a.min(b), a.max(b))) })
            }
            "sequence" | "folge" if n == 4 || n == 5 => {
                let Expr::Var(k) = &args[1] else { return bad() };
                let a = self.number(&args[2])?;
                let b = self.number(&args[3])?;
                let step = if n == 5 { self.number(&args[4])? } else { 1.0 };
                if step == 0.0 || ((b - a) / step) > 10000.0 {
                    return Err("Zu viele Elemente".into());
                }
                let mut pts = Vec::new();
                let mut t = a;
                while (step > 0.0 && t <= b + 1e-9) || (step < 0.0 && t >= b - 1e-9) {
                    let e = crate::expr_util::substitute(&args[0], k, &num(t));
                    match self.geo_of(&e, "x")? {
                        Geo::Point(p) => pts.push(p),
                        Geo::Number(v) => pts.push([t, v]),
                        _ => return Err("Folge muss Punkte oder Zahlen liefern".into()),
                    }
                    t += step;
                }
                Ok(Geo::Points(pts))
            }
            "intersect" | "schneide" if n == 2 => {
                let g1 = self.geo_of(&args[0], "x")?;
                let g2 = self.geo_of(&args[1], "x")?;
                let pts = intersect(&g1, &g2)?;
                Ok(points_geo(pts))
            }
            "root" | "nullstelle" | "roots" | "nullstellen" => {
                let (expr, f) = self.func(args.first().ok_or("Funktion fehlt")?)?;
                let xs = if n == 3 {
                    m::find_roots(&|x| f.eval(&[x]), self.number(&args[1])?, self.number(&args[2])?)
                } else if n == 1 {
                    solve_zero(&expr, "x")
                } else {
                    return bad();
                };
                Ok(points_geo(xs.into_iter().map(|x| [x, 0.0]).collect()))
            }
            "extremum" => {
                let (expr, f) = self.func(args.first().ok_or("Funktion fehlt")?)?;
                let d1 = m::simplify(&m::derivative(&expr, "x"));
                let fd = m::compile(&d1, &["x"], &NoCtx).map_err(|e| e.0)?;
                let xs: Vec<f64> = if n == 3 {
                    m::find_extrema(&|x| f.eval(&[x]), self.number(&args[1])?, self.number(&args[2])?)
                        .into_iter()
                        .map(|e| e.x)
                        .collect()
                } else {
                    solve_zero(&d1, "x")
                        .into_iter()
                        .filter(|x| sign_change(&|t| fd.eval(&[t]), *x))
                        .collect()
                };
                Ok(points_geo(xs.into_iter().map(|x| [x, f.eval(&[x])]).collect()))
            }
            "turningpoint" | "wendepunkt" if n == 1 => {
                let (expr, f) = self.func(&args[0])?;
                let d2 = m::simplify(&m::derivative(&m::derivative(&expr, "x"), "x"));
                let fd2 = m::compile(&d2, &["x"], &NoCtx).map_err(|e| e.0)?;
                let xs: Vec<f64> =
                    solve_zero(&d2, "x").into_iter().filter(|x| sign_change(&|t| fd2.eval(&[t]), *x)).collect();
                Ok(points_geo(xs.into_iter().map(|x| [x, f.eval(&[x])]).collect()))
            }
            "derivative" | "ableitung" if n == 1 || n == 2 => {
                let (expr, _) = self.func(&args[0])?;
                let times = if n == 2 { self.number(&args[1])?.round().max(0.0) as usize } else { 1 };
                let mut d = expr;
                for _ in 0..times {
                    d = m::simplify(&m::derivative(&d, "x"));
                }
                let f = m::compile(&d, &["x"], &NoCtx).map_err(|e| e.0)?;
                Ok(Geo::Function { expr: d, f, dom: None })
            }
            "tangent" | "tangente" if n == 2 => {
                let x0 = match self.point(&args[0]) {
                    Ok(p) => p[0],
                    Err(_) => self.number(&args[0])?,
                };
                let (expr, f) = self.func(&args[1])?;
                let d1 = m::simplify(&m::derivative(&expr, "x"));
                let slope = m::compile(&d1, &["x"], &NoCtx).map_err(|e| e.0)?.eval(&[x0]);
                let y0 = f.eval(&[x0]);
                if !slope.is_finite() || !y0.is_finite() {
                    return Err("Tangente an dieser Stelle nicht definiert".into());
                }
                Ok(Geo::Line { a: -slope, b: 1.0, c: y0 - slope * x0 })
            }
            "integral" if n == 1 => {
                let (expr, _) = self.func(&args[0])?;
                let anti = m::integrate(&expr, "x").ok_or("Keine Stammfunktion gefunden")?;
                let f = m::compile(&anti, &["x"], &NoCtx).map_err(|e| e.0)?;
                Ok(Geo::Function { expr: anti, f, dom: None })
            }
            "integral" if n == 3 => {
                let (_, f) = self.func(&args[0])?;
                let a = self.number(&args[1])?;
                let b = self.number(&args[2])?;
                let value = m::integrate_numeric(&|x| f.eval(&[x]), a, b);
                Ok(Geo::Area { f, a, b, value })
            }
            "perpendicular" | "senkrechte" | "orthogonalgerade" if n == 2 => {
                let p = self.point(&args[0])?;
                let (a, b, _) = self.geo_of(&args[1], "x")?.as_line().ok_or("Gerade erwartet")?;
                Ok(Geo::Line { a: -b, b: a, c: -b * p[0] + a * p[1] })
            }
            "parallel" | "parallele" if n == 2 => {
                let p = self.point(&args[0])?;
                let (a, b, _) = self.geo_of(&args[1], "x")?.as_line().ok_or("Gerade erwartet")?;
                Ok(Geo::Line { a, b, c: a * p[0] + b * p[1] })
            }
            _ => bad(),
        }
    }
}

fn sign_change(f: &dyn Fn(f64) -> f64, x: f64) -> bool {
    let h = 1e-4 * x.abs().max(1.0);
    let l = f(x - h);
    let r = f(x + h);
    l.is_finite() && r.is_finite() && l.signum() != r.signum()
}

fn points_geo(pts: Vec<Pt>) -> Geo {
    if pts.len() == 1 { Geo::Point(pts[0]) } else { Geo::Points(pts) }
}

pub fn fmt_param(t: f64) -> String {
    let r = (t * 10000.0).round() / 10000.0;
    m::format_number(if r == 0.0 { 0.0 } else { r })
}

fn rename_ident(source: &str, old: &str, new: &str) -> String {
    let mut out = String::new();
    let mut ident = String::new();
    let flush = |ident: &mut String, out: &mut String| {
        if !ident.is_empty() {
            let base = ident.split("_d").next().unwrap_or(ident).to_string();
            if *ident == old {
                out.push_str(new);
            } else if base == old && ident.len() > base.len() {
                out.push_str(new);
                out.push_str(&ident[base.len()..]);
            } else {
                out.push_str(ident);
            }
            ident.clear();
        }
    };
    for c in source.chars() {
        if c.is_alphanumeric() || c == '_' {
            ident.push(c);
        } else {
            flush(&mut ident, &mut out);
            out.push(c);
        }
    }
    flush(&mut ident, &mut out);
    out
}

pub fn fmt_deg(rad: f64) -> String {
    let d = (rad.to_degrees() * 100.0).round() / 100.0;
    m::format_number(d)
}
