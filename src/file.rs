use crate::doc::{ObjDef, SliderRange};
use eframe::egui::Color32;

pub struct Saved {
    pub mode: String,
    pub view: Option<(f64, f64, f64, bool, bool)>,
    pub defs: Vec<ObjDef>,
    pub cas: Vec<String>,
    pub cells: Vec<String>,
    pub space: Vec<String>,
    pub plain: Vec<String>,
}

pub fn to_text(mode: &str, view: (f64, f64, f64, bool, bool), defs: &[ObjDef], cas: &[String], cells: &[String], space: &[String]) -> String {
    let mut out = String::from("plotwerk 1\n");
    out.push_str(&format!("mode {mode}\n"));
    out.push_str(&format!("view {} {} {} {} {}\n", view.0, view.1, view.2, view.3 as u8, view.4 as u8));
    for d in defs {
        let slider = d.slider.as_ref().map(|s| format!("{},{},{}", s.min, s.max, s.step)).unwrap_or_else(|| "-".into());
        let [r, g, b, _] = d.color.to_array();
        out.push_str(&format!(
            "obj {}\t{}\t{}\t{}\t{},{},{}\t{}\t{}\n",
            d.name,
            d.params.join(","),
            d.equation as u8,
            d.visible as u8,
            r,
            g,
            b,
            slider,
            d.source.replace('\n', " ")
        ));
    }
    for c in cas {
        out.push_str(&format!("cas {}\n", c.replace('\n', " ")));
    }
    for c in cells {
        out.push_str(&format!("cell {c}\n"));
    }
    for c in space {
        out.push_str(&format!("space {c}\n"));
    }
    out
}

pub fn from_text(text: &str) -> Result<Saved, String> {
    let mut saved = Saved { mode: "graph".into(), view: None, defs: Vec::new(), cas: Vec::new(), cells: Vec::new(), space: Vec::new(), plain: Vec::new() };
    let mut lines = text.lines();
    let first = lines.next().unwrap_or("").trim_start_matches('\u{feff}').trim();
    if first != "plotwerk 1" {
        saved.plain = text.lines().map(|l| l.trim().to_string()).filter(|l| !l.is_empty() && !l.starts_with('#')).collect();
        return Ok(saved);
    }
    for line in lines {
        if let Some(rest) = line.strip_prefix("mode ") {
            saved.mode = rest.trim().to_string();
        } else if let Some(rest) = line.strip_prefix("view ") {
            let v: Vec<f64> = rest.split_whitespace().filter_map(|s| s.parse().ok()).collect();
            if v.len() == 5 {
                saved.view = Some((v[0], v[1], v[2], v[3] != 0.0, v[4] != 0.0));
            }
        } else if let Some(rest) = line.strip_prefix("obj ") {
            let f: Vec<&str> = rest.splitn(7, '\t').collect();
            if f.len() != 7 {
                return Err(format!("Ungültige Zeile: {line}"));
            }
            let rgb: Vec<u8> = f[4].split(',').filter_map(|s| s.parse().ok()).collect();
            let color = if rgb.len() == 3 { Color32::from_rgb(rgb[0], rgb[1], rgb[2]) } else { Color32::BLACK };
            let slider = if f[5] == "-" {
                None
            } else {
                let s: Vec<f64> = f[5].split(',').filter_map(|s| s.parse().ok()).collect();
                (s.len() == 3).then(|| SliderRange { min: s[0], max: s[1], step: s[2] })
            };
            let style = if f.len() == 8 {
                let v: Vec<f32> = f[6].split(',').filter_map(|s| s.parse().ok()).collect();
                if v.len() == 3 { ObjDef { width: v[0], dash: v[1] as u8, label: v[2] != 0.0, ..ObjDef::default() } } else { ObjDef::default() }
            } else {
                ObjDef::default()
            };
            saved.defs.push(ObjDef {
                name: f[0].to_string(),
                params: if f[1].is_empty() { vec![] } else { f[1].split(',').map(str::to_string).collect() },
                equation: f[2] == "1",
                visible: f[3] == "1",
                color,
                slider,
                source: f[f.len() - 1].to_string(),
                ..style
            });
        } else if let Some(rest) = line.strip_prefix("cas ") {
            saved.cas.push(rest.to_string());
        } else if let Some(rest) = line.strip_prefix("cell ") {
            saved.cells.push(rest.to_string());
        } else if let Some(rest) = line.strip_prefix("space ") {
            saved.space.push(rest.to_string());
        }
    }
    Ok(saved)
}
