#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod app;
mod cas_panel;
mod doc;
mod exam;
mod expr_util;
mod file;
mod geo;
mod icons;
mod pretty;
mod prob;
mod space3d;
mod table;
mod theme;
mod tools;
mod view;

use eframe::egui;

fn main() -> eframe::Result {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_title("Plotwerk")
            .with_inner_size([1360.0, 860.0])
            .with_min_inner_size([960.0, 620.0]),
        ..Default::default()
    };
    eframe::run_native("Plotwerk", options, Box::new(|cc| Ok(Box::new(app::PlotwerkApp::new(cc)))))
}
