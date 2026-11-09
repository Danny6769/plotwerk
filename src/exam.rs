use chrono::{DateTime, Local};
use std::time::{Duration, Instant};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Preset {
    Full,
    NoCas,
}

impl Preset {
    pub fn label(self) -> &'static str {
        match self {
            Preset::Full => "Standard (Grafik, Geometrie, CAS)",
            Preset::NoCas => "Ohne CAS (Grafik, Geometrie)",
        }
    }

    pub fn cas_allowed(self) -> bool {
        matches!(self, Preset::Full)
    }
}

#[derive(Clone, Debug)]
pub struct Event {
    pub time: DateTime<Local>,
    pub text: String,
    pub violation: bool,
}

pub struct Session {
    pub preset: Preset,
    pub start: DateTime<Local>,
    pub started: Instant,
    pub events: Vec<Event>,
    pub violated: bool,
    was_focused: bool,
    was_fullscreen: bool,
    was_minimized: bool,
}

pub struct Report {
    pub preset: Preset,
    pub start: DateTime<Local>,
    pub end: DateTime<Local>,
    pub duration: Duration,
    pub events: Vec<Event>,
    pub violated: bool,
    pub code: String,
}

pub enum Exam {
    Off,
    Setup(Preset),
    Running(Session),
    ConfirmEnd(Session),
    Finished(Report),
}

pub fn fmt_duration(d: Duration) -> String {
    let s = d.as_secs();
    format!("{:02}:{:02}:{:02}", s / 3600, (s / 60) % 60, s % 60)
}

impl Session {
    pub fn new(preset: Preset) -> Self {
        let now = Local::now();
        Self {
            preset,
            start: now,
            started: Instant::now(),
            events: vec![Event { time: now, text: "Prüfung gestartet".into(), violation: false }],
            violated: false,
            was_focused: true,
            was_fullscreen: false,
            was_minimized: false,
        }
    }

    pub fn elapsed(&self) -> Duration {
        self.started.elapsed()
    }

    fn log(&mut self, text: &str, violation: bool) {
        self.events.push(Event { time: Local::now(), text: text.into(), violation });
        if violation {
            self.violated = true;
        }
    }

    pub fn monitor(&mut self, ctx: &eframe::egui::Context) {
        use eframe::egui::{ViewportCommand, WindowLevel};
        let (focused, fullscreen, minimized) = ctx.input(|i| {
            let vp = i.viewport();
            (i.focused, vp.fullscreen.unwrap_or(false), vp.minimized.unwrap_or(false))
        });
        let grace = self.started.elapsed() > Duration::from_millis(1500);
        if grace {
            if self.was_focused && !focused {
                self.log("Programm verlassen (Fokus verloren)", true);
            } else if !self.was_focused && focused {
                self.log("Zurück im Programm", false);
            }
            if self.was_fullscreen && !fullscreen {
                self.log("Vollbild verlassen", true);
                ctx.send_viewport_cmd(ViewportCommand::Fullscreen(true));
                ctx.send_viewport_cmd(ViewportCommand::WindowLevel(WindowLevel::AlwaysOnTop));
            }
            if !self.was_minimized && minimized {
                self.log("Fenster minimiert", true);
            }
            self.was_focused = focused;
            self.was_fullscreen = fullscreen;
            self.was_minimized = minimized;
        } else {
            self.was_focused = focused;
            self.was_fullscreen = fullscreen || self.was_fullscreen;
        }
        ctx.request_repaint_after(Duration::from_millis(250));
    }

    pub fn finish(mut self) -> Report {
        let end = Local::now();
        self.events.push(Event { time: end, text: "Prüfung beendet".into(), violation: false });
        let duration = self.elapsed();
        let mut text = format!("{}|{}|{}|{}", self.start.to_rfc3339(), end.to_rfc3339(), duration.as_secs(), self.violated);
        for e in &self.events {
            text.push_str(&format!("|{}|{}", e.time.to_rfc3339(), e.text));
        }
        let mut h: u64 = 0xcbf29ce484222325;
        for b in text.bytes() {
            h ^= b as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
        let code = format!("{:04X}-{:04X}-{:04X}", (h >> 48) & 0xffff, (h >> 32) & 0xffff, (h >> 16) & 0xffff);
        Report { preset: self.preset, start: self.start, end, duration, events: self.events, violated: self.violated, code }
    }
}
