use once_cell::sync::Lazy;
use std::sync::Mutex;

use crate::utils::constants::SIXTY_FOUR_K;

static LOG: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new(String::new()));
const MAX_LOG_SIZE: usize = SIXTY_FOUR_K;

pub fn push_log<S: AsRef<str>>(data: S) {
    let mut log = LOG.lock().expect("couldn't get debug log!");
    log.push_str(data.as_ref());
    log.push_str("\n");
    if log.len() >= MAX_LOG_SIZE {
        let first_ten_lines = log
            .char_indices()
            .filter(|(_, c)| *c == '\n')
            .nth(10)
            .map(|(n, _)| n)
            .unwrap_or(0);
        log.drain(0..first_ten_lines);
    }
}

pub fn log() -> String {
    LOG.lock().expect("couldn't get debug log!").clone()
}

pub fn reset_log() {
    let _ = LOG.lock().map(|mut it| it.clear());
}

#[macro_export]
macro_rules! debug_log {
    ($($args:tt)*) => {{
        $crate::debug::push_log(format!($($args)*));
    }}
}
