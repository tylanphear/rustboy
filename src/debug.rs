pub mod log {
    use crate::utils::{constants::SIXTY_FOUR_K, BoundedLog};
    use once_cell::sync::Lazy;
    use std::sync::Mutex;
    pub static LOG: Lazy<Mutex<BoundedLog<SIXTY_FOUR_K, 1000>>> =
        Lazy::new(|| Mutex::new(Default::default()));

    pub fn push<S: AsRef<str>>(data: S) {
        let mut log = LOG.lock().expect("couldn't get debug log!");
        log.push(data.as_ref());
        log.push("\n");
    }

    pub fn reset() {
        if let Ok(ref mut log) = LOG.lock() {
            log.clear();
        }
    }

    #[macro_export]
    macro_rules! debug_log {
        ($($args:tt)*) => {{
            $crate::debug::log::push(format!($($args)*));
        }}
    }

    #[macro_export]
    macro_rules! debug_log_as_str {
        ($($args:tt)*) => {{
            $crate::debug::log::LOG.lock().unwrap().as_str()
        }};
    }
}

pub mod break_ {
    use std::sync::atomic::AtomicBool;
    pub static TOGGLE: AtomicBool = AtomicBool::new(false);
    pub fn check_and_unset() -> bool {
        TOGGLE.swap(false, std::sync::atomic::Ordering::AcqRel)
    }
    #[macro_export]
    macro_rules! debug_break {
    () => {
        $crate::debug::break_::TOGGLE.store(true, std::sync::atomic::Ordering::Release);
    };
    ($($args:tt)*) => {{
        $crate::debug_log!($($args)*);
        $crate::debug_break!();
    }};
}
}
