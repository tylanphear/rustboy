pub mod log {
    use crate::utils::{constants::SIXTY_FOUR_K, BoundedLog};
    use once_cell::sync::Lazy;
    use std::sync::Mutex;

    #[derive(Debug, Default)]
    pub struct DebugLog {
        pub data: BoundedLog<SIXTY_FOUR_K, 1000>,
        pub dirty: bool,
    }

    pub static LOG: Lazy<Mutex<DebugLog>> = Lazy::new(Mutex::default);

    pub fn push<S: AsRef<str>>(data: S) {
        let mut log = LOG.lock().unwrap();
        log.data.push(data.as_ref());
        log.data.push("\n");
        log.dirty = true;
    }

    pub fn reset() {
        let mut log = LOG.lock().unwrap();
        log.data.clear();
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
            $crate::debug::log::LOG.lock().unwrap().data.as_str()
        }};
    }

    #[macro_export]
    macro_rules! debug_log_was_written {
        ($($args:tt)*) => {{
            let mut log = $crate::debug::log::LOG.lock().unwrap();
            let res = log.dirty;
            log.dirty = false;
            res
        }};
    }
}

pub mod break_ {
    use std::sync::atomic::AtomicBool;
    pub static TOGGLE: AtomicBool = AtomicBool::new(false);
    pub fn check_and_reset() -> bool {
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
