pub mod log {
    use crate::utils::{constants::SIXTY_FOUR_K, BoundedLog};
    use once_cell::sync::Lazy;
    use parking_lot::RwLock;
    pub static LOG: Lazy<RwLock<BoundedLog<SIXTY_FOUR_K, 1000>>> =
        Lazy::new(|| RwLock::new(Default::default()));

    pub fn push<S: AsRef<str>>(data: S) {
        let mut log = LOG.write();
        log.push(data.as_ref());
        log.push("\n");
    }

    pub fn reset() {
        LOG.write().clear()
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
            $crate::debug::log::LOG.read().as_str()
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
