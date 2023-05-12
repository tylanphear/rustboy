pub mod log {
    use crate::utils::{constants::SIXTY_FOUR_K, BoundedLog};
    use once_cell::sync::Lazy;
    use std::sync::Mutex;
    const MAX_SIZE: usize = SIXTY_FOUR_K;
    pub static LOG: Lazy<Mutex<BoundedLog<MAX_SIZE, 1000>>> =
        Lazy::new(|| Mutex::new(Default::default()));

    pub fn push<S: AsRef<str>>(data: S) {
        let mut log = LOG.lock().expect("couldn't get debug log!");
        log.push(data.as_ref());
        log.push("\n");
    }

    pub fn reset() {
        let _ = LOG.lock().map(|mut it| it.clear());
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

use std::sync::atomic::AtomicBool;
pub static BREAK: AtomicBool = AtomicBool::new(false);
#[macro_export]
macro_rules! debug_break {
    () => {
        $crate::debug::BREAK.store(true, std::sync::atomic::Ordering::Release);
    };
    ($($args:tt)*) => {{
        $crate::debug_log!($($args)*);
        debug_break!();
    }};
}
