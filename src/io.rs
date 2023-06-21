pub mod joypad;
pub mod lcd;
pub mod timer;
use joypad::Joypad;
use lcd::LCDController;
use serde::{Deserialize, Serialize};
use timer::Timer;

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct IOController {
    pub lcd: LCDController,
    pub timer: Timer,
    pub joypad: Joypad,
}

impl IOController {
    pub fn new() -> IOController {
        Self::default()
    }

    pub fn reset(&mut self) {
        self.lcd.reset();
        self.timer.reset();
    }

    pub fn load(&self, address: u16) -> u8 {
        match address {
            0xFF00 => self.joypad.load(address),
            0xFF01..=0xFF02 =>
            /* todo: serial */
            {
                //crate::debug_log!("todo: serial (load {address:04X})");
                0
            }
            0xFF40..=0xFF4B => self.lcd.load(address),
            0xFF10..=0xFF3F =>
            /* sound controller */
            {
                //crate::debug_log!("todo: sound (load {address:04X})");
                0
            }
            0xFF04..=0xFF07 => self.timer.load(address),
            0xFF03 | 0xFF08..=0xFF0E => {
                /* unused */
                0xFF
            }
            _ => panic!("{address:04X}: read outside of IO range!"),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            0xFF00 => self.joypad.store(address, val),
            0xFF40..=0xFF4B => self.lcd.store(address, val),
            0xFF10..=0xFF3F =>
            /* sound controller */
            {
                //crate::debug_log!("todo: sound (store {address:04X})");
                return;
            }
            0xFF01..=0xFF02 =>
            /* todo: serial */
            {
                //crate::debug_log!("todo: serial (store {address:04X})");
                return;
            }
            0xFF04..=0xFF07 => self.timer.store(address, val),
            0xFF03 | 0xFF08..=0xFF0E => { /* unused */ }
            _ => panic!("{address:04X} write outside of IO range!"),
        }
    }
}
