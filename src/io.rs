pub mod apu;
pub mod joypad;
pub mod ppu;
pub mod timer;
use apu::APU;
use joypad::Joypad;
use ppu::LCDController;
use serde::{Deserialize, Serialize};
use timer::Timer;

use crate::utils::Clock;

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct IOController {
    pub lcd: LCDController,
    pub apu: APU,
    pub timer: Timer,
    pub joypad: Joypad,
}

impl IOController {
    pub fn reset(&mut self) {
        self.lcd.reset();
        self.timer.reset();
        self.apu.reset();
    }

    pub fn tick(&mut self, clock: &Clock) -> crate::cpu::Interrupts {
        use timer::regs::DIV;

        let mut interrupts = crate::cpu::Interrupts::default();
        self.lcd.tick(clock, &mut interrupts);
        self.timer.tick(clock, &mut interrupts);
        self.joypad.tick(&mut interrupts);
        self.apu.tick(clock, self.timer.load(DIV));
        interrupts
    }

    pub fn load(&self, address: u16) -> u8 {
        match address {
            0xFF00 => self.joypad.load(address),
            0xFF01..=0xFF02 =>
            /* todo: serial */
            {
                crate::debug_log!("todo: serial (load {address:04X})");
                0
            }
            0xFF04..=0xFF07 => self.timer.load(address),
            0xFF10..=0xFF3F => self.apu.load(address),
            0xFF40..=0xFF4B => self.lcd.load(address),
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
            0xFF01..=0xFF02 =>
            /* todo: serial */
            {
                crate::debug_log!(
                    "todo: serial (store {val:02X} {address:04X})"
                );
            }
            0xFF04..=0xFF07 => self.timer.store(address, val),
            0xFF10..=0xFF3F => self.apu.store(address, val),
            0xFF40..=0xFF4B => self.lcd.store(address, val),
            0xFF03 | 0xFF08..=0xFF0E => { /* unused */ }
            _ => panic!("{address:04X} write outside of IO range!"),
        }
    }
}
