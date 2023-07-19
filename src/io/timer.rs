use serde::{Deserialize, Serialize};

use crate::utils::MClock;

pub mod regs {
    pub const DIV: u16 = 0xFF04;
    pub const TIMA: u16 = 0xFF05;
    pub const TMA: u16 = 0xFF06;
    pub const TAC: u16 = 0xFF07;
}

pub const DIV_CLOCK_FREQUENCY: u64 = 16384;
pub const DIV_CLOCK_PERIOD: u64 =
    crate::cpu::M_CLOCK_FREQUENCY / DIV_CLOCK_FREQUENCY;

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Timer {
    clock: MClock,
    div: u8,
    tima: u8,
    tma: u8,
    tac: u8,
}

impl Timer {
    pub fn reset(&mut self) {
        *self = Default::default();
    }

    pub fn tick(&mut self, interrupts: &mut crate::cpu::Interrupts) {
        let (clock, should_tick) = self.clock.tick();
        if !should_tick {
            return;
        }

        // The main clock ticks at 4Mhz. This means a tick occurs every
        // 1/2^22 seconds. The DIV clock is expected to tick every 16384 Hz.
        // This means a tick every 1/16384 seconds. This means we need to tick one
        // DIV clock every 2^22/16384 == 64 main clock ticks.

        if clock % DIV_CLOCK_PERIOD == 0 {
            self.div = self.div.wrapping_add(1);
        }

        if !self.enabled() {
            return;
        }

        if clock % self.clock_rate() == 0 {
            let (new_tima, overflowed) = self.tima.overflowing_add(1);
            if overflowed {
                interrupts.request_timer();
                self.tima = self.tma;
            } else {
                self.tima = new_tima;
            }
        }
    }

    pub fn load(&self, address: u16) -> u8 {
        match address {
            regs::DIV => self.div,
            regs::TIMA => self.tima,
            regs::TMA => self.tma,
            regs::TAC => self.tac,
            _ => unreachable!(),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            regs::DIV => {
                crate::debug_log!("DIV written to!");
                self.div = 0;
            }
            regs::TIMA => self.tima = val,
            regs::TMA => self.tma = val,
            regs::TAC => self.tac = val & 0x7,
            _ => unreachable!(),
        }
    }

    fn enabled(&self) -> bool {
        self.tac & 0x1 != 0
    }

    fn clock_rate(&self) -> u64 {
        match self.tac & 0b11 {
            0b00 => 1024, // clocks   4096 Hz
            0b01 => 16,   // clocks 262144 Hz
            0b10 => 64,   // clocks  65536 Hz
            0b11 => 256,  // clocks  16386 Hz
            _ => unreachable!(),
        }
    }
}

impl crate::utils::Dump for Timer {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "DIV :  {:02X} ({:04X})", self.div, regs::DIV)?;
        writeln!(out, "TIMA:  {:02X} ({:04X})", self.tima, regs::TIMA)?;
        writeln!(out, "TMA :  {:02X} ({:04X})", self.tma, regs::TMA)?;
        writeln!(out, "TAC : {:03b} ({:04X})", self.tac, regs::TAC)?;
        writeln!(out)?;
        writeln!(
            out,
            "CLOCK: {:04X} (rate: {})",
            self.clock.val(),
            self.clock_rate()
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn div_ticks_every_16384_hz() {
        const TICKS_FOR_16384_HZ: u64 = crate::cpu::T_CLOCK_FREQUENCY / 16384;

        let mut timer = Timer::default();
        for _ in 0..TICKS_FOR_16384_HZ {
            assert_eq!(timer.div, 0);
            timer.tick(&mut crate::cpu::Interrupts::default());
        }
        assert_eq!(timer.div, 1);
    }
}
