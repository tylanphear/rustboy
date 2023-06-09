use crate::debug_break;

pub mod regs {
    pub const DIV: u16 = 0xFF04;
    pub const TIMA: u16 = 0xFF05;
    pub const TMA: u16 = 0xFF06;
    pub const TAC: u16 = 0xFF07;
}

#[derive(Debug, Default)]
pub struct Timer {
    clock: u16,
    tima: u8,
    tma: u8,
    tac: u8,
}

impl Timer {
    pub fn reset(&mut self) {
        *self = Default::default();
    }

    pub fn tick(&mut self) -> bool {
        self.clock = self.clock.wrapping_add(4);

        let mut need_interrupt = false;
        if self.enabled() {
            if self.clock % self.clock_rate() == 0 {
                let (new_tima, overflowed) = self.tima.overflowing_add(1);
                if overflowed {
                    need_interrupt = true;
                    self.tima = self.tma;
                } else {
                    self.tima = new_tima;
                }
            }
        }
        need_interrupt
    }

    pub fn load(&self, address: u16) -> u8 {
        match address {
            regs::DIV => self.clock.to_le_bytes()[1],
            regs::TIMA => self.tima,
            regs::TMA => self.tma,
            regs::TAC => self.tac,
            _ => todo!("timer registers"),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            regs::DIV => self.clock = 0,
            regs::TIMA => self.tima = val,
            regs::TMA => self.tma = val,
            regs::TAC => self.tac = val & 0x7,
            _ => todo!("timer registers"),
        }
    }

    fn enabled(&self) -> bool {
        self.tac & 0x1 != 0
    }

    fn clock_rate(&self) -> u16 {
        match self.tac & 0x3 {
            0b00 => 1024, // clocks   4096 Hz
            0b01 => 16,   // clocks 262144 Hz
            0b10 => 64,   // clocks  65536 Hz
            0b11 => 256,  // clocks  16386 Hz
            _ => unreachable!(),
        }
    }

    pub fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(
            out,
            "DIV :  {:02X} ({:04X})",
            self.clock.to_le_bytes()[1],
            regs::DIV
        )?;
        writeln!(out, "TIMA:  {:02X} ({:04X})", self.tima, regs::TIMA)?;
        writeln!(out, "TMA :  {:02X} ({:04X})", self.tma, regs::TMA)?;
        writeln!(out, "TAC : {:03b} ({:04X})", self.tac, regs::TAC)?;
        writeln!(out)?;
        writeln!(
            out,
            "CLOCK: {:04X} (rate: {})",
            self.clock,
            self.clock_rate()
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_name() {}
}
