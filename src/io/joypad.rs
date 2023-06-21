#[derive(Debug, Serialize, Deserialize)]
pub struct Joypad {
    state: u8,
    select: u8,
    need_interrupt: bool,
}

impl Default for Joypad {
    fn default() -> Self {
        Joypad {
            state: 0xFF,
            select: 0b00,
            need_interrupt: false,
        }
    }
}

#[rustfmt::skip]
mod keys {
    pub const DOWN:   u8 = 0b1000_0000;
    pub const UP:     u8 = 0b0100_0000;
    pub const LEFT:   u8 = 0b0010_0000;
    pub const RIGHT:  u8 = 0b0001_0000;
    pub const START:  u8 = 0b0000_1000;
    pub const SELECT: u8 = 0b0000_0100;
    pub const B:      u8 = 0b0000_0010;
    pub const A:      u8 = 0b0000_0001;
}
pub use keys::*;
use serde::{Deserialize, Serialize};

impl Joypad {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn up(&mut self, key: u8) {
        self.state |= key;
    }

    pub fn down(&mut self, key: u8) {
        self.state &= !key;
        self.need_interrupt = true;
    }

    pub fn tick(&mut self) -> bool {
        std::mem::take(&mut self.need_interrupt)
    }

    pub fn load(&self, address: u16) -> u8 {
        assert_eq!(address, 0xFF00);
        let buttons = (self.state >> 0) & 0b1111;
        let dpad = (self.state >> 4) & 0b1111;
        match self.select {
            0b11 => 0b1111_1111,
            0b10 => 0b1100_0000 | self.select << 4 | dpad,
            0b01 => 0b1100_0000 | self.select << 4 | buttons,
            0b00 => 0b1100_0000 | self.select << 4 | dpad | buttons,
            _ => unreachable!(),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        assert_eq!(address, 0xFF00);
        self.select = (val >> 4) & 0b11;
    }

    pub fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "        DULRSLBA")?;
        writeln!(out, "state:  {:08b}", self.state)?;
        writeln!(out, "select: {:02b}", self.select)?;
        writeln!(out, "interrupt: {}", self.need_interrupt)?;
        Ok(())
    }
}
