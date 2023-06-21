use serde::{Deserialize, Serialize};

use crate::utils::constants::{EIGHT_K, SIXTEEN_K};
use crate::utils::mem::Mem;

const ROM_BANK_SIZE: usize = SIXTEEN_K;

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct MBC1 {
    ram_enable: bool,
    bank_mode_select: u8,
    rom_bank_lower5: u8,
    ram_or_rom_upper2: u8,
    sram: Mem<EIGHT_K>,
}

impl MBC1 {
    pub fn reset(&mut self) {
        *self = MBC1::default();
    }

    fn rom_bank_1_addr_base(&self) -> usize {
        // Map bank num 0x00 to 0x01, 0x20 to 0x21, etc.
        let rom_bank_num = (self.rom_bank_lower5 as usize)
            | (self.rom_bank_lower5 == 0) as usize
            | (self.ram_or_rom_upper2 as usize) << 5;
        rom_bank_num * ROM_BANK_SIZE
    }

    pub fn load(&self, address: u16, data: &[u8]) -> u8 {
        match address {
            0xA000..=0xBFFF if !self.ram_enable => 0xFF,
            _ => data[self.addr_to_offset(address)],
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            // RAM Enable
            0x0000..=0x1FFF => match val {
                0xA => self.ram_enable = true,
                0x0 => self.ram_enable = false,
                _ => {}
            },
            // ROM Bank (lower)
            0x2000..=0x3FFF => {
                self.rom_bank_lower5 = val & 0x1F;
            }
            // RAM Bank (or upper bits of ROM Bank)
            0x4000..=0x5FFF => {
                self.ram_or_rom_upper2 = val & 0x3;
            }
            // ROM/RAM Mode
            0x6000..=0x7FFF => {
                self.bank_mode_select = val & 0x1;
                return;
            }
            0xA000..=0xBFFF => {
                self.sram[address - 0xA000] = val;
            }
            _ => unreachable!(
                "Store outside of MBC range? ({val:02X} to {address:04X})"
            ),
        }
    }

    pub fn addr_to_offset(&self, address: u16) -> usize {
        match address {
            0x0000..=0x3FFF => match self.bank_mode_select {
                0 => crate::utils::lo_bits_of(address as usize, 14),
                1 => {
                    crate::utils::lo_bits_of(address as usize, 14)
                        | ((self.ram_or_rom_upper2 as usize) << 19)
                }
                _ => unreachable!(),
            },
            0x4000..=0x7FFF => {
                crate::utils::lo_bits_of(address as usize, 14)
                    | self.rom_bank_1_addr_base()
            }
            0xA000..=0xBFFF => match self.bank_mode_select {
                0 => crate::utils::lo_bits_of(address as usize, 13),
                1 => {
                    crate::utils::lo_bits_of(address as usize, 13)
                        | ((self.ram_or_rom_upper2 as usize) << 13)
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
}

impl crate::utils::Dump for MBC1 {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "MBC1")?;
        writeln!(
            out,
            "{}",
            if self.ram_enable { "RAM ON" } else { "RAM OFF" }
        )?;
        writeln!(out, "RAM BANK 1: {:08X}", self.rom_bank_1_addr_base())?;
        writeln!(out, "MODE: {}", self.bank_mode_select)?;
        Ok(())
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MBC3 {
    ram_and_timer_enable: bool,
    rom_bank: u8,
    ram_bank: u8,
    sram: Mem<{ 4 * EIGHT_K }>,
}
impl Default for MBC3 {
    fn default() -> Self {
        Self {
            ram_and_timer_enable: false,
            rom_bank: 1,
            ram_bank: 0,
            sram: Mem::default(),
        }
    }
}

impl MBC3 {
    pub fn reset(&mut self) {
        *self = MBC3::default();
    }

    fn rom_bank_1_addr_base(&self) -> usize {
        (self.rom_bank as usize) * ROM_BANK_SIZE
    }

    pub fn addr_to_offset(&self, address: u16) -> usize {
        match address {
            0x0000..=0x3FFF => address as usize,
            0x4000..=0x7FFF => {
                crate::utils::lo_bits_of(address as usize, 14)
                    | self.rom_bank_1_addr_base()
            }
            _ => unreachable!(),
        }
    }

    fn addr_to_sram_offset(&self, address: u16) -> usize {
        let bank_offset = (self.ram_bank as usize) * (EIGHT_K as usize);
        ((address - 0xA000) as usize) + bank_offset
    }

    pub fn load(&self, address: u16, data: &[u8]) -> u8 {
        match address {
            0xA000..=0xBFFF if !self.ram_and_timer_enable => 0xFF,
            0xA000..=0xBFFF if self.ram_and_timer_enable => {
                self.sram[self.addr_to_sram_offset(address)]
            }
            0x0000..=0x7FFF => data[self.addr_to_offset(address)],
            _ => unreachable!(),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            0x0000..=0x1FFF => match val {
                0xA => self.ram_and_timer_enable = true,
                0x0 => self.ram_and_timer_enable = false,
                _ => {}
            },
            0x2000..=0x3FFF => match val {
                0x00 => self.rom_bank = 1,
                0x01..=0x7F => self.rom_bank = val,
                _ => {}
            },
            0x4000..=0x5FFF => match val {
                0x00..=0x03 => self.ram_bank = val,
                0x08..=0x0C => todo!("rtc select"),
                _ => {}
            },
            0x6000..=0x7FFF => todo!("latch clock data"),
            0xA000..=0xBFFF if self.ram_and_timer_enable => {
                let offset = self.addr_to_sram_offset(address);
                self.sram[offset] = val;
            }
            _ => unreachable!(),
        }
    }
}

impl crate::utils::Dump for MBC3 {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "MBC3")?;
        writeln!(
            out,
            "{}",
            if self.ram_and_timer_enable {
                "RAM/TIMER ENABLED"
            } else {
                "RAM/TIMER DISABLED"
            }
        )?;
        writeln!(
            out,
            "ROM BANK: {:02X} ({:08X})",
            self.rom_bank,
            self.rom_bank_1_addr_base()
        )?;
        writeln!(out, "RAM BANK: {:02X}", self.ram_bank)?;
        Ok(())
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MBC5 {
    ram_enable: bool,
    rom_bank_lower8: u8,
    rom_bank_upper1: u8,
    ram_bank: u8,
    sram: Mem<{ 16 * EIGHT_K }>,
}
impl Default for MBC5 {
    fn default() -> Self {
        Self {
            ram_enable: false,
            rom_bank_lower8: 0,
            rom_bank_upper1: 0,
            ram_bank: 0,
            sram: Mem::default(),
        }
    }
}

impl MBC5 {
    pub fn reset(&mut self) {
        *self = MBC5::default();
    }

    fn rom_bank_1_addr_base(&self) -> usize {
        let rom_bank = (self.rom_bank_lower8 as usize)
            | (self.rom_bank_upper1 as usize) << 8;
        rom_bank * ROM_BANK_SIZE
    }

    pub fn addr_to_offset(&self, address: u16) -> usize {
        match address {
            0x0000..=0x3FFF => address as usize,
            0x4000..=0x7FFF => {
                crate::utils::lo_bits_of(address as usize, 14)
                    | self.rom_bank_1_addr_base()
            }
            _ => unreachable!(),
        }
    }

    fn addr_to_sram_offset(&self, address: u16) -> usize {
        let bank_offset = (self.ram_bank as usize) * (EIGHT_K as usize);
        ((address - 0xA000) as usize) + bank_offset
    }

    pub fn load(&self, address: u16, data: &[u8]) -> u8 {
        match address {
            0xA000..=0xBFFF if !self.ram_enable => 0xFF,
            0xA000..=0xBFFF if self.ram_enable => {
                self.sram[self.addr_to_sram_offset(address)]
            }
            0x0000..=0x7FFF => data[self.addr_to_offset(address)],
            _ => unreachable!(),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            0x0000..=0x1FFF => match val {
                0xA => self.ram_enable = true,
                0x0 => self.ram_enable = false,
                _ => {}
            },
            0x2000..=0x2FFF => self.rom_bank_lower8 = val,
            0x3000..=0x3FFF => self.rom_bank_upper1 = val & 0x1,
            0x4000..=0x5FFF => match val {
                0x00..=0x0F => self.ram_bank = val,
                _ => {}
            },
            0x6000..=0x7FFF => {}
            0xA000..=0xBFFF if self.ram_enable => {
                let offset = self.addr_to_sram_offset(address);
                self.sram[offset] = val;
            }
            _ => unreachable!(),
        }
    }
}

impl crate::utils::Dump for MBC5 {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "MBC5")?;
        writeln!(
            out,
            "{}",
            if self.ram_enable {
                "RAM/TIMER ENABLED"
            } else {
                "RAM/TIMER DISABLED"
            }
        )?;
        writeln!(
            out,
            "ROM BANK: {:02X} ({:08X})",
            self.rom_bank_lower8,
            self.rom_bank_1_addr_base()
        )?;
        writeln!(out, "RAM BANK: {:02X}", self.ram_bank)?;
        Ok(())
    }
}
