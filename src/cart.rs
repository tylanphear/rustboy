use std::path::PathBuf;

use serde::{Deserialize, Serialize};

mod mappers;

use crate::utils::constants::{EIGHT_K, THIRTY_TWO_K};

pub mod header {
    pub const ENTRY: usize = 0x0100;
    pub const LOGO_START: usize = 0x0104;
    pub const LOGO_END: usize = 0x0133;
    pub const TITLE_START: usize = 0x0134;
    pub const TITLE_END: usize = 0x0143;
    pub const CGDB_FLAG: usize = 0x0143;
    pub const CART_TYPE: usize = 0x0147;
    pub const ROM_SIZE: usize = 0x0148;
    pub const RAM_SIZE: usize = 0x0149;
}
use header::*;

#[derive(Debug, Serialize, Deserialize)]
pub struct Cartridge {
    data: Vec<u8>,
    mbc: MBC,
}

impl Cartridge {
    pub fn new(data: Vec<u8>) -> Self {
        assert!(
            data.len() >= THIRTY_TWO_K,
            "cart must be at least 32K bytes long"
        );
        if data[CGDB_FLAG] == 0xC0 {
            panic!("No support for CGB-only carts!");
        }
        use flags::*;
        use mappers::*;
        let num_rom_banks = 2 << data[ROM_SIZE];
        let mbc = match data[CART_TYPE] {
            0x00 => MBC::None(Simple::default()),
            0x01 => MBC::MBC1(MBC1::new(num_rom_banks, NONE)),
            0x02 => MBC::MBC1(MBC1::new(num_rom_banks, RAM)),
            0x03 => MBC::MBC1(MBC1::new(num_rom_banks, RAM | BATTERY)),
            0x0F => MBC::MBC3(MBC3::new(TIMER | BATTERY)),
            0x10 => MBC::MBC3(MBC3::new(TIMER | RAM | BATTERY)),
            0x11 => MBC::MBC3(MBC3::new(NONE)),
            0x12 => MBC::MBC3(MBC3::new(RAM)),
            0x13 => MBC::MBC3(MBC3::new(RAM | BATTERY)),
            0x19..=0x1E => MBC::MBC5(MBC5::default()),
            type_ => todo!("cartridge type {type_:02X} unsupported"),
        };
        Cartridge { data, mbc }
    }

    pub fn reset(&mut self) {
        match &mut self.mbc {
            MBC::None(mbc) => mbc.reset(),
            MBC::MBC1(mbc) => mbc.reset(),
            MBC::MBC3(mbc) => mbc.reset(),
            MBC::MBC5(mbc) => mbc.reset(),
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match &mut self.mbc {
            MBC::None(mbc) => mbc.store(address, val),
            MBC::MBC1(mbc) => mbc.store(address, val),
            MBC::MBC3(mbc) => mbc.store(address, val),
            MBC::MBC5(mbc) => mbc.store(address, val),
        }
    }

    pub fn load(&self, address: u16) -> u8 {
        match &self.mbc {
            MBC::None(mbc) => mbc.load(address, &self.data),
            MBC::MBC1(mbc) => mbc.load(address, &self.data),
            MBC::MBC3(mbc) => mbc.load(address, &self.data),
            MBC::MBC5(mbc) => mbc.load(address, &self.data),
        }
    }

    pub fn block_load(&self, address: u16, len: usize) -> &[u8] {
        match &self.mbc {
            MBC::None(mbc) => mbc.block_load(address, len, &self.data),
            MBC::MBC1(mbc) => mbc.block_load(address, len, &self.data),
            MBC::MBC3(mbc) => mbc.block_load(address, len, &self.data),
            MBC::MBC5(mbc) => mbc.block_load(address, len, &self.data),
        }
    }

    pub fn name(&self) -> String {
        let bytes = self.data[TITLE_START..TITLE_END]
            .iter()
            .copied()
            .take_while(|b| *b != b'\0');
        String::from_utf8(bytes.collect()).unwrap()
    }

    pub fn type_(&self) -> u8 {
        self.data[CART_TYPE]
    }

    pub fn rom_size(&self) -> usize {
        THIRTY_TWO_K * (1usize << self.data[0x148])
    }

    pub fn ram_size(&self) -> usize {
        match self.data[0x149] {
            0x00 => 0,
            0x01 => unreachable!("unused"),
            0x02 => EIGHT_K,
            0x03 => 4 * EIGHT_K,
            0x04 => 16 * EIGHT_K,
            0x05 => 8 * EIGHT_K,
            _ => unreachable!(),
        }
    }

    pub fn dump_sram(&self) {
        let path = PathBuf::from(self.name()).with_extension("sram");
        let sram = match &self.mbc {
            MBC::None(..) => None,
            MBC::MBC1(mbc) => mbc.sram(),
            MBC::MBC3(mbc) => mbc.sram(),
            MBC::MBC5(mbc) => Some(mbc.sram()),
        };
        let Some(bytes) = sram else { return };
        if let Err(e) = std::fs::write(path, bytes) {
            crate::debug_log!("error writing sram: {e}");
        }
    }

    pub fn load_sram(&mut self) {
        let path = PathBuf::from(self.name()).with_extension("sram");
        let bytes = match std::fs::read(path) {
            Ok(bytes) => bytes,
            Err(e) => {
                if e.kind() != std::io::ErrorKind::NotFound {
                    crate::debug_log!("error loading sram: {e}")
                }
                return;
            }
        };
        match &mut self.mbc {
            MBC::None(..) => return,
            MBC::MBC1(mbc) => mbc.load_sram(&bytes),
            MBC::MBC3(mbc) => mbc.load_sram(&bytes),
            MBC::MBC5(mbc) => mbc.load_sram(&bytes),
        }
    }
}

impl crate::utils::Dump for Cartridge {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "Name: {}", self.name())?;
        writeln!(out, "Type: {:02X}", self.type_())?;
        writeln!(out, "ROM Size: {:X}", self.rom_size())?;
        writeln!(out, "RAM Size: {:X}", self.ram_size())?;
        self.mbc.dump(out)?;
        Ok(())
    }
}

#[derive(Debug, Serialize, Deserialize)]
enum MBC {
    // TODO: extract this as a proper type -- also I don't think the
    // implementation is correct
    None(mappers::Simple),
    MBC1(mappers::MBC1),
    MBC3(mappers::MBC3),
    MBC5(mappers::MBC5),
}

impl crate::utils::Dump for MBC {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        match self {
            MBC::None { .. } => writeln!(out, "No MBC"),
            MBC::MBC1(mbc) => mbc.dump(out),
            MBC::MBC3(mbc) => mbc.dump(out),
            MBC::MBC5(mbc) => mbc.dump(out),
        }
    }
}
