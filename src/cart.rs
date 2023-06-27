use serde::{Deserialize, Serialize};

mod mappers;

use crate::utils::constants::{EIGHT_K, THIRTY_TWO_K};

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
        let mut this = Cartridge {
            data,
            mbc: MBC::None,
        };
        this.mbc = match this.type_() {
            0x00 => MBC::None,
            0x01..=0x03 => MBC::MBC1(mappers::MBC1::default()),
            0x0F..=0x13 => MBC::MBC3(mappers::MBC3::default()),
            0x19 => MBC::MBC5(mappers::MBC5::default()),
            type_ => todo!("cartridge type {type_:02X} unsupported"),
        };
        this
    }

    pub fn reset(&mut self) {
        match &mut self.mbc {
            MBC::None => {}
            MBC::MBC1(mbc) => mbc.reset(),
            MBC::MBC3(mbc) => mbc.reset(),
            MBC::MBC5(mbc) => mbc.reset(),
        }
    }

    fn addr_to_offset(&self, address: u16) -> usize {
        match &self.mbc {
            MBC::None => address as usize,
            MBC::MBC1(mbc) => mbc.addr_to_offset(address),
            MBC::MBC3(mbc) => mbc.addr_to_offset(address),
            MBC::MBC5(mbc) => mbc.addr_to_offset(address),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match &mut self.mbc {
            MBC::None => {}
            MBC::MBC1(mbc) => mbc.store(address, val),
            MBC::MBC3(mbc) => mbc.store(address, val),
            MBC::MBC5(mbc) => mbc.store(address, val),
        }
    }

    pub fn load(&self, address: u16) -> u8 {
        match &self.mbc {
            MBC::None => self.data[address as usize],
            MBC::MBC1(mbc) => mbc.load(address, &self.data),
            MBC::MBC3(mbc) => mbc.load(address, &self.data),
            MBC::MBC5(mbc) => mbc.load(address, &self.data),
        }
    }

    pub fn block_load(&self, address: u16, len: usize) -> &[u8] {
        match &self.mbc {
            MBC::None => {
                let offset = self.addr_to_offset(address);
                let len_to_load = std::cmp::min(self.data.len() - offset, len);
                &self.data[offset..][..len_to_load]
            }
            MBC::MBC1(mbc) => mbc.block_load(address, len, &self.data),
            MBC::MBC3(mbc) => mbc.block_load(address, len, &self.data),
            MBC::MBC5(mbc) => mbc.block_load(address, len, &self.data),
        }
    }

    pub fn name(&self) -> String {
        let bytes = self.data[0x134..]
            .iter()
            .copied()
            .take_while(|b| *b != b'\0');
        String::from_utf8(bytes.collect()).unwrap()
    }

    pub fn type_(&self) -> u8 {
        self.data[0x147]
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

#[derive(Debug, Default, Serialize, Deserialize)]
enum MBC {
    #[default]
    None,
    MBC1(mappers::MBC1),
    MBC3(mappers::MBC3),
    MBC5(mappers::MBC5),
}

impl crate::utils::Dump for MBC {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        match self {
            MBC::None => writeln!(out, "No MBC"),
            MBC::MBC1(mbc) => mbc.dump(out),
            MBC::MBC3(mbc) => mbc.dump(out),
            MBC::MBC5(mbc) => mbc.dump(out),
        }
    }
}
