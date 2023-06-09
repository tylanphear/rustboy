use crate::utils::constants::{SIXTEEN_K, THIRTY_TWO_K};

const ROM_BANK_SIZE: usize = SIXTEEN_K;

#[derive(Debug)]
pub struct Cartridge {
    data: Vec<u8>,
    mbc: MBC,
}

#[derive(Debug, Default)]
enum MBC {
    #[default]
    None,
    MBC1 {
        ram_enable: bool,
        mode: u8,
        rom_bank_lower5: u8,
        ram_or_rom_upper2: u8,
    },
}

impl Cartridge {
    pub fn new(data: Vec<u8>) -> Self {
        assert!(
            data.len() >= THIRTY_TWO_K,
            "cart must be at least 32K bytes long"
        );
        let type_ = data[0x147];
        let mbc = match type_ {
            0 => MBC::None,
            1 => MBC::MBC1 {
                ram_enable: false,
                mode: 0,
                rom_bank_lower5: 0,
                ram_or_rom_upper2: 0,
            },
            _ => todo!("cartridge type {type_} unsupported"),
        };
        Cartridge { data, mbc }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match self.mbc {
            MBC::None => {}
            MBC::MBC1 {
                ref mut ram_enable,
                ref mut rom_bank_lower5,
                ref mut ram_or_rom_upper2,
                ref mut mode,
            } => match address {
                // RAM Enable
                0x0000..=0x1FFF => {
                    *ram_enable = (val & 0xF) == 0xA;
                }
                // ROM Bank (lower)
                0x2000..=0x3FFF => {
                    // Take the lower 5 bits -- if the result is 0, offset by 1
                    // (mapping 0x00 to 0x01, 0x20 to 0x21, etc.)
                    *rom_bank_lower5 = val & 0x1F + (val & 0x1F == 0) as u8;
                }
                // RAM Bank (or upper bits of ROM Bank)
                0x4000..=0x5FFF => {
                    *ram_or_rom_upper2 = val & 0x3;
                }
                // ROM/RAM Mode
                0x6000..=0x7FFF => {
                    *mode = val & 0x1;
                    return;
                }
                0xA000..=0xBFFF => {
                    todo!("properly implement SRAM");
                }
                _ => unreachable!(
                    "Store outside of MBC range? ({val:02X} to {address:04X})"
                ),
            },
        }
    }

    fn load_addr_to_offset(&self, address: u16) -> usize {
        match self.mbc {
            MBC::None => address as usize,
            MBC::MBC1 {
                rom_bank_lower5,
                ram_or_rom_upper2,
                mode,
                ..
            } => match address {
                0x0000..=0x3FFF => address as usize,
                0x4000..=0x7FFF => {
                    let rom_bank_num =
                        rom_bank_lower5 | (ram_or_rom_upper2 << 5);
                    (rom_bank_num as usize) * ROM_BANK_SIZE
                        + (address - 0x4000) as usize
                }
                0xA000..=0xBFFF if mode == 0x1 => {
                    todo!("properly implement SRAM");
                    let rom_bank_num =
                        rom_bank_lower5 | (ram_or_rom_upper2 << 5);
                    (rom_bank_num as usize) * ROM_BANK_SIZE
                        + (address - 0x4000) as usize
                }
                _ => unreachable!(),
            },
        }
    }

    pub fn load(&self, address: u16) -> u8 {
        assert!(address < 0x8000, "MBC only handles ROM-area loads");
        self.data[self.load_addr_to_offset(address)]
    }

    pub fn block_load(&self, address: u16, len: usize) -> &[u8] {
        assert!(address < 0x8000, "MBC only handles ROM-area loads");
        let offset = self.load_addr_to_offset(address);
        let len_to_load = std::cmp::min(self.data.len() - offset, len);
        &self.data[offset..][..len_to_load]
    }
}
