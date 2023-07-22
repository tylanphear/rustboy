use serde::{Deserialize, Serialize};

use crate::cart::Cartridge;
use crate::io::IOController;
use crate::utils::{Clock, Mem, TClock};
use crate::{cpu, io::ppu};

mod regs {
    pub const BIOS_ROM_DISABLE: u16 = 0xFF50;
}

pub const NUM_OAM_CLOCKS: u16 = 160;

// Memory Map
// 0x0000 - 0x3FFF | ROM0   | Non-switchable ROM
// 0x4000 - 0x7FFF | ROMX   | Switchable ROM
// 0x8000 - 0x9FFF | VRAM   | Video RAM (switchable [0-1] in GBC)
// 0xA000 - 0xBFFF | SRAM   | External RAM, persistent
// 0xC000 - 0xCFFF | WRAM0  | Work RAM
// 0xD000 - 0xDFFF | WRAMX  | Work RAM (switchable [1-7] in GBC)
// 0xE000 - 0xFDFF | ECHO   | Echo RAM
// 0xFE00 - 0xFE9F | OAM    | Object Attribute Table (sprite table)
// 0xFEA0 - 0xFEFF | UNUSED | Ignored/empty (mostly)
// 0xFF00 - 0xFF7F | IO     | mem-mapped I/O registers
// 0xFF80 - 0xFFFE | HRAM   | Internal CPU RAM
// 0xFFFF          | IE     | Interrupt Enable register

#[allow(non_snake_case)]
#[derive(Debug, Serialize, Deserialize)]
pub struct MMU {
    bios: Mem<{ 0x0100 - 0x0000 }>,
    // vram: Mem<{ 0xA000 - 0x8000 }>,
    ram0: Mem<{ 0xD000 - 0xC000 }>,
    ramx: Mem<{ 0xE000 - 0xC000 }>,
    // echo     0xFDFF - 0xE000
    pub io: IOController, // 0xFF00 - 0xFF7F
    hram: Mem<{ 0xFFFF - 0xFF80 }>,
    iflags: u8,
    ie: u8,

    bios_enabled: bool,
    pub(crate) cartridge: Option<Cartridge>,

    oam_timer: TClock,
    oam_base_addr: u16,
    oam_clocks_left: u16,

    #[serde(skip)]
    watchpoints: std::collections::HashSet<u16>,
}

impl MMU {
    pub fn new() -> Self {
        Self {
            bios: Default::default(),
            ram0: Default::default(),
            ramx: Default::default(),
            io: Default::default(),
            hram: Default::default(),
            cartridge: None,
            bios_enabled: true,
            iflags: 0b11100000,
            ie: 0,
            oam_timer: Default::default(),
            oam_base_addr: 0,
            oam_clocks_left: 0,
            watchpoints: Default::default(),
        }
    }

    pub fn load_bios(&mut self, data: &[u8]) {
        assert!(data.len() == 0x100, "bios must be exactly 0x100 bytes long");
        self.bios.copy_from_slice(data);
        self.bios_enabled = true;
    }

    pub fn load_cart(&mut self, data: Vec<u8>) {
        let mut cart = Cartridge::new(data);
        cart.load_sram();
        self.cartridge = Some(cart);
    }

    pub fn cart(&self) -> &Cartridge {
        self.cartridge.as_ref().unwrap()
    }

    pub fn reset(&mut self) {
        self.bios.clear();
        self.ram0.clear();
        self.ramx.clear();
        self.io.reset();
        self.hram.clear();
        self.bios_enabled = true;
        if let Some(cart) = &mut self.cartridge {
            cart.reset();
        }
        self.watchpoints.clear();
    }

    pub fn tick(&mut self, clock: &Clock) {
        let (_, should_tick) = self.oam_timer.tick(clock);
        if self.oam_clocks_left == 0 || !should_tick {
            return;
        }

        let offset = NUM_OAM_CLOCKS - self.oam_clocks_left;
        let addr = self.oam_base_addr + offset;
        let byte = self.load8_unchecked(addr);
        self.io.lcd.dma_do_transfer(offset, byte);

        self.oam_clocks_left = self.oam_clocks_left.saturating_sub(1);
    }

    pub fn load8_unchecked(&self, address: u16) -> u8 {
        match address {
            cpu::reg::IF => self.iflags,
            regs::BIOS_ROM_DISABLE => 0xE | !self.bios_enabled as u8,
            ppu::reg::DMA => self.oam_base_addr.to_le_bytes()[0],
            // | BIOS   | Bootstrap program (when enabled)
            0x0000..=0x00FF if self.bios_enabled => self.bios[address - 0x0000],
            // | ROM0   | Cartridge ROM
            0x0000..=0x7FFF => self.cartridge.as_ref().unwrap().load(address),
            // | VRAM   | Video RAM
            0x8000..=0x9FFF => self.io.lcd.load(address),
            // | SRAM   | External RAM, persistent
            0xA000..=0xBFFF => self.cartridge.as_ref().unwrap().load(address),
            // | WRAM0  | Work RAM
            0xC000..=0xCFFF => self.ram0[address - 0xC000],
            // | WRAMX  | Work RAM
            0xD000..=0xDFFF => self.ramx[address - 0xD000],
            // | ECHO   | Echo RAM (top half)
            0xE000..=0xEFFF => self.ram0[address - 0xE000],
            // | ECHO   | Echo RAM (bot half)
            0xF000..=0xFDFF => self.ramx[address - 0xF000],
            // | OAM    | Object Attribute Table
            0xFE00..=0xFE9F => self.io.lcd.load(address),
            // | UNUSED | Ignored/empty (mostly)
            0xFEA0..=0xFEFF => 0xFF,
            // | IO     | mem-mapped I/O registers
            0xFF00..=0xFF4B => self.io.load(address),
            // | UNUSED | Ignored/empty (mostly)
            0xFF4C..=0xFF7F => 0xFF,
            // | HRAM   | Internal CPU RAM
            0xFF80..=0xFFFE => self.hram[address - 0xFF80],
            // | IE     | Interrupt Enable register
            cpu::reg::IE => self.ie,
        }
    }

    pub fn register_watchpoint(&mut self, address: u16) {
        self.watchpoints.insert(address);
    }

    pub fn dump_watchpoints<W: std::fmt::Write>(
        &self,
        out: &mut W,
    ) -> std::fmt::Result {
        if self.watchpoints.is_empty() {
            return Ok(());
        }
        writeln!(out, "current watchpoints:")?;
        let sorted_addrs = {
            let mut keys: Vec<_> = self.watchpoints.iter().collect();
            keys.sort();
            keys
        };
        for bp in &sorted_addrs {
            writeln!(out, "{:04X}", bp)?;
        }
        Ok(())
    }

    #[inline]
    pub fn load8(&self, address: u16) -> u8 {
        if self.watchpoints.contains(&address) {
            crate::debug_break!("watchpoint {address:04X} hit!");
        }
        self.load8_unchecked(address)
    }

    pub fn load16(&self, address: u16) -> u16 {
        let byte1 = self.load8(address.wrapping_add(0));
        let byte2 = self.load8(address.wrapping_add(1));
        u16::from_le_bytes([byte1, byte2])
    }

    #[inline]
    pub fn store8(&mut self, address: u16, val: u8) {
        if self.watchpoints.contains(&address) {
            crate::debug_break!("watchpoint {address:04X} hit!");
        }
        // Only HRAM is writeable during OAM DMA
        if self.oam_clocks_left > 0 {
            match address {
                0xFF80..=0xFFFE => self.hram[address - 0xFF80] = val,
                _ => {}
            };
        }
        self.store8_unchecked(address, val)
    }

    pub fn store8_unchecked(&mut self, address: u16, val: u8) {
        match address {
            // | ROM0   | Non-switchable ROM
            0x0000..=0x3FFF |
            // | ROMX   | Switchable ROM
            0x4000..=0x7FFF => self.cartridge.as_mut().unwrap().store(address, val),
            // | VRAM   | Video RAM
            0x8000..=0x9FFF => self.io.lcd.store(address, val),
            // | SRAM   | External RAM, persistent
            0xA000..=0xBFFF => self.cartridge.as_mut().unwrap().store(address, val),
            // | WRAM0  | Work RAM
            0xC000..=0xCFFF => self.ram0[address - 0xC000] = val,
            // | WRAMX  | Work RAM
            0xD000..=0xDFFF => self.ramx[address - 0xD000] = val,
            // | ECHO   | Echo RAM (top half)
            0xE000..=0xEFFF => self.ram0[address - 0xE000] = val,
            // | ECHO   | Echo RAM (bot half)
            0xF000..=0xFDFF => self.ramx[address - 0xF000] = val,
            // | OAM    | Object Attribute Table
            0xFE00..=0xFE9F => self.io.lcd.store(address, val),
            // | UNUSED | Ignored/empty (mostly)
            0xFEA0..=0xFEFF => {}
            cpu::reg::IF => self.iflags = val | 0b11100000,
            regs::BIOS_ROM_DISABLE => if val & 0x1 != 0 {
                self.bios_enabled = false;
            }
            ppu::reg::DMA => {
                assert_eq!(self.oam_clocks_left, 0);
                self.oam_clocks_left = NUM_OAM_CLOCKS;
                self.oam_base_addr = u16::from_le_bytes([0x00, val]);
            }
            // | IO     | mem-mapped I/O registers
            0xFF00..=0xFF4B => self.io.store(address, val),
            // | UNUSED | Ignored/empty (mostly)
            0xFF4C..=0xFF7F => {}
            // | HRAM   | Internal CPU RAM
            0xFF80..=0xFFFE => self.hram[address - 0xFF80] = val,
            // | IE     | Interrupt Enable register
            0xFFFF => self.ie = val,
        };
    }

    pub fn store16(&mut self, address: u16, val: u16) {
        let [lo, hi] = val.to_le_bytes();
        self.store8(address + 0, lo);
        self.store8(address + 1, hi);
    }

    pub fn block_load(&self, address: u16, len: usize) -> &[u8] {
        fn load_<const S: usize>(
            mem: &Mem<S>,
            start: u16,
            address: u16,
            len: usize,
        ) -> &[u8] {
            mem.safe_slice((address - start) as usize, len)
        }
        match address {
            // | BIOS   | Bootstrap program (when enabled)
            0x0000..=0x00FF if self.bios_enabled => load_(&self.bios, 0x0000, address, len),
            // | ROM0   | Non-switchable ROM
            0x0000..=0x3FFF |
            // | ROMX   | Switchable ROM
            0x4000..=0x7FFF => self.cartridge.as_ref().unwrap().block_load(address, len),
            // | VRAM   | Video RAM
            0x8000..=0x9FFF => self.io.lcd.block_load(address, len),
            // | SRAM   | External RAM, persistent
            0xA000..=0xBFFF => self.cartridge.as_ref().unwrap().block_load(address, len),
            // | WRAM0  | Work RAM
            0xC000..=0xCFFF => load_(&self.ram0, 0xC000, address, len),
            // | WRAMX  | Work RAM
            0xD000..=0xDFFF => load_(&self.ramx, 0xD000, address, len),
            // | ECHO   | Echo RAM (top half)
            0xE000..=0xEFFF => load_(&self.ram0, 0xE000, address, len),
            // | ECHO   | Echo RAM (bot half)
            0xF000..=0xFDFF => load_(&self.ramx, 0xF000, address, len),
            // | OAM    | Object Attribute Table
            0xFE00..=0xFE9F => self.io.lcd.block_load(address, len),
            // | UNUSED | Ignored/empty (mostly)
            0xFEA0..=0xFEFF |
            // | IO     | mem-mapped I/O registers
            0xFF00..=0xFF7F => &[],
            // | HRAM   | Internal CPU RAM
            0xFF80..=0xFFFE => load_(&self.hram, 0xFF80, address, len),
            // | IE     | Interrupt Enable register
            0xFFFF => &[],
        }
    }
}
