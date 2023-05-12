use std::cell::RefCell;
use std::collections::HashMap;

use crate::cart::Cartridge;
use crate::io::IOController;
use crate::{cpu, io::lcd};

mod regs {
    pub const BIOS_ROM_DISABLE: u16 = 0xFF50;
}

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
#[derive(Debug)]
pub struct MMU {
    bios: Mem<{ 0x0100 - 0x0000 }>,
    // vram: Mem<{ 0xA000 - 0x8000 }>,
    sram: Mem<{ 0xC000 - 0xA000 }>,
    ram0: Mem<{ 0xD000 - 0xC000 }>,
    ramx: Mem<{ 0xE000 - 0xC000 }>,
    // echo     0xFDFF - 0xE000
    pub io: IOController, // 0xFF00 - 0xFF7F
    hram: Mem<{ 0xFFFF - 0xFF80 }>,
    iflags: u8,
    ie: u8,

    cartridge: Option<Cartridge>,
    bios_enabled: bool,
    clocks_til_oam_dma: u64,
}

impl MMU {
    pub fn new() -> Self {
        Self {
            bios: Default::default(),
            sram: Default::default(),
            ram0: Default::default(),
            ramx: Default::default(),
            io: IOController::new(),
            hram: Default::default(),
            cartridge: None,
            bios_enabled: false,
            iflags: 0b11100000,
            ie: 0,
            clocks_til_oam_dma: 0,
        }
    }

    pub fn load_bios(&mut self, data: &[u8]) {
        assert!(data.len() == 0x100, "bios must be exactly 0x100 bytes long");
        self.bios.0.copy_from_slice(&data);
        self.bios_enabled = true;
    }

    pub fn load_cart(&mut self, data: Vec<u8>) {
        let cart = Cartridge::new(data);
        self.cartridge = Some(cart);
    }

    pub fn clear(&mut self) {
        self.bios.clear();
        self.sram.clear();
        self.ram0.clear();
        self.ramx.clear();
        self.io.reset();
        self.hram.clear();
    }

    pub fn tick(&mut self) {
        if self.clocks_til_oam_dma == 0 {
            return;
        }

        self.clocks_til_oam_dma = self.clocks_til_oam_dma.saturating_sub(4);
        if self.clocks_til_oam_dma > 0 {
            return;
        }

        let data = self.io.lcd.dma_prepare_transfer(self);
        self.io.lcd.dma_do_transfer(data);
    }

    pub fn load8(&self, address: u16) -> u8 {
        // Only HRAM is accessible during OAM DMA
        if self.clocks_til_oam_dma > 0 {
            return match address {
                0xFF80..=0xFFFE => self.hram[address - 0xFF80],
                _ => 0xFF,
            };
        }
        match address {
            // | BIOS   | Bootstrap program (when enabled)
            0x0000..=0x00FF if self.bios_enabled => self.bios[address - 0x0000],
            // | ROM0   | Cartridge ROM
            0x0000..=0x7FFF => self.cartridge.as_ref().unwrap().load(address),
            // | VRAM   | Video RAM
            0x8000..=0x9FFF => self.io.lcd.load(address),
            // | SRAM   | External RAM, persistent
            0xA000..=0xBFFF => self.sram[address - 0xA000],
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
            0xFEA0..=0xFEFF => 0,
            cpu::regs::IF => self.iflags,
            regs::BIOS_ROM_DISABLE => !self.bios_enabled as u8,
            // | IO     | mem-mapped I/O registers
            0xFF00..=0xFF4B => self.io.load(address),
            // | UNUSED | Ignored/empty (mostly)
            0xFF4C..=0xFF7F => 0,
            // | HRAM   | Internal CPU RAM
            0xFF80..=0xFFFE => self.hram[address - 0xFF80],
            // | IE     | Interrupt Enable register
            0xFFFF => self.ie,
        }
    }

    pub fn load16(&self, address: u16) -> u16 {
        let byte1 = self.load8(address.wrapping_add(0));
        let byte2 = self.load8(address.wrapping_add(1));
        u16::from_le_bytes([byte1, byte2])
    }

    pub fn store8(&mut self, address: u16, val: u8) {
        //self.watchpoints.borrow_mut().get_mut(&address).map(|p| {
        //    *p += 1;
        //});
        match address {
            // | ROM0   | Non-switchable ROM
            0x0000..=0x3FFF |
            // | ROMX   | Switchable ROM
            0x4000..=0x7FFF => self.cartridge.as_mut().unwrap().store(address, val),
            // | VRAM   | Video RAM
            0x8000..=0x9FFF => self.io.lcd.store(address, val),
            // | SRAM   | External RAM, persistent
            0xA000..=0xBFFF => self.sram[address - 0xA000] = val,
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
            0xFEA0..=0xFEFF => return,
            cpu::regs::IF => self.iflags = val | 0b11100000,
            regs::BIOS_ROM_DISABLE => if val == 0x1 {
                self.bios_enabled = false;
            }
            lcd::reg::DMA => {
                assert_eq!(self.clocks_til_oam_dma, 0);
                self.clocks_til_oam_dma = 140;
                self.io.lcd.store(lcd::reg::DMA, val);
            }
            // | IO     | mem-mapped I/O registers
            0xFF00..=0xFF4B => self.io.store(address, val),
            // | UNUSED | Ignored/empty (mostly)
            0xFF4C..=0xFF7F => return,
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
            let adjusted_addr = (address - start) as usize;
            let len_to_load = std::cmp::min(mem.size() - adjusted_addr, len);
            &mem.0[adjusted_addr..][..len_to_load]
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
            0xA000..=0xBFFF => load_(&self.sram, 0xA000, address, len),
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

#[derive(Debug)]
pub struct Mem<const SIZE: usize>(Box<[u8; SIZE]>);
impl<const SIZE: usize> Default for Mem<SIZE> {
    fn default() -> Self {
        Mem(Box::new([0; SIZE]))
    }
}
impl<const SIZE: usize> Mem<SIZE> {
    pub fn clear(&mut self) {
        *self.0.as_mut() = [0; SIZE];
    }

    pub fn size(&self) -> usize {
        SIZE
    }

    pub fn slice(&self, addr: u16, n: usize) -> &[u8] {
        let start = addr as usize;
        &self.0[start..start + n]
    }

    pub fn copy_from_slice(&mut self, data: &[u8]) {
        assert!(data.len() <= self.size());
        self.0.copy_from_slice(data);
    }
}

impl<const S: usize> std::ops::Index<u16> for Mem<S> {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl<const S: usize> std::ops::IndexMut<u16> for Mem<S> {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}
