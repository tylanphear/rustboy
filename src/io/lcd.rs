use crate::debug_log;
use crate::mmu::Mem;

// LCD display is 160x144 pixels
const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;
const TILE_WIDTH_IN_PIXELS: usize = 8;
const TILE_HEIGHT_IN_PIXELS: usize = 8;
const BITS_PER_PIXEL: usize = 2;
const PIXELS_PER_TILE: usize = TILE_WIDTH_IN_PIXELS * TILE_HEIGHT_IN_PIXELS;
const BITS_PER_TILE: usize = PIXELS_PER_TILE * BITS_PER_PIXEL;
const BYTES_PER_TILE: usize = BITS_PER_TILE / 8;

const NUM_TILES_IN_X: usize = 32;
const NUM_TILES_IN_Y: usize = 32;
const BG_WIDTH: usize = NUM_TILES_IN_X * TILE_WIDTH_IN_PIXELS;
const BG_HEIGHT: usize = NUM_TILES_IN_Y * TILE_HEIGHT_IN_PIXELS;
const TILE_MAP_SIZE: usize = NUM_TILES_IN_X * NUM_TILES_IN_Y;

// 4 gray shades:
pub mod colors {
    pub const WHITE: u8 = 0;
    pub const LGRAY: u8 = 1;
    pub const DGRAY: u8 = 2;
    pub const BLACK: u8 = 3;
}

#[rustfmt::skip]
pub mod reg {
    // Bit 7 - LCD Power           (0=Off, 1=On)
    // Bit 6 - Window Tile Map     (0=0x9800-0x9BFF, 1=0x9C00-0x9FFF)
    // Bit 5 - Window Enable       (0=Disabled, 1=Enabled)
    // Bit 4 - BG & Window Tileset (0=0x8800-0x97FF, 1=0x8000-0x8FFF)
    // Bit 3 - BG Tile Map         (0=0x9800-0x9BFF, 1=0x9C00-0x9FFF)
    // Bit 2 - Sprite Size         (0=8x8, 1=8x16)
    // Bit 1 - Sprites Enabled     (0=Disabled, 1=Enabled)
    // Bit 0 - BG Enabled          (0=Disabled, 1=Enabled)
    pub const LCDC: u16 = 0xFF40;

    // Bit 7 - Unused (Always 1)
    // Bit 6 - LY=LYC Check Enable        (1=Enable) (RW)
    // Bit 5 - Mode 2 OAM Check Enable    (1=Enable) (RW)
    // Bit 4 - Mode 1 VBlank Check Enable (1=Enable) (RW)
    // Bit 3 - Mode 0 HBlank Check Enable (1=Enable) (RW)
    // Bit 2 - LY=LYC Comparison Signal   (1:LYC=LY) (RO)
    // Bit 1 |
    // Bit 0 - Screen Mode                (Mode 0-3) (RO)
    //         0: HBlank
    //         1: VBlank
    //         2: Searching OAM
    //         3: Tranferring data to LCD
    pub const STAT: u16 = 0xFF41;

    pub const SCY : u16 = 0xFF42;
    pub const SCX : u16 = 0xFF43;
    pub const LY  : u16 = 0xFF44;
    pub const LYC : u16 = 0xFF45;
    pub const DMA : u16 = 0xFF46;

    // Bit 7-6 - Shade for Color 3
    // Bit 5-4 - Shade for Color 2
    // Bit 3-2 - Shade for Color 1
    // Bit 1-0 - Shade for Color 0
    pub const BGP : u16 = 0xFF47;

    pub const OBP0: u16 = 0xFF48;
    pub const OBP1: u16 = 0xFF49;
    pub const WY  : u16 = 0xFF4A;
    pub const WX  : u16 = 0xFF4B;
}

use crate::{mmu::MMU, utils};

// Each scanline lasts 456 clocks (114 internal)
//    4 clocks ( 1 internal) - (Mode 0)
//   80 clocks (20 internal) - (Mode 2)
//  364 clocks (91 internal) - (Mode 3)
//    8 clocks ( 2 internal) - (Mode 0)
//
// On lines 144-153, LCD is in Mode 1 (VBlank):
//   - Lasts for 4560 clocks
//   - VBlank interrupt is triggered by setting IF flag on cycle after LY=144

const CLOCKS_PER_SCANLINE: u64 = 114;
const NUM_SCANLINES: u8 = 153;
const VRAM_START: u16 = 0x8000;
const VRAM_END: u16 = VRAM_START + (VRAM_SIZE as u16) - 1;
const VRAM_SIZE: usize = 0x2000;
const OAM_START: u16 = 0xFE00;
const OAM_END: u16 = OAM_START + (OAM_SIZE as u16) - 1;
const OAM_SIZE: usize = 0xA0;

type Vram = Mem<VRAM_SIZE>;
type Oam = Mem<OAM_SIZE>;

#[derive(Debug, Default)]
pub struct LCDController {
    clock: u64,
    registers: [u8; 0xFF4C - 0xFF40],
    pub(crate) vram: Vram,
    pub(crate) oam: Oam,
}

impl LCDController {
    pub fn reset(&mut self) {
        self.clock = 0;
        self.registers = Default::default();
        self.vram.clear();
        self.oam.clear();
    }

    pub fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(
            out,
            "LCDC: {0:08b} ({1:04X})",
            self.read_reg(reg::LCDC),
            reg::LCDC
        )?;
        writeln!(
            out,
            "STAT: {0:08b} ({1:04X})",
            self.read_reg(reg::STAT),
            reg::STAT
        )?;
        writeln!(
            out,
            "SCX : {0:08} ({1:04X})",
            self.read_reg(reg::SCX),
            reg::SCX
        )?;
        writeln!(
            out,
            "SCY : {0:08} ({1:04X})",
            self.read_reg(reg::SCY),
            reg::SCY
        )?;
        writeln!(
            out,
            "LY  : {0:08} ({1:04X})",
            self.read_reg(reg::LY),
            reg::LY
        )?;
        writeln!(
            out,
            "BGP : {0:08b} ({1:04X})",
            self.read_reg(reg::BGP),
            reg::BGP
        )?;
        writeln!(
            out,
            "WY  : {0:08b} ({1:04X})",
            self.read_reg(reg::WY),
            reg::WY
        )?;
        writeln!(
            out,
            "WX  : {0:08b} ({1:04X})",
            self.read_reg(reg::WX),
            reg::WX
        )?;
        writeln!(out, "BG TILE MAP: {0:04X}", self.bg_tile_map_addr(),)?;
        Ok(())
    }

    fn write_reg(&mut self, reg: u16, val: u8) {
        self.registers[(reg - 0xFF40) as usize] = val;
    }

    fn read_reg(&self, reg: u16) -> u8 {
        self.registers[(reg - 0xFF40) as usize]
    }

    fn set_mode(&mut self, mode: u8) {
        let stat = self.read_reg(reg::STAT) & 0b11111000;
        self.write_reg(reg::STAT, stat | mode);
    }

    pub(crate) fn mode(&self) -> u8 {
        self.read_reg(reg::STAT) & 0b00000111
    }

    pub fn tick(&mut self) -> bool {
        if !self.is_powered_on() {
            return false;
        }

        self.clock = self.clock.wrapping_add(1);

        // If we've reached the end of our scanline, wrap the internal clock
        // and advance to the next scanline.
        if self.clock == CLOCKS_PER_SCANLINE {
            self.clock -= CLOCKS_PER_SCANLINE;
            let next_scanline = self.read_reg(reg::LY) + 1;
            self.write_reg(reg::LY, next_scanline % NUM_SCANLINES);
        }

        // Assume we don't need a VBlank, at first.
        let mut need_vblank = false;

        // Check our current scanline and set mode accordingly.
        match self.read_reg(reg::LY) {
            // Not in VBlank
            0..=143 => match self.clock {
                0 => self.set_mode(0),
                1..=19 => self.set_mode(2),
                20..=111 => self.set_mode(3),
                112..=113 => self.set_mode(0),
                CLOCKS_PER_SCANLINE.. => unreachable!(),
            },
            // VBlank is about to begin
            144 => match self.clock {
                0 => self.set_mode(0),
                1 => {
                    // VBlank begins -- signal interrupt
                    self.set_mode(1);
                    need_vblank = true;
                }
                2..=113 => self.set_mode(1),
                CLOCKS_PER_SCANLINE.. => unreachable!(),
            },
            // Still in VBlank
            145..=152 => self.set_mode(1),
            // Last VBlank cycle
            NUM_SCANLINES => match self.clock {
                0 => self.set_mode(1),
                1..=113 => {
                    self.set_mode(1);
                    self.write_reg(reg::LY, 0);
                }
                CLOCKS_PER_SCANLINE.. => unreachable!(),
            },
            _ => unreachable!(),
        };
        need_vblank
    }

    pub fn dma_prepare_transfer(&self, mmu: &MMU) -> Vec<u8> {
        let base = self.read_reg(reg::DMA);
        let addr = (base as u16) * 0x100;
        debug_log!("preparing for DMA transfer from {addr:04X}");
        mmu.block_load(addr, OAM_SIZE).to_vec()
    }

    pub fn dma_do_transfer(&mut self, data: Vec<u8>) {
        debug_log!("transferring {:02X?}... to OAM", &data[0..2]);
        assert_eq!(data.len(), OAM_SIZE);
        self.oam.copy_from_slice(&data);
    }

    fn is_powered_on(&self) -> bool {
        utils::bit_set(self.read_reg(reg::LCDC), 7)
    }

    fn oam_is_inaccessible(&self) -> bool {
        self.is_powered_on() && (self.mode() == 2 || self.mode() == 3)
    }

    fn vram_is_inaccessible(&self) -> bool {
        self.is_powered_on() && self.mode() == 3
    }

    pub fn load(&self, address: u16) -> u8 {
        match address {
            0xFF40..=0xFF4B => self.read_reg(address),
            VRAM_START..=VRAM_END if self.vram_is_inaccessible() => 0xFF,
            VRAM_START..=VRAM_END => self.vram[address - VRAM_START],
            OAM_START..=OAM_END if self.oam_is_inaccessible() => 0xFF,
            OAM_START..=OAM_END => self.oam[address - OAM_START],
            _ => panic!("read outside of LCD range!"),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            reg::STAT => {
                self.write_reg(reg::STAT, val & 0x80);
            }
            reg::LCDC => {
                let was_powered_on = self.is_powered_on();
                self.write_reg(reg::LCDC, val);
                if !was_powered_on && self.is_powered_on() {
                    self.write_reg(reg::LY, 0);
                }
            }
            0xFF40..=0xFF4B => {
                self.write_reg(address, val);
            }
            VRAM_START..=VRAM_END if self.vram_is_inaccessible() => {}
            VRAM_START..=VRAM_END => self.vram[address - VRAM_START] = val,
            OAM_START..=OAM_END if self.oam_is_inaccessible() => {}
            OAM_START..=OAM_END => self.oam[address - OAM_START] = val,
            _ => panic!("read outside of LCD range!"),
        }
    }

    pub fn block_load(&self, address: u16, len: usize) -> &[u8] {
        match address {
            // | VRAM   | Video RAM
            0x8000..=0x9FFF => {
                let addr = (address - VRAM_START) as usize;
                let len_to_load = std::cmp::min(VRAM_SIZE - addr, len as usize);
                self.vram.slice(addr as u16, len_to_load)
            }
            // | OAM    | Object Attribute Table
            0xFE00..=0xFE9F => {
                let addr = (address - OAM_START) as usize;
                let len_to_load = std::cmp::min(OAM_SIZE - addr, len as usize);
                self.oam.slice(addr as u16, len_to_load)
            }
            _ => panic!("unsupported block load!"),
        }
    }

    pub fn clear(screen: &mut [u8], color: u8) {
        screen.fill(color);
    }

    fn tile_at_signed_addr(&self, num: u8) -> &[u8] {
        const BASE: u16 = 0x9000;
        let addr = ((BASE as isize)
            + (num as i8 as isize) * (BYTES_PER_TILE as isize))
            as u16;
        self.vram.slice(addr - VRAM_START, BYTES_PER_TILE)
    }

    fn tile_at_unsigned_addr(&self, num: u8) -> &[u8] {
        const BASE: u16 = 0x8000;
        let addr = BASE + (num as usize * BYTES_PER_TILE) as u16;
        self.vram.slice(addr - VRAM_START, BYTES_PER_TILE)
    }

    fn bg_tile_map_addr(&self) -> u16 {
        const BG_TILE_MAP_1: u16 = 0x9800;
        const BG_TILE_MAP_2: u16 = 0x9C00;
        match utils::bit_set(self.read_reg(reg::LCDC), 3) {
            false => BG_TILE_MAP_1,
            true => BG_TILE_MAP_2,
        }
    }

    fn bg_tile_map(&self) -> &[u8] {
        self.vram
            .slice(self.bg_tile_map_addr() - VRAM_START, TILE_MAP_SIZE)
    }

    pub fn draw(&self, screen: &mut [u8]) {
        if !self.is_powered_on() {
            Self::clear(screen, colors::BLACK);
            return;
        }

        if self.mode() != 3 {
            return;
        }

        let bg_palette = [
            (self.read_reg(reg::BGP) & 0x03) >> 0, // COLOR NUM 0 (WHITE)
            (self.read_reg(reg::BGP) & 0x0C) >> 2, // COLOR NUM 1 (LGRAY)
            (self.read_reg(reg::BGP) & 0x30) >> 4, // COLOR NUM 2 (DGRAY)
            (self.read_reg(reg::BGP) & 0xC0) >> 6, // COLOR NUM 3 (BLACK)
        ];

        // Bit 0 - BG Enabled
        if !utils::bit_set(self.read_reg(reg::LCDC), 0) {
            Self::clear(screen, bg_palette[0]);
            return;
        }

        let nth_tile = match utils::bit_set(self.read_reg(reg::LCDC), 4) {
            false => Self::tile_at_signed_addr,
            true => Self::tile_at_unsigned_addr,
        };

        let (screen_x, screen_y) =
            (self.read_reg(reg::SCX), self.read_reg(reg::SCY));

        // Iterate over screen pixels and draw background
        let bg_tile_map = self.bg_tile_map();
        for y in 0..SCREEN_HEIGHT {
            for x in 0..SCREEN_WIDTH {
                let (bg_x, bg_y) = (
                    utils::wrapping_add(x, screen_x as usize, BG_WIDTH),
                    utils::wrapping_add(y, screen_y as usize, BG_HEIGHT),
                );

                // First locate which tile this pixel is in. Imagining a 20x18
                // grid of tiles, identify our tile's (x, y) by dividing each
                // component by the width and height, respectively. From this
                // coordinate, we can determine our tile offset, look up our
                // tile number, and finally get our tile's data.
                let (tile_x, tile_y) =
                    (bg_x / TILE_WIDTH_IN_PIXELS, bg_y / TILE_HEIGHT_IN_PIXELS);
                let tile_offset = tile_y * NUM_TILES_IN_X + tile_x;

                let tile_num = bg_tile_map[tile_offset];
                let tile = nth_tile(self, tile_num as u8);

                // Next, determine our sub-pixel location (within the tile), by
                // re-orienting our point's origin to the starting (x,y) of our
                // tile.
                let (sub_x, sub_y) =
                    (bg_x % TILE_WIDTH_IN_PIXELS, bg_y % TILE_HEIGHT_IN_PIXELS);
                let subpixel = sub_y * TILE_WIDTH_IN_PIXELS + sub_x;

                // Get our current pixel's color number from its tile, then map
                // that to according to the current palette, and write it to
                // the screen buffer.
                let cnum = pixel_color_num_in_tile(tile, subpixel);
                let color = bg_palette[cnum as usize];
                screen[y * SCREEN_WIDTH + x] = color;
            }
        }
    }
}

fn color_bit_in_tile(tile: &[u8], n: usize, offset: usize) -> u8 {
    // Start the numbering at the MSB of the 0th byte, proceeding down to the
    // LSB of the 0th byte, then advance to the MSB of the 2nd byte, and so on
    // until we've reached the LSB of the 15th byte, e.g:
    //
    // 0b01111100 (byte 0) 0b00000110 (byte 2)
    // 0b01111100 (byte 1) 0b11000000 (byte 3)
    //   03333300            22000110
    // n 01234567          n 89ABCDEF
    let (byte, bit) = ((n / 8) * 2, 7 - (n % 8));
    utils::get_bit(tile[byte as usize + offset], bit as u8)
}

fn pixel_color_num_in_tile(tile: &[u8], pixel: usize) -> u8 {
    (color_bit_in_tile(tile, pixel, 1) << 1) | color_bit_in_tile(tile, pixel, 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    struct ColorNumIterator<'a> {
        tile: &'a [u8],
        pixel: usize,
    }

    impl<'a> ColorNumIterator<'a> {
        pub fn new(d: &'a [u8]) -> ColorNumIterator<'a> {
            Self { tile: d, pixel: 0 }
        }
        fn next_color(&self) -> u8 {
            pixel_color_num_in_tile(&self.tile, self.pixel)
        }
    }

    impl Iterator for ColorNumIterator<'_> {
        type Item = u8;

        fn next(&mut self) -> Option<Self::Item> {
            if self.pixel >= PIXELS_PER_TILE {
                return None;
            }
            let next = self.next_color();
            self.pixel += 1;
            Some(next)
        }
    }

    fn to_colors(v: Vec<u8>) -> Vec<u8> {
        assert_eq!(v.len(), 16);
        ColorNumIterator::new(&v).collect()
    }

    #[rustfmt::skip]
    #[test]
    fn color_nums() {
        assert_eq!(
            to_colors(vec![
                0x7C, 0x7C, 0x00, 0xC6, 0xC6, 0x00, 0x00, 0xFE, 0xC6, 0xC6,
                0x00, 0xC6, 0xC6, 0x00, 0x00, 0x00
            ]),
            vec![
                0, 3, 3, 3, 3, 3, 0, 0,
                2, 2, 0, 0, 0, 2, 2, 0,
                1, 1, 0, 0, 0, 1, 1, 0,
                2, 2, 2, 2, 2, 2, 2, 0,
                3, 3, 0, 0, 0, 3, 3, 0,
                2, 2, 0, 0, 0, 2, 2, 0,
                1, 1, 0, 0, 0, 1, 1, 0,
                0, 0, 0, 0, 0, 0, 0, 0,
            ]
        );
        assert_eq!(
            to_colors(vec![
                0xFF, 0x00, 0x7E, 0xFF, 0x85, 0x81, 0x89, 0x83, 0x93, 0x85, 0xA5,
                0x8B, 0xC9, 0x97, 0x7E, 0xFF
            ]),
            vec![
                1, 1, 1, 1, 1, 1, 1, 1,
                2, 3, 3, 3, 3, 3, 3, 2,
                3, 0, 0, 0, 0, 1, 0, 3,
                3, 0, 0, 0, 1, 0, 2, 3,
                3, 0, 0, 1, 0, 2, 1, 3,
                3, 0, 1, 0, 2, 1, 2, 3,
                3, 1, 0, 2, 1, 2, 2, 3,
                2, 3, 3, 3, 3, 3, 3, 2,
            ]
        );
    }
}
