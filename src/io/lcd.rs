use std::cell::Cell;

use serde::{Deserialize, Serialize};

use crate::utils;
use crate::utils::mem::Mem;

// LCD display is 160x144 pixels
pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144;
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
const TILE_MAP_1: u16 = 0x9800;
const TILE_MAP_2: u16 = 0x9C00;
const TILE_SET_1: u16 = 0x8000;
const TILE_SET_2: u16 = 0x8800;

// 4 gray shades:
pub mod colors {
    pub const WHITE: u8 = 0;
    pub const LGRAY: u8 = 1;
    pub const DGRAY: u8 = 2;
    pub const BLACK: u8 = 3;
}

#[rustfmt::skip]
pub mod reg {
    /// Bit 7 - LCD Power           (0=Off, 1=On)
    /// Bit 6 - Window Tile Map     (0=0x9800-0x9BFF, 1=0x9C00-0x9FFF)
    /// Bit 5 - Window Enable       (0=Disabled, 1=Enabled)
    /// Bit 4 - BG & Window Tileset (0=0x8800-0x97FF, 1=0x8000-0x8FFF)
    /// Bit 3 - BG Tile Map         (0=0x9800-0x9BFF, 1=0x9C00-0x9FFF)
    /// Bit 2 - Sprite Size         (0=8x8, 1=8x16)
    /// Bit 1 - Sprites Enabled     (0=Disabled, 1=Enabled)
    /// Bit 0 - BG Enabled          (0=Disabled, 1=Enabled)
    pub const LCDC: u16 = 0xFF40;

    /// Bit 7 - Unused (Always 1)
    /// Bit 6 - LY=LYC Check Enable        (1=Enable) (RW)
    /// Bit 5 - Mode 2 OAM Check Enable    (1=Enable) (RW)
    /// Bit 4 - Mode 1 VBlank Check Enable (1=Enable) (RW)
    /// Bit 3 - Mode 0 HBlank Check Enable (1=Enable) (RW)
    /// Bit 2 - LY=LYC Comparison Signal   (1:LYC=LY) (RO)
    /// Bit 1 |
    /// Bit 0 - Screen Mode                (Mode 0-3) (RO)
    ///         0: HBlank
    ///         1: VBlank
    ///         2: Searching OAM
    ///         3: Tranferring data to LCD
    pub const STAT: u16 = 0xFF41;

    /// BG Scroll Y
    pub const SCY : u16 = 0xFF42;
    /// BG Scroll X
    pub const SCX : u16 = 0xFF43;

    /// Current scanline
    pub const LY  : u16 = 0xFF44;
    /// Scanline compare register
    pub const LYC : u16 = 0xFF45;

    /// Address to request DMA to OAM
    pub const DMA : u16 = 0xFF46;

    /// Bit 7-6 - Shade for Color 3
    /// Bit 5-4 - Shade for Color 2
    /// Bit 3-2 - Shade for Color 1
    /// Bit 1-0 - Shade for Color 0
    pub const BGP : u16 = 0xFF47;

    pub const OBP0: u16 = 0xFF48;
    pub const OBP1: u16 = 0xFF49;
    pub const WY  : u16 = 0xFF4A;
    pub const WX  : u16 = 0xFF4B;
}

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
const NUM_SCANLINES: u8 = 154;
const VRAM_START: u16 = 0x8000;
const VRAM_END: u16 = VRAM_START + (VRAM_SIZE as u16) - 1;
const VRAM_SIZE: usize = 0x2000;
const OAM_START: u16 = 0xFE00;
const OAM_END: u16 = OAM_START + (OAM_SIZE as u16) - 1;
const OAM_SIZE: usize = 0xA0;

type Vram = Mem<VRAM_SIZE>;
type Oam = Mem<OAM_SIZE>;

enum TileAddressMode {
    Signed,
    Unsigned,
}

enum ObjSize {
    Normal,
    Large,
}

pub type ScreenBuffer = Mem<{ SCREEN_WIDTH * SCREEN_HEIGHT }>;
pub trait ExternalScreenBuffer {
    const NUM_RAW_PIXELS: usize;
    fn write_pixel(&mut self, idx: usize, raw_pixel: u8);
    fn slice(&self, start: usize, end: usize) -> &[u8];
}

pub struct RenderUpdate {
    pub start_scanline: usize,
    pub end_scanline: usize,
}

impl RenderUpdate {
    pub fn num_scanlines(&self) -> usize {
        self.end_scanline - self.start_scanline
    }
    pub fn section<'a, T: ExternalScreenBuffer>(&self, buf: &'a T) -> &'a [u8] {
        buf.slice(
            self.start_scanline * SCREEN_WIDTH,
            self.end_scanline * SCREEN_WIDTH,
        )
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct LCDController {
    screen: ScreenBuffer,
    clock: u64,
    registers: [u8; 0xFF4C - 0xFF40],
    pub(crate) vram: Vram,
    pub(crate) oam: Oam,
    last_rendered_scanline: Cell<usize>,
    stat_signal: bool,
}

impl LCDController {
    pub fn reset(&mut self) {
        self.clock = 0;
        self.registers = Default::default();
        self.vram.clear();
        self.oam.clear();
        self.last_rendered_scanline = Cell::new(0);
        self.stat_signal = false;
    }

    fn write_reg(&mut self, reg: u16, val: u8) {
        self.registers[(reg - 0xFF40) as usize] = val;
    }

    fn read_reg(&self, reg: u16) -> u8 {
        self.registers[(reg - 0xFF40) as usize]
    }

    fn set_mode(&mut self, mode: u8) {
        let stat = self.read_reg(reg::STAT) & 0b11111100;
        self.write_reg(reg::STAT, stat | mode);
    }

    pub(crate) fn mode(&self) -> u8 {
        self.read_reg(reg::STAT) & 0b00000111
    }

    pub fn tick(&mut self) -> (bool, bool) {
        if !self.lcd_display_enabled() {
            return (false, false);
        }

        // Assume we don't need any interrupts, at first.
        let mut need_vblank_interrupt = false;
        let mut need_stat_interrupt = false;
        let stat_interrupt_on_lyc = utils::bit_set(self.read_reg(reg::STAT), 6);
        let stat_interrupt_on_mode2 =
            utils::bit_set(self.read_reg(reg::STAT), 5);
        let stat_interrupt_on_mode1 =
            utils::bit_set(self.read_reg(reg::STAT), 4);
        let stat_interrupt_on_mode0 =
            utils::bit_set(self.read_reg(reg::STAT), 3);

        self.clock = self.clock.wrapping_add(1);

        // Check for LY == LYC
        let ly_equals_lyc = self.read_reg(reg::LY) == self.read_reg(reg::LYC);
        self.write_reg(
            reg::STAT,
            (self.read_reg(reg::STAT) & 0b1111_1011)
                | ((ly_equals_lyc as u8) << 2),
        );

        // If we've reached the end of our scanline, wrap the internal clock
        // and advance to the next scanline.
        if self.clock == CLOCKS_PER_SCANLINE {
            self.clock = 0;
            self.write_reg(reg::LY, self.read_reg(reg::LY).wrapping_add(1));
        }

        let old_stat_signal = self.stat_signal;

        // Check our current scanline and set mode accordingly.
        match self.read_reg(reg::LY) {
            // Not in VBlank
            0..=143 => match self.clock {
                0 => self.set_mode(0),
                1 => self.set_mode(2),
                21 => self.set_mode(3),
                112 => self.set_mode(0),
                _ => {}
            },
            // VBlank is about to begin
            144 => match self.clock {
                0 => self.set_mode(0),
                1 => {
                    // VBlank begins -- signal interrupt
                    self.set_mode(1);
                    need_vblank_interrupt = true;
                }
                _ => {}
            },
            // Last VBlank cycle
            NUM_SCANLINES => match self.clock {
                0 => self.set_mode(1),
                1 => self.write_reg(reg::LY, 0),
                _ => {}
            },
            _ => {}
        };

        self.stat_signal = (ly_equals_lyc && stat_interrupt_on_lyc)
            || (self.mode() == 0 && stat_interrupt_on_mode0)
            || (self.mode() == 1 && stat_interrupt_on_mode1)
            || (self.mode() == 2 && stat_interrupt_on_mode2);
        if !old_stat_signal && self.stat_signal {
            need_stat_interrupt = true;
        }

        self.update_screen();

        (need_vblank_interrupt, need_stat_interrupt)
    }

    #[inline]
    pub fn dma_do_transfer(&mut self, offset: u16, byte: u8) {
        assert!((offset as usize) < OAM_SIZE);
        self.oam[offset] = byte;
    }

    fn lcd_display_enabled(&self) -> bool {
        utils::bit_set(self.read_reg(reg::LCDC), 7)
    }

    fn window_enabled(&self) -> bool {
        utils::bit_set(self.read_reg(reg::LCDC), 5)
    }

    fn bg_and_window_enabled(&self) -> bool {
        utils::bit_set(self.read_reg(reg::LCDC), 0)
    }

    fn oam_is_inaccessible(&self) -> bool {
        self.lcd_display_enabled() && (self.mode() == 2 || self.mode() == 3)
    }

    fn obj_enabled(&self) -> bool {
        utils::bit_set(self.read_reg(reg::LCDC), 1)
    }

    fn obj_size(&self) -> ObjSize {
        match utils::bit_set(self.read_reg(reg::LCDC), 2) {
            false => ObjSize::Normal,
            true => ObjSize::Large,
        }
    }

    fn vram_is_inaccessible(&self) -> bool {
        self.lcd_display_enabled() && self.mode() == 3
    }

    pub fn load(&self, address: u16) -> u8 {
        match address {
            reg::DMA => panic!("should be handled by MMU!"),
            reg::LY if !self.lcd_display_enabled() => 0x00,
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
                self.write_reg(reg::STAT, (val & 0b01111000) | 0b10000000);
            }
            reg::LY => {}
            reg::DMA => panic!("should be handled by MMU!"),
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
                let len_to_load = std::cmp::min(VRAM_SIZE - addr, len);
                self.vram.slice(addr as u16, len_to_load)
            }
            // | OAM    | Object Attribute Table
            0xFE00..=0xFE9F => {
                let addr = (address - OAM_START) as usize;
                let len_to_load = std::cmp::min(OAM_SIZE - addr, len);
                self.oam.slice(addr as u16, len_to_load)
            }
            _ => panic!("unsupported block load!"),
        }
    }

    pub fn clear(&mut self, color: u8) {
        self.screen.fill(color);
    }

    #[inline]
    fn tile_at_signed_addr(vram: &Vram, num: u8) -> &[u8] {
        const BASE: u16 = TILE_SET_2 + 0x800;
        let tile_offset = (num as i8 as isize) * BYTES_PER_TILE as isize;
        let addr = ((BASE as isize) + tile_offset) as u16;
        vram.slice(addr - VRAM_START, BYTES_PER_TILE)
    }

    #[inline]
    fn tile_at_unsigned_addr(vram: &Vram, num: u8) -> &[u8] {
        const BASE: u16 = TILE_SET_1;
        let tile_offset = num as usize * BYTES_PER_TILE;
        let addr = BASE + tile_offset as u16;
        vram.slice(addr - VRAM_START, BYTES_PER_TILE)
    }

    #[inline]
    fn bg_tile_map_addr(&self) -> u16 {
        match utils::bit_set(self.read_reg(reg::LCDC), 3) {
            false => TILE_MAP_1,
            true => TILE_MAP_2,
        }
    }

    #[inline]
    fn window_tile_map_addr(&self) -> u16 {
        match utils::bit_set(self.read_reg(reg::LCDC), 6) {
            false => TILE_MAP_1,
            true => TILE_MAP_2,
        }
    }

    #[inline]
    pub fn render<S: ExternalScreenBuffer>(
        &self,
        buffer: &mut S,
    ) -> RenderUpdate {
        let last_scanline = self.last_rendered_scanline.get();
        let current_scanline = self.read_reg(reg::LY) as usize;
        let (start_scanline, end_scanline) = if last_scanline < current_scanline
        {
            (
                std::cmp::min(SCREEN_HEIGHT, last_scanline),
                std::cmp::min(SCREEN_HEIGHT, current_scanline),
            )
        } else {
            (0, SCREEN_HEIGHT)
        };
        let (start, end) =
            (start_scanline * SCREEN_WIDTH, end_scanline * SCREEN_WIDTH);
        for (idx, raw_pixel) in
            self.screen.as_slice()[start..end].iter().enumerate()
        {
            buffer.write_pixel(start + idx, *raw_pixel);
        }
        self.last_rendered_scanline.set(current_scanline);
        RenderUpdate {
            start_scanline,
            end_scanline,
        }
    }

    fn reg_to_palette(reg: u8) -> [u8; 4] {
        [
            (reg >> 0) & 0b11, // COLOR NUM 0 (WHITE)
            (reg >> 2) & 0b11, // COLOR NUM 1 (LGRAY)
            (reg >> 4) & 0b11, // COLOR NUM 2 (DGRAY)
            (reg >> 6) & 0b11, // COLOR NUM 3 (BLACK)
        ]
    }

    pub fn update_screen(&mut self) {
        if !self.lcd_display_enabled() {
            self.clear(colors::WHITE);
            return;
        }

        if self.mode() != 3 {
            return;
        }

        let bg_palette = Self::reg_to_palette(self.read_reg(reg::BGP));

        if !self.bg_and_window_enabled() {
            self.clear(bg_palette[0]);
            return;
        }

        let scanline = self.read_reg(reg::LY);
        if scanline < 144 {
            self.draw_background_line(&bg_palette, scanline);

            if self.window_enabled() {
                self.draw_window_line(&bg_palette, scanline);
            }

            if self.obj_enabled() {
                self.draw_oam_line(scanline);
            }
        }
    }

    fn draw_background_line(&mut self, bg_palette: &[u8; 4], scanline: u8) {
        let y = scanline as usize;

        let bg_tile_map = self
            .vram
            .slice(self.bg_tile_map_addr() - VRAM_START, TILE_MAP_SIZE);
        let tile_address_mode = self.tile_address_mode();
        let get_tile = |x: usize, y: usize| {
            let tile_num = bg_tile_map[y * NUM_TILES_IN_X + x];
            match tile_address_mode {
                TileAddressMode::Signed => {
                    Self::tile_at_signed_addr(&self.vram, tile_num)
                }
                TileAddressMode::Unsigned => {
                    Self::tile_at_unsigned_addr(&self.vram, tile_num)
                }
            }
        };

        let (scroll_x, scroll_y) = (
            self.read_reg(reg::SCX) as usize,
            self.read_reg(reg::SCY) as usize,
        );
        let scrolled_y = utils::wrapping_add(y, scroll_y, BG_HEIGHT);
        let tile_y = scrolled_y / TILE_HEIGHT_IN_PIXELS;
        let sub_tile_y = scrolled_y % TILE_HEIGHT_IN_PIXELS;

        // Handle the first tile specially. This is to handle the case if SCX %
        // TILE_WIDTH_IN_PIXELS != 0. We will need to draw the last
        // `TILE_WIDTH_IN_PIXELS - (SCX % TILE_WIDTH_IN_PIXELS)` pixels
        // specially.
        let initial_shifted_pixels = scroll_x % TILE_WIDTH_IN_PIXELS;
        let first_tile_pixels = TILE_WIDTH_IN_PIXELS - initial_shifted_pixels;
        let initial_tile_x = scroll_x / TILE_WIDTH_IN_PIXELS;
        let initial_tile = get_tile(initial_tile_x, tile_y);
        for sub_tile_x in initial_shifted_pixels..TILE_WIDTH_IN_PIXELS {
            let subpixel = sub_tile_y * TILE_WIDTH_IN_PIXELS + sub_tile_x;
            self.screen
                [y * SCREEN_WIDTH + sub_tile_x - initial_shifted_pixels] =
                bg_palette
                    [pixel_color_num_in_tile(initial_tile, subpixel) as usize];
        }
        // Having made sure we handled the first tile, now draw all of the full tiles.
        let (first_x, last_x) =
            (first_tile_pixels, SCREEN_WIDTH - initial_shifted_pixels);
        for x in (first_x..last_x).step_by(TILE_WIDTH_IN_PIXELS) {
            let scrolled_x = utils::wrapping_add(x, scroll_x, BG_WIDTH);
            let tile_x = scrolled_x / TILE_WIDTH_IN_PIXELS;
            let tile = get_tile(tile_x, tile_y);
            for sub_tile_x in 0..TILE_WIDTH_IN_PIXELS {
                let subpixel = sub_tile_y * TILE_WIDTH_IN_PIXELS + sub_tile_x;
                self.screen[y * SCREEN_WIDTH + x + sub_tile_x] = bg_palette
                    [pixel_color_num_in_tile(tile, subpixel) as usize];
            }
        }

        // If we had a partial first tile, we have a partial last tile. Handle
        // that case here.
        let remaining_pixels = SCREEN_WIDTH - last_x;
        if remaining_pixels > 0 {
            let scrolled_remainder_x =
                utils::wrapping_add(last_x, scroll_x, BG_WIDTH);
            let remainder_tile_x = scrolled_remainder_x / TILE_WIDTH_IN_PIXELS;
            let remainder_tile = get_tile(remainder_tile_x, tile_y);
            for sub_tile_x in 0..remaining_pixels {
                let subpixel = sub_tile_y * TILE_WIDTH_IN_PIXELS + sub_tile_x;
                self.screen[y * SCREEN_WIDTH + last_x + sub_tile_x] =
                    bg_palette[pixel_color_num_in_tile(remainder_tile, subpixel)
                        as usize];
            }
        }
    }

    fn draw_window_line(&mut self, bg_palette: &[u8; 4], scanline: u8) {
        let y = scanline as usize;
        let win_y = self.read_reg(reg::WY) as usize;
        if y < win_y {
            return;
        }
        let window_tile_map = self
            .vram
            .slice(self.window_tile_map_addr() - VRAM_START, TILE_MAP_SIZE);
        for x in 0..SCREEN_WIDTH {
            let win_x_plus_7 = self.read_reg(reg::WX) as usize;
            if x + 7 < win_x_plus_7 {
                continue;
            }
            let color = self.get_tile_pixel_color(
                (x + 7) - win_x_plus_7,
                y - win_y,
                window_tile_map,
                bg_palette,
            );
            self.screen[y * SCREEN_WIDTH + x] = color;
        }
    }

    fn draw_oam_line(&mut self, scanline: u8) {
        let obj_size = self.obj_size();
        let obj_palette_0 = Self::reg_to_palette(self.read_reg(reg::OBP0));
        let obj_palette_1 = Self::reg_to_palette(self.read_reg(reg::OBP1));
        for sprite in self.oam.as_slice().chunks(4) {
            let [y_plus_16, x_plus_8, tile_num, attrs] = *sprite else {
                unreachable!("odd sized sprite?")
            };

            // We want to know whether the current scanline falls within this sprite.
            // To do so, we check if top_of_sprite_y <= scanline < bot_of_sprite_y.
            //
            // Notice that top_of_sprite_y = y_plus_16 - 16;
            // Notice that bot_of_sprite_y = top_of_sprite_y + obj_height;
            //
            // Hence y_plus_16 - 16 <= scanline < y_plus_16 - 16 + obj_height, or
            //       y_plus_16 <= scanline + 16 < y_plus_16 + obj_height
            //
            // Inverting conditions to decide when we should *skip* the sprite, we see:
            //       y_plus_16 > scanline + 16 ||
            //       scanline + 16 >= y_plus_16 + obj_height
            let obj_height = match obj_size {
                ObjSize::Normal => 8,
                ObjSize::Large => 16,
            };
            if y_plus_16 > scanline + 16
                || scanline + 16 >= y_plus_16 + obj_height
            {
                continue;
            }

            let bg_takes_priority = utils::bit_set(attrs, 7);
            let y_flip = utils::bit_set(attrs, 6);
            let x_flip = utils::bit_set(attrs, 5);
            let palette = match utils::bit_set(attrs, 4) {
                false => obj_palette_0,
                true => obj_palette_1,
            };

            let tile_y = scanline + 16 - y_plus_16;
            let tile_y = match y_flip {
                false => tile_y,
                true => obj_height - tile_y - 1,
            };

            match obj_size {
                ObjSize::Normal => {
                    Self::draw_obj_line(
                        &mut self.screen,
                        Self::tile_at_unsigned_addr(&self.vram, tile_num),
                        &palette,
                        tile_y as usize,
                        scanline as usize,
                        x_plus_8 as usize,
                        x_flip,
                        bg_takes_priority,
                    );
                }
                ObjSize::Large => {
                    if tile_y < 8 {
                        // top tile
                        Self::draw_obj_line(
                            &mut self.screen,
                            Self::tile_at_unsigned_addr(
                                &self.vram,
                                tile_num & 0xFE,
                            ),
                            &palette,
                            tile_y as usize,
                            scanline as usize,
                            x_plus_8 as usize,
                            x_flip,
                            bg_takes_priority,
                        );
                    } else {
                        // bot tile
                        Self::draw_obj_line(
                            &mut self.screen,
                            Self::tile_at_unsigned_addr(
                                &self.vram,
                                tile_num | 0x01,
                            ),
                            &palette,
                            (tile_y - 8) as usize,
                            scanline as usize,
                            x_plus_8 as usize,
                            x_flip,
                            bg_takes_priority,
                        );
                    };
                }
            }
        }
    }

    #[inline]
    fn draw_obj_line(
        screen: &mut ScreenBuffer,
        tile_data: &[u8],
        palette: &[u8; 4],
        tile_y: usize,
        screen_y: usize,
        x_plus_8: usize,
        x_flip: bool,
        bg_takes_priority: bool,
    ) {
        for tile_x in 0..8 {
            let pixel = tile_y * 8 + tile_x;
            let tile_x = match x_flip {
                false => tile_x,
                true => 8 - tile_x - 1,
            };
            if x_plus_8 + tile_x < 8 {
                continue;
            }
            let screen_x = x_plus_8 + tile_x - 8;
            if screen_x >= SCREEN_WIDTH {
                continue;
            }
            let color_num = pixel_color_num_in_tile(tile_data, pixel);
            if color_num == 0 {
                continue;
            }
            let color = palette[color_num as usize];
            let offset = screen_y * SCREEN_WIDTH + screen_x;
            let bg_color = screen[offset];
            if bg_takes_priority && bg_color != colors::WHITE {
                continue;
            }
            screen[offset] = color;
        }
    }

    fn get_tile_pixel_color(
        &self,
        x: usize,
        y: usize,
        tile_map: &[u8],
        palette: &[u8; 4],
    ) -> u8 {
        // First locate which tile this pixel is in. Imagining a 20x18 grid of
        // tiles, identify our tile's (x, y) by dividing each component by the
        // width and height, respectively. From this coordinate, we can
        // determine our tile offset, look up our tile number, and get our
        // tile's data.
        let (tile_x, tile_y) =
            (x / TILE_WIDTH_IN_PIXELS, y / TILE_HEIGHT_IN_PIXELS);
        let tile_offset = tile_y * NUM_TILES_IN_X + tile_x;

        let tile_num = tile_map[tile_offset];
        let tile = match self.tile_address_mode() {
            TileAddressMode::Signed => {
                Self::tile_at_signed_addr(&self.vram, tile_num)
            }
            TileAddressMode::Unsigned => {
                Self::tile_at_unsigned_addr(&self.vram, tile_num)
            }
        };

        // Next, determine our sub-tile pixel location by re-orienting our
        // point's origin to the starting (x,y) of our tile.
        let (sub_x, sub_y) =
            (x % TILE_WIDTH_IN_PIXELS, y % TILE_HEIGHT_IN_PIXELS);
        let subpixel = sub_y * TILE_WIDTH_IN_PIXELS + sub_x;

        // Get our current pixel's color number from its tile, then map
        // that to according to the current palette, and write it to
        // the screen buffer.
        let color_num = pixel_color_num_in_tile(tile, subpixel);
        palette[color_num as usize]
    }

    fn tile_address_mode(&self) -> TileAddressMode {
        match utils::bit_set(self.read_reg(reg::LCDC), 4) {
            false => TileAddressMode::Signed,
            true => TileAddressMode::Unsigned,
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
    utils::get_bit(tile[byte + offset], bit as u8)
}

fn pixel_color_num_in_tile(tile: &[u8], pixel: usize) -> u8 {
    (color_bit_in_tile(tile, pixel, 1) << 1) | color_bit_in_tile(tile, pixel, 0)
}

impl crate::utils::Dump for LCDController {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(
            out,
            "LCDC: {0:08b} ({1:04X})",
            self.load(reg::LCDC),
            reg::LCDC
        )?;
        writeln!(
            out,
            "STAT: {0:08b} ({1:04X})",
            self.load(reg::STAT),
            reg::STAT
        )?;
        writeln!(out, "SCX : {0:08} ({1:04X})", self.load(reg::SCX), reg::SCX)?;
        writeln!(out, "SCY : {0:08} ({1:04X})", self.load(reg::SCY), reg::SCY)?;
        writeln!(out, "LY  : {0:08} ({1:04X})", self.load(reg::LY), reg::LY)?;
        writeln!(out, "LYC : {0:08} ({1:04X})", self.load(reg::LYC), reg::LYC)?;
        writeln!(
            out,
            "BGP : {0:08b} ({1:04X})",
            self.load(reg::BGP),
            reg::BGP
        )?;
        writeln!(out, "WY  : {0:08} ({1:04X})", self.load(reg::WY), reg::WY)?;
        writeln!(out, "WX  : {0:08} ({1:04X})", self.load(reg::WX), reg::WX)?;
        if self.bg_and_window_enabled() {
            writeln!(out, "BG TILE MAP: {0:04X}", self.bg_tile_map_addr())?;
        } else {
            writeln!(out, "BG DISABLED")?;
        }
        if self.window_enabled() {
            writeln!(out, "WI TILE MAP: {0:04X}", self.window_tile_map_addr())?;
        } else {
            writeln!(out, "WI DISABLED")?;
        }
        writeln!(
            out,
            "TILESET: {0:04X}",
            match self.tile_address_mode() {
                TileAddressMode::Signed => TILE_SET_2,
                TileAddressMode::Unsigned => TILE_SET_1,
            }
        )?;
        Ok(())
    }
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

    #[test]
    fn vblank_interrupt_is_triggered() {
        let mut cpu = crate::CPU::new();
        cpu.stop();
        // First enable LCD power
        cpu.mmu.store8_unchecked(crate::io::lcd::reg::LCDC, 0x80);
        assert_eq!(cpu.mmu.io.lcd.mode(), 0);
        // Simulate 144 lines before VBlank
        for _ in 0..144 {
            for _ in 0..CLOCKS_PER_SCANLINE {
                cpu.tick();
            }
        }
        assert_eq!(cpu.mmu.io.lcd.mode(), 0);
        cpu.tick();
        assert_eq!(cpu.mmu.io.lcd.mode(), 1);
        assert_eq!(cpu.mmu.load8(crate::cpu::regs::IF) & 0x1, 0x1);
    }
}
