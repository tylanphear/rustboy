use serde::{Deserialize, Serialize};

pub mod mem;

pub mod constants {
    pub const EIGHT_K: usize = 0x2000;
    pub const SIXTEEN_K: usize = 0x4000;
    pub const THIRTY_TWO_K: usize = 0x8000;
    pub const SIXTY_FOUR_K: usize = 0x10000;
}

#[inline]
pub(crate) fn get_bit(byte: u8, idx: u8) -> u8 {
    (byte & (1 << idx)) >> idx
}

#[inline]
pub(crate) fn bit_set(byte: u8, idx: u8) -> bool {
    get_bit(byte, idx) != 0
}

#[inline]
pub(crate) fn lo_bits_of(x: usize, n: usize) -> usize {
    x & ((1 << n) - 1)
}

#[inline]
pub(crate) fn set_bit(byte: u8, idx: u8, flag: u8) -> u8 {
    let flag_bool = flag != 0;
    let flag_mask = -(flag_bool as i8) as u8;
    let bit_mask = 1 << idx;
    (byte & !bit_mask) | (flag_mask & bit_mask)
}

#[inline]
pub(crate) fn wrapping_add(a: usize, b: usize, bound: usize) -> usize {
    if a + b >= bound {
        (a + b) % bound
    } else {
        a + b
    }
}

#[inline]
pub(crate) fn round_to_nearest_multiple_of(a: usize, n: usize) -> usize {
    ((a + (n - 1)) / n) * n
}

pub(crate) fn disp_chunks(
    block: &[u8],
    start: usize,
    chunk_size: usize,
) -> String {
    let mut disp = String::from("       ");
    for idx in 0..chunk_size {
        disp.push_str(&format!("{:02X}  ", idx));
    }
    disp.push_str("\n");
    let chunks = block.len() / chunk_size;
    for n in 0..chunks {
        let needle = n * chunk_size;
        disp.push_str(&format!(
            "{0:04X}: {1:02X?}\n",
            start + needle,
            &block[needle..needle + chunk_size]
        ));
    }
    disp
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Reg16([u8; 2]);

impl Reg16 {
    pub fn as_word(&self) -> u16 {
        u16::from_le_bytes(self.0)
    }
    pub fn set_word(&mut self, val: u16) {
        self.0 = val.to_le_bytes();
    }
    pub fn inc(&mut self) {
        self.set_word(self.as_word().wrapping_add(1));
    }
    pub fn dec(&mut self) {
        self.set_word(self.as_word().wrapping_sub(1));
    }
}
impl std::ops::Index<usize> for Reg16 {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}
impl std::ops::IndexMut<usize> for Reg16 {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Regs {
    //                | 15 .. 8   | 7 .. 0    |
    pub af: Reg16, // | A (af[1]) | F (af[0]) |
    pub bc: Reg16, // | B (bc[1]) | C (bc[0]) |
    pub de: Reg16, // | D (de[1]) | E (de[0]) |
    pub hl: Reg16, // | H (hl[1]) | L (hl[0]) |
    pub sp: u16,
    pub pc: u16,
}

impl Regs {
    pub fn carry_flag(&self) -> u8 {
        crate::utils::get_bit(self.af[0], 4)
    }
    pub fn set_carry_flag(&mut self, val: u8) {
        self.af[0] = crate::utils::set_bit(self.af[0], 4, val);
    }
    pub fn halfcarry_flag(&self) -> u8 {
        crate::utils::get_bit(self.af[0], 5)
    }
    pub fn set_halfcarry_flag(&mut self, val: u8) {
        self.af[0] = crate::utils::set_bit(self.af[0], 5, val);
    }
    pub fn sub_flag(&self) -> u8 {
        crate::utils::get_bit(self.af[0], 6)
    }
    pub fn set_sub_flag(&mut self, val: u8) {
        self.af[0] = crate::utils::set_bit(self.af[0], 6, val);
    }
    pub fn zero_flag(&self) -> u8 {
        crate::utils::get_bit(self.af[0], 7)
    }
    pub fn set_zero_flag(&mut self, val: u8) {
        self.af[0] = crate::utils::set_bit(self.af[0], 7, val);
    }
}

#[derive(Debug)]
pub struct BoundedLog<const MAX_SIZE: usize, const DRAIN_LINES: usize> {
    buffer: String,
}

impl<const S: usize, const L: usize> Default for BoundedLog<S, L> {
    fn default() -> Self {
        Self {
            buffer: String::with_capacity(S),
        }
    }
}

impl<const S: usize, const L: usize> BoundedLog<S, L> {
    pub fn as_str(&self) -> &str {
        &self.buffer
    }

    pub fn clear(&mut self) {
        self.buffer.clear();
    }

    pub fn push(&mut self, s: &str) {
        if self.buffer.len() + s.len() > S {
            // Drain off the first L lines when the buffer gets sufficiently full
            let idx = self
                .buffer
                .char_indices()
                .filter_map(|(idx, c)| (c == '\n').then_some(idx))
                .nth(L)
                .unwrap_or(0);
            self.buffer.drain(0..idx);
        }
        self.buffer.push_str(s);
    }
}

pub fn spin_sleep(duration: std::time::Duration) {
    let start = std::time::Instant::now();
    // Trust std::thread::sleep within 125 micros
    std::thread::sleep(
        duration.saturating_sub(std::time::Duration::from_micros(125)),
    );

    // Now that we've slept for duration - 125 micros, busy wait for the
    // remainder time, if any.
    while start.elapsed() < duration {
        std::thread::yield_now();
    }
}

pub trait Dump {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result;
}
