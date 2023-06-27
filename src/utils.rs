use serde::{Deserialize, Serialize};

pub mod mem;

pub mod constants {
    pub const EIGHT_K: usize = 0x2000;
    pub const SIXTEEN_K: usize = 0x4000;
    pub const THIRTY_TWO_K: usize = 0x8000;
    pub const SIXTY_FOUR_K: usize = 0x10000;
}

#[inline]
pub(crate) fn get_bit<N>(byte: N, idx: usize) -> N
where
    N: num::traits::PrimInt,
{
    (byte & (N::one() << idx)) >> idx
}

#[inline]
pub(crate) fn bit_set<N>(byte: N, idx: usize) -> bool
where
    N: num::traits::PrimInt,
{
    get_bit::<N>(byte, idx) != N::zero()
}

#[inline]
pub(crate) fn lo_bits_of(x: usize, n: usize) -> usize {
    x & ((1 << n) - 1)
}

#[inline]
pub(crate) fn set_bit<N: num::PrimInt + From<u8>>(
    byte: N,
    idx: usize,
    flag: N,
) -> N {
    let flag_mask = flag.signed_shl(idx as u32);
    let bit_mask = N::one() << idx;
    (byte & !bit_mask) | (flag_mask & bit_mask)
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
    disp.push('\n');
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_bits() {
        for n in 0..7 {
            assert_eq!(get_bit(0b1111_1111, n), 1);
        }
        assert_eq!(get_bit(0b1111_0000, 7), 1);
        assert_eq!(get_bit(0b1111_0000, 4), 1);
        assert_eq!(get_bit(0b1111_0000, 3), 0);
        assert_eq!(get_bit(0b1111_0000, 0), 0);
    }

    #[test]
    fn set_bits() {
        // 8 bit ops
        assert_eq!(set_bit(0b0000_0000, 0, 0), 0b0000_0000);
        assert_eq!(set_bit(0b0000_0000, 1, 0), 0b0000_0000);
        assert_eq!(set_bit(0b0000_0000, 7, 0), 0b0000_0000);
        assert_eq!(set_bit(0b0000_0000, 0, 1), 0b0000_0001);
        assert_eq!(set_bit(0b0000_0000, 1, 1), 0b0000_0010);
        assert_eq!(set_bit(0b0000_0000, 7, 1), 0b1000_0000);
        assert_eq!(set_bit(0b1111_1111, 0, 0), 0b1111_1110);
        assert_eq!(set_bit(0b1111_1111, 1, 0), 0b1111_1101);
        assert_eq!(set_bit(0b1111_1111, 7, 0), 0b0111_1111);
        assert_eq!(set_bit(0b1111_1111, 0, 1), 0b1111_1111);
        assert_eq!(set_bit(0b1111_1111, 1, 1), 0b1111_1111);
        assert_eq!(set_bit(0b1111_1111, 7, 1), 0b1111_1111);

        // 16 bit ops
        assert_eq!(set_bit(0b0000_0000_0000_0000, 0, 0), 0b0000_0000_0000_0000);
        assert_eq!(set_bit(0b0000_0000_0000_0000, 1, 0), 0b0000_0000_0000_0000);
        assert_eq!(set_bit(0b0000_0000_0000_0000, 7, 0), 0b0000_0000_0000_0000);
        assert_eq!(
            set_bit(0b0000_0000_0000_0000, 11, 0),
            0b0000_0000_0000_0000
        );
        assert_eq!(set_bit(0b0000_0000_0000_0000, 0, 1), 0b0000_0000_0000_0001);
        assert_eq!(set_bit(0b0000_0000_0000_0000, 1, 1), 0b0000_0000_0000_0010);
        assert_eq!(set_bit(0b0000_0000_0000_0000, 7, 1), 0b0000_0000_1000_0000);
        assert_eq!(
            set_bit(0b0000_0000_0000_0000, 11, 1),
            0b0000_1000_0000_0000
        );
        assert_eq!(set_bit(0b1111_1111_1111_1111, 0, 0), 0b1111_1111_1111_1110);
        assert_eq!(set_bit(0b1111_1111_1111_1111, 1, 0), 0b1111_1111_1111_1101);
        assert_eq!(set_bit(0b1111_1111_1111_1111, 7, 0), 0b1111_1111_0111_1111);
        assert_eq!(
            set_bit(0b1111_1111_1111_1111, 11, 0),
            0b1111_0111_1111_1111
        );
        assert_eq!(set_bit(0b1111_1111_1111_1111, 0, 1), 0b1111_1111_1111_1111);
        assert_eq!(set_bit(0b1111_1111_1111_1111, 1, 1), 0b1111_1111_1111_1111);
        assert_eq!(set_bit(0b1111_1111_1111_1111, 7, 1), 0b1111_1111_1111_1111);
        assert_eq!(
            set_bit(0b1111_1111_1111_1111, 11, 1),
            0b1111_1111_1111_1111
        );
    }
}
