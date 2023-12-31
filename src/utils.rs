use serde::{Deserialize, Serialize};

mod mem;
mod timer;

pub use mem::Mem;
pub use timer::{Clock, Divider, MClock, TClock};

pub mod constants {
    pub const EIGHT_K: usize = 0x2000;
    pub const SIXTEEN_K: usize = 0x4000;
    pub const THIRTY_TWO_K: usize = 0x8000;
    pub const SIXTY_FOUR_K: usize = 0x10000;
}

pub(crate) fn partial_min<T: PartialOrd>(a: T, b: T) -> T {
    match a.partial_cmp(&b) {
        Some(ordering) => {
            if ordering.is_lt() {
                a
            } else {
                b
            }
        }
        None => a,
    }
}

pub(crate) fn partial_max<T: PartialOrd>(a: T, b: T) -> T {
    match a.partial_cmp(&b) {
        Some(ordering) => {
            if ordering.is_gt() {
                a
            } else {
                b
            }
        }
        None => a,
    }
}

#[inline]
pub(crate) fn get_bit<N>(byte: N, idx: usize) -> N
where
    N: num_traits::PrimInt,
{
    (byte & (N::one() << idx)) >> idx
}

#[inline]
pub(crate) fn bit_set<N>(byte: N, idx: usize) -> bool
where
    N: num_traits::PrimInt,
{
    get_bit::<N>(byte, idx) != N::zero()
}

#[inline]
pub(crate) fn lo_bits_of(x: usize, n: usize) -> usize {
    x & ((1 << n) - 1)
}

#[inline]
pub(crate) fn set_bit<N: num_traits::PrimInt + From<u8>>(
    byte: N,
    idx: usize,
    val: N,
) -> N {
    let flag_mask = val.signed_shl(idx as u32);
    let bit_mask = N::one() << idx;
    (byte & !bit_mask) | (flag_mask & bit_mask)
}

#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(transparent)]
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

#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FallingEdgeDetector<const BIT: usize, N: num_traits::PrimInt> {
    val: N,
}

impl<const BIT: usize, N: num_traits::PrimInt> FallingEdgeDetector<BIT, N> {
    pub fn new(val: N) -> Self {
        Self { val }
    }
    pub fn set_and_check(&mut self, new: N) -> bool {
        let old_edge = get_bit(self.val, BIT);
        self.val = new;
        let new_edge = get_bit(self.val, BIT);
        old_edge == N::one() && new_edge == N::zero()
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

impl<const MAX_SIZE: usize, const DRAIN_LINES: usize>
    BoundedLog<MAX_SIZE, DRAIN_LINES>
{
    pub fn as_str(&self) -> &str {
        &self.buffer
    }

    pub fn clear(&mut self) {
        self.buffer.clear();
    }

    pub fn push(&mut self, s: &str) {
        if self.buffer.len() + s.len() > MAX_SIZE {
            // Drain off the first L lines when the buffer gets sufficiently full
            let idx = self
                .buffer
                .char_indices()
                .filter_map(|(idx, c)| (c == '\n').then_some(idx))
                .nth(DRAIN_LINES)
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

    macro_rules! assert_bin_eq {
        ($a:expr, $b:expr, $msg:literal) => {
            let (a, b) = ($a, $b);
            if a != b {
                panic!(
                    "assertion failed: {:08b} != {:08b}{}",
                    a,
                    b,
                    if $msg != "" {
                        format!(": {}", $msg)
                    } else {
                        String::new()
                    }
                )
            }
        };
        ($a:expr, $b:expr) => {
            assert_bin_eq!($a, $b, "")
        };
    }

    #[test]
    fn get_bits() {
        for n in 0..7 {
            assert_bin_eq!(get_bit(0b1111_1111, n), 1);
            assert_bin_eq!(get_bit(0b0000_0000, n), 0);
        }
        assert_bin_eq!(get_bit(0b1111_0000, 7), 1);
        assert_bin_eq!(get_bit(0b1111_0000, 4), 1);
        assert_bin_eq!(get_bit(0b0000_1100, 7), 0);
        assert_bin_eq!(get_bit(0b0000_1100, 4), 0);
        assert_bin_eq!(get_bit(0b1111_0000, 3), 0);
        assert_bin_eq!(get_bit(0b1111_0000, 0), 0);
        assert_bin_eq!(get_bit(0b0011_1100, 3), 1);
        assert_bin_eq!(get_bit(0b1111_0011, 0), 1);
    }

    #[test]
    fn set_bits() {
        for n in 0..7 {
            assert_bin_eq!(set_bit(0b0000_0000u8, n, 0), 0, "n = {n}");
            assert_bin_eq!(set_bit(0b0000_0000u8, n, 1), 1 << n, "n = {n}");
            assert_bin_eq!(set_bit(0b1111_1111u8, n, 0), !(1 << n), "n = {n}");
            assert_bin_eq!(
                set_bit(0b1111_1111u8, n, 1),
                0b1111_1111,
                "n = {n}"
            );
        }
        // 8 bit ops
        assert_bin_eq!(set_bit(0b0000_0000, 0, 0), 0b0000_0000);
        assert_bin_eq!(set_bit(0b0000_0000, 1, 0), 0b0000_0000);
        assert_bin_eq!(set_bit(0b0000_0000, 7, 0), 0b0000_0000);
        assert_bin_eq!(set_bit(0b0000_0000, 0, 1), 0b0000_0001);
        assert_bin_eq!(set_bit(0b0000_0000, 1, 1), 0b0000_0010);
        assert_bin_eq!(set_bit(0b0000_0000, 7, 1), 0b1000_0000);
        assert_bin_eq!(set_bit(0b1111_1111, 0, 0), 0b1111_1110);
        assert_bin_eq!(set_bit(0b1111_1111, 1, 0), 0b1111_1101);
        assert_bin_eq!(set_bit(0b1111_1111, 7, 0), 0b0111_1111);
        assert_bin_eq!(set_bit(0b1111_1111, 0, 1), 0b1111_1111);
        assert_bin_eq!(set_bit(0b1111_1111, 1, 1), 0b1111_1111);
        assert_bin_eq!(set_bit(0b1111_1111, 7, 1), 0b1111_1111);

        // 16 bit ops
        assert_bin_eq!(
            set_bit(0b0000_0000_0000_0000, 0, 0),
            0b0000_0000_0000_0000
        );
        assert_bin_eq!(
            set_bit(0b0000_0000_0000_0000, 1, 0),
            0b0000_0000_0000_0000
        );
        assert_bin_eq!(
            set_bit(0b0000_0000_0000_0000, 7, 0),
            0b0000_0000_0000_0000
        );
        assert_bin_eq!(
            set_bit(0b0000_0000_0000_0000, 11, 0),
            0b0000_0000_0000_0000
        );
        assert_bin_eq!(
            set_bit(0b0000_0000_0000_0000, 0, 1),
            0b0000_0000_0000_0001
        );
        assert_bin_eq!(
            set_bit(0b0000_0000_0000_0000, 1, 1),
            0b0000_0000_0000_0010
        );
        assert_bin_eq!(
            set_bit(0b0000_0000_0000_0000, 7, 1),
            0b0000_0000_1000_0000
        );
        assert_bin_eq!(
            set_bit(0b0000_0000_0000_0000, 11, 1),
            0b0000_1000_0000_0000
        );
        assert_bin_eq!(
            set_bit(0b1111_1111_1111_1111, 0, 0),
            0b1111_1111_1111_1110
        );
        assert_bin_eq!(
            set_bit(0b1111_1111_1111_1111, 1, 0),
            0b1111_1111_1111_1101
        );
        assert_bin_eq!(
            set_bit(0b1111_1111_1111_1111, 7, 0),
            0b1111_1111_0111_1111
        );
        assert_bin_eq!(
            set_bit(0b1111_1111_1111_1111, 11, 0),
            0b1111_0111_1111_1111
        );
        assert_bin_eq!(
            set_bit(0b1111_1111_1111_1111, 0, 1),
            0b1111_1111_1111_1111
        );
        assert_bin_eq!(
            set_bit(0b1111_1111_1111_1111, 1, 1),
            0b1111_1111_1111_1111
        );
        assert_bin_eq!(
            set_bit(0b1111_1111_1111_1111, 7, 1),
            0b1111_1111_1111_1111
        );
        assert_bin_eq!(
            set_bit(0b1111_1111_1111_1111, 11, 1),
            0b1111_1111_1111_1111
        );
    }

    #[test]
    fn lo_bits() {
        assert_bin_eq!(lo_bits_of(0b1111_0000, 5), 0b10000);
        assert_bin_eq!(lo_bits_of(0b1111_1110, 5), 0b11110);
        assert_bin_eq!(lo_bits_of(0b1111_1110, 0), 0b0);
        assert_bin_eq!(lo_bits_of(0b1111_1110, 8), 0b1111_1110);
    }
}
