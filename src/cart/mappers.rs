use std::cell::Cell;

use serde::{Deserialize, Serialize};

use crate::utils::constants::{EIGHT_K, SIXTEEN_K};
use crate::utils::mem::Mem;

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Flags(u8);

impl Flags {
    pub fn is_set(&self, rhs: Flags) -> bool {
        (self.0 & rhs.0) != 0
    }
}

impl std::ops::BitOr for Flags {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0.bitor(rhs.0))
    }
}

impl std::ops::BitAnd for Flags {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0.bitand(rhs.0))
    }
}

pub mod flags {
    use super::Flags;
    pub const NONE: Flags = Flags(0x0);
    pub const RAM: Flags = Flags(0x1);
    pub const BATTERY: Flags = Flags(0x2);
    pub const TIMER: Flags = Flags(0x4);
}

impl Default for Flags {
    fn default() -> Self {
        flags::NONE
    }
}

const ROM_BANK_SIZE: usize = SIXTEEN_K;

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Simple {
    ram: Mem<EIGHT_K>,
}

impl Simple {
    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            0xA000..=0xBFFF => self.ram[address - 0xA000] = val,
            _ => {}
        }
    }

    pub fn load(&self, address: u16, data: &[u8]) -> u8 {
        match address {
            0x0000..=0x7FFF => data[address as usize],
            0xA000..=0xBFFF => self.ram[address - 0xA000],
            _ => unreachable!(),
        }
    }

    pub fn block_load<'a>(
        &'a self,
        address: u16,
        len: usize,
        data: &'a [u8],
    ) -> &'a [u8] {
        match address {
            0x0000..=0x7FFF => {
                let offset = address as usize;
                let len_to_load = std::cmp::min(data.len() - offset, len);
                &data[offset..][..len_to_load]
            }
            0xA000..=0xBFFF => {
                self.ram.safe_slice((address - 0xA000) as usize, len)
            }
            _ => unreachable!(),
        }
    }

    pub fn reset(&mut self) {
        self.ram.clear();
    }
}

/// MBC1
///
/// Regions:
///  * 0x0000 - 0x3FFF : ROM Bank X0    [read-only]
///  * 0x4000 - 0x7FFF : ROM Bank 01-7F [read-only]
///  * 0xA000 - 0xBFFF : RAM Bank 00-03 [read/write]
///
/// Registers:
///  * 0x0000 - 0x1FFF : RAM Enable
///  * 0x2000 - 0x3FFF : ROM Bank Number [5-bits]
///  * 0x4000 - 0x5FFF : RAM Bank Number (or upper ROM) [2 bits]
///  * 0x6000 - 0x7FFF : Banking Mode Select [1-bit]
#[derive(Debug, Serialize, Deserialize)]
pub struct MBC1 {
    ram_enable: bool,
    bank_mode_select: u8,
    rom_bank_lower5: u8,
    ram_or_rom_upper2: u8,
    ram_or_rom_upper2_is_rom: bool,
    flags: Flags,
    sram: Mem<EIGHT_K>,
}

impl MBC1 {
    pub fn new(num_rom_banks: u8, flags: Flags) -> Self {
        Self {
            ram_enable: false,
            bank_mode_select: 0,
            rom_bank_lower5: 0,
            ram_or_rom_upper2: 0,
            ram_or_rom_upper2_is_rom: num_rom_banks >= 64,
            flags,
            sram: Default::default(),
        }
    }

    pub fn reset(&mut self) {
        self.ram_enable = false;
        self.bank_mode_select = 0;
        self.rom_bank_lower5 = 0;
        self.ram_or_rom_upper2 = 0;
        if !self.ram_is_persistent() {
            self.sram.clear();
        }
    }

    pub fn ram_is_persistent(&self) -> bool {
        self.flags.is_set(flags::BATTERY)
    }

    fn rom_bank_1_addr_base(&self) -> usize {
        // Map bank num 0x00 to 0x01, 0x20 to 0x21, etc.
        let rom_bank_num = if self.ram_or_rom_upper2_is_rom {
            (self.rom_bank_lower5 as usize)
                | (self.rom_bank_lower5 == 0) as usize
                | ((self.ram_or_rom_upper2 as usize) << 5)
        } else {
            (self.rom_bank_lower5 as usize)
                | (self.rom_bank_lower5 == 0) as usize
        };
        rom_bank_num * ROM_BANK_SIZE
    }

    pub fn load(&self, address: u16, data: &[u8]) -> u8 {
        match address {
            0xA000..=0xBFFF if !self.ram_enable => 0xFF,
            0xA000..=0xBFFF => self.sram[address - 0xA000],
            _ => data[self.addr_to_offset(address)],
        }
    }

    pub fn block_load<'a>(
        &'a self,
        address: u16,
        len: usize,
        data: &'a [u8],
    ) -> &'a [u8] {
        match address {
            0x0000..=0x7FFF => {
                let offset = self.addr_to_offset(address);
                let len_to_load = std::cmp::min(data.len() - offset, len);
                &data[offset..][..len_to_load]
            }
            0xA000..=0xBFFF => {
                let offset = (address - 0xA000) as usize;
                self.sram.safe_slice(offset, len)
            }
            _ => unreachable!(),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            // RAM Enable
            0x0000..=0x1FFF => match val {
                0xA => self.ram_enable = true,
                0x0 => self.ram_enable = false,
                _ => {}
            },
            // ROM Bank (lower)
            0x2000..=0x3FFF => self.rom_bank_lower5 = val & 0x1F,
            // RAM Bank (or upper bits of ROM Bank)
            0x4000..=0x5FFF => self.ram_or_rom_upper2 = val & 0x3,
            // ROM/RAM Mode
            0x6000..=0x7FFF => self.bank_mode_select = val & 0x1,
            0xA000..=0xBFFF if !self.ram_enable => {}
            0xA000..=0xBFFF => self.sram[address - 0xA000] = val,
            _ => unreachable!(
                "Store outside of MBC range? ({val:02X} to {address:04X})"
            ),
        }
    }

    pub fn addr_to_offset(&self, address: u16) -> usize {
        match address {
            0x0000..=0x3FFF => match self.bank_mode_select {
                0 => crate::utils::lo_bits_of(address as usize, 14),
                1 => {
                    crate::utils::lo_bits_of(address as usize, 14)
                        | ((self.ram_or_rom_upper2 as usize) << 19)
                }
                _ => unreachable!(),
            },
            0x4000..=0x7FFF => {
                crate::utils::lo_bits_of(address as usize, 14)
                    | self.rom_bank_1_addr_base()
            }
            0xA000..=0xBFFF => match self.bank_mode_select {
                0 => crate::utils::lo_bits_of(address as usize, 13),
                1 => {
                    crate::utils::lo_bits_of(address as usize, 13)
                        | ((self.ram_or_rom_upper2 as usize) << 13)
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn sram(&self) -> Option<&[u8]> {
        if self.ram_is_persistent() {
            Some(self.sram.as_slice())
        } else {
            None
        }
    }

    pub fn load_sram(&mut self, bytes: &[u8]) {
        if self.ram_is_persistent() {
            assert_eq!(bytes.len(), self.sram.len());
            self.sram.copy_from_slice(bytes);
        }
    }
}

impl crate::utils::Dump for MBC1 {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "MBC1")?;
        writeln!(
            out,
            "{}",
            if self.ram_enable { "RAM ON" } else { "RAM OFF" }
        )?;
        writeln!(out, "ROM BANK 1: {:08X}", self.rom_bank_1_addr_base())?;
        writeln!(out, "MODE: {}", self.bank_mode_select)?;
        {
            write!(out, "FLAGS: ")?;
            let mut first = true;
            for (name, flag) in [
                ("RAM", flags::RAM),
                ("BATTERY", flags::BATTERY),
                ("TIMER", flags::TIMER),
            ] {
                if self.flags.is_set(flag) {
                    if !first {
                        write!(out, " | ")?;
                    }
                    first = false;
                    write!(out, "{name}")?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize)]
enum LatchState {
    #[default]
    Unlatched,
    Latch0,
    Latch1,
}

/// ClockRegs format:
///   SMHDD
/// where: S -> Seconds
///        M -> Minutes
///        D -> Day lower 8
///        D -> Day upper 1, Carry bit, Halt flag
type ClockRegs = [u8; 5];

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
struct RTC {
    latch: LatchState,
    select: u8,
    clock_regs: Cell<ClockRegs>,
    latched_regs: Cell<ClockRegs>,
}

impl RTC {
    fn new() -> Self {
        let this = Self::default();
        this.read_clock_regs();
        this
    }

    fn read_clock_regs(&self) {
        const SECONDS_PER_MINUTE: u64 = 60;
        const MINUTES_PER_HOUR: u64 = 60;
        const HOURS_PER_DAY: u64 = 24;
        const DAYS_PER_YEAR: u64 = 365;
        const SECONDS_PER_HOUR: u64 = SECONDS_PER_MINUTE * MINUTES_PER_HOUR;
        const SECONDS_PER_DAY: u64 = SECONDS_PER_HOUR * HOURS_PER_DAY;
        const SECONDS_PER_YEAR: u64 = SECONDS_PER_DAY * DAYS_PER_YEAR;
        const MINUTES_PER_DAY: u64 = MINUTES_PER_HOUR * HOURS_PER_DAY;
        const MINUTES_PER_YEAR: u64 = MINUTES_PER_DAY * DAYS_PER_YEAR;
        const HOURS_PER_YEAR: u64 = HOURS_PER_DAY * DAYS_PER_YEAR;

        let seconds_since_epoch = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("Time travel?")
            .as_secs();
        // TODO: This appears to calculate the wrong day (rewrite with chrono?)
        let (seconds, minutes, hours, days) = (
            seconds_since_epoch
                % SECONDS_PER_YEAR    // seconds within year
                % SECONDS_PER_DAY     // seconds within day
                % SECONDS_PER_HOUR    // seconds within hour
                % SECONDS_PER_MINUTE, // seconds within minute
            (seconds_since_epoch / SECONDS_PER_MINUTE)
                % MINUTES_PER_YEAR    // minutes within year
                % MINUTES_PER_DAY     // minutes within day
                % MINUTES_PER_HOUR,   // minutes within hour
            (seconds_since_epoch / SECONDS_PER_HOUR)
                % HOURS_PER_YEAR    // hours within year
                % HOURS_PER_DAY,    // hours within day
            (seconds_since_epoch / SECONDS_PER_DAY)
                % DAYS_PER_YEAR,
        );
        assert!(seconds < 60, "{seconds} >= 60!");
        assert!(minutes < 60, "{minutes} >= 60!");
        assert!(hours < 24, "{hours} >= 24!");
        assert!(days < 365, "{days} >= 365!");
        let (days_lower_8, days_upper_1) = (days & 0xFF, (days >> 8) & 0b1);
        let mut clock_regs = ClockRegs::default();
        clock_regs[0] = seconds as u8;
        clock_regs[1] = minutes as u8;
        clock_regs[2] = hours as u8;
        clock_regs[3] = days_lower_8 as u8;
        crate::utils::set_bit(clock_regs[4], 0, days_upper_1 as u8);
        self.clock_regs.set(clock_regs);
        crate::debug_log!("read current time: {:?}", self.clock_regs);
    }

    fn write_latch(&mut self, val: u8) {
        // Writing 0, then 1 latches the current regs
        // All other writes are ignored
        match (val, self.latch) {
            (0x00, LatchState::Unlatched | LatchState::Latch1) => {
                self.latch = LatchState::Latch0;
            }
            (0x01, LatchState::Latch0) => {
                self.latch = LatchState::Latch1;
                self.read_clock_regs();
                self.latched_regs = self.clock_regs.clone();
            }
            _ => {}
        }
    }

    fn read(&self) -> u8 {
        let reg = self.select as usize;
        if let LatchState::Latch1 = self.latch {
            self.latched_regs.get()[reg]
        } else {
            self.read_clock_regs();
            self.clock_regs.get()[reg]
        }
    }

    fn write(&mut self, val: u8) {
        let reg = self.select as usize;
        self.clock_regs.get_mut()[reg] = val;
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MBC3 {
    ram_and_rtc_enable: bool,
    rom_bank: u8,
    ram_bank: u8,
    flags: Flags,
    rtc: RTC,
    sram: Mem<{ 4 * EIGHT_K }>,
}

impl MBC3 {
    pub fn new(flags: Flags) -> Self {
        Self {
            ram_and_rtc_enable: false,
            rom_bank: 1,
            ram_bank: 0,
            flags,
            rtc: RTC::new(),
            sram: Mem::default(),
        }
    }

    pub fn reset(&mut self) {
        self.ram_and_rtc_enable = false;
        self.rom_bank = 0;
        self.ram_bank = 0;
    }

    pub fn ram_is_persistent(&self) -> bool {
        self.flags.is_set(flags::BATTERY)
    }

    fn rom_bank_1_addr_base(&self) -> usize {
        (self.rom_bank as usize) * ROM_BANK_SIZE
    }

    pub fn addr_to_offset(&self, address: u16) -> usize {
        match address {
            0x0000..=0x3FFF => address as usize,
            0x4000..=0x7FFF => {
                crate::utils::lo_bits_of(address as usize, 14)
                    | self.rom_bank_1_addr_base()
            }
            _ => unreachable!(),
        }
    }

    fn addr_to_sram_offset(&self, address: u16) -> usize {
        let bank_offset = (self.ram_bank as usize) * EIGHT_K;
        ((address - 0xA000) as usize) + bank_offset
    }

    pub fn load(&self, address: u16, data: &[u8]) -> u8 {
        match address {
            0xA000..=0xBFFF if !self.ram_and_rtc_enable => 0xFF,
            0xA000..=0xBFFF if self.ram_and_rtc_enable => {
                match self.rtc.select {
                    0x0 => self.sram[self.addr_to_sram_offset(address)],
                    0x8..=0xC => self.rtc.read(),
                    _ => unreachable!(),
                }
            }
            0x0000..=0x7FFF => data[self.addr_to_offset(address)],
            _ => unreachable!(),
        }
    }

    pub fn block_load<'a>(
        &'a self,
        address: u16,
        len: usize,
        data: &'a [u8],
    ) -> &'a [u8] {
        match address {
            0x0000..=0x7FFF => {
                let offset = self.addr_to_offset(address);
                let len_to_load = std::cmp::min(data.len() - offset, len);
                &data[offset..][..len_to_load]
            }
            0xA000..=0xBFFF => {
                let offset = self.addr_to_sram_offset(address);
                self.sram.safe_slice(offset, len)
            }
            _ => unreachable!(),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            0x0000..=0x1FFF => match val {
                0xA => self.ram_and_rtc_enable = true,
                0x0 => self.ram_and_rtc_enable = false,
                _ => {}
            },
            0x2000..=0x3FFF => match val {
                0x00 => self.rom_bank = 1,
                0x01..=0x7F => self.rom_bank = val,
                _ => {}
            },
            0x4000..=0x5FFF => match val {
                0x00..=0x03 => self.ram_bank = val,
                0x08..=0x0C => self.rtc.select = val,
                _ => {}
            },
            0x6000..=0x7FFF => self.rtc.write_latch(val),
            0xA000..=0xBFFF if self.ram_and_rtc_enable => {
                match self.rtc.select {
                    // RAM
                    0x0 => {
                        let offset = self.addr_to_sram_offset(address);
                        self.sram[offset] = val;
                    }
                    0x8..=0xC => self.rtc.write(val),
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn sram(&self) -> Option<&[u8]> {
        if self.ram_is_persistent() {
            Some(self.sram.as_slice())
        } else {
            None
        }
    }

    pub fn load_sram(&mut self, bytes: &[u8]) {
        if self.ram_is_persistent() {
            assert_eq!(bytes.len(), self.sram.len());
            self.sram.copy_from_slice(bytes);
        }
    }
}

impl crate::utils::Dump for MBC3 {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "MBC3")?;
        writeln!(
            out,
            "{}",
            if self.ram_and_rtc_enable {
                "RAM/TIMER ENABLED"
            } else {
                "RAM/TIMER DISABLED"
            }
        )?;
        writeln!(
            out,
            "ROM BANK: {:02X} ({:08X})",
            self.rom_bank,
            self.rom_bank_1_addr_base()
        )?;
        writeln!(out, "RAM BANK: {:02X}", self.ram_bank)?;
        Ok(())
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MBC5 {
    ram_enable: bool,
    rom_bank_lower8: u8,
    rom_bank_upper1: u8,
    ram_bank: u8,
    sram: Mem<{ 16 * EIGHT_K }>,
}
impl Default for MBC5 {
    fn default() -> Self {
        Self {
            ram_enable: false,
            rom_bank_lower8: 0,
            rom_bank_upper1: 0,
            ram_bank: 0,
            sram: Mem::default(),
        }
    }
}

impl MBC5 {
    pub fn reset(&mut self) {
        *self = Self {
            sram: std::mem::take(&mut self.sram),
            ..MBC5::default()
        };
    }

    fn rom_bank_1_addr_base(&self) -> usize {
        let rom_bank = (self.rom_bank_lower8 as usize)
            | (self.rom_bank_upper1 as usize) << 8;
        rom_bank * ROM_BANK_SIZE
    }

    pub fn addr_to_offset(&self, address: u16) -> usize {
        match address {
            0x0000..=0x3FFF => address as usize,
            0x4000..=0x7FFF => {
                crate::utils::lo_bits_of(address as usize, 14)
                    | self.rom_bank_1_addr_base()
            }
            _ => unreachable!(),
        }
    }

    fn addr_to_sram_offset(&self, address: u16) -> usize {
        let bank_offset = (self.ram_bank as usize) * EIGHT_K;
        ((address - 0xA000) as usize) + bank_offset
    }

    pub fn load(&self, address: u16, data: &[u8]) -> u8 {
        match address {
            0xA000..=0xBFFF if !self.ram_enable => 0xFF,
            0xA000..=0xBFFF if self.ram_enable => {
                self.sram[self.addr_to_sram_offset(address)]
            }
            0x0000..=0x7FFF => data[self.addr_to_offset(address)],
            _ => unreachable!(),
        }
    }

    pub fn block_load<'a>(
        &'a self,
        address: u16,
        len: usize,
        data: &'a [u8],
    ) -> &'a [u8] {
        match address {
            0x0000..=0x7FFF => {
                let offset = self.addr_to_offset(address);
                let len_to_load = std::cmp::min(data.len() - offset, len);
                &data[offset..][..len_to_load]
            }
            0xA000..=0xBFFF => {
                let offset = self.addr_to_sram_offset(address);
                self.sram.safe_slice(offset, len)
            }
            _ => unreachable!(),
        }
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            0x0000..=0x1FFF => match val {
                0xA => self.ram_enable = true,
                0x0 => self.ram_enable = false,
                _ => {}
            },
            0x2000..=0x2FFF => self.rom_bank_lower8 = val,
            0x3000..=0x3FFF => self.rom_bank_upper1 = val & 0x1,
            0x4000..=0x5FFF => match val {
                0x00..=0x0F => self.ram_bank = val,
                _ => {}
            },
            0x6000..=0x7FFF => {}
            0xA000..=0xBFFF if self.ram_enable => {
                let offset = self.addr_to_sram_offset(address);
                self.sram[offset] = val;
            }
            _ => unreachable!(),
        }
    }

    pub fn sram(&self) -> &[u8] {
        self.sram.as_slice()
    }

    pub fn load_sram(&mut self, bytes: &[u8]) {
        assert_eq!(bytes.len(), self.sram.len());
        self.sram.copy_from_slice(bytes);
    }
}

impl crate::utils::Dump for MBC5 {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "MBC5")?;
        writeln!(
            out,
            "{}",
            if self.ram_enable {
                "RAM/TIMER ENABLED"
            } else {
                "RAM/TIMER DISABLED"
            }
        )?;
        writeln!(
            out,
            "ROM BANK: {:02X} ({:08X})",
            self.rom_bank_lower8,
            self.rom_bank_1_addr_base()
        )?;
        writeln!(out, "RAM BANK: {:02X}", self.ram_bank)?;
        Ok(())
    }
}
