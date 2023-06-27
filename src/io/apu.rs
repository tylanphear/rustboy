use crate::utils::mem::Mem;
use serde::{Deserialize, Serialize};

// If n = sample_rate,
//
//  1s = n samples
//  frequency = 1s / period
//  period = n samples / frequency

const SQUARE_WAVES: [[f32; 8]; 4] = [
    /* Duty 0 */ [-1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, 1.0],
    /* Duty 1 */ [1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, 1.0],
    /* Duty 2 */ [1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0, 1.0],
    /* Duty 3 */ [-1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, -1.0],
];

pub const MAIN_CLOCK_FREQUENCY: u64 = crate::T_CLOCK_FREQUENCY;

/// Gameboy audio consists of 4 channels:
///
///  1. Square wave with volume envelope and frequency sweep
///  2. Square wave with volume envelope only
///  3. Programmable wave channel w/ 32 4-bit samples
///  4. Noise channel with volume envelope

pub mod regs {
    /// (CH1) -PPP NSSS sweep Period, Negate, Shift
    pub const NR10: u16 = 0xFF10;
    /// (CH1) DDLL LLLL Duty, Length load (64-L)
    pub const NR11: u16 = 0xFF11;
    /// (CH1) VVVV APPP starting Volume, envelope Add mode, Period
    pub const NR12: u16 = 0xFF12;
    /// (CH1) FFFF FFFF Frequency LSB
    pub const NR13: u16 = 0xFF13;
    /// (CH1) TL-- -FFF Trigger, Length enable, Frequency MSB
    pub const NR14: u16 = 0xFF14;

    /// (CH2) DDLL LLLL Duty, Length load (64-L)
    pub const NR21: u16 = 0xFF16;
    /// (CH2) VVVV APPP starting Volume, envelope Add mode, Period
    pub const NR22: u16 = 0xFF17;
    /// (CH2) FFFF FFFF Frequency LSB
    pub const NR23: u16 = 0xFF18;
    /// (CH2) TL-- -FFF Trigger, Length enable, Frequency MSB
    pub const NR24: u16 = 0xFF19;

    /// (CH3) E--- ---- DAC power
    pub const NR30: u16 = 0xFF1A;
    /// (CH3) LLLL LLLL Length load (256-L)
    pub const NR31: u16 = 0xFF1B;
    /// (CH3) --VV ---- Volume code (00 -> 0%, 01 -> 100%, 10 -> 50%, 11 -> 25%)
    pub const NR32: u16 = 0xFF1C;
    /// (CH3) FFFF FFFF Frequency LSB
    pub const NR33: u16 = 0xFF1D;
    /// (CH3) TL-- -FFF Trigger, Length enable, Frequency MSB
    pub const NR34: u16 = 0xFF1E;

    /// (CH4) --LL LLLL Length load (64-L)
    pub const NR41: u16 = 0xFF20;
    /// (CH4) VVVV APPP starting Volume, envelope Add mode, Period
    pub const NR42: u16 = 0xFF21;
    /// (CH4) SSSS WDDD clock Shift, Width mode of LFSR, Divisor code
    pub const NR43: u16 = 0xFF22;
    /// (CH4) TL-- ---- Trigger, Length enable
    pub const NR44: u16 = 0xFF23;

    /// ALLL BRRR vin L enable (A), Left vol, vin R enable (B), Right vol
    pub const NR50: u16 = 0xFF24;
    /// 4321 4321 Mix channel N into left, Mix channel N into right
    pub const NR51: u16 = 0xFF25;
    /// P--- 4321 Power control/status, Channel length statuses
    pub const NR52: u16 = 0xFF26;
}

pub const CHANNEL_3_TABLE_START: usize = 0xFF30;
pub const CHANNEL_3_TABLE_END: usize = 0xFF40;
pub const CHANNEL_3_TABLE_SIZE: usize =
    CHANNEL_3_TABLE_END - CHANNEL_3_TABLE_START;
pub const REGS_START: usize = regs::NR10 as usize;
pub const REGS_END: usize = CHANNEL_3_TABLE_END as usize;
pub const NUM_OUTPUT_CHANNELS: usize = 2;

fn waveform_to_float(val: u8) -> f32 {
    1.0 - 2.0 * (val as f32) / 15.0
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct EnvelopeUnit {
    volume: u8,
    increase: bool,
    sweep_pace: u8,
}

impl EnvelopeUnit {
    fn tick(&mut self, clock: u64) {
        if clock % (MAIN_CLOCK_FREQUENCY / 64) != 0 {
            return;
        }
        let envelope_ticks = clock / (MAIN_CLOCK_FREQUENCY / 64);
        let pace = self.sweep_pace as u64;
        if pace == 0 || envelope_ticks % pace != 0 {
            return;
        }
        if self.increase {
            if self.volume != 0xF {
                self.volume += 1;
            }
        } else {
            if self.volume != 0 {
                self.volume -= 1;
            }
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct SquareGenerationCircuit {
    enabled: bool,

    length_timer: u8,
    length_enabled: bool,

    duty_cycle: u8,
    duty_counter: u8,

    period: u16,
    freq_sweep_pace: u8,

    envelope: EnvelopeUnit,
}

impl SquareGenerationCircuit {
    fn trigger(&mut self) {
        self.enabled = true;
        self.duty_counter = 0;
    }

    fn tick(&mut self, clock: u64) -> f32 {
        if self.length_enabled && clock % (MAIN_CLOCK_FREQUENCY / 256) == 0 {
            if self.length_timer == 64 {
                self.enabled = false;
            } else {
                self.length_timer += 1;
            }
        }
        if !self.enabled {
            return 0.0;
        }

        let period = self.period as u64;
        if period == 0 {
            return 0.0;
        }

        self.envelope.tick(clock);

        let sample_period = 2048 - (self.period as u64);
        if clock % sample_period == 0 {
            self.duty_counter += 1;
            if self.duty_counter == 8 {
                self.duty_counter = 0;
            }
        }

        let waveform =
            SQUARE_WAVES[self.duty_cycle as usize][self.duty_counter as usize];

        let attenuation = (self.envelope.volume as f32) / (0xF as f32);
        waveform * attenuation
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct WaveGenerationCircuit {
    enabled: bool,
    sample_index: u8,
    length_timer: u8,
    length_enabled: bool,
    volume_code: u8,
    period: u16,
    pattern: [u8; CHANNEL_3_TABLE_SIZE],
}

impl WaveGenerationCircuit {
    fn trigger(&mut self, pattern: &[u8; 16]) {
        self.enabled = true;
        self.sample_index = 0;
        self.pattern = *pattern;
    }

    fn tick(&mut self, clock: u64) -> f32 {
        if self.length_enabled && clock % (MAIN_CLOCK_FREQUENCY / 256) == 0 {
            if self.length_timer == 64 {
                self.enabled = false;
            } else {
                self.length_timer += 1;
            }
        }
        if !self.enabled {
            return 0.0;
        }

        let period = self.period as usize;
        if period == 0 {
            return 0.0;
        }

        // TODO: WRONG
        let sample_period = 2048 - (self.period as u64);
        if clock % sample_period == 0 {
            self.sample_index += 1;
            if self.sample_index == 32 {
                self.sample_index = 0;
            }
        }

        let sample_num = (self.sample_index / 2) as usize;
        let sample_nibble = (self.sample_index % 2) as usize;
        let waveform = match sample_nibble {
            0 => self.pattern[sample_num] & 0xF,
            1 => self.pattern[sample_num] >> 4,
            _ => unreachable!(),
        };
        let attenuation_shift = match self.volume_code {
            0b00 => 4,
            0b01 => 0,
            0b10 => 1,
            0b11 => 2,
            _ => unreachable!(),
        };
        waveform_to_float(waveform >> attenuation_shift)
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct NoiseGenerationCircuit {
    enabled: bool,
    lsfr: u16,

    length_timer: u8,
    length_enabled: bool,

    envelope: EnvelopeUnit,

    divisor_code: u8,
    shift: u8,
}

impl NoiseGenerationCircuit {
    fn trigger(&mut self) {
        self.enabled = true;
        self.lsfr = 0;
    }

    fn tick(&mut self, clock: u64) -> f32 {
        if self.length_enabled && clock % (MAIN_CLOCK_FREQUENCY / 256) == 0 {
            if self.length_timer == 64 {
                self.enabled = false;
            } else {
                self.length_timer += 1;
            }
        }
        if !self.enabled {
            return 0.0;
        }

        self.envelope.tick(clock);

        let divisor = match self.divisor_code {
            0b000 => 8,
            0b001 => 16,
            0b010 => 32,
            0b011 => 48,
            0b100 => 64,
            0b101 => 80,
            0b110 => 96,
            0b111 => 112,
            _ => unreachable!(),
        };

        let period = (MAIN_CLOCK_FREQUENCY / 4) / (divisor * (1 << self.shift));
        if clock % period == 0 {
            let lsfr_0 = (self.lsfr >> 0) & 0b1;
            let lsfr_1 = (self.lsfr >> 1) & 0b1;
            let new_top_bit = !(lsfr_0 ^ lsfr_1) & 0b1;
            self.lsfr = crate::utils::set_bit(self.lsfr, 15, new_top_bit) >> 1;
        }

        if crate::utils::bit_set(self.lsfr, 0) {
            waveform_to_float(crate::utils::get_bit(self.lsfr, 0) as u8)
        } else {
            0.0
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct APU {
    registers: Mem<{ 0xFF40 - 0xFF10 }>,
    buffer: Vec<f32>,
    sample_num: u64,
    samples_ready: usize,
    clock: u64,
    channel1: SquareGenerationCircuit,
    channel2: SquareGenerationCircuit,
    channel3: WaveGenerationCircuit,
    channel4: NoiseGenerationCircuit,
    last_rendered_sample: usize,
}

impl APU {
    pub fn reset(&mut self) {
        self.registers.clear();
        self.buffer.fill(0.0);
        self.sample_num = 0;
        self.channel1 = Default::default();
        self.channel2 = Default::default();
        self.channel3 = Default::default();
        self.channel4 = Default::default();
    }

    fn read_reg(&self, address: u16) -> u8 {
        self.registers[address - 0xFF10]
    }

    fn write_reg(&mut self, address: u16, val: u8) {
        self.registers[address - 0xFF10] = val;
    }

    fn is_powered_on(&self) -> bool {
        crate::utils::bit_set(self.read_reg(regs::NR52), 7)
    }

    fn sample_rate(&self) -> u64 {
        (self.buffer.len() / NUM_OUTPUT_CHANNELS) as u64
    }

    fn sample_period(&self) -> u64 {
        MAIN_CLOCK_FREQUENCY / self.sample_rate()
    }

    pub fn tick(&mut self) {
        if self.sample_rate() == 0 {
            return;
        }

        let pan = self.read_reg(regs::NR51);
        let panned = |left: usize, right: usize, output: f32| {
            (
                if crate::utils::bit_set(pan, left) {
                    output
                } else {
                    0.0
                },
                if crate::utils::bit_set(pan, right) {
                    output
                } else {
                    0.0
                },
            )
        };

        let (channel_1_left, channel_1_right) =
            panned(4, 0, self.channel1.tick(self.clock));
        let (channel_2_left, channel_2_right) =
            panned(5, 1, self.channel2.tick(self.clock));
        let (channel_3_left, channel_3_right) =
            panned(6, 2, self.channel3.tick(self.clock));
        let (channel_4_left, channel_4_right) =
            panned(7, 3, self.channel4.tick(self.clock));

        let nr52 = self.read_reg(regs::NR52);
        let status = ((self.channel3.enabled as u8) << 2)
            | ((self.channel2.enabled as u8) << 1)
            | ((self.channel1.enabled as u8) << 0);
        self.write_reg(regs::NR52, (nr52 & !(0b1111)) | status);

        self.clock = self.clock.wrapping_add(1);
        if self.clock % self.sample_period() != 0 {
            return;
        }

        let (left_channel, right_channel) = {
            (
                (channel_1_left
                    + channel_2_left
                    + channel_3_left
                    + channel_4_left)
                    / 4.0,
                (channel_1_right
                    + channel_2_right
                    + channel_3_right
                    + channel_4_right)
                    / 4.0,
            )
        };

        // 2 channels interleaved [L, R, L, R, ...]
        self.buffer[(2 * self.sample_num) as usize] = left_channel;
        self.buffer[(2 * self.sample_num + 1) as usize] = right_channel;
        self.sample_num += 1;
        if self.sample_num == self.sample_rate() {
            self.sample_num = 0;
        }
        self.samples_ready += 2;
    }

    pub fn set_sample_rate(&mut self, rate: usize) {
        self.buffer.clear();
        self.buffer.resize(rate * NUM_OUTPUT_CHANNELS, 0.0);
    }

    pub fn load(&self, address: u16) -> u8 {
        self.read_reg(address)
    }

    pub fn store(&mut self, address: u16, val: u8) {
        self.write_reg(address, val);
        match address {
            regs::NR11 => {
                self.channel1.duty_cycle = val >> 6;
                self.channel1.length_timer = val & 0b0011_1111;
            }
            regs::NR21 => {
                self.channel2.duty_cycle = val >> 6;
                self.channel2.length_timer = val & 0b0011_1111;
            }
            regs::NR12 => {
                self.channel1.envelope.volume = val >> 4;
                self.channel1.envelope.increase = crate::utils::bit_set(val, 3);
                self.channel1.envelope.sweep_pace = val & 0b111;
                if self.channel1.envelope.volume == 0 {
                    self.channel1.enabled = false;
                }
            }
            regs::NR22 => {
                self.channel2.envelope.volume = val >> 4;
                self.channel2.envelope.increase = crate::utils::bit_set(val, 3);
                self.channel2.envelope.sweep_pace = val & 0b111;
                if self.channel2.envelope.volume == 0 {
                    self.channel2.enabled = false;
                }
            }
            regs::NR13 => {
                self.channel1.period =
                    (self.channel1.period & 0xFF00) | (val as u16);
            }
            regs::NR23 => {
                self.channel2.period =
                    (self.channel2.period & 0xFF00) | (val as u16);
            }
            regs::NR14 => {
                self.channel1.period = (self.channel1.period & 0x00FF)
                    | (((val & 0b111) as u16) << 8);
                self.channel1.length_enabled = crate::utils::bit_set(val, 6);
                if crate::utils::bit_set(val, 7) {
                    self.channel1.trigger();
                    crate::debug_log!("channel 1 triggered!");
                    crate::debug_log!("{:#?}", self.channel1);
                }
            }
            regs::NR24 => {
                self.channel2.period = (self.channel2.period & 0x00FF)
                    | (((val & 0b111) as u16) << 8);
                self.channel2.length_enabled = crate::utils::bit_set(val, 6);
                if crate::utils::bit_set(val, 7) {
                    self.channel2.trigger();
                    crate::debug_log!("channel 2 triggered!");
                    crate::debug_log!("{:#?}", self.channel2);
                }
            }
            regs::NR30 => {
                self.channel3.enabled = crate::utils::bit_set(val, 7);
            }
            regs::NR31 => {
                self.channel3.length_timer = val;
            }
            regs::NR32 => {
                self.channel3.volume_code = (val >> 4) & 0b11;
            }
            regs::NR33 => {
                self.channel3.period =
                    (self.channel3.period & 0xFF00) | (val as u16);
            }
            regs::NR34 => {
                self.channel3.period = (self.channel3.period & 0x00FF)
                    | (((val & 0b111) as u16) << 8);
                self.channel3.length_enabled = crate::utils::bit_set(val, 6);
                if crate::utils::bit_set(val, 7) {
                    self.channel3.trigger(
                        self.registers.slice_n::<CHANNEL_3_TABLE_SIZE>(
                            CHANNEL_3_TABLE_START - REGS_START,
                        ),
                    );
                    crate::debug_log!("channel 3 triggered!");
                    crate::debug_log!("{:#?}", self.channel3);
                }
            }
            regs::NR41 => {
                self.channel4.length_timer = val & 0b0011_1111;
            }
            regs::NR42 => {
                self.channel4.envelope.volume = val >> 4;
                self.channel4.envelope.increase = crate::utils::bit_set(val, 3);
                self.channel4.envelope.sweep_pace = val & 0b111;
                if self.channel4.envelope.volume == 0 {
                    self.channel4.enabled = false;
                }
            }
            regs::NR43 => {
                self.channel4.divisor_code = val & 0b111;
                self.channel4.shift = val >> 4;
            }
            regs::NR44 => {
                self.channel4.length_enabled = crate::utils::bit_set(val, 6);
                if crate::utils::bit_set(val, 7) {
                    self.channel4.trigger();
                    crate::debug_log!("channel 4 triggered!");
                    crate::debug_log!("{:#?}", self.channel4);
                }
            }
            _ => {}
        }
    }

    pub fn render(&mut self, data: &mut [f32]) {
        if self.samples_ready < data.len() {
            return;
        }
        self.samples_ready -= data.len();
        if self.last_rendered_sample + data.len() < self.buffer.len() {
            data.copy_from_slice(
                &self.buffer[self.last_rendered_sample..][..data.len()],
            );
        } else {
            let elems_til_end = self.buffer.len() - self.last_rendered_sample;
            data[0..elems_til_end]
                .copy_from_slice(&self.buffer[self.last_rendered_sample..]);
            let remainder_elems = data.len() - elems_til_end;
            data[elems_til_end..]
                .copy_from_slice(&self.buffer[0..remainder_elems]);
        }
        self.last_rendered_sample += data.len();
        if self.last_rendered_sample >= self.buffer.len() {
            self.last_rendered_sample %= self.buffer.len();
        }
    }
}

impl crate::utils::Dump for APU {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        if self.is_powered_on() {
            writeln!(out, "ENABLED")?;
        } else {
            writeln!(out, "DISABLED")?;
        }
        writeln!(out, "SAMPLE RATE (EXT): {}", self.sample_rate())?;
        writeln!(out, "SAMPLE NUM: {}", self.sample_num)?;
        writeln!(out, "LAST SAMPLE: {}", self.last_rendered_sample)?;
        writeln!(out, "BUF:")?;
        writeln!(out, "{:?}", &self.buffer[00..08])?;
        writeln!(out, "{:?}", &self.buffer[08..16])?;
        writeln!(out, "{:?}", &self.buffer[16..24])?;
        writeln!(out, "{:?}", &self.buffer[24..32])?;
        writeln!(out, "{:?}", &self.buffer[32..40])?;
        writeln!(out, "{:?}", &self.buffer[40..48])?;
        writeln!(out, "{:?}", &self.buffer[48..56])?;
        writeln!(out, "{:?}", &self.buffer[56..64])?;
        Ok(())
    }
}
