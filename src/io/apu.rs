use crate::utils::Mem;
use crate::utils::FallingEdgeDetector;
use serde::{Deserialize, Serialize};

// If n = sample_rate,
//
//  1s = n samples
//  frequency = 1s / period
//  period = n samples / frequency

pub const MAIN_CLOCK_FREQUENCY: u64 = crate::cpu::T_CLOCK_FREQUENCY;

/// Gameboy audio consists of 4 channels:
///
///  1. Square wave with volume envelope and frequency sweep
///  2. Square wave with volume envelope only
///  3. Programmable wave channel w/ 32 4-bit samples
///  4. Noise channel with volume envelope

pub mod reg {
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

    /// UNUSED
    pub const NR20: u16 = 0xFF15;
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

    /// UNUSED
    pub const NR40: u16 = 0xFF1F;
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

pub const CHANNEL_3_TABLE_START: u16 = 0xFF30;
pub const CHANNEL_3_TABLE_LAST: u16 = 0xFF3F;
pub const CHANNEL_3_TABLE_END: u16 = 0xFF40;
pub const CHANNEL_3_TABLE_SIZE: usize =
    (CHANNEL_3_TABLE_END - CHANNEL_3_TABLE_START) as usize;
pub const REGS_START: u16 = reg::NR10;
pub const REGS_END: u16 = CHANNEL_3_TABLE_START;

const FALLOFF_FACTOR: f32 = 2.0;
#[derive(Debug, Default, Serialize, Deserialize)]
struct DAC {
    output: f32,
}

impl DAC {
    fn convert(
        &mut self,
        enabled: bool,
        channel_enabled: bool,
        mut input: u8,
    ) -> f32 {
        if !channel_enabled {
            // disabled channel yields digital 0
            input = 0;
        }
        if enabled {
            // TODO: smooth transition old output to new input?
            self.output = Self::digital_to_analog(input);
        } else {
            self.output /= FALLOFF_FACTOR;
        }
        self.output
    }

    fn digital_to_analog(input: u8) -> f32 {
        1.0 - 2.0 * (input as f32) * (1.0 / 15.0)
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct LengthUnit<const LIMIT: usize> {
    enabled: bool,
    count: u16,
}

impl<const LIMIT: usize> LengthUnit<LIMIT> {
    fn set_count(&mut self, v: u8) {
        self.count = (LIMIT - (v as usize)) as u16;
    }

    fn trigger(&mut self) {
        if self.count == 0 {
            self.count = LIMIT as u16;
        }
    }

    fn tick(&mut self, clock: &Clock, unit_enabled: &mut bool) {
        if clock.length_tick {
            if !self.enabled || self.count == 0 {
                return;
            }
            self.count -= 1;
            if self.count == 0 {
                *unit_enabled = false;
            }
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct EnvelopeUnit {
    clock: u64,
    volume: u8,
    increase: bool,
    pace: u8,
}

impl EnvelopeUnit {
    fn trigger(&mut self, pace: u8, volume: u8, increase: bool) {
        self.pace = pace;
        self.volume = volume;
        self.increase = increase;
    }

    fn tick(&mut self, clock: &Clock) {
        if clock.envelope_tick {
            self.clock = self.clock.wrapping_add(1);
            if self.pace == 0 {
                return;
            }
            if self.clock % (self.pace as u64) == 0 {
                match self.increase {
                    true if self.volume != 0xF => {
                        self.volume += 1;
                    }
                    false if self.volume != 0 => {
                        self.volume -= 1;
                    }
                    _ => {}
                }
            }
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct FrequencyUnit {
    clock: u64,
    enabled: bool,
    negate: bool,
    shadow_period: u16,
    shift: u8,
    pace: u8,
}

impl FrequencyUnit {
    fn trigger(&mut self, regs: &Registers) {
        self.pace = (regs.read(reg::NR10) >> 4) & 0b111;
        self.negate = crate::utils::bit_set(regs.read(reg::NR10), 3);
        self.shift = regs.read(reg::NR10) & 0b111;
        self.enabled = self.pace != 0 || self.shift != 0;
        if self.enabled {
            self.shadow_period = regs.channel_period(1);
            self.compute_next_period();
        }
    }

    fn compute_next_period(&mut self) -> Option<u16> {
        let current_period = self.shadow_period;
        let next_period = match self.negate {
            true => current_period - (current_period >> self.shift),
            false => current_period + (current_period >> self.shift),
        };
        if next_period > 0x7FF {
            return None;
        }
        self.shadow_period = next_period;
        Some(self.shadow_period)
    }

    fn tick(
        &mut self,
        clock: &Clock,
        channel_enabled: &mut bool,
        period: &mut u16,
    ) {
        if clock.sweep_tick {
            self.clock = self.clock.wrapping_add(1);
            if !self.enabled || self.pace == 0 {
                return;
            }
            if self.clock % (self.pace as u64) == 0 {
                if let Some(next) = self.compute_next_period() {
                    *period = next;
                } else {
                    *channel_enabled = false;
                }
            }
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct SquareCircuit<const SWEEP: bool> {
    enabled: bool,
    dac: DAC,

    length_unit: LengthUnit<64>,

    duty_cycle: u8,
    duty_step: u8,
    duty_timer: u64,

    period: u16,
    freq_unit: FrequencyUnit,

    envelope: EnvelopeUnit,
}

const SQUARE_WAVES: [[u8; 8]; 4] = [
    /* Duty 0 */ [0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x0],
    /* Duty 1 */ [0x0, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x0],
    /* Duty 2 */ [0x0, 0x1, 0x1, 0x1, 0x1, 0x0, 0x0, 0x0],
    /* Duty 3 */ [0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1],
];

impl<const SWEEP: bool> SquareCircuit<SWEEP> {
    const CHANNEL: usize = if SWEEP { 1 } else { 2 };

    fn trigger(&mut self, regs: &Registers) {
        self.enabled = true;
        self.length_unit.trigger();

        self.duty_timer = 0;

        self.period = {
            // weird behavior: bottom two bits are not set on trigger.
            (self.period & 0b11) | (regs.channel_period(Self::CHANNEL) & !0b11)
        };
        self.envelope.trigger(
            regs.channel_envelope_pace(Self::CHANNEL),
            regs.channel_envelope_volume(Self::CHANNEL),
            regs.channel_envelope_increase(Self::CHANNEL),
        );
        if SWEEP {
            self.freq_unit.trigger(regs);
        }
        if !regs.channel_dac_enabled(Self::CHANNEL) {
            self.enabled = false;
        }
    }

    fn write(&mut self, regs: &Registers, address: u16, val: u8) {
        match address {
            reg::NR10 => {
                let negate = crate::utils::bit_set(val, 3);
                if !negate && self.sweep.negate {
                    self.enabled = false;
                }
                self.sweep.shift = val & 0b111;
            }
            reg::NR11 | reg::NR21 => {
                self.duty_cycle = val >> 6;
                self.length_unit.set_count(val & 0b0011_1111);
            }
            reg::NR12 | reg::NR22 => {
                if self.enabled {
                    /* writes observed on channel trigger */
                } else {
                    self.envelope.pace =
                        regs.channel_envelope_pace(Self::CHANNEL);
                    self.envelope.volume =
                        regs.channel_envelope_volume(Self::CHANNEL);
                    self.envelope.increase =
                        regs.channel_envelope_increase(Self::CHANNEL);
                }
            }
            reg::NR13 | reg::NR23 => {
                self.period = regs.channel_period(Self::CHANNEL);
            }
            reg::NR14 | reg::NR24 => {
                self.length_unit.enabled = crate::utils::bit_set(val, 6);
                self.period = regs.channel_period(Self::CHANNEL);
                let trigger = crate::utils::bit_set(val, 7);
                if trigger {
                    self.trigger(regs);
                }
            }
            _ => unreachable!(),
        }
    }

    fn tick(&mut self, regs: &mut Registers, clock: &Clock) -> f32 {
        let period = self.period as u64;

        if SWEEP {
            self.freq_unit
                .tick(clock, &mut self.enabled, &mut self.period);
        }
        self.length_unit.tick(clock, &mut self.enabled);
        self.envelope.tick(clock);

        self.duty_timer = self.duty_timer.wrapping_add(1);
        if period == 0 {
            return 0.0;
        }

        let sample_period = 2048 - self.period as u64;
        if self.duty_timer % sample_period == 0 {
            self.duty_step += 1;
            if self.duty_step == 8 {
                self.duty_step = 0;
            }
        }

        let waveform =
            SQUARE_WAVES[self.duty_cycle as usize][self.duty_step as usize];

        let dac_enabled = regs.channel_dac_enabled(Self::CHANNEL);
        self.dac.convert(
            dac_enabled,
            self.enabled,
            waveform * self.envelope.volume,
        )
    }
}

impl<const SWEEP: bool> crate::utils::Dump for SquareCircuit<SWEEP> {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "{self:#?}")
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct WaveCircuit {
    enabled: bool,
    dac: DAC,

    sample_index: u8,
    sample_buffer: u8,

    length_unit: LengthUnit<256>,
    volume_code: u8,
    period: u16,
    pattern: [u8; CHANNEL_3_TABLE_SIZE],
}

impl WaveCircuit {
    fn trigger(&mut self, regs: &Registers) {
        self.enabled = true;
        self.sample_index = 0;
        self.pattern = *regs.channel_3_wave_table();
        self.period = regs.channel_period(3);
        self.length_unit.trigger();
        if !regs.channel_dac_enabled(3) {
            self.enabled = false;
        }
    }

    fn write(&mut self, regs: &Registers, address: u16, val: u8) {
        match address {
            reg::NR30 => {
                if !regs.channel_dac_enabled(3) {
                    self.enabled = false;
                }
            }
            reg::NR31 => {
                self.length_unit.set_count(val);
            }
            reg::NR32 => {
                self.volume_code = (val >> 4) & 0b11;
            }
            reg::NR33 => {
                self.period = regs.channel_period(3);
            }
            reg::NR34 => {
                self.length_unit.enabled = crate::utils::bit_set(val, 6);
                self.period = regs.channel_period(3);
                let trigger = crate::utils::bit_set(val, 7);
                if trigger && regs.channel_dac_enabled(3) {
                    self.trigger(regs);
                }
            }
            CHANNEL_3_TABLE_START..=CHANNEL_3_TABLE_LAST => {
                if self.enabled {
                    // TODO: implement weird behavior
                } else {
                    self.pattern[(address - CHANNEL_3_TABLE_START) as usize] =
                        val;
                }
            }
            _ => unreachable!(),
        }
    }

    fn tick(&mut self, regs: &Registers, clock: &Clock) -> f32 {
        let period = self.period as usize;
        if period == 0 {
            return 0.0;
        }

        // Waveform is emitted from buffer
        let waveform = self.sample_buffer;

        let sample_period = 2048 - (self.period as u64);
        if clock.cycles % sample_period == 0 {
            self.sample_index += 1;
            if self.sample_index == 32 {
                self.sample_index = 0;
            }

            // Update buffer for new nibble
            let sample_num = (self.sample_index / 2) as usize;
            let sample_nibble = (self.sample_index % 2) as usize;
            let next_sample = match sample_nibble {
                0 => self.pattern[sample_num] & 0xF,
                1 => self.pattern[sample_num] >> 4,
                _ => unreachable!(),
            };
            self.sample_buffer = next_sample;
        }

        let attenuation_shift = match self.volume_code & 0b11 {
            0b00 => 4,
            0b01 => 0,
            0b10 => 1,
            0b11 => 2,
            _ => unreachable!(),
        };
        self.length_unit.tick(clock, &mut self.enabled);

        let dac_enabled = regs.channel_dac_enabled(3);
        self.dac.convert(
            dac_enabled,
            self.enabled,
            waveform >> attenuation_shift,
        )
    }
}

impl crate::utils::Dump for WaveCircuit {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "{self:#?}")
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct LFSR {
    state: u16,
    short_mode: bool,
}

impl LFSR {
    pub fn tick(&mut self) {
        let bit0 = crate::utils::get_bit(self.state, 0);
        let bit1 = crate::utils::get_bit(self.state, 1);
        let new_top_bit = (!(bit0 ^ bit1)) & 0b1;
        self.state = crate::utils::set_bit(self.state, 15, new_top_bit);
        if self.short_mode {
            self.state = crate::utils::set_bit(self.state, 7, new_top_bit);
        }
        self.state >>= 1;
    }

    fn output(&self) -> u8 {
        (!crate::utils::bit_set(self.state, 0)) as u8
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct NoiseCircuit {
    enabled: bool,
    dac: DAC,

    lfsr: LFSR,

    length_unit: LengthUnit<64>,
    envelope: EnvelopeUnit,

    divider_code: u8,
    shift: u8,
}

impl NoiseCircuit {
    fn trigger(&mut self, regs: &Registers) {
        self.enabled = true;
        self.lfsr.state = 0;
        self.length_unit.trigger();
        self.envelope.trigger(
            regs.channel_envelope_pace(4),
            regs.channel_envelope_volume(4),
            regs.channel_envelope_increase(4),
        );
        if !regs.channel_dac_enabled(4) {
            self.enabled = false;
        }
    }

    fn tick(&mut self, regs: &Registers, clock: &Clock) -> f32 {
        // LFSR frequency is (M / 4) / (R * 2^S) where
        //    M = main clock frequency
        //    R = divider code (where 0 => 1/2)
        //    S = clock shift
        const BASE_FREQUENCY: u64 = MAIN_CLOCK_FREQUENCY / 4;
        let frequency = match self.divider_code & 0b111 {
            0 => 2 * BASE_FREQUENCY / (1 << self.shift),
            r => BASE_FREQUENCY / ((1 << self.shift) * (r as u64)),
        };

        if clock.cycles % (MAIN_CLOCK_FREQUENCY / frequency) == 0 {
            self.lfsr.tick();
        }
        let waveform = self.lfsr.output() * self.envelope.volume;

        self.length_unit.tick(clock, &mut self.enabled);
        self.envelope.tick(clock);

        let dac_enabled = regs.channel_dac_enabled(4);
        self.dac.convert(dac_enabled, self.enabled, waveform)
    }
}

impl crate::utils::Dump for NoiseCircuit {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        writeln!(out, "{self:#?}")
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(transparent)]
struct Registers {
    data: Mem<{ 0xFF40 - 0xFF10 }>,
}

impl Registers {
    fn read(&self, address: u16) -> u8 {
        self.data[address - 0xFF10]
    }

    fn write(&mut self, address: u16, val: u8) {
        self.data[address - 0xFF10] = val;
    }

    fn modify(&mut self, address: u16, f: impl FnOnce(u8) -> u8) {
        self.data[address - 0xFF10] = f(self.data[address - 0xFF10]);
    }

    fn clear(&mut self) {
        self.data.clear();
    }

    fn channel_3_wave_table(&self) -> &[u8; 16] {
        self.data.slice_n::<CHANNEL_3_TABLE_SIZE>(
            CHANNEL_3_TABLE_START as usize - REGS_START as usize,
        )
    }

    fn channel_envelope_volume(&self, channel: usize) -> u8 {
        match channel {
            1 => self.read(reg::NR12) >> 4,
            2 => self.read(reg::NR22) >> 4,
            4 => self.read(reg::NR42) >> 4,
            _ => unreachable!(),
        }
    }

    fn channel_envelope_increase(&self, channel: usize) -> bool {
        match channel {
            1 => crate::utils::bit_set(self.read(reg::NR12), 3),
            2 => crate::utils::bit_set(self.read(reg::NR22), 3),
            4 => crate::utils::bit_set(self.read(reg::NR42), 3),
            _ => unreachable!(),
        }
    }

    fn channel_dac_enabled(&self, channel: usize) -> bool {
        match channel {
            1 => self.read(reg::NR12) & 0xF8 != 0,
            2 => self.read(reg::NR22) & 0xF8 != 0,
            3 => self.read(reg::NR30) & 0x80 != 0,
            4 => self.read(reg::NR42) & 0xF8 != 0,
            _ => unreachable!(),
        }
    }

    fn channel_envelope_pace(&self, channel: usize) -> u8 {
        match channel {
            1 => self.read(reg::NR12) & 0b111,
            2 => self.read(reg::NR22) & 0b111,
            4 => self.read(reg::NR42) & 0b111,
            _ => unreachable!(),
        }
    }

    fn channel_period(&self, channel: usize) -> u16 {
        match channel {
            1 => u16::from_le_bytes([
                self.read(reg::NR13),
                self.read(reg::NR14) & 0b111,
            ]),
            2 => u16::from_le_bytes([
                self.read(reg::NR23),
                self.read(reg::NR24) & 0b111,
            ]),
            3 => u16::from_le_bytes([
                self.read(reg::NR33),
                self.read(reg::NR34) & 0b111,
            ]),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct HighPassFilter {
    capacitor: f32,
}
const CHARGE_FACTOR: f32 = {
    0.999832 // == 0.999958.powi(4)
};

impl HighPassFilter {
    fn reset(&mut self) {
        self.capacitor = 0.0;
    }

    fn filter(&mut self, dacs_enabled: bool, input: f32) -> f32 {
        let mut output = 0.0;
        if dacs_enabled {
            output = input - self.capacitor;
            self.capacitor = input - output * CHARGE_FACTOR;
        }
        output
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct Clock {
    cycles: u64,
    div_apu_counter: u64,

    length_tick: bool,
    envelope_tick: bool,
    sweep_tick: bool,

    div_edge_detector: FallingEdgeDetector</*BIT=*/ 4, u8>,
}

impl Clock {
    fn tick(&mut self, new_div: u8) {
        self.cycles = self.cycles.wrapping_add(1);
        self.length_tick = false;
        self.envelope_tick = false;
        self.sweep_tick = false;
        if self.div_edge_detector.set_and_check(new_div) {
            self.div_apu_counter = self.div_apu_counter.wrapping_add(1);
            let step = self.div_apu_counter % 8;
            match step {
                0 | 2 | 4 | 6 => self.length_tick = true,
                _ => {}
            }
            match step {
                7 => self.envelope_tick = true,
                _ => {}
            }
            match step {
                2 | 6 => self.sweep_tick = true,
                _ => {}
            }
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct APU {
    clock: Clock,
    buffer: Vec<f32>,

    num_channels: usize,
    sample_period: u64,
    sample_rate: u64,

    regs: Registers,
    sample_num: u64,
    samples_ready: usize,
    last_rendered_sample: usize,
    channel1: SquareCircuit</*SWEEP=*/ true>,
    channel2: SquareCircuit</*SWEEP=*/ false>,
    channel3: WaveCircuit,
    channel4: NoiseCircuit,
    high_pass_left: HighPassFilter,
    high_pass_right: HighPassFilter,
    external_volumes: [f32; 5],
}

impl APU {
    pub fn reset(&mut self) {
        self.clock = Clock::default();
        self.buffer.fill(0.0);
        self.regs.clear();
        self.sample_num = 0;
        self.samples_ready = 0;
        self.last_rendered_sample = 0;
        self.channel1 = Default::default();
        self.channel2 = Default::default();
        self.channel3 = Default::default();
        self.channel4 = Default::default();
        self.high_pass_left.reset();
        self.high_pass_right.reset();
    }

    fn is_powered_on(&self) -> bool {
        crate::utils::bit_set(self.regs.read(reg::NR52), 7)
    }

    pub fn tick(&mut self, new_div: u8) {
        if self.sample_rate == 0 {
            self.clock.tick(new_div);
            return;
        }

        let pan = self.regs.read(reg::NR51);
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
            panned(4, 0, self.channel1.tick(&mut self.regs, &self.clock));
        let (channel_2_left, channel_2_right) =
            panned(5, 1, self.channel2.tick(&mut self.regs, &self.clock));
        let (channel_3_left, channel_3_right) =
            panned(6, 2, self.channel3.tick(&self.regs, &self.clock));
        let (channel_4_left, channel_4_right) =
            panned(7, 3, self.channel4.tick(&self.regs, &self.clock));

        self.regs.modify(reg::NR52, |reg| {
            reg & !(0b1111)
                | ((self.channel4.enabled as u8) << 3)
                | ((self.channel3.enabled as u8) << 2)
                | ((self.channel2.enabled as u8) << 1)
                | ((self.channel1.enabled as u8) << 0)
        });

        self.clock.tick(new_div);

        // Check if compute cycles % sample_period == 0, i.e. if we should emit
        // a sample this tick (relies on sample_period being a power of 2).
        if self.clock.cycles & (self.sample_period - 1) != 0 {
            return;
        }

        let (left_vol, right_vol) = (
            (self.regs.read(reg::NR50) >> 4) & 0b111,
            (self.regs.read(reg::NR50) >> 0) & 0b111,
        );

        let ext_vol_percent = |n: usize| -> f32 {
            // external volume ranges from 0 to 100
            // we want 0 to give a multiplier of 0,
            //    and 100 to give a multiplier of 1,
            // with exponential scaling from 0 to 100
            //
            let pct = self.external_volumes[n] / 100.0;
            if pct == 0.0 {
                0.0
            } else {
                f32::powf(10.0, 3.0 * pct - 3.0)
            }
        };
        let mix = |ch1: f32, ch2: f32, ch3: f32, ch4: f32, volume: u8| {
            let channels = (ch1 * ext_vol_percent(1)
                + ch2 * ext_vol_percent(2)
                + ch3 * ext_vol_percent(3)
                + ch4 * ext_vol_percent(4))
                / 4.0;
            let amplification_factor = (volume as f32 + 1.0) / 16.0;
            channels * amplification_factor * ext_vol_percent(0)
        };
        let dacs_enabled = self.regs.channel_dac_enabled(1)
            || self.regs.channel_dac_enabled(2)
            || self.regs.channel_dac_enabled(3)
            || self.regs.channel_dac_enabled(4);
        let (left, right) = (
            self.high_pass_left.filter(
                dacs_enabled,
                mix(
                    channel_1_left,
                    channel_2_left,
                    channel_3_left,
                    channel_4_left,
                    left_vol,
                ),
            ),
            self.high_pass_right.filter(
                dacs_enabled,
                mix(
                    channel_1_right,
                    channel_2_right,
                    channel_3_right,
                    channel_4_right,
                    right_vol,
                ),
            ),
        );

        match self.num_channels {
            1 => {
                self.buffer[self.sample_num as usize] = (left + right) / 2.0;
            }
            2 => {
                // 2 channels interleaved [L, R, L, R, ...]
                let (l, r) = (2 * self.sample_num, 2 * self.sample_num + 1);
                self.buffer[l as usize] = left;
                self.buffer[r as usize] = right;
            }
            _ => unreachable!(),
        }
        self.sample_num += 1;
        if self.sample_num == self.sample_rate {
            self.sample_num = 0;
        }
        self.samples_ready += self.num_channels;
    }

    pub fn init(&mut self, sample_rate: usize, channels: usize) {
        assert!(
            channels == 1 || channels == 2,
            "can only support mono/stereo configs!"
        );
        self.buffer.clear();
        self.buffer.resize(sample_rate * channels, 0.0);
        self.num_channels = channels;
        self.sample_rate = sample_rate as u64;
        self.sample_period = MAIN_CLOCK_FREQUENCY / self.sample_rate;
        assert!(self.sample_period.is_power_of_two());
    }

    pub fn load(&self, address: u16) -> u8 {
        let val = self.regs.read(address);
        let mask = match address {
            reg::NR10 => 0x80,
            reg::NR20 => 0xFF,
            reg::NR30 => 0x7F,
            reg::NR40 => 0xFF,
            reg::NR50 => 0x00,
            reg::NR11 | reg::NR21 => 0x3F,
            reg::NR31 | reg::NR41 => 0xFF,
            reg::NR51 => 0x00,
            reg::NR12 | reg::NR22 | reg::NR42 => 0x00,
            reg::NR32 => 0x9F,
            reg::NR13 | reg::NR23 | reg::NR33 => 0xFF,
            reg::NR43 => 0x00,
            reg::NR14 | reg::NR24 | reg::NR34 | reg::NR44 => 0xBF,
            reg::NR52 => 0x70,
            0xFF27..=0xFF2F => 0xFF,
            _ => 0x0,
        };
        let result = val | mask;
        crate::debug_log!("apu: {address:04X} -> {result:08b}");
        result
    }

    pub fn store(&mut self, address: u16, val: u8) {
        match address {
            // Only top bit is writeable
            reg::NR52 => {
                let power = crate::utils::bit_set(val, 7);
                if power {
                    self.power_on();
                } else {
                    self.power_off();
                }
            }
            _ if !self.is_powered_on() => {
                crate::debug_log!("apu: (ignored) {address:04X} <- {val:08b}");
            }
            _ => {
                self.regs.write(address, val);
                crate::debug_log!("apu: {address:04X} <- {val:08b}");
            }
        };
        match address {
            reg::NR10 | reg::NR11 | reg::NR12 | reg::NR13 | reg::NR14 => {
                self.channel1.write(&self.regs, address, val);
            }
            reg::NR21 | reg::NR22 | reg::NR23 | reg::NR24 => {
                self.channel2.write(&self.regs, address, val);
            }
            reg::NR30
            | reg::NR31
            | reg::NR32
            | reg::NR34
            | CHANNEL_3_TABLE_START..=CHANNEL_3_TABLE_LAST => {
                self.channel3.write(&self.regs, address, val);
            }
            reg::NR41 => {
                self.channel4.length_unit.set_count(val & 0b0011_1111);
            }
            reg::NR42 => { /* writes observed on channel trigger */ }
            reg::NR43 => {
                self.channel4.divider_code = val & 0b111;
                self.channel4.lfsr.short_mode = crate::utils::bit_set(val, 3);
                self.channel4.shift = val >> 4;
            }
            reg::NR44 => {
                self.channel4.length_unit.enabled =
                    crate::utils::bit_set(val, 6);
                let trigger = crate::utils::bit_set(val, 7);
                if trigger {
                    self.channel4.trigger(&self.regs);
                }
            }
            _ => {}
        }
    }

    fn power_on(&mut self) {
        crate::debug_log!("apu: power on");
        self.regs
            .modify(reg::NR52, |reg| crate::utils::set_bit(reg, 7, 1));
        self.sequencer.div_apu_counter = 0;
        self.channel1.duty_step = 0;
        self.channel2.duty_step = 0;
        self.channel3.pattern.fill(0);
    }

    fn power_off(&mut self) {
        crate::debug_log!("apu: power off");
        for reg in REGS_START..REGS_END {
            if reg != reg::NR52 {
                self.store(reg as u16, 0);
            }
        }
        self.channel1.enabled = false;
        self.channel2.enabled = false;
        self.channel3.enabled = false;
        self.channel4.enabled = false;
        self.regs
            .modify(reg::NR52, |reg| crate::utils::set_bit(reg, 7, 0));
    }

    pub fn render(&mut self, data: &mut [f32], external_volumes: &[f32; 5]) {
        self.external_volumes = *external_volumes;
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
        writeln!(out, "NR52: {:02X}", self.regs.read(reg::NR52))?;
        writeln!(out, "SAMPLE RATE (EXT): {}", self.sample_rate)?;
        writeln!(out, "SAMPLE NUM: {}", self.sample_num)?;
        writeln!(out, "LAST SAMPLE: {}", self.last_rendered_sample)?;
        writeln!(out, "CHANNEL 1")?;
        self.channel1.dump(out)?;
        writeln!(out, "CHANNEL 2")?;
        self.channel2.dump(out)?;
        writeln!(out, "CHANNEL 3")?;
        self.channel3.dump(out)?;
        writeln!(out, "CHANNEL 4")?;
        self.channel4.dump(out)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn div_apu_clock_ticks_at_512_hz() {
        use crate::io::timer::regs::DIV;
        const TICKS_FOR_512_HZ: u64 = crate::cpu::T_CLOCK_FREQUENCY / 512;

        let mut timer = crate::io::Timer::default();
        let mut clock = Clock::default();
        for n in 0..100 {
            for tick in 0..TICKS_FOR_512_HZ {
                assert_eq!(clock.div_apu_counter, n);
                if tick != 0 {
                    assert_ne!(clock.cycles % TICKS_FOR_512_HZ, 0);
                }
                timer.tick(&mut crate::cpu::Interrupts::default());
                clock.tick(timer.load(DIV));
            }
            assert_eq!(clock.div_apu_counter, n + 1);
            assert_eq!(clock.cycles % TICKS_FOR_512_HZ, 0);
        }
    }
}
