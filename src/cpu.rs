use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::mmu::MMU;
use crate::opcodes::{self, Op, OpStatus};
use crate::utils::{Clock, Regs, TClock};

/// M clock (mem/io) clock runs at 4Mhz
pub const M_CLOCK_FREQUENCY: u64 = 4_194_304;
/// T clock (cpu/timer) clock runs at 1MHz (1/4 M clock)
pub const T_CLOCK_FREQUENCY: u64 = M_CLOCK_FREQUENCY / 4;

pub mod reg {
    /// Bit 4 - Joypad
    /// Bit 3 - Serial
    /// Bit 2 - Timer
    /// Bit 1 - LCD STAT
    /// Bit 0 - VBlank
    pub const IE: u16 = 0xFFFF;

    /// Bit 4 - Joypad
    /// Bit 3 - Serial
    /// Bit 2 - Timer
    /// Bit 1 - LCD STAT
    /// Bit 0 - VBlank
    pub const IF: u16 = 0xFF0F;
}
use reg::*;

#[derive(Default)]
pub struct Interrupts(u8);
impl Interrupts {
    pub fn request_vblank(&mut self) {
        self.0 |= 1 << 0;
    }
    pub fn vblank_requested(&self) -> bool {
        (self.0 & 1 << 0) != 0
    }
    pub fn request_lcd_stat(&mut self) {
        self.0 |= 1 << 1;
    }
    pub fn lcd_requested(&self) -> bool {
        (self.0 & 1 << 1) != 0
    }
    pub fn request_timer(&mut self) {
        self.0 |= 1 << 2;
    }
    pub fn timer_requested(&self) -> bool {
        (self.0 & 1 << 2) != 0
    }
    pub fn request_joypad(&mut self) {
        self.0 |= 1 << 4;
    }
    pub fn joypad_requested(&self) -> bool {
        (self.0 & 1 << 4) != 0
    }
    pub fn any_requested(&self) -> bool {
        self.0 != 0
    }
    pub fn mask(&self) -> u8 {
        self.0
    }
}

#[derive(Default)]
pub struct Tick {
    pub breakpoint_was_hit: bool,
    pub pc: u16,
    pub op_was_retired: bool,
}
const CYCLES_PER_TICK: u64 = 4;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
enum State {
    Stopped,
    Halted,
    ExecutingOp(u64),
    RetiringOp(u64),
    InterruptDelay(u64),
}

#[derive(Debug, Default)]
pub struct Breakpoints {
    addr2hits: HashMap<u16, usize>,
}

impl Breakpoints {
    pub fn clear(&mut self) {
        self.addr2hits.clear();
    }

    pub fn register(&mut self, addr: u16) {
        self.addr2hits.insert(addr, 0);
    }

    pub fn deregister(&mut self, addr: u16) {
        self.addr2hits.remove(&addr);
    }

    pub fn reset_count(&mut self, addr: u16) {
        if let Some(count) = self.addr2hits.get_mut(&addr) {
            *count = 0;
        }
    }

    fn check_hit(&mut self, addr: &u16) -> bool {
        if self.addr2hits.is_empty() {
            return false;
        }
        if let Some(count) = self.addr2hits.get_mut(addr) {
            *count = count.saturating_add(1);
            return true;
        }
        false
    }

    pub fn dump(&self) -> String {
        if self.addr2hits.is_empty() {
            return String::new();
        }
        let mut ret = format!("current breakpoints:\n");
        let sorted_addrs = {
            let mut keys: Vec<_> = self.addr2hits.keys().collect();
            keys.sort();
            keys
        };
        for bp in &sorted_addrs {
            ret.push_str(&format!("{:04X}\n", bp));
        }
        ret
    }
}

#[derive(Serialize, Deserialize)]
pub struct CPU {
    clock: TClock,
    pub regs: Regs,
    pub mmu: MMU,
    ime: bool, // Interrupt Master Enable

    enable_interrupts_in_n_ops: u8,
    current_op: Option<&'static Op>,
    state: State,

    #[serde(skip)]
    pub breakpoints: Breakpoints,
}

impl Default for CPU {
    fn default() -> Self {
        Self {
            clock: Default::default(),
            regs: Regs::default(),
            mmu: MMU::new(),
            ime: false,
            enable_interrupts_in_n_ops: 0,
            current_op: None,
            state: State::RetiringOp(0),
            breakpoints: Default::default(),
        }
    }
}

impl CPU {
    pub fn current_op(&self) -> Option<&'static Op> {
        self.current_op
    }

    pub fn executing_op(&self) -> bool {
        matches!(self.state, State::ExecutingOp(_))
    }

    pub fn tick(&mut self, clock: &Clock) -> Tick {
        // Start this tick at the current PC -- this may change if an interrupt
        // gets dispatched.
        let mut this_tick = Tick::default();
        this_tick.pc = self.regs.pc;

        // Tick devices (MMU/LCD/Timer) and check for interrupts that need to
        // be flagged in IF. Do this *before* we possibly dispatch an interrupt
        // this tick.
        self.tick_devices_and_check_interrupts(clock);

        let (_, should_tick) = self.clock.tick(clock);
        if !should_tick {
            return this_tick;
        }

        // Check if we're halted. If so, and there is an interrupt requested,
        // un-halt and continue execution.
        match self.state {
            State::Halted => self.unhalt_if_interrupt_requested(),
            State::InterruptDelay(ref mut delay) => {
                *delay -= CYCLES_PER_TICK;
                if *delay == 0 {
                    self.state = State::RetiringOp(0);
                }
            }
            _ => {}
        }

        // CPU is semi-pipelined: fetch/execute can occur on the same tick.
        // e.g:
        //
        // | Execute | . | R | * | R |
        // | Fetch   | * | * | . | * |
        // | Tick    | 1 | 2 | 3 | 4 |
        //
        // Pipeline stage 0: handle executing or retiring ops.
        match self.state {
            State::ExecutingOp(ref mut cycles) => {
                *cycles -= CYCLES_PER_TICK;
                if *cycles == 0 {
                    // Execute the instruction and check for extra cycles (in
                    // the case of branches).
                    self.execute_one_op(self.current_op.unwrap());
                }
            }
            State::RetiringOp(ref mut cycles) => {
                if *cycles > 0 {
                    *cycles -= CYCLES_PER_TICK;
                }
            }
            _ => {}
        };

        // Pipeline stage 1: fetch the next instruction.
        if self.state == State::RetiringOp(0) {
            this_tick.op_was_retired = true;

            // We're ready to fetch a new instruction. First, check if we
            // need to enable interrupts, then possibly handle an interrupt
            // (i.e. jump to the interrupt vector).
            self.enable_interrupts_if_scheduled();
            let interrupt_was_dispatched = self.handle_interrupts();
            if interrupt_was_dispatched {
                this_tick.pc = self.regs.pc;
                return this_tick;
            }

            // Now perform the actual fetch, and mark our current state as
            // executing.
            let next_op = self.fetch_and_decode_one_op();
            self.current_op = Some(next_op);
            self.state = State::ExecutingOp(next_op.num_clocks as u64);

            if self.breakpoints.check_hit(&self.regs.pc) {
                this_tick.breakpoint_was_hit = true;
            }
        }

        this_tick
    }

    fn tick_devices_and_check_interrupts(&mut self, clock: &Clock) {
        self.mmu.tick(clock);
        let interrupts = self.mmu.io.tick(clock);
        if interrupts.any_requested() {
            self.mmu.store8_unchecked(
                IF,
                self.mmu.load8_unchecked(IF) | interrupts.mask(),
            );
        }
    }

    pub fn unhalt_if_interrupt_requested(&mut self) {
        if self.mmu.load8_unchecked(IE) & self.mmu.load8_unchecked(IF) & 0x1F
            != 0
        {
            self.state = State::RetiringOp(0);
        }
    }

    pub fn stop(&mut self) {
        self.state = State::Stopped;
    }

    pub fn halt(&mut self) {
        self.state = State::Halted;
    }

    pub fn stopped(&self) -> bool {
        self.state == State::Stopped
    }

    pub fn reset(&mut self) {
        self.regs = Regs::default();
        self.mmu.reset();
        self.ime = false;
        self.enable_interrupts_in_n_ops = 0;
        self.state = State::RetiringOp(0);
        self.current_op = None;
    }

    fn fetch_and_decode_one_op(&self) -> &'static Op {
        let first_byte = self.mmu.load8(self.regs.pc);
        if first_byte != 0xCB {
            opcodes::op_from_code(first_byte)
        } else {
            let next_byte = self.mmu.load8(self.regs.pc + 1);
            opcodes::op_from_prefixed_code(next_byte)
        }
    }

    fn execute_one_op(&mut self, op: &Op) {
        let status = op.execute(self);
        match status {
            OpStatus::Normal => {
                self.regs.pc += op.num_bytes as u16;
                if self.state != State::Halted && self.state != State::Stopped {
                    self.state = State::RetiringOp(0);
                }
            }
            OpStatus::BranchTaken(extra_cycles) => {
                self.state = State::RetiringOp(extra_cycles as u64);
            }
        }
    }

    fn handle_interrupts(&mut self) -> bool {
        if !self.ime {
            return false;
        }

        // An interrupt is requested if the corresponding bit of IE & IF is set,
        // with the bottom-most bit taking priority in cases where more than one
        // bit is set.
        let interrupt_mask = self.mmu.load8_unchecked(IE)
            & self.mmu.load8_unchecked(IF)
            & 0b00011111;
        let interrupt_requested = interrupt_mask.trailing_zeros() as u16;
        if interrupt_requested > 5 {
            return false;
        }

        // Bit 4 - Joypad         - 0x0060
        // Bit 3 - Serial         - 0x0058
        // Bit 2 - Timer          - 0x0050
        // Bit 1 - LCD STAT       - 0x0048
        // Bit 0 - Vertical Blank - 0x0040
        let interrupt_vector = 0x40 + 0x8 * interrupt_requested;

        // Clear the correpsonding bit in IF to signal the interrupt was dispatched.
        let if_ = self.mmu.load8_unchecked(IF);
        self.mmu
            .store8_unchecked(IF, if_ & !(1 << interrupt_requested));

        // Disable interrupts.
        self.disable_interrupts();

        // Save PC onto stack.
        self.regs.sp = self.regs.sp.wrapping_sub(2);
        self.mmu.store16(self.regs.sp, self.regs.pc);

        // Jump to interrupt vector
        self.regs.pc = interrupt_vector;

        // It takes 20 clocks to dispatch an interrupt
        // (TODO: +4 clocks in halt mode(?))
        self.state = State::InterruptDelay(20);
        true
    }

    pub fn disable_interrupts(&mut self) {
        self.ime = false;
    }

    pub fn enable_interrupts(&mut self) {
        self.ime = true;
    }

    fn enable_interrupts_if_scheduled(&mut self) {
        if self.enable_interrupts_in_n_ops == 0 {
            return;
        }

        self.enable_interrupts_in_n_ops -= 1;
        if self.enable_interrupts_in_n_ops > 0 {
            return;
        }

        self.enable_interrupts();
    }

    pub fn enable_interrupts_next_inst(&mut self) {
        self.enable_interrupts_in_n_ops = 2;
    }
}

impl crate::utils::Dump for CPU {
    fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
        write!(out, "State: ")?;
        match self.state {
            State::Stopped => writeln!(out, "stopped"),
            State::Halted => writeln!(out, "halted"),
            State::RetiringOp(n) => writeln!(out, "retiring ({n})"),
            State::ExecutingOp(n) => writeln!(out, "executing ({n})"),
            State::InterruptDelay(n) => {
                writeln!(out, "dispatching interrupt ({n})")
            }
        }?;
        writeln!(out, "A : {0:02X}", self.regs.af[1])?;
        writeln!(
            out,
            "BC: {0:04X} [B: {1:02X} C: {2:02X}]",
            self.regs.bc.as_word(),
            self.regs.bc[1],
            self.regs.bc[0]
        )?;
        writeln!(
            out,
            "DE: {0:04X} [D: {1:02X} E: {2:02X}]",
            self.regs.de.as_word(),
            self.regs.de[1],
            self.regs.de[0]
        )?;
        writeln!(
            out,
            "HL: {0:04X} [H: {1:02X} L: {2:02X}]",
            self.regs.hl.as_word(),
            self.regs.hl[1],
            self.regs.hl[0]
        )?;
        writeln!(out, "SP: {0:04X}", self.regs.sp)?;
        writeln!(out, "PC: {0:04X}", self.regs.pc)?;
        writeln!(
            out,
            "Flags: {0}{1}{2}{3} (ZSHC)",
            self.regs.zero_flag(),
            self.regs.sub_flag(),
            self.regs.halfcarry_flag(),
            self.regs.carry_flag()
        )?;
        writeln!(
            out,
            "Interrupts: {}",
            if self.ime { "enabled" } else { "disabled" }
        )?;
        writeln!(out, "IF: {0:08b}", self.mmu.load8_unchecked(IF))?;
        writeln!(out, "IE: {0:08b}", self.mmu.load8_unchecked(IE))?;
        let stack_size = (u16::MAX - self.regs.sp) as usize;
        writeln!(
            out,
            "Stack: {0:02X?}",
            self.mmu
                .block_load(self.regs.sp, usize::min(stack_size, 16))
        )?;
        Ok(())
    }
}
