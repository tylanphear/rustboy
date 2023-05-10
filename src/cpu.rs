use std::collections::HashMap;

use crate::mmu::MMU;
use crate::opcodes::{self, Op, OpStatus};
use crate::utils::Regs;

pub mod regs {
    // Bit 4 - Joypad
    // Bit 3 - Serial
    // Bit 2 - Timer
    // Bit 1 - LCD STAT
    // Bit 0 - VBlank
    pub const IE: u16 = 0xFFFF;

    // Bit 4 - Joypad
    // Bit 3 - Serial
    // Bit 2 - Timer
    // Bit 1 - LCD STAT
    // Bit 0 - VBlank
    pub const IF: u16 = 0xFF0F;
}
use regs::*;

#[derive(Clone, Copy)]
struct CurrentOp {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum State {
    Stopped,
    Executing(u64),
    Retiring(u64),
}

#[derive(Debug, Default)]
pub struct Breakpoints {
    addr2hits: HashMap<u16, usize>,
}

impl Breakpoints {
    pub fn reset(&mut self) {
        self.addr2hits.clear();
    }

    pub fn register(&mut self, addr: u16) {
        self.addr2hits.insert(addr, 0);
    }

    pub fn deregister(&mut self, addr: u16) {
        self.addr2hits.remove(&addr);
    }

    pub fn count(&self, addr: u16) -> usize {
        self.addr2hits.get(&addr).copied().unwrap_or(0)
    }

    pub fn reset_count(&mut self, addr: u16) {
        *self.addr2hits.get_mut(&addr).unwrap() = 0;
    }

    fn check_hit(&mut self, addr: &u16) {
        if let Some(count) = self.addr2hits.get_mut(addr) {
            *count = count.saturating_add(1);
        }
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

pub struct CPU {
    pub regs: Regs,
    pub mmu: MMU,
    ime: bool, // Interrupt Master Enable

    enable_interrupts_next_inst: bool,
    current_op: Option<&'static Op>,
    state: State,

    pub breakpoints: Breakpoints,
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            regs: Regs::default(),
            mmu: MMU::new(),
            ime: false,
            enable_interrupts_next_inst: false,
            current_op: None,
            state: State::Retiring(0),
            breakpoints: Default::default(),
        }
    }

    pub fn current_op(&self) -> Option<&'static Op> {
        self.current_op
    }

    pub fn tick(&mut self) -> (bool, bool) {
        const CYCLES_PER_TICK: u64 = 4;

        let (mut op_was_retired, mut op_was_fetched) = (false, false);
        match self.state {
            State::Retiring(0) => {
                self.breakpoints.check_hit(&self.regs.pc);
                let next_op = self.fetch_and_decode_one_inst();
                self.current_op = Some(next_op);
                self.state = State::Executing(next_op.clocks as u64);
                op_was_fetched = true;
            }
            _ => {}
        };

        match self.state {
            State::Executing(CYCLES_PER_TICK) => {
                let extra_cycles =
                    self.execute_one_inst(self.current_op.unwrap());
                self.state = State::Retiring(extra_cycles);
                op_was_retired = true;
            }
            State::Executing(ref mut cycles)
            | State::Retiring(ref mut cycles) => {
                *cycles -= CYCLES_PER_TICK;
            }
            _ => {}
        };

        self.mmu.tick();
        let need_vblank_interrupt = self.mmu.io.lcd.tick();
        let need_timer_interrupt = self.mmu.io.timer.tick();
        if need_timer_interrupt || need_vblank_interrupt {
            let iflags = self.mmu.load8(IF);
            let timer = (need_timer_interrupt as u8) << 1;
            let vblank = (need_vblank_interrupt as u8) << 2;
            self.mmu.store8(IF, iflags | timer | vblank);
        }

        (op_was_fetched, op_was_retired)
    }

    pub fn stop(&mut self) {
        self.state = State::Stopped;
    }

    pub fn reset(&mut self) {
        self.regs = Regs::default();
        self.mmu.clear();
        self.ime = false;
        self.enable_interrupts_next_inst = false;
        self.state = State::Retiring(0);
        self.current_op = None;
        self.breakpoints.reset();
    }

    fn fetch_and_decode_one_inst(&self) -> &'static Op {
        let first_byte = self.mmu.load8(self.regs.pc);
        if first_byte != 0xCB {
            opcodes::from_code(first_byte)
        } else {
            let next_byte = self.mmu.load8(self.regs.pc + 1);
            opcodes::from_prefixed_code(next_byte)
        }
    }

    fn execute_one_inst(&mut self, op: &Op) -> u64 {
        let status = (op.handler)(self, op.args, op.bytes);
        match status {
            OpStatus::Normal => {
                self.regs.pc += op.bytes as u16;
                0
            }
            OpStatus::BranchTaken(extra_cycles) => extra_cycles as u64,
        }
    }

    fn handle_interrupts(&mut self) {
        if !self.ime {
            return;
        }

        // An interrupt is requested if the corresponding bit of IE & IF is set,
        // with the bottom-most bit taking priority in cases where more than one
        // bit is set.
        let interrupt_mask =
            self.mmu.load8(IE) & self.mmu.load8(IF) & 0b00011111;
        let interrupt_requested = interrupt_mask.trailing_zeros() as u16;
        if interrupt_requested > 5 {
            return;
        }

        // Bit 4 - Joypad         - 0x0060
        // Bit 3 - Serial         - 0x0058
        // Bit 2 - Timer          - 0x0050
        // Bit 1 - LCD STAT       - 0x0048
        // Bit 0 - Vertical Blank - 0x0040
        let interrupt_vector = 0x40 + 0x8 * interrupt_requested;

        // Clear IF to signal the interrupt was dispatched.
        self.mmu.store8(IF, 0b11100000);

        // Disable interrupts.
        self.disable_interrupts();

        // Save PC onto stack.
        self.regs.sp = self.regs.sp.wrapping_sub(2);
        self.mmu.store16(self.regs.sp, self.regs.pc);

        // jump to interrupt vector
        self.regs.pc = interrupt_vector;

        // TODO: It takes 20 clocks to dispatch an interrupt
        // (TODO: +4 clocks in halt mode(?))
        // self.tick(20);
    }

    pub fn disable_interrupts(&mut self) {
        self.ime = false;
    }

    pub fn enable_interrupts(&mut self) {
        self.ime = true;
    }

    pub fn enable_interrupts_next_inst(&mut self) {
        self.enable_interrupts_next_inst = true;
    }

    pub fn dump<W: std::fmt::Write>(&self, out: &mut W) -> std::fmt::Result {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vblank_interrupt_is_triggered() {
        let mut cpu = CPU::new();
        cpu.stop();
        // First enable LCD power
        cpu.mmu.store8(crate::io::lcd::reg::LCDC, 0x80);
        assert_eq!(cpu.mmu.io.lcd.mode(), 0);
        // Simulate 144 lines before VBlank
        for _ in 0..144 {
            for _ in 0..456 {
                cpu.tick();
            }
        }
        assert_eq!(cpu.mmu.io.lcd.mode(), 0);
        cpu.tick();
        assert_eq!(cpu.mmu.io.lcd.mode(), 1);
        assert_eq!(cpu.mmu.load8(IF) & 0x1, 0x1);
    }
}
