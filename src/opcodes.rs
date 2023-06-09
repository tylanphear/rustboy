#![deny(unreachable_patterns)]

use crate::cpu::CPU;
use crate::utils;

pub type OpHandler = fn(&mut CPU, [u8; 2], u8) -> OpStatus;
pub enum OpStatus {
    Normal,
    BranchTaken(u8),
}

#[derive(Clone, Copy)]
pub struct Op {
    pub num_bytes: u8,
    pub clocks: u8,
    pub handler: OpHandler,
    pub args: [u8; 2],
    pub repr: &'static str,
}

impl Op {
    pub fn to_string(&self, bytes: &[u8]) -> String {
        if self.repr.contains("d8") {
            self.repr.replace("d8", &format!("{:02X}", bytes[1]))
        } else if self.repr.contains("d16") {
            self.repr.replace(
                "d16",
                &format!("{:04X}", u16::from_le_bytes([bytes[1], bytes[2]])),
            )
        } else if self.repr.contains("a8") {
            self.repr
                .replace("a8", &format!("{:04X}", 0xFF00 + (bytes[1] as u16)))
        } else if self.repr.contains("a16") {
            self.repr.replace(
                "a16",
                &format!("{:04X}", u16::from_le_bytes([bytes[1], bytes[2]])),
            )
        } else if self.repr.contains("r8") {
            self.repr.replace("r8", &format!("{}", bytes[1] as i8))
        } else {
            self.repr.to_string()
        }
    }
}

pub mod trap {
    pub const TODO: u8 = 0;
    pub const NOT_EXISTS: u8 = 1;
    pub const UNREACHABLE: u8 = 2;
}

pub mod alu {
    pub const ADD: u8 = 0;
    pub const ADC: u8 = 1;
    pub const SUB: u8 = 2;
    pub const SBC: u8 = 3;
    pub const AND: u8 = 4;
    pub const OR: u8 = 5;
    pub const XOR: u8 = 6;
    pub const CP: u8 = 7;
}

pub mod alu2 {
    pub const INC: u8 = 0;
    pub const DEC: u8 = 1;
    pub const SCF: u8 = 2;
    pub const CCF: u8 = 3;
    pub const CPL: u8 = 4;
    pub const DAA: u8 = 5;
}

pub mod alu16 {
    pub const INC16: u8 = 0;
    pub const DEC16: u8 = 1;
    pub const ADDHL: u8 = 2;
    pub const ADDSP: u8 = 3;
}

pub mod reg {
    pub const A: u8 = 0;
    pub const B: u8 = 1;
    pub const C: u8 = 2;
    pub const D: u8 = 3;
    pub const E: u8 = 4;
    pub const H: u8 = 5;
    pub const L: u8 = 6;
    pub const MEM_BC: u8 = 7;
    pub const MEM_DE: u8 = 8;
    pub const MEM_HL: u8 = 9;
    pub const MEM_HLI: u8 = 10;
    pub const MEM_HLD: u8 = 11;
    pub const MEMH_C: u8 = 12;
    pub const A8: u8 = 13;
    pub const MEM_IMM16: u8 = 14;
    pub const D8: u8 = 15;
}

pub mod reg16 {
    pub const BC: u8 = 0;
    pub const DE: u8 = 1;
    pub const HL: u8 = 2;
    pub const AF: u8 = 3;
    pub const SP: u8 = 4;
    pub const D16: u8 = 5;
}

pub mod ld16 {
    pub const LOADI: u8 = 0;
    pub const PUSH: u8 = 1;
    pub const POP: u8 = 2;
    pub const SAVESP: u8 = 3;
    pub const LDSPHL: u8 = 4;
    pub const LDHLSPr8: u8 = 5;
}

pub mod control {
    pub const JR: u8 = 0;
    pub const JP: u8 = 1;
    pub const CALL: u8 = 2;
    pub const RET: u8 = 3;
    pub const RETI: u8 = 4;
    pub const JP_HL: u8 = 5;
    pub const RST: u8 = 6;

    pub const CONDNZ: u8 = 0;
    pub const CONDNC: u8 = 1;
    pub const CONDZ: u8 = 2;
    pub const CONDC: u8 = 3;
    pub const UNCOND: u8 = 4;
}

pub mod cb_rot {
    pub const RL: u8 = 0;
    pub const RLA: u8 = 1;
    pub const RLC: u8 = 2;
    pub const RLCA: u8 = 3;
    pub const RR: u8 = 4;
    pub const RRA: u8 = 5;
    pub const RRC: u8 = 6;
    pub const RRCA: u8 = 7;
    pub const SWAP: u8 = 8;
    pub const SLA: u8 = 9;
    pub const SRA: u8 = 10;
    pub const SRL: u8 = 11;
}

pub mod misc {
    pub const HALT: u8 = 0;
    pub const DI: u8 = 1;
    pub const EI: u8 = 2;
    pub const STOP: u8 = 3;
}

use alu::*;
use alu16::*;
use alu2::*;
use cb_rot::*;
use control::*;
use ld16::*;
use misc::*;
use reg::*;
use reg16::*;
use trap::*;

trait TruncFrom<N> {
    fn trunc_from(n: N) -> Self;
}
impl TruncFrom<usize> for u8 {
    fn trunc_from(n: usize) -> Self {
        n as u8
    }
}
impl TruncFrom<usize> for u16 {
    fn trunc_from(n: usize) -> Self {
        n as u16
    }
}

fn add_sub_with_carry<N>(
    is_add: bool,
    a: N,
    b: N,
    carryin: u8,
    carry_mask: usize,
    halfcarry_mask: usize,
) -> (N, u8, u8)
where
    N: Into<usize> + TruncFrom<usize>,
{
    let (a, b): (usize, usize) = (a.into(), b.into());
    let res = if is_add {
        a.wrapping_add(b).wrapping_add(carryin as usize)
    } else {
        a.wrapping_sub(b).wrapping_sub(carryin as usize)
    };
    let carryout = (((a ^ b ^ res) & carry_mask) != 0) as u8;
    let halfcarryout = (((a ^ b ^ res) & halfcarry_mask) != 0) as u8;
    (
        <N as TruncFrom<usize>>::trunc_from(res),
        carryout,
        halfcarryout,
    )
}

fn add_with_carry<N>(
    a: N,
    b: N,
    carryin: u8,
    carry_mask: usize,
    halfcarry_mask: usize,
) -> (N, u8, u8)
where
    N: Into<usize> + TruncFrom<usize>,
{
    add_sub_with_carry(true, a, b, carryin, carry_mask, halfcarry_mask)
}

fn sub_with_carry<N>(
    a: N,
    b: N,
    carryin: u8,
    carry_mask: usize,
    halfcarry_mask: usize,
) -> (N, u8, u8)
where
    N: Into<usize> + TruncFrom<usize>,
{
    add_sub_with_carry(false, a, b, carryin, carry_mask, halfcarry_mask)
}

fn check_zero_flag(cpu: &mut CPU, val: u8) {
    cpu.regs.set_zero_flag((val == 0) as u8)
}

fn readsrc(cpu: &mut CPU, reg: u8) -> u8 {
    match reg {
        reg::A => cpu.regs.af[1],
        reg::B => cpu.regs.bc[1],
        reg::C => cpu.regs.bc[0],
        reg::D => cpu.regs.de[1],
        reg::E => cpu.regs.de[0],
        reg::H => cpu.regs.hl[1],
        reg::L => cpu.regs.hl[0],
        reg::MEM_BC => cpu.mmu.load8(cpu.regs.bc.as_word()),
        reg::MEM_DE => cpu.mmu.load8(cpu.regs.de.as_word()),
        reg::MEM_HL => cpu.mmu.load8(cpu.regs.hl.as_word()),
        reg::MEM_HLI => {
            let val = cpu.mmu.load8(cpu.regs.hl.as_word());
            cpu.regs.hl.inc();
            val
        }
        reg::MEM_HLD => {
            let val = cpu.mmu.load8(cpu.regs.hl.as_word());
            cpu.regs.hl.dec();
            val
        }
        reg::MEMH_C => {
            let address = 0xFF00 + (cpu.regs.bc[0] as u16);
            cpu.mmu.load8(address)
        }
        reg::A8 => {
            let imm = cpu.mmu.load8(cpu.regs.pc + 1);
            let address = 0xFF00 + (imm as u16);
            cpu.mmu.load8(address)
        }
        reg::MEM_IMM16 => {
            let address = cpu.mmu.load16(cpu.regs.pc + 1);
            cpu.mmu.load8(address)
        }
        reg::D8 => cpu.mmu.load8(cpu.regs.pc + 1),
        _ => unreachable!("{reg}"),
    }
}

fn readsrc16(cpu: &CPU, reg16: u8) -> u16 {
    match reg16 {
        reg16::AF => cpu.regs.af.as_word(),
        reg16::BC => cpu.regs.bc.as_word(),
        reg16::DE => cpu.regs.de.as_word(),
        reg16::HL => cpu.regs.hl.as_word(),
        reg16::SP => cpu.regs.sp,
        reg16::D16 => cpu.mmu.load16(cpu.regs.pc + 1),
        _ => unreachable!(),
    }
}

fn writeback(cpu: &mut CPU, reg: u8, val: u8) {
    match reg {
        reg::A => cpu.regs.af[1] = val,
        reg::B => cpu.regs.bc[1] = val,
        reg::C => cpu.regs.bc[0] = val,
        reg::D => cpu.regs.de[1] = val,
        reg::E => cpu.regs.de[0] = val,
        reg::H => cpu.regs.hl[1] = val,
        reg::L => cpu.regs.hl[0] = val,
        reg::MEM_BC => cpu.mmu.store8(cpu.regs.bc.as_word(), val),
        reg::MEM_DE => cpu.mmu.store8(cpu.regs.de.as_word(), val),
        reg::MEM_HL => cpu.mmu.store8(cpu.regs.hl.as_word(), val),
        reg::MEM_HLI => {
            cpu.mmu.store8(cpu.regs.hl.as_word(), val);
            cpu.regs.hl.inc();
        }
        reg::MEM_HLD => {
            cpu.mmu.store8(cpu.regs.hl.as_word(), val);
            cpu.regs.hl.dec();
        }
        reg::A8 => {
            let imm = cpu.mmu.load8(cpu.regs.pc + 1);
            let address = 0xFF00 + (imm as u16);
            cpu.mmu.store8(address, val);
        }
        reg::MEM_IMM16 => {
            let address = cpu.mmu.load16(cpu.regs.pc + 1);
            cpu.mmu.store8(address, val);
        }
        reg::MEMH_C => {
            let address = 0xFF00 + (cpu.regs.bc[0] as u16);
            cpu.mmu.store8(address, val);
        }
        _ => unreachable!(),
    };
}

fn writeback16(cpu: &mut CPU, reg16: u8, val: u16) {
    match reg16 {
        reg16::AF => cpu.regs.af.set_word(val & 0xFFF0),
        reg16::BC => cpu.regs.bc.set_word(val),
        reg16::DE => cpu.regs.de.set_word(val),
        reg16::HL => cpu.regs.hl.set_word(val),
        reg16::SP => cpu.regs.sp = val,
        _ => unreachable!(),
    }
}

macro_rules! op {
    ($repr:literal, $num_bytes:literal, $clocks:literal, $handler:ident, $arg_1:expr, $arg_2:expr) => {
        Op {
            num_bytes: $num_bytes,
            clocks: $clocks,
            handler: $handler,
            args: [$arg_1, $arg_2],
            repr: $repr,
        }
    };
    ($repr:literal, $num_bytes:literal, $clocks:literal, $handler:ident, $arg:expr) => {
        op!($repr, $num_bytes, $clocks, $handler, $arg, 0)
    };
    ($repr:literal, $num_bytes:literal, $clocks:literal, $handler:ident) => {
        op!($repr, $num_bytes, $clocks, $handler, 0, 0)
    };
    ($todo_text:literal) => {
        op!($todo_text, 1, 1, trap, TODO, 0)
    };
}

const OPS: [Op; 0x100] = [
    /* 0x00 */ op!("NOP", 1, 4, nop),
    /* 0x01 */ op!("LD BC, d16", 3, 12, load16, LOADI, BC),
    /* 0x02 */ op!("LD (BC), A", 1, 8, load, MEM_BC, A),
    /* 0x03 */ op!("INC BC", 1, 8, alu16, INC16, BC),
    /* 0x04 */ op!("INC B", 1, 4, alu2, INC, B),
    /* 0x05 */ op!("DEC B", 1, 4, alu2, DEC, B),
    /* 0x06 */ op!("LD B, d8", 2, 8, load, B, D8),
    /* 0x07 */ op!("RLCA", 1, 4, rot, RLCA),
    /* 0x08 */ op!("LD (a16), SP", 3, 20, load16, SAVESP),
    /* 0x09 */ op!("ADD HL, BC", 1, 8, alu16, ADDHL, BC),
    /* 0x0A */ op!("LD A, (BC)", 1, 8, load, A, MEM_BC),
    /* 0x0B */ op!("DEC BC", 1, 8, alu16, DEC16, BC),
    /* 0x0C */ op!("INC C", 1, 4, alu2, INC, C),
    /* 0x0D */ op!("DEC C", 1, 4, alu2, DEC, C),
    /* 0x0E */ op!("LD C, d8", 2, 8, load, C, D8),
    /* 0x0F */ op!("RRCA", 1, 4, rot, RRCA),
    /* 0x10 */ op!("STOP d8", 2, 4, misc, STOP),
    /* 0x11 */ op!("LD DE, d16", 3, 12, load16, LOADI, DE),
    /* 0x12 */ op!("LD (DE), A", 1, 8, load, MEM_DE, A),
    /* 0x13 */ op!("INC DE", 1, 8, alu16, INC16, DE),
    /* 0x14 */ op!("INC D", 1, 4, alu2, INC, D),
    /* 0x15 */ op!("DEC D", 1, 4, alu2, DEC, D),
    /* 0x16 */ op!("LD D, d8", 2, 8, load, D, D8),
    /* 0x17 */ op!("RLA", 1, 4, rot, RLA),
    /* 0x18 */ op!("JR r8", 2, 8, cfu, JR, UNCOND),
    /* 0x19 */ op!("ADD HL, DE", 1, 8, alu16, ADDHL, DE),
    /* 0x1A */ op!("LD A, (DE)", 1, 8, load, A, MEM_DE),
    /* 0x1B */ op!("DEC DE", 1, 8, alu16, DEC16, DE),
    /* 0x1C */ op!("INC E", 1, 4, alu2, INC, E),
    /* 0x1D */ op!("DEC E", 1, 4, alu2, DEC, E),
    /* 0x1E */ op!("LD E, d8", 2, 8, load, E, D8),
    /* 0x1F */ op!("RRA", 1, 4, rot, RRA),
    /* 0x20 */ op!("JR NZ, r8", 2, 8, cfu, JR, CONDNZ),
    /* 0x21 */ op!("LD HL, d16", 3, 12, load16, LOADI, HL),
    /* 0x22 */ op!("LD (HL+), A", 1, 8, load, MEM_HLI, A),
    /* 0x23 */ op!("INC HL", 1, 8, alu16, INC16, HL),
    /* 0x24 */ op!("INC H", 1, 4, alu2, INC, H),
    /* 0x25 */ op!("DEC H", 1, 4, alu2, DEC, H),
    /* 0x26 */ op!("LD H, d8", 2, 8, load, H, D8),
    /* 0x27 */ op!("DAA", 1, 4, alu2, DAA),
    /* 0x28 */ op!("JR Z, r8", 2, 8, cfu, JR, CONDZ),
    /* 0x29 */ op!("ADD HL, HL", 1, 8, alu16, ADDHL, HL),
    /* 0x2A */ op!("LD A, (HL+)", 1, 8, load, A, MEM_HLI),
    /* 0x2B */ op!("DEC HL", 1, 8, alu16, DEC16, HL),
    /* 0x2C */ op!("INC L", 1, 4, alu2, INC, L),
    /* 0x2D */ op!("DEC L", 1, 4, alu2, DEC, L),
    /* 0x2E */ op!("LD L, d8", 2, 8, load, L, D8),
    /* 0x2F */ op!("CPL", 1, 4, alu2, CPL),
    /* 0x30 */ op!("JR NC, r8", 2, 8, cfu, JR, CONDNC),
    /* 0x31 */ op!("LD SP, d16", 3, 12, load16, LOADI, SP),
    /* 0x32 */ op!("LD (HL-), A", 1, 8, load, MEM_HLD, A),
    /* 0x33 */ op!("INC SP", 1, 8, alu16, INC16, SP),
    /* 0x34 */ op!("INC (HL)", 1, 12, alu2, INC, MEM_HL),
    /* 0x35 */ op!("DEC (HL)", 1, 12, alu2, DEC, MEM_HL),
    /* 0x36 */ op!("LD (HL),d8", 2, 12, load, MEM_HL, D8),
    /* 0x37 */ op!("SCF", 1, 4, alu2, SCF),
    /* 0x38 */ op!("JR C, r8", 2, 8, cfu, JR, CONDC),
    /* 0x39 */ op!("ADD HL, SP", 1, 8, alu16, ADDHL, SP),
    /* 0x3A */ op!("LD A, (HL-)", 1, 8, load, A, MEM_HLD),
    /* 0x3B */ op!("DEC SP", 1, 8, alu16, DEC16, SP),
    /* 0x3C */ op!("INC A", 1, 4, alu2, INC, A),
    /* 0x3D */ op!("DEC A", 1, 4, alu2, DEC, A),
    /* 0x3E */ op!("LD A, d8", 2, 8, load, A, D8),
    /* 0x3F */ op!("CCF", 1, 4, alu2, CCF),
    /* 0x40 */ op!("LD B, B", 1, 4, nop),
    /* 0x41 */ op!("LD B, C", 1, 4, load, B, C),
    /* 0x42 */ op!("LD B, D", 1, 4, load, B, D),
    /* 0x43 */ op!("LD B, E", 1, 4, load, B, E),
    /* 0x44 */ op!("LD B, H", 1, 4, load, B, H),
    /* 0x45 */ op!("LD B, L", 1, 4, load, B, L),
    /* 0x46 */ op!("LD B, (HL)", 1, 8, load, B, MEM_HL),
    /* 0x47 */ op!("LD B, A", 1, 4, load, B, A),
    /* 0x48 */ op!("LD C, B", 1, 4, load, C, B),
    /* 0x49 */ op!("LD C, C", 1, 4, nop),
    /* 0x4A */ op!("LD C, D", 1, 4, load, C, D),
    /* 0x4B */ op!("LD C, E", 1, 4, load, C, E),
    /* 0x4C */ op!("LD C, H", 1, 4, load, C, H),
    /* 0x4D */ op!("LD C, L", 1, 4, load, C, L),
    /* 0x4E */ op!("LD C, (HL)", 1, 8, load, C, MEM_HL),
    /* 0x4F */ op!("LD C, A", 1, 4, load, C, A),
    /* 0x50 */ op!("LD D, B", 1, 4, load, D, B),
    /* 0x51 */ op!("LD D, C", 1, 4, load, D, C),
    /* 0x52 */ op!("LD D, D", 1, 4, nop),
    /* 0x53 */ op!("LD D, E", 1, 4, load, D, E),
    /* 0x54 */ op!("LD D, H", 1, 4, load, D, H),
    /* 0x55 */ op!("LD D, L", 1, 4, load, D, L),
    /* 0x56 */ op!("LD D, (HL)", 1, 8, load, D, MEM_HL),
    /* 0x57 */ op!("LD D, A", 1, 4, load, D, A),
    /* 0x58 */ op!("LD E, B", 1, 4, load, E, B),
    /* 0x59 */ op!("LD E, C", 1, 4, load, E, C),
    /* 0x5A */ op!("LD E, D", 1, 4, load, E, D),
    /* 0x5B */ op!("LD E, E", 1, 4, nop),
    /* 0x5C */ op!("LD E, F", 1, 4, load, E, H),
    /* 0x5D */ op!("LD E, G", 1, 4, load, E, L),
    /* 0x5E */ op!("LD E, (HL)", 1, 8, load, E, MEM_HL),
    /* 0x5F */ op!("LD E, A", 1, 4, load, E, A),
    /* 0x60 */ op!("LD H, B", 1, 4, load, H, B),
    /* 0x61 */ op!("LD H, C", 1, 4, load, H, C),
    /* 0x62 */ op!("LD H, D", 1, 4, load, H, D),
    /* 0x63 */ op!("LD H, E", 1, 4, load, H, E),
    /* 0x64 */ op!("LD H, H", 1, 4, nop),
    /* 0x65 */ op!("LD H, L", 1, 4, load, H, L),
    /* 0x66 */ op!("LD H, (HL)", 1, 8, load, H, MEM_HL),
    /* 0x67 */ op!("LD H, A", 1, 4, load, H, A),
    /* 0x68 */ op!("LD L, B", 1, 4, load, L, B),
    /* 0x69 */ op!("LD L, C", 1, 4, load, L, C),
    /* 0x6A */ op!("LD L, D", 1, 4, load, L, D),
    /* 0x6B */ op!("LD L, E", 1, 4, load, L, E),
    /* 0x6C */ op!("LD L, H", 1, 4, load, L, H),
    /* 0x6D */ op!("LD L, L", 1, 4, nop),
    /* 0x6E */ op!("LD L, (HL)", 1, 8, load, L, MEM_HL),
    /* 0x6F */ op!("LD L, A", 1, 4, load, L, A),
    /* 0x70 */ op!("LD (HL), B", 1, 8, load, MEM_HL, B),
    /* 0x71 */ op!("LD (HL), C", 1, 8, load, MEM_HL, C),
    /* 0x72 */ op!("LD (HL), D", 1, 8, load, MEM_HL, D),
    /* 0x73 */ op!("LD (HL), E", 1, 8, load, MEM_HL, E),
    /* 0x74 */ op!("LD (HL), H", 1, 8, load, MEM_HL, H),
    /* 0x75 */ op!("LD (HL), L", 1, 8, load, MEM_HL, L),
    /* 0x76 */ op!("HALT", 1, 4, misc, HALT),
    /* 0x77 */ op!("LD (HL), A", 1, 8, load, MEM_HL, A),
    /* 0x78 */ op!("LD A, B", 1, 4, load, A, B),
    /* 0x79 */ op!("LD A, C", 1, 4, load, A, C),
    /* 0x7A */ op!("LD A, D", 1, 4, load, A, D),
    /* 0x7B */ op!("LD A, E", 1, 4, load, A, E),
    /* 0x7C */ op!("LD A, H", 1, 4, load, A, H),
    /* 0x7D */ op!("LD A, L", 1, 4, load, A, L),
    /* 0x7E */ op!("LD A, (HL)", 1, 8, load, A, MEM_HL),
    /* 0x7F */ op!("LD A, A", 1, 4, nop),
    /* 0x80 */ op!("ADD A, B", 1, 4, alu, ADD, B),
    /* 0x81 */ op!("ADD A, C", 1, 4, alu, ADD, C),
    /* 0x82 */ op!("ADD A, D", 1, 4, alu, ADD, D),
    /* 0x83 */ op!("ADD A, E", 1, 4, alu, ADD, E),
    /* 0x84 */ op!("ADD A, H", 1, 4, alu, ADD, H),
    /* 0x85 */ op!("ADD A, L", 1, 4, alu, ADD, L),
    /* 0x86 */ op!("ADD A, (HL)", 1, 8, alu, ADD, MEM_HL),
    /* 0x87 */ op!("ADD A, A", 1, 4, alu, ADD, A),
    /* 0x88 */ op!("ADC A, B", 1, 4, alu, ADC, B),
    /* 0x89 */ op!("ADC A, C", 1, 4, alu, ADC, C),
    /* 0x8A */ op!("ADC A, D", 1, 4, alu, ADC, D),
    /* 0x8B */ op!("ADC A, E", 1, 4, alu, ADC, E),
    /* 0x8C */ op!("ADC A, H", 1, 4, alu, ADC, H),
    /* 0x8D */ op!("ADC A, L", 1, 4, alu, ADC, L),
    /* 0x8E */ op!("ADC A, (HL)", 1, 8, alu, ADC, MEM_HL),
    /* 0x8F */ op!("ADC A, A", 1, 4, alu, ADC, A),
    /* 0x90 */ op!("SUB B", 1, 4, alu, SUB, B),
    /* 0x91 */ op!("SUB C", 1, 4, alu, SUB, C),
    /* 0x92 */ op!("SUB D", 1, 4, alu, SUB, D),
    /* 0x93 */ op!("SUB E", 1, 4, alu, SUB, E),
    /* 0x94 */ op!("SUB H", 1, 4, alu, SUB, H),
    /* 0x95 */ op!("SUB L", 1, 4, alu, SUB, L),
    /* 0x96 */ op!("SUB (HL)", 1, 8, alu, SUB, MEM_HL),
    /* 0x97 */ op!("SUB A", 1, 4, alu, SUB, A),
    /* 0x98 */ op!("SBC B", 1, 4, alu, SBC, B),
    /* 0x99 */ op!("SBC C", 1, 4, alu, SBC, C),
    /* 0x9A */ op!("SBC D", 1, 4, alu, SBC, D),
    /* 0x9B */ op!("SBC E", 1, 4, alu, SBC, E),
    /* 0x9C */ op!("SBC H", 1, 4, alu, SBC, H),
    /* 0x9D */ op!("SBC L", 1, 4, alu, SBC, L),
    /* 0x9E */ op!("SBC (HL)", 1, 8, alu, SBC, MEM_HL),
    /* 0x9F */ op!("SUB A", 1, 4, alu, SBC, A),
    /* 0xA0 */ op!("AND B", 1, 4, alu, AND, B),
    /* 0xA1 */ op!("AND C", 1, 4, alu, AND, C),
    /* 0xA2 */ op!("AND D", 1, 4, alu, AND, D),
    /* 0xA3 */ op!("AND E", 1, 4, alu, AND, E),
    /* 0xA4 */ op!("AND H", 1, 4, alu, AND, H),
    /* 0xA5 */ op!("AND L", 1, 4, alu, AND, L),
    /* 0xA6 */ op!("AND (HL)", 1, 8, alu, AND, MEM_HL),
    /* 0xA7 */ op!("AND A", 1, 4, alu, AND, A),
    /* 0xA8 */ op!("XOR B", 1, 4, alu, XOR, B),
    /* 0xA9 */ op!("XOR C", 1, 4, alu, XOR, C),
    /* 0xAA */ op!("XOR D", 1, 4, alu, XOR, D),
    /* 0xAB */ op!("XOR E", 1, 4, alu, XOR, E),
    /* 0xAC */ op!("XOR H", 1, 4, alu, XOR, H),
    /* 0xAD */ op!("XOR L", 1, 4, alu, XOR, L),
    /* 0xAE */ op!("XOR (HL)", 1, 8, alu, XOR, MEM_HL),
    /* 0xAF */ op!("XOR A", 1, 4, alu, XOR, A),
    /* 0xB0 */ op!("OR B", 1, 4, alu, OR, B),
    /* 0xB1 */ op!("OR C", 1, 4, alu, OR, C),
    /* 0xB2 */ op!("OR D", 1, 4, alu, OR, D),
    /* 0xB3 */ op!("OR E", 1, 4, alu, OR, E),
    /* 0xB4 */ op!("OR H", 1, 4, alu, OR, H),
    /* 0xB5 */ op!("OR L", 1, 4, alu, OR, L),
    /* 0xB6 */ op!("OR (HL)", 1, 8, alu, OR, MEM_HL),
    /* 0xB7 */ op!("OR A", 1, 4, alu, OR, A),
    /* 0xB8 */ op!("CP B", 1, 4, alu, CP, B),
    /* 0xB9 */ op!("CP C", 1, 4, alu, CP, C),
    /* 0xBA */ op!("CP D", 1, 4, alu, CP, D),
    /* 0xBB */ op!("CP E", 1, 4, alu, CP, E),
    /* 0xBC */ op!("CP H", 1, 4, alu, CP, H),
    /* 0xBD */ op!("CP L", 1, 4, alu, CP, L),
    /* 0xBE */ op!("CP (HL)", 1, 8, alu, CP, MEM_HL),
    /* 0xBF */ op!("CP A", 1, 4, alu, CP, A),
    /* 0xC0 */ op!("RET NZ", 1, 8, cfu, RET, CONDNZ),
    /* 0xC1 */ op!("POP BC", 1, 12, load16, POP, BC),
    /* 0xC2 */ op!("JP NZ, a16", 3, 12, cfu, JP, CONDNZ),
    /* 0xC3 */ op!("JP a16", 3, 12, cfu, JP, UNCOND),
    /* 0xC4 */ op!("CALL NZ, a16", 3, 12, cfu, CALL, CONDNZ),
    /* 0xC5 */ op!("PUSH BC", 1, 16, load16, PUSH, BC),
    /* 0xC6 */ op!("ADD A, d8", 2, 8, alu, ADD, D8),
    /* 0xC7 */ op!("RST 00H", 1, 16, cfu, RST, 0x00),
    /* 0xC8 */ op!("RET Z", 1, 8, cfu, RET, CONDZ),
    /* 0xC9 */ op!("RET", 1, 4, cfu, RET, UNCOND),
    /* 0xCA */ op!("JP Z, a16", 3, 12, cfu, JP, CONDZ),
    /* 0xCB */ op!("PREFIX CB", 0, 0, trap, UNREACHABLE),
    /* 0xCC */ op!("CALL Z, a16", 3, 12, cfu, CALL, CONDZ),
    /* 0xCD */ op!("CALL a16", 3, 12, cfu, CALL, UNCOND),
    /* 0xCE */ op!("ADC A, d8,", 2, 8, alu, ADC, D8),
    /* 0xCF */ op!("RST 08H", 1, 16, cfu, RST, 0x08),
    /* 0xD0 */ op!("RET NC", 1, 8, cfu, RET, CONDNC),
    /* 0xD1 */ op!("POP DE", 1, 12, load16, POP, DE),
    /* 0xD2 */ op!("JP NC, a16", 3, 12, cfu, JP, CONDNC),
    /* 0xD3 */ op!("0xD3", 1, 0, trap, NOT_EXISTS, 0),
    /* 0xD4 */ op!("CALL NC, a16", 3, 12, cfu, CALL, CONDNC),
    /* 0xD5 */ op!("PUSH DE", 1, 16, load16, PUSH, DE),
    /* 0xD6 */ op!("SUB A, d8", 2, 8, alu, SUB, D8),
    /* 0xD7 */ op!("RST 10H", 1, 16, cfu, RST, 0x10),
    /* 0xD8 */ op!("RET C", 1, 8, cfu, RET, CONDC),
    /* 0xD9 */ op!("RETI", 1, 4, cfu, RETI),
    /* 0xDA */ op!("JP C, a16", 3, 12, cfu, JP, CONDC),
    /* 0xDB */ op!("0xDB", 1, 0, trap, NOT_EXISTS),
    /* 0xDC */ op!("CALL C, a16", 3, 12, cfu, CALL, CONDC),
    /* 0xDD */ op!("0xDD", 1, 0, trap, NOT_EXISTS),
    /* 0xDE */ op!("SBC A, d8", 2, 8, alu, SBC, D8),
    /* 0xDF */ op!("RST 18H", 1, 16, cfu, RST, 0x18),
    /* 0xE0 */ op!("LDH (a8), A", 2, 12, load, A8, A),
    /* 0xE1 */ op!("POP HL", 1, 12, load16, POP, HL),
    /* 0xE2 */ op!("LD (C),A", 1, 8, load, MEMH_C, A),
    /* 0xE3 */ op!("0xE3", 1, 0, trap, NOT_EXISTS),
    /* 0xE4 */ op!("0xE4", 1, 0, trap, NOT_EXISTS),
    /* 0xE5 */ op!("PUSH HL", 1, 16, load16, PUSH, HL),
    /* 0xE6 */ op!("AND d8", 2, 8, alu, AND, D8),
    /* 0xE7 */ op!("RST 20H", 1, 16, cfu, RST, 0x20),
    /* 0xE8 */ op!("ADD SP, r8", 2, 16, alu16, ADDSP),
    /* 0xE9 */ op!("JP HL", 1, 4, cfu, JP_HL),
    /* 0xEA */ op!("LD (a16), A", 3, 16, load, MEM_IMM16, A),
    /* 0xEB */ op!("0xEB", 1, 0, trap, NOT_EXISTS),
    /* 0xEC */ op!("0xEC", 1, 0, trap, NOT_EXISTS),
    /* 0xED */ op!("0xED", 1, 0, trap, NOT_EXISTS),
    /* 0xEE */ op!("XOR d8,", 2, 8, alu, XOR, D8),
    /* 0xEF */ op!("RST 28H", 1, 16, cfu, RST, 0x28),
    /* 0xF0 */ op!("LDH A, (a8)", 2, 12, load, A, A8),
    /* 0xF1 */ op!("POP AF", 1, 12, load16, POP, AF),
    /* 0xF2 */ op!("LD A, (C)", 1, 8, load, A, MEMH_C),
    /* 0xF3 */ op!("DI", 1, 4, misc, DI),
    /* 0xF4 */ op!("0xF4", 1, 0, trap, NOT_EXISTS),
    /* 0xF5 */ op!("PUSH AF", 1, 16, load16, PUSH, AF),
    /* 0xF6 */ op!("OR d8", 2, 8, alu, OR, D8),
    /* 0xF7 */ op!("RST 30H", 1, 16, cfu, RST, 0x30),
    /* 0xF8 */ op!("LD HL, SP + r8", 2, 12, load16, LDHLSPr8),
    /* 0xF9 */ op!("LD SP, HL", 1, 8, load16, LDSPHL),
    /* 0xFA */ op!("LD A, (a16)", 3, 16, load, A, MEM_IMM16),
    /* 0xFB */ op!("EI", 1, 4, misc, EI),
    /* 0xFC */ op!("0xFC", 1, 0, trap, NOT_EXISTS),
    /* 0xFD */ op!("0xFD", 1, 0, trap, NOT_EXISTS),
    /* 0xFE */ op!("CP d8", 2, 8, alu, CP, D8),
    /* 0xFF */ op!("RST 38H", 1, 16, cfu, RST, 0x38),
];
const PREFIXED_OPS: [Op; 0x100] = [
    /* 0x00 */ op!("RLC B", 2, 8, rot, RLC, B),
    /* 0x01 */ op!("RLC C", 2, 8, rot, RLC, C),
    /* 0x02 */ op!("RLC D", 2, 8, rot, RLC, D),
    /* 0x03 */ op!("RLC E", 2, 8, rot, RLC, E),
    /* 0x04 */ op!("RLC H", 2, 8, rot, RLC, H),
    /* 0x05 */ op!("RLC L", 2, 8, rot, RLC, L),
    /* 0x06 */ op!("RLC (HL)", 2, 16, rot, RLC, MEM_HL),
    /* 0x07 */ op!("RLC A", 2, 8, rot, RLC, A),
    /* 0x08 */ op!("RRC B", 2, 8, rot, RRC, B),
    /* 0x09 */ op!("RRC C", 2, 8, rot, RRC, C),
    /* 0x0A */ op!("RRC D", 2, 8, rot, RRC, D),
    /* 0x0B */ op!("RRC E", 2, 8, rot, RRC, E),
    /* 0x0C */ op!("RRC H", 2, 8, rot, RRC, H),
    /* 0x0D */ op!("RRC L", 2, 8, rot, RRC, L),
    /* 0x0E */ op!("RRC (HL)", 2, 16, rot, RRC, MEM_HL),
    /* 0x0F */ op!("RRC A", 2, 8, rot, RRC, A),
    /* 0x10 */ op!("RL B", 2, 8, rot, RL, B),
    /* 0x11 */ op!("RL C", 2, 8, rot, RL, C),
    /* 0x12 */ op!("RL D", 2, 8, rot, RL, D),
    /* 0x13 */ op!("RL E", 2, 8, rot, RL, E),
    /* 0x14 */ op!("RL H", 2, 8, rot, RL, H),
    /* 0x15 */ op!("RL L", 2, 8, rot, RL, L),
    /* 0x16 */ op!("RL (HL)", 2, 16, rot, RL, MEM_HL),
    /* 0x17 */ op!("RL A", 2, 8, rot, RL, A),
    /* 0x18 */ op!("RR B", 2, 8, rot, RR, B),
    /* 0x19 */ op!("RR C", 2, 8, rot, RR, C),
    /* 0x1A */ op!("RR D", 2, 8, rot, RR, D),
    /* 0x1B */ op!("RR E", 2, 8, rot, RR, E),
    /* 0x1C */ op!("RR H", 2, 8, rot, RR, H),
    /* 0x1D */ op!("RR L", 2, 8, rot, RR, L),
    /* 0x1E */ op!("RR (HL)", 2, 16, rot, RR, MEM_HL),
    /* 0x1F */ op!("RR A", 2, 8, rot, RR, A),
    /* 0x20 */ op!("SLA B", 2, 8, rot, SLA, B),
    /* 0x21 */ op!("SLA C", 2, 8, rot, SLA, C),
    /* 0x22 */ op!("SLA D", 2, 8, rot, SLA, D),
    /* 0x23 */ op!("SLA E", 2, 8, rot, SLA, E),
    /* 0x24 */ op!("SLA H", 2, 8, rot, SLA, H),
    /* 0x25 */ op!("SLA L", 2, 8, rot, SLA, L),
    /* 0x26 */ op!("SLA (HL)", 2, 16, rot, SLA, MEM_HL),
    /* 0x27 */ op!("SLA A", 2, 8, rot, SLA, A),
    /* 0x28 */ op!("SRA B", 2, 8, rot, SRA, B),
    /* 0x29 */ op!("SRA C", 2, 8, rot, SRA, C),
    /* 0x2A */ op!("SRA D", 2, 8, rot, SRA, D),
    /* 0x2B */ op!("SRA E", 2, 8, rot, SRA, E),
    /* 0x2C */ op!("SRA H", 2, 8, rot, SRA, H),
    /* 0x2D */ op!("SRA L", 2, 8, rot, SRA, L),
    /* 0x2E */ op!("SRA (HL)", 2, 16, rot, SRA, MEM_HL),
    /* 0x2F */ op!("SRA A", 2, 8, rot, SRA, A),
    /* 0x30 */ op!("SWAP B", 2, 8, rot, SWAP, B),
    /* 0x31 */ op!("SWAP C", 2, 8, rot, SWAP, C),
    /* 0x32 */ op!("SWAP D", 2, 8, rot, SWAP, D),
    /* 0x33 */ op!("SWAP E", 2, 8, rot, SWAP, E),
    /* 0x34 */ op!("SWAP H", 2, 8, rot, SWAP, H),
    /* 0x35 */ op!("SWAP L", 2, 8, rot, SWAP, L),
    /* 0x36 */ op!("SWAP (HL)", 2, 16, rot, SWAP, MEM_HL),
    /* 0x37 */ op!("SWAP A", 2, 8, rot, SWAP, A),
    /* 0x38 */ op!("SRL B", 2, 8, rot, SRL, B),
    /* 0x39 */ op!("SRL C", 2, 8, rot, SRL, C),
    /* 0x3A */ op!("SRL D", 2, 8, rot, SRL, D),
    /* 0x3B */ op!("SRL E", 2, 8, rot, SRL, E),
    /* 0x3C */ op!("SRL H", 2, 8, rot, SRL, H),
    /* 0x3D */ op!("SRL L", 2, 8, rot, SRL, L),
    /* 0x3E */ op!("SRL (HL)", 2, 16, rot, SRL, MEM_HL),
    /* 0x3F */ op!("SRL A", 2, 8, rot, SRL, A),
    /* 0x40 */ op!("BIT 0, B", 2, 8, bit, 0, B),
    /* 0x41 */ op!("BIT 0, C", 2, 8, bit, 0, C),
    /* 0x42 */ op!("BIT 0, D", 2, 8, bit, 0, D),
    /* 0x43 */ op!("BIT 0, E", 2, 8, bit, 0, E),
    /* 0x44 */ op!("BIT 0, H", 2, 8, bit, 0, H),
    /* 0x45 */ op!("BIT 0, L", 2, 8, bit, 0, L),
    /* 0x46 */ op!("BIT 0, (HL)", 2, 12, bit, 0, MEM_HL),
    /* 0x47 */ op!("BIT 0, A", 2, 8, bit, 0, A),
    /* 0x48 */ op!("BIT 1, B", 2, 8, bit, 1, B),
    /* 0x49 */ op!("BIT 1, C", 2, 8, bit, 1, C),
    /* 0x4A */ op!("BIT 1, D", 2, 8, bit, 1, D),
    /* 0x4B */ op!("BIT 1, E", 2, 8, bit, 1, E),
    /* 0x4C */ op!("BIT 1, H", 2, 8, bit, 1, H),
    /* 0x4D */ op!("BIT 1, L", 2, 8, bit, 1, L),
    /* 0x4E */ op!("BIT 1, (HL)", 2, 12, bit, 1, MEM_HL),
    /* 0x4F */ op!("BIT 1, A", 2, 8, bit, 1, A),
    /* 0x50 */ op!("BIT 2, B", 2, 8, bit, 2, B),
    /* 0x51 */ op!("BIT 2, C", 2, 8, bit, 2, C),
    /* 0x52 */ op!("BIT 2, D", 2, 8, bit, 2, D),
    /* 0x53 */ op!("BIT 2, E", 2, 8, bit, 2, E),
    /* 0x54 */ op!("BIT 2, H", 2, 8, bit, 2, H),
    /* 0x55 */ op!("BIT 2, L", 2, 8, bit, 2, L),
    /* 0x56 */ op!("BIT 2, (HL)", 2, 12, bit, 2, MEM_HL),
    /* 0x57 */ op!("BIT 2, A", 2, 8, bit, 2, A),
    /* 0x58 */ op!("BIT 3, B", 2, 8, bit, 3, B),
    /* 0x59 */ op!("BIT 3, C", 2, 8, bit, 3, C),
    /* 0x5A */ op!("BIT 3, D", 2, 8, bit, 3, D),
    /* 0x5B */ op!("BIT 3, E", 2, 8, bit, 3, E),
    /* 0x5C */ op!("BIT 3, H", 2, 8, bit, 3, H),
    /* 0x5D */ op!("BIT 3, L", 2, 8, bit, 3, L),
    /* 0x5E */ op!("BIT 3, (HL)", 2, 12, bit, 3, MEM_HL),
    /* 0x5F */ op!("BIT 3, A", 2, 8, bit, 3, A),
    /* 0x60 */ op!("BIT 4, B", 2, 8, bit, 4, B),
    /* 0x61 */ op!("BIT 4, C", 2, 8, bit, 4, C),
    /* 0x62 */ op!("BIT 4, D", 2, 8, bit, 4, D),
    /* 0x63 */ op!("BIT 4, E", 2, 8, bit, 4, E),
    /* 0x64 */ op!("BIT 4, H", 2, 8, bit, 4, H),
    /* 0x65 */ op!("BIT 4, L", 2, 8, bit, 4, L),
    /* 0x66 */ op!("BIT 4, (HL)", 2, 12, bit, 4, MEM_HL),
    /* 0x67 */ op!("BIT 4, A", 2, 8, bit, 4, A),
    /* 0x68 */ op!("BIT 5, B", 2, 8, bit, 5, B),
    /* 0x69 */ op!("BIT 5, C", 2, 8, bit, 5, C),
    /* 0x6A */ op!("BIT 5, D", 2, 8, bit, 5, D),
    /* 0x6B */ op!("BIT 5, E", 2, 8, bit, 5, E),
    /* 0x6C */ op!("BIT 5, H", 2, 8, bit, 5, H),
    /* 0x6D */ op!("BIT 5, L", 2, 8, bit, 5, L),
    /* 0x6E */ op!("BIT 5, (HL)", 2, 12, bit, 5, MEM_HL),
    /* 0x6F */ op!("BIT 5, A", 2, 8, bit, 5, A),
    /* 0x70 */ op!("BIT 6, B", 2, 8, bit, 6, B),
    /* 0x71 */ op!("BIT 6, C", 2, 8, bit, 6, C),
    /* 0x72 */ op!("BIT 6, D", 2, 8, bit, 6, D),
    /* 0x73 */ op!("BIT 6, E", 2, 8, bit, 6, E),
    /* 0x74 */ op!("BIT 6, H", 2, 8, bit, 6, H),
    /* 0x75 */ op!("BIT 6, L", 2, 8, bit, 6, L),
    /* 0x76 */ op!("BIT 6, (HL)", 2, 12, bit, 6, MEM_HL),
    /* 0x77 */ op!("BIT 6, A", 2, 8, bit, 6, A),
    /* 0x78 */ op!("BIT 7, B", 2, 8, bit, 7, B),
    /* 0x79 */ op!("BIT 7, C", 2, 8, bit, 7, C),
    /* 0x7A */ op!("BIT 7, D", 2, 8, bit, 7, D),
    /* 0x7B */ op!("BIT 7, E", 2, 8, bit, 7, E),
    /* 0x7C */ op!("BIT 7, H", 2, 8, bit, 7, H),
    /* 0x7D */ op!("BIT 7, L", 2, 8, bit, 7, L),
    /* 0x7E */ op!("BIT 7, (HL)", 2, 12, bit, 7, MEM_HL),
    /* 0x7F */ op!("BIT 7, A", 2, 8, bit, 7, A),
    /* 0x80 */ op!("RES 0, B", 2, 8, resb, 0, B),
    /* 0x81 */ op!("RES 0, C", 2, 8, resb, 0, C),
    /* 0x82 */ op!("RES 0, D", 2, 8, resb, 0, D),
    /* 0x83 */ op!("RES 0, E", 2, 8, resb, 0, E),
    /* 0x84 */ op!("RES 0, H", 2, 8, resb, 0, H),
    /* 0x85 */ op!("RES 0, L", 2, 8, resb, 0, L),
    /* 0x86 */ op!("RES 0, (HL)", 2, 16, resb, 0, MEM_HL),
    /* 0x87 */ op!("RES 0, A", 2, 8, resb, 0, A),
    /* 0x88 */ op!("RES 1, B", 2, 8, resb, 1, B),
    /* 0x89 */ op!("RES 1, C", 2, 8, resb, 1, C),
    /* 0x8A */ op!("RES 1, D", 2, 8, resb, 1, D),
    /* 0x8B */ op!("RES 1, E", 2, 8, resb, 1, E),
    /* 0x8C */ op!("RES 1, H", 2, 8, resb, 1, H),
    /* 0x8D */ op!("RES 1, L", 2, 8, resb, 1, L),
    /* 0x8E */ op!("RES 1, (HL)", 2, 16, resb, 1, MEM_HL),
    /* 0x8F */ op!("RES 1, A", 2, 8, resb, 1, A),
    /* 0x90 */ op!("RES 2, B", 2, 8, resb, 2, B),
    /* 0x91 */ op!("RES 2, C", 2, 8, resb, 2, C),
    /* 0x92 */ op!("RES 2, D", 2, 8, resb, 2, D),
    /* 0x93 */ op!("RES 2, E", 2, 8, resb, 2, E),
    /* 0x94 */ op!("RES 2, H", 2, 8, resb, 2, H),
    /* 0x95 */ op!("RES 2, L", 2, 8, resb, 2, L),
    /* 0x96 */ op!("RES 2, (HL)", 2, 16, resb, 2, MEM_HL),
    /* 0x97 */ op!("RES 2, A", 2, 8, resb, 2, A),
    /* 0x98 */ op!("RES 3, B", 2, 8, resb, 3, B),
    /* 0x99 */ op!("RES 3, C", 2, 8, resb, 3, C),
    /* 0x9A */ op!("RES 3, D", 2, 8, resb, 3, D),
    /* 0x9B */ op!("RES 3, E", 2, 8, resb, 3, E),
    /* 0x9C */ op!("RES 3, H", 2, 8, resb, 3, H),
    /* 0x9D */ op!("RES 3, L", 2, 8, resb, 3, L),
    /* 0x9E */ op!("RES 3, (HL)", 2, 16, resb, 3, MEM_HL),
    /* 0x9F */ op!("RES 3, A", 2, 8, resb, 3, A),
    /* 0xA0 */ op!("RES 4, B", 2, 8, resb, 4, B),
    /* 0xA1 */ op!("RES 4, C", 2, 8, resb, 4, C),
    /* 0xA2 */ op!("RES 4, D", 2, 8, resb, 4, D),
    /* 0xA3 */ op!("RES 4, E", 2, 8, resb, 4, E),
    /* 0xA4 */ op!("RES 4, H", 2, 8, resb, 4, H),
    /* 0xA5 */ op!("RES 4, L", 2, 8, resb, 4, L),
    /* 0xA6 */ op!("RES 4, (HL)", 2, 16, resb, 4, MEM_HL),
    /* 0xA7 */ op!("RES 4, A", 2, 8, resb, 4, A),
    /* 0xA8 */ op!("RES 5, B", 2, 8, resb, 5, B),
    /* 0xA9 */ op!("RES 5, C", 2, 8, resb, 5, C),
    /* 0xAA */ op!("RES 5, D", 2, 8, resb, 5, D),
    /* 0xAB */ op!("RES 5, E", 2, 8, resb, 5, E),
    /* 0xAC */ op!("RES 5, H", 2, 8, resb, 5, H),
    /* 0xAD */ op!("RES 5, L", 2, 8, resb, 5, L),
    /* 0xAE */ op!("RES 5, (HL)", 2, 16, resb, 5, MEM_HL),
    /* 0xAF */ op!("RES 5, A", 2, 8, resb, 5, A),
    /* 0xB0 */ op!("RES 6, B", 2, 8, resb, 6, B),
    /* 0xB1 */ op!("RES 6, C", 2, 8, resb, 6, C),
    /* 0xB2 */ op!("RES 6, D", 2, 8, resb, 6, D),
    /* 0xB3 */ op!("RES 6, E", 2, 8, resb, 6, E),
    /* 0xB4 */ op!("RES 6, H", 2, 8, resb, 6, H),
    /* 0xB5 */ op!("RES 6, L", 2, 8, resb, 6, L),
    /* 0xB6 */ op!("RES 6, (HL)", 2, 16, resb, 6, MEM_HL),
    /* 0xB7 */ op!("RES 6, A", 2, 8, resb, 6, A),
    /* 0xB8 */ op!("RES 7, B", 2, 8, resb, 7, B),
    /* 0xB9 */ op!("RES 7, C", 2, 8, resb, 7, C),
    /* 0xBA */ op!("RES 7, D", 2, 8, resb, 7, D),
    /* 0xBB */ op!("RES 7, E", 2, 8, resb, 7, E),
    /* 0xBC */ op!("RES 7, H", 2, 8, resb, 7, H),
    /* 0xBD */ op!("RES 7, L", 2, 8, resb, 7, L),
    /* 0xBE */ op!("RES 7, (HL)", 2, 16, resb, 7, MEM_HL),
    /* 0xBF */ op!("RES 7, A", 2, 8, resb, 7, A),
    /* 0xC0 */ op!("SET 0, B", 2, 8, setb, 0, B),
    /* 0xC1 */ op!("SET 0, C", 2, 8, setb, 0, C),
    /* 0xC2 */ op!("SET 0, D", 2, 8, setb, 0, D),
    /* 0xC3 */ op!("SET 0, E", 2, 8, setb, 0, E),
    /* 0xC4 */ op!("SET 0, H", 2, 8, setb, 0, H),
    /* 0xC5 */ op!("SET 0, L", 2, 8, setb, 0, L),
    /* 0xC6 */ op!("SET 0, (HL)", 2, 16, setb, 0, MEM_HL),
    /* 0xC7 */ op!("SET 0, A", 2, 8, setb, 0, A),
    /* 0xC8 */ op!("SET 1, B", 2, 8, setb, 1, B),
    /* 0xC9 */ op!("SET 1, C", 2, 8, setb, 1, C),
    /* 0xCA */ op!("SET 1, D", 2, 8, setb, 1, D),
    /* 0xCB */ op!("SET 1, E", 2, 8, setb, 1, E),
    /* 0xCC */ op!("SET 1, H", 2, 8, setb, 1, H),
    /* 0xCD */ op!("SET 1, L", 2, 8, setb, 1, L),
    /* 0xCE */ op!("SET 1, (HL)", 2, 16, setb, 1, MEM_HL),
    /* 0xCF */ op!("SET 1, A", 2, 8, setb, 1, A),
    /* 0xD0 */ op!("SET 2, B", 2, 8, setb, 2, B),
    /* 0xD1 */ op!("SET 2, C", 2, 8, setb, 2, C),
    /* 0xD2 */ op!("SET 2, D", 2, 8, setb, 2, D),
    /* 0xD3 */ op!("SET 2, E", 2, 8, setb, 2, E),
    /* 0xD4 */ op!("SET 2, H", 2, 8, setb, 2, H),
    /* 0xD5 */ op!("SET 2, L", 2, 8, setb, 2, L),
    /* 0xD6 */ op!("SET 2, (HL)", 2, 16, setb, 2, MEM_HL),
    /* 0xD7 */ op!("SET 2, A", 2, 8, setb, 2, A),
    /* 0xD8 */ op!("SET 3, B", 2, 8, setb, 3, B),
    /* 0xD9 */ op!("SET 3, C", 2, 8, setb, 3, C),
    /* 0xDA */ op!("SET 3, D", 2, 8, setb, 3, D),
    /* 0xDB */ op!("SET 3, E", 2, 8, setb, 3, E),
    /* 0xDC */ op!("SET 3, H", 2, 8, setb, 3, H),
    /* 0xDD */ op!("SET 3, L", 2, 8, setb, 3, L),
    /* 0xDE */ op!("SET 3, (HL)", 2, 16, setb, 3, MEM_HL),
    /* 0xDF */ op!("SET 3, A", 2, 8, setb, 3, A),
    /* 0xE0 */ op!("SET 4, B", 2, 8, setb, 4, B),
    /* 0xE1 */ op!("SET 4, C", 2, 8, setb, 4, C),
    /* 0xE2 */ op!("SET 4, D", 2, 8, setb, 4, D),
    /* 0xE3 */ op!("SET 4, E", 2, 8, setb, 4, E),
    /* 0xE4 */ op!("SET 4, H", 2, 8, setb, 4, H),
    /* 0xE5 */ op!("SET 4, L", 2, 8, setb, 4, L),
    /* 0xE6 */ op!("SET 4, (HL)", 2, 16, setb, 4, MEM_HL),
    /* 0xE7 */ op!("SET 4, A", 2, 8, setb, 4, A),
    /* 0xE8 */ op!("SET 5, B", 2, 8, setb, 5, B),
    /* 0xE9 */ op!("SET 5, C", 2, 8, setb, 5, C),
    /* 0xEA */ op!("SET 5, D", 2, 8, setb, 5, D),
    /* 0xEB */ op!("SET 5, E", 2, 8, setb, 5, E),
    /* 0xEC */ op!("SET 5, H", 2, 8, setb, 5, H),
    /* 0xED */ op!("SET 5, L", 2, 8, setb, 5, L),
    /* 0xEE */ op!("SET 5, (HL)", 2, 16, setb, 5, MEM_HL),
    /* 0xEF */ op!("SET 5, A", 2, 8, setb, 5, A),
    /* 0xF0 */ op!("SET 6, B", 2, 8, setb, 6, B),
    /* 0xF1 */ op!("SET 6, C", 2, 8, setb, 6, C),
    /* 0xF2 */ op!("SET 6, D", 2, 8, setb, 6, D),
    /* 0xF3 */ op!("SET 6, E", 2, 8, setb, 6, E),
    /* 0xF4 */ op!("SET 6, H", 2, 8, setb, 6, H),
    /* 0xF5 */ op!("SET 6, L", 2, 8, setb, 6, L),
    /* 0xF6 */ op!("SET 6, (HL)", 2, 16, setb, 6, MEM_HL),
    /* 0xF7 */ op!("SET 6, A", 2, 8, setb, 6, A),
    /* 0xF8 */ op!("SET 7, B", 2, 8, setb, 7, B),
    /* 0xF9 */ op!("SET 7, C", 2, 8, setb, 7, C),
    /* 0xFA */ op!("SET 7, D", 2, 8, setb, 7, D),
    /* 0xFB */ op!("SET 7, E", 2, 8, setb, 7, E),
    /* 0xFC */ op!("SET 7, H", 2, 8, setb, 7, H),
    /* 0xFD */ op!("SET 7, L", 2, 8, setb, 7, L),
    /* 0xFE */ op!("SET 7, (HL)", 2, 16, setb, 7, MEM_HL),
    /* 0xFF */ op!("SET 7, A", 2, 8, setb, 7, A),
];

fn nop(_: &mut CPU, _: [u8; 2], _: u8) -> OpStatus {
    OpStatus::Normal
}

fn alu(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [action, src] = args;
    let a = cpu.regs.af[1];
    let val = readsrc(cpu, src);
    match action {
        alu::ADD => {
            let (res, carry, halfcarry) =
                add_with_carry(a, val, 0, 0x100, 0x10);
            cpu.regs.af[1] = res;
            check_zero_flag(cpu, res);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_carry_flag(carry);
            cpu.regs.set_halfcarry_flag(halfcarry);
        }
        alu::ADC => {
            let (res, carry, halfcarry) =
                add_with_carry(a, val, cpu.regs.carry_flag(), 0x100, 0x10);
            cpu.regs.af[1] = res;
            check_zero_flag(cpu, res);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_carry_flag(carry);
            cpu.regs.set_halfcarry_flag(halfcarry);
        }
        alu::SUB => {
            let (res, carry, halfcarry) =
                sub_with_carry(a, val, 0, 0x100, 0x10);
            cpu.regs.af[1] = res;
            check_zero_flag(cpu, res);
            cpu.regs.set_sub_flag(1);
            cpu.regs.set_carry_flag(carry);
            cpu.regs.set_halfcarry_flag(halfcarry);
        }
        alu::SBC => {
            let (res, carry, halfcarry) =
                sub_with_carry(a, val, cpu.regs.carry_flag(), 0x100, 0x10);
            cpu.regs.af[1] = res;
            check_zero_flag(cpu, res);
            cpu.regs.set_sub_flag(1);
            cpu.regs.set_carry_flag(carry);
            cpu.regs.set_halfcarry_flag(halfcarry);
        }
        alu::AND => {
            let result = a & val;
            cpu.regs.af[1] = result;
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(1);
            cpu.regs.set_carry_flag(0);
        }
        alu::OR => {
            let result = a | val;
            cpu.regs.af[1] = result;
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(0);
        }
        alu::XOR => {
            let result = a ^ val;
            cpu.regs.af[1] = result;
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(0);
        }
        alu::CP => {
            let (res, carry, halfcarry) =
                sub_with_carry(a, val, 0, 0x100, 0x10);
            check_zero_flag(cpu, res);
            cpu.regs.set_sub_flag(1);
            cpu.regs.set_carry_flag(carry);
            cpu.regs.set_halfcarry_flag(halfcarry);
        }
        _ => unreachable!(),
    };
    OpStatus::Normal
}

fn alu2(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [opcode, reg] = args;
    let val = readsrc(cpu, reg);
    match opcode {
        alu2::INC => {
            let (res, _, halfcarry) = add_with_carry(val, 1, 0, 0x100, 0x10);
            writeback(cpu, reg, res);
            check_zero_flag(cpu, res);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(halfcarry);
        }
        alu2::DEC => {
            let (res, _, halfcarry) = sub_with_carry(val, 1, 0, 0x100, 0x10);
            writeback(cpu, reg, res);
            check_zero_flag(cpu, res);
            cpu.regs.set_sub_flag(1);
            cpu.regs.set_halfcarry_flag(halfcarry);
        }
        alu2::SCF => {
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(1);
        }
        alu2::CCF => {
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(if cpu.regs.carry_flag() == 0 {
                1
            } else {
                0
            });
        }
        alu2::CPL => {
            cpu.regs.af[1] = !cpu.regs.af[1];
            cpu.regs.set_sub_flag(1);
            cpu.regs.set_halfcarry_flag(1);
        }
        alu2::DAA => {
            // Convert A to packed BCD
            let val = readsrc(cpu, reg::A);
            let mut res = val;
            if cpu.regs.sub_flag() == 0 {
                if cpu.regs.carry_flag() == 1 || res > 0x99 {
                    res = res.wrapping_add(0x60);
                    cpu.regs.set_carry_flag(1);
                }
                if cpu.regs.halfcarry_flag() == 1 || (res & 0xF) > 0x09 {
                    res = res.wrapping_add(0x06);
                }
            } else {
                if cpu.regs.carry_flag() == 1 {
                    res = res.wrapping_sub(0x60);
                }
                if cpu.regs.halfcarry_flag() == 1 {
                    res = res.wrapping_sub(0x06)
                }
            }
            writeback(cpu, reg::A, res);
            check_zero_flag(cpu, res);
            cpu.regs.set_halfcarry_flag(0);
        }
        _ => unreachable!(),
    };
    OpStatus::Normal
}

fn load(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [dst, src] = args;
    let val = readsrc(cpu, src);
    writeback(cpu, dst, val);
    OpStatus::Normal
}

fn load16(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [action, reg] = args;
    match action {
        ld16::LOADI => {
            let imm = readsrc16(cpu, D16);
            writeback16(cpu, reg, imm);
        }
        ld16::SAVESP => {
            let address = readsrc16(cpu, D16);
            cpu.mmu.store16(address, cpu.regs.sp);
        }
        ld16::LDSPHL => {
            cpu.regs.sp = cpu.regs.hl.as_word();
        }
        ld16::LDHLSPr8 => {
            let r8 = readsrc(cpu, D8) as i8 as i16;
            let sp = cpu.regs.sp;
            let (res, carry, halfcarry) =
                add_with_carry(sp, r8 as u16, 0, 0x100, 0x10);
            cpu.regs.hl.set_word(res);
            cpu.regs.set_zero_flag(0);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_carry_flag(carry);
            cpu.regs.set_halfcarry_flag(halfcarry);
        }
        ld16::PUSH => {
            let val = readsrc16(cpu, reg);
            let [lo, hi] = val.to_le_bytes();
            cpu.mmu.store8(cpu.regs.sp - 1, hi);
            cpu.mmu.store8(cpu.regs.sp - 2, lo);
            cpu.regs.sp = cpu.regs.sp.wrapping_sub(2);
        }
        ld16::POP => {
            let lo = cpu.mmu.load8(cpu.regs.sp);
            let hi = cpu.mmu.load8(cpu.regs.sp + 1);
            let val = u16::from_le_bytes([lo, hi]);
            writeback16(cpu, reg, val);
            cpu.regs.sp = cpu.regs.sp.wrapping_add(2);
        }
        _ => unreachable!(),
    }
    OpStatus::Normal
}

fn alu16(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [action, reg] = args;
    match action {
        alu16::INC16 => {
            let result = readsrc16(cpu, reg).wrapping_add(1);
            writeback16(cpu, reg, result);
        }
        alu16::DEC16 => {
            let result = readsrc16(cpu, reg).wrapping_sub(1);
            writeback16(cpu, reg, result);
        }
        alu16::ADDHL => {
            let src = readsrc16(cpu, reg);
            let hl = cpu.regs.hl.as_word();
            let (res, carry, halfcarry) =
                add_with_carry(hl, src, 0, 0x10000, 0x1000);
            writeback16(cpu, HL, res);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_carry_flag(carry);
            cpu.regs.set_halfcarry_flag(halfcarry);
        }
        alu16::ADDSP => {
            let sp = cpu.regs.sp;
            let imm = readsrc(cpu, D8) as i8 as i16;
            let (res, carry, halfcarry) =
                add_with_carry(sp, imm as u16, 0, 0x100, 0x10);
            writeback16(cpu, SP, res);
            cpu.regs.set_zero_flag(0);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_carry_flag(carry);
            cpu.regs.set_halfcarry_flag(halfcarry);
        }
        _ => unreachable!(),
    };
    OpStatus::Normal
}

fn cfu(cpu: &mut CPU, args: [u8; 2], op_bytes: u8) -> OpStatus {
    let [action, condition] = args;
    let cond = match condition {
        control::CONDNZ => cpu.regs.zero_flag() == 0,
        control::CONDNC => cpu.regs.carry_flag() == 0,
        control::CONDZ => cpu.regs.zero_flag() == 1,
        control::CONDC => cpu.regs.carry_flag() == 1,
        control::UNCOND => true,
        _ =>
        /* RST case */
        {
            true
        }
    };
    match action {
        control::JR => {
            let offset = readsrc(cpu, D8) as i8 as i16;
            let next_pc = cpu.regs.pc.wrapping_add(op_bytes as u16);
            let target = next_pc.wrapping_add_signed(offset);
            if cond {
                cpu.regs.pc = target;
                return OpStatus::BranchTaken(4);
            }
        }
        control::JP => {
            let target = readsrc16(cpu, D16);
            if cond {
                cpu.regs.pc = target;
                return OpStatus::BranchTaken(4);
            }
        }
        control::JP_HL => {
            cpu.regs.pc = cpu.regs.hl.as_word();
            return OpStatus::BranchTaken(0);
        }
        control::CALL => {
            let pc_after = cpu.regs.pc.wrapping_add(op_bytes as u16);
            let target = readsrc16(cpu, D16);
            if cond {
                cpu.regs.pc = target;
                cpu.regs.sp = cpu.regs.sp.wrapping_sub(2);
                cpu.mmu.store16(cpu.regs.sp, pc_after);
                return OpStatus::BranchTaken(12);
            }
        }
        control::RET => {
            let saved_pc = cpu.mmu.load16(cpu.regs.sp);
            if cond {
                cpu.regs.pc = saved_pc;
                cpu.regs.sp = cpu.regs.sp.wrapping_add(2);
                return OpStatus::BranchTaken(12);
            }
        }
        control::RETI => {
            let saved_pc = cpu.mmu.load16(cpu.regs.sp);
            cpu.regs.pc = saved_pc;
            cpu.regs.sp = cpu.regs.sp.wrapping_add(2);
            cpu.enable_interrupts();
            return OpStatus::BranchTaken(12);
        }
        control::RST => {
            let pc_after = cpu.regs.pc.wrapping_add(op_bytes as u16);
            let target = args[1] as u16;
            cpu.regs.pc = target;
            cpu.regs.sp = cpu.regs.sp.wrapping_sub(2);
            cpu.mmu.store16(cpu.regs.sp, pc_after);
            return OpStatus::BranchTaken(0);
        }
        _ => unreachable!(),
    };
    OpStatus::Normal
}

fn bit(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [idx, src] = args;
    let val = readsrc(cpu, src) & (1 << idx);
    check_zero_flag(cpu, val);
    cpu.regs.set_sub_flag(0);
    cpu.regs.set_halfcarry_flag(1);
    OpStatus::Normal
}

fn setb(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [idx, reg] = args;
    let val = readsrc(cpu, reg);
    let result = utils::set_bit(val, idx, 1);
    writeback(cpu, reg, result);
    OpStatus::Normal
}

fn resb(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [idx, reg] = args;
    let val = readsrc(cpu, reg);
    let result = utils::set_bit(val, idx, 0);
    writeback(cpu, reg, result);
    OpStatus::Normal
}

fn rot(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [action, reg] = args;
    match action {
        // Rotate A left (ignore carry)
        cb_rot::RLCA => {
            let val = readsrc(cpu, reg::A);
            let result = val.rotate_left(1);
            writeback(cpu, reg::A, result);
            cpu.regs.set_zero_flag(0);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 7));
        }
        // Rotate reg left (ignore carry)
        cb_rot::RLC => {
            let val = readsrc(cpu, reg);
            let result = val.rotate_left(1);
            writeback(cpu, reg, result);
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 7));
        }
        // Rotate A left with carry
        cb_rot::RLA => {
            let val = readsrc(cpu, reg::A);
            let result = (val << 1) | cpu.regs.carry_flag();
            writeback(cpu, reg::A, result);
            cpu.regs.set_zero_flag(0);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 7));
        }
        // Rotate reg left with carry
        cb_rot::RL => {
            let val = readsrc(cpu, reg);
            let result = (val << 1) | cpu.regs.carry_flag();
            writeback(cpu, reg, result);
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 7));
        }
        // Rotate A right (ignore carry)
        cb_rot::RRCA => {
            let val = readsrc(cpu, reg::A);
            let result = val.rotate_right(1);
            writeback(cpu, reg::A, result);
            cpu.regs.set_zero_flag(0);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 0));
        }
        // Rotate reg right (ignore carry)
        cb_rot::RRC => {
            let val = readsrc(cpu, reg);
            let result = val.rotate_right(1);
            writeback(cpu, reg, result);
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 0));
        }
        // Rotate A right with carry
        cb_rot::RRA => {
            let val = readsrc(cpu, reg::A);
            let result = (val >> 1) | (cpu.regs.carry_flag() << 7);
            writeback(cpu, reg::A, result);
            cpu.regs.set_zero_flag(0);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 0));
        }
        // Rotate reg right with carry
        cb_rot::RR => {
            let val = readsrc(cpu, reg);
            let result = (val >> 1) | (cpu.regs.carry_flag() << 7);
            writeback(cpu, reg, result);
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 0));
        }
        // Logical shift A left
        cb_rot::SLA => {
            let val = readsrc(cpu, reg);
            let result = val << 1;
            writeback(cpu, reg, result);
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 7));
        }
        // Arithmetic shift A right
        cb_rot::SRA => {
            let val = readsrc(cpu, reg);
            let result = (val >> 1) | (val & 0x80);
            writeback(cpu, reg, result);
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 0));
        }
        // Logical shift reg right
        cb_rot::SRL => {
            let val = readsrc(cpu, reg);
            let result = val >> 1;
            writeback(cpu, reg, result);
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(utils::get_bit(val, 0));
        }
        // Swap nibbles
        cb_rot::SWAP => {
            let val = readsrc(cpu, reg);
            let (upper, lower) = ((val & 0xF0) >> 4, (val & 0x0F) >> 0);
            let result = (lower << 4) | (upper << 0);
            writeback(cpu, reg, result);
            check_zero_flag(cpu, result);
            cpu.regs.set_sub_flag(0);
            cpu.regs.set_halfcarry_flag(0);
            cpu.regs.set_carry_flag(0);
        }
        _ => unreachable!(),
    };
    OpStatus::Normal
}

fn trap(cpu: &mut CPU, args: [u8; 2], op_bytes: u8) -> OpStatus {
    let [action, _] = args;
    let bytes = cpu.mmu.block_load(cpu.regs.pc, op_bytes as usize);
    match action {
        trap::TODO => {
            unreachable!("Crash: op {bytes:02X?} not yet implemented!")
        }
        trap::NOT_EXISTS => {
            unreachable!("Crash: op {bytes:02X?} doesn't exist!")
        }
        trap::UNREACHABLE => {
            unreachable!("Crash: op {bytes:02X?} should be unreachable!")
        }
        _ => unreachable!(),
    }
}

fn misc(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [action, _] = args;
    match action {
        misc::HALT => cpu.halt(),
        misc::EI => cpu.enable_interrupts_next_inst(),
        misc::DI => cpu.disable_interrupts(),
        misc::STOP => cpu.stop(),
        _ => unreachable!(),
    }
    OpStatus::Normal
}

#[inline]
pub fn op_from_code(code: u8) -> &'static Op {
    &OPS[code as usize]
}

#[inline]
pub fn op_from_prefixed_code(code: u8) -> &'static Op {
    &PREFIXED_OPS[code as usize]
}

pub fn is_unconditional_jump(code: u8) -> bool {
    match code {
        | 0x18 // JR r8
        | 0xC3 // JP a16
        | 0xC9 // RET
        | 0xD9 // RETI
        | 0xE9 // JP HL
        => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_alu(
        cpu: &mut CPU,
        op: u8,
        a: u8,
        b: u8,
        carryin: u8,
        res: u8,
        zf: u8,
        cf: u8,
        hf: u8,
    ) {
        cpu.regs.set_carry_flag(carryin);
        writeback(cpu, A, a);
        writeback(cpu, B, b);
        alu(cpu, [op, B], 0);
        assert_eq!(cpu.regs.af[1], res, "result");
        if op == SUB || op == SBC {
            assert_eq!(cpu.regs.sub_flag(), 1, "SF");
        } else {
            assert_eq!(cpu.regs.sub_flag(), 0, "SF");
        }
        assert_eq!(cpu.regs.zero_flag(), zf, "ZF");
        assert_eq!(cpu.regs.carry_flag(), cf, "CF");
        assert_eq!(cpu.regs.halfcarry_flag(), hf, "HF");
    }

    #[test]
    fn add() {
        let mut cpu = CPU::new();
        let mut check = |a, b, res, zf, cf, hf| {
            check_alu(&mut cpu, ADD, a, b, 0, res, zf, cf, hf)
        };
        check(0x00, 0x00, 0x00, 1, 0, 0);
        check(0x00, 0x01, 0x01, 0, 0, 0);
        check(0x01, 0x01, 0x02, 0, 0, 0);
        check(0x00, 0xFF, 0xFF, 0, 0, 0);
        check(0x01, 0xFF, 0x00, 1, 1, 1);
        check(0x02, 0xFF, 0x01, 0, 1, 1);
        check(0xFF, 0xFF, 0xFE, 0, 1, 1);
        check(0x01, 0x0F, 0x10, 0, 0, 1);
    }

    #[test]
    fn adc() {
        let mut cpu = CPU::new();
        let mut check = |a, b, carryin, res, zf, cf, hf| {
            check_alu(&mut cpu, ADC, a, b, carryin, res, zf, cf, hf)
        };
        check(0x00, 0x00, 0, 0x00, 1, 0, 0);
        check(0x00, 0x00, 1, 0x01, 0, 0, 0);
        check(0x00, 0x01, 0, 0x01, 0, 0, 0);
        check(0x00, 0x01, 1, 0x02, 0, 0, 0);
        check(0x00, 0x0F, 0, 0x0F, 0, 0, 0);
        check(0x00, 0x0F, 1, 0x10, 0, 0, 1);
        check(0x01, 0x0F, 0, 0x10, 0, 0, 1);
        check(0x01, 0x0F, 1, 0x11, 0, 0, 1);
        check(0x10, 0x01, 0, 0x11, 0, 0, 0);
        check(0x10, 0x01, 1, 0x12, 0, 0, 0);
        check(0x10, 0xF0, 0, 0x00, 1, 1, 0);
        check(0x10, 0xF0, 1, 0x01, 0, 1, 0);
        check(0x01, 0xFF, 0, 0x00, 1, 1, 1);
        check(0x01, 0xFF, 1, 0x01, 0, 1, 1);
        check(0x00, 0xFF, 0, 0xFF, 0, 0, 0);
        check(0x00, 0xFF, 1, 0x00, 1, 1, 1);
        check(0x01, 0xFE, 0, 0xFF, 0, 0, 0);
        check(0x01, 0xFE, 1, 0x00, 1, 1, 1);
    }

    #[test]
    fn sub() {
        let mut cpu = CPU::new();
        let mut check = |a, b, result, zf, cf, hf| {
            check_alu(&mut cpu, SUB, a, b, 0, result, zf, cf, hf)
        };
        check(0x00, 0x00, 0x00, 1, 0, 0);
        check(0x00, 0x01, 0xFF, 0, 1, 1);
        check(0x01, 0x01, 0x00, 1, 0, 0);
        check(0x00, 0xFF, 0x01, 0, 1, 1);
        check(0x01, 0xFF, 0x02, 0, 1, 1);
        check(0x02, 0xFF, 0x03, 0, 1, 1);
        check(0xFF, 0xFF, 0x00, 1, 0, 0);
        check(0x01, 0x0F, 0xF2, 0, 1, 1);
        check(0xFF, 0x01, 0xFE, 0, 0, 0);
        check(0xF0, 0x01, 0xEF, 0, 0, 1);
    }

    #[test]
    fn sbc() {
        let mut cpu = CPU::new();
        let mut check = |a, b, carryin, result, zf, cf, hf| {
            check_alu(&mut cpu, SBC, a, b, carryin, result, zf, cf, hf)
        };
        check(0x00, 0x00, 0, 0x00, 1, 0, 0);
        check(0x00, 0x00, 1, 0xFF, 0, 1, 1);
        check(0x00, 0x01, 0, 0xFF, 0, 1, 1);
        check(0x00, 0x01, 1, 0xFE, 0, 1, 1);
        check(0x01, 0x01, 0, 0x00, 1, 0, 0);
        check(0x01, 0x01, 1, 0xFF, 0, 1, 1);
        check(0x00, 0xFF, 0, 0x01, 0, 1, 1);
        check(0x00, 0xFF, 1, 0x00, 1, 1, 1);
        check(0x01, 0xFF, 0, 0x02, 0, 1, 1);
        check(0x01, 0xFF, 1, 0x01, 0, 1, 1);
        check(0x02, 0xFF, 0, 0x03, 0, 1, 1);
        check(0x02, 0xFF, 1, 0x02, 0, 1, 1);
        check(0xFF, 0xFF, 0, 0x00, 1, 0, 0);
        check(0xFF, 0xFF, 1, 0xFF, 0, 1, 1);
        check(0x01, 0x0F, 0, 0xF2, 0, 1, 1);
        check(0x01, 0x0F, 1, 0xF1, 0, 1, 1);
        check(0xFF, 0x01, 0, 0xFE, 0, 0, 0);
        check(0xFF, 0x01, 1, 0xFD, 0, 0, 0);
        check(0xF0, 0x01, 0, 0xEF, 0, 0, 1);
        check(0xF0, 0x01, 1, 0xEE, 0, 0, 1);
    }

    #[test]
    fn addhl() {
        let mut cpu = CPU::new();
        let mut check = |hl, bc, result, carry, halfcarry| {
            writeback16(&mut cpu, HL, hl);
            writeback16(&mut cpu, BC, bc);
            alu16(&mut cpu, [ADDHL, BC], 0);
            assert_eq!(cpu.regs.hl.as_word(), result, "result");
            assert_eq!(cpu.regs.zero_flag(), 0, "ZF");
            assert_eq!(cpu.regs.sub_flag(), 0, "SF");
            assert_eq!(cpu.regs.carry_flag(), carry, "CF");
            assert_eq!(cpu.regs.halfcarry_flag(), halfcarry, "HF");
        };
        check(0x0000, 0x0000, 0x0000, 0, 0);
        check(0x0001, 0x0000, 0x0001, 0, 0);
        check(0x0010, 0x0001, 0x0011, 0, 0);
        check(0x0001, 0x000F, 0x0010, 0, 0);
        check(0x1000, 0xF000, 0x0000, 1, 0);
        check(0x0100, 0x0F00, 0x1000, 0, 1);
        check(0x0001, 0xFFFF, 0x0000, 1, 1);
    }

    #[test]
    fn addsp() {
        let mut cpu = CPU::new();
        let mut check = |sp, imm8, result, carry, halfcarry| {
            cpu.regs.pc = 0xD000;
            cpu.mmu.store8(cpu.regs.pc + 1, imm8);
            writeback16(&mut cpu, SP, sp);
            alu16(&mut cpu, [ADDSP, 0], 0);
            assert_eq!(cpu.regs.sp, result, "result");
            assert_eq!(cpu.regs.zero_flag(), 0, "ZF");
            assert_eq!(cpu.regs.sub_flag(), 0, "SF");
            assert_eq!(cpu.regs.carry_flag(), carry, "CF");
            assert_eq!(cpu.regs.halfcarry_flag(), halfcarry, "HF");
        };
        check(0x0000, 0x00, 0x0000, 0, 0);
        check(0x0000, 0x01, 0x0001, 0, 0);
        check(0x0010, 0x01, 0x0011, 0, 0);
        check(0x00FF, 0x01, 0x0100, 1, 1);
        check(0x00FF, 0xFF, 0x00FE, 1, 1);
        check(0x0000, 0xFF, 0xFFFF, 0, 0);
        check(0x00F0, 0xFF, 0x00EF, 1, 0);
    }

    #[test]
    fn daa() {
        let mut cpu = CPU::new();
        let mut check = |a, result, flags: u8, zero, carry, halfcarry| {
            cpu.regs.af[1] = a;
            cpu.regs.af[0] = flags << 4;
            alu2(&mut cpu, [alu2::DAA, 0], 0);
            assert_eq!(cpu.regs.af[1], result, "result");
            assert_eq!(cpu.regs.zero_flag(), zero, "ZF");
            assert_eq!(cpu.regs.sub_flag(), 0, "SF");
            assert_eq!(cpu.regs.carry_flag(), carry, "CF");
            assert_eq!(cpu.regs.halfcarry_flag(), halfcarry, "HF");
        };
        check(0x00, 0x00, 0b000, 1, 0, 0);
        check(0x01, 0x01, 0b000, 0, 0, 0);
        check(0x0A, 0x10, 0b000, 0, 0, 0);
        check(0xA0, 0x00, 0b000, 1, 1, 0);
    }
}
