#![deny(unreachable_patterns)]

use crate::cpu::CPU;
use crate::utils;

pub type OpHandler = fn(&mut CPU, [u8; 2], u8) -> OpStatus;
pub enum OpStatus {
    Normal,
    BranchTaken(u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Op {
    pub code: u8,
    pub is_prefixed: bool,
    pub num_bytes: u8,
    pub num_clocks: u8,
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

    pub fn execute(&self, cpu: &mut CPU) -> OpStatus {
        let handler = HANDLERS[self.is_prefixed as usize][self.code as usize];
        handler(cpu, self.args, self.num_bytes)
    }
}

impl serde::Serialize for &'static Op {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let masked_code = crate::utils::set_bit(
            self.code as u16,
            15,
            self.is_prefixed as u16,
        );
        serializer.serialize_u16(masked_code)
    }
}

impl<'a> serde::Deserialize<'a> for &'static Op {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'a>,
    {
        let masked_code = u16::deserialize(deserializer)?;
        let is_prefixed = crate::utils::bit_set(masked_code, 15);
        let code = crate::utils::set_bit(masked_code, 15, 0) as u8;
        Ok(if is_prefixed {
            &PREFIXED_OPS[code as usize]
        } else {
            &OPS[code as usize]
        })
    }
}

pub mod trap {
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
    #[allow(non_upper_case_globals)]
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
macro_rules! opcodes {
    ([
        $(($code_1:literal,
           $repr_1:literal,
           $num_bytes_1:literal,
           $num_clocks_1:literal,
           $handler_1:ident,
           $arg1_1:expr,
           $arg2_1:expr
        )),+$(,)?
    ]
        [
        $(($code_2:literal,
           $repr_2:literal,
           $num_bytes_2:literal,
           $num_clocks_2:literal,
           $handler_2:ident,
           $arg1_2:expr,
           $arg2_2:expr
        )),+$(,)?
    ] ) => {
        const OPS: [Op; 0x100] = [$(
            Op {
                code: $code_1,
                is_prefixed: false,
                num_bytes: $num_bytes_1,
                num_clocks: $num_clocks_1,
                args: [$arg1_1, $arg2_1],
                repr: $repr_1,
            }
        ),+
        ];
        const PREFIXED_OPS: [Op; 0x100] = [$(
            Op {
                code: $code_2,
                is_prefixed: true,
                num_bytes: $num_bytes_2,
                num_clocks: $num_clocks_2,
                args: [$arg1_2, $arg2_2],
                repr: $repr_2,
            }
        ),+
        ];
        const HANDLERS: [[OpHandler; 0x100]; 2] = [
            [$($handler_1),*],
            [$($handler_2),*],
        ];
    };
}

opcodes!(
    /* regular ops */ [
        (0x00, "NOP", 1, 4, nop, 0, 0),
        (0x01, "LD BC, d16", 3, 12, load16, LOADI, BC),
        (0x02, "LD (BC), A", 1, 8, load, MEM_BC, A),
        (0x03, "INC BC", 1, 8, alu16, INC16, BC),
        (0x04, "INC B", 1, 4, alu2, INC, B),
        (0x05, "DEC B", 1, 4, alu2, DEC, B),
        (0x06, "LD B, d8", 2, 8, load, B, D8),
        (0x07, "RLCA", 1, 4, rot, RLCA, 0),
        (0x08, "LD (a16), SP", 3, 20, load16, SAVESP, 0),
        (0x09, "ADD HL, BC", 1, 8, alu16, ADDHL, BC),
        (0x0A, "LD A, (BC)", 1, 8, load, A, MEM_BC),
        (0x0B, "DEC BC", 1, 8, alu16, DEC16, BC),
        (0x0C, "INC C", 1, 4, alu2, INC, C),
        (0x0D, "DEC C", 1, 4, alu2, DEC, C),
        (0x0E, "LD C, d8", 2, 8, load, C, D8),
        (0x0F, "RRCA", 1, 4, rot, RRCA, 0),
        (0x10, "STOP d8", 2, 4, misc, STOP, 0),
        (0x11, "LD DE, d16", 3, 12, load16, LOADI, DE),
        (0x12, "LD (DE), A", 1, 8, load, MEM_DE, A),
        (0x13, "INC DE", 1, 8, alu16, INC16, DE),
        (0x14, "INC D", 1, 4, alu2, INC, D),
        (0x15, "DEC D", 1, 4, alu2, DEC, D),
        (0x16, "LD D, d8", 2, 8, load, D, D8),
        (0x17, "RLA", 1, 4, rot, RLA, 0),
        (0x18, "JR r8", 2, 8, cfu, JR, UNCOND),
        (0x19, "ADD HL, DE", 1, 8, alu16, ADDHL, DE),
        (0x1A, "LD A, (DE)", 1, 8, load, A, MEM_DE),
        (0x1B, "DEC DE", 1, 8, alu16, DEC16, DE),
        (0x1C, "INC E", 1, 4, alu2, INC, E),
        (0x1D, "DEC E", 1, 4, alu2, DEC, E),
        (0x1E, "LD E, d8", 2, 8, load, E, D8),
        (0x1F, "RRA", 1, 4, rot, RRA, 0),
        (0x20, "JR NZ, r8", 2, 8, cfu, JR, CONDNZ),
        (0x21, "LD HL, d16", 3, 12, load16, LOADI, HL),
        (0x22, "LD (HL+), A", 1, 8, load, MEM_HLI, A),
        (0x23, "INC HL", 1, 8, alu16, INC16, HL),
        (0x24, "INC H", 1, 4, alu2, INC, H),
        (0x25, "DEC H", 1, 4, alu2, DEC, H),
        (0x26, "LD H, d8", 2, 8, load, H, D8),
        (0x27, "DAA", 1, 4, alu2, DAA, 0),
        (0x28, "JR Z, r8", 2, 8, cfu, JR, CONDZ),
        (0x29, "ADD HL, HL", 1, 8, alu16, ADDHL, HL),
        (0x2A, "LD A, (HL+)", 1, 8, load, A, MEM_HLI),
        (0x2B, "DEC HL", 1, 8, alu16, DEC16, HL),
        (0x2C, "INC L", 1, 4, alu2, INC, L),
        (0x2D, "DEC L", 1, 4, alu2, DEC, L),
        (0x2E, "LD L, d8", 2, 8, load, L, D8),
        (0x2F, "CPL", 1, 4, alu2, CPL, 0),
        (0x30, "JR NC, r8", 2, 8, cfu, JR, CONDNC),
        (0x31, "LD SP, d16", 3, 12, load16, LOADI, SP),
        (0x32, "LD (HL-), A", 1, 8, load, MEM_HLD, A),
        (0x33, "INC SP", 1, 8, alu16, INC16, SP),
        (0x34, "INC (HL)", 1, 12, alu2, INC, MEM_HL),
        (0x35, "DEC (HL)", 1, 12, alu2, DEC, MEM_HL),
        (0x36, "LD (HL), d8", 2, 12, load, MEM_HL, D8),
        (0x37, "SCF", 1, 4, alu2, SCF, 0),
        (0x38, "JR C, r8", 2, 8, cfu, JR, CONDC),
        (0x39, "ADD HL, SP", 1, 8, alu16, ADDHL, SP),
        (0x3A, "LD A, (HL-)", 1, 8, load, A, MEM_HLD),
        (0x3B, "DEC SP", 1, 8, alu16, DEC16, SP),
        (0x3C, "INC A", 1, 4, alu2, INC, A),
        (0x3D, "DEC A", 1, 4, alu2, DEC, A),
        (0x3E, "LD A, d8", 2, 8, load, A, D8),
        (0x3F, "CCF", 1, 4, alu2, CCF, 0),
        (0x40, "LD B, B", 1, 4, nop, 0, 0),
        (0x41, "LD B, C", 1, 4, load, B, C),
        (0x42, "LD B, D", 1, 4, load, B, D),
        (0x43, "LD B, E", 1, 4, load, B, E),
        (0x44, "LD B, H", 1, 4, load, B, H),
        (0x45, "LD B, L", 1, 4, load, B, L),
        (0x46, "LD B, (HL)", 1, 8, load, B, MEM_HL),
        (0x47, "LD B, A", 1, 4, load, B, A),
        (0x48, "LD C, B", 1, 4, load, C, B),
        (0x49, "LD C, C", 1, 4, nop, 0, 0),
        (0x4A, "LD C, D", 1, 4, load, C, D),
        (0x4B, "LD C, E", 1, 4, load, C, E),
        (0x4C, "LD C, H", 1, 4, load, C, H),
        (0x4D, "LD C, L", 1, 4, load, C, L),
        (0x4E, "LD C, (HL)", 1, 8, load, C, MEM_HL),
        (0x4F, "LD C, A", 1, 4, load, C, A),
        (0x50, "LD D, B", 1, 4, load, D, B),
        (0x51, "LD D, C", 1, 4, load, D, C),
        (0x52, "LD D, D", 1, 4, nop, 0, 0),
        (0x53, "LD D, E", 1, 4, load, D, E),
        (0x54, "LD D, H", 1, 4, load, D, H),
        (0x55, "LD D, L", 1, 4, load, D, L),
        (0x56, "LD D, (HL)", 1, 8, load, D, MEM_HL),
        (0x57, "LD D, A", 1, 4, load, D, A),
        (0x58, "LD E, B", 1, 4, load, E, B),
        (0x59, "LD E, C", 1, 4, load, E, C),
        (0x5A, "LD E, D", 1, 4, load, E, D),
        (0x5B, "LD E, E", 1, 4, nop, 0, 0),
        (0x5C, "LD E, F", 1, 4, load, E, H),
        (0x5D, "LD E, G", 1, 4, load, E, L),
        (0x5E, "LD E, (HL)", 1, 8, load, E, MEM_HL),
        (0x5F, "LD E, A", 1, 4, load, E, A),
        (0x60, "LD H, B", 1, 4, load, H, B),
        (0x61, "LD H, C", 1, 4, load, H, C),
        (0x62, "LD H, D", 1, 4, load, H, D),
        (0x63, "LD H, E", 1, 4, load, H, E),
        (0x64, "LD H, H", 1, 4, nop, 0, 0),
        (0x65, "LD H, L", 1, 4, load, H, L),
        (0x66, "LD H, (HL)", 1, 8, load, H, MEM_HL),
        (0x67, "LD H, A", 1, 4, load, H, A),
        (0x68, "LD L, B", 1, 4, load, L, B),
        (0x69, "LD L, C", 1, 4, load, L, C),
        (0x6A, "LD L, D", 1, 4, load, L, D),
        (0x6B, "LD L, E", 1, 4, load, L, E),
        (0x6C, "LD L, H", 1, 4, load, L, H),
        (0x6D, "LD L, L", 1, 4, nop, 0, 0),
        (0x6E, "LD L, (HL)", 1, 8, load, L, MEM_HL),
        (0x6F, "LD L, A", 1, 4, load, L, A),
        (0x70, "LD (HL), B", 1, 8, load, MEM_HL, B),
        (0x71, "LD (HL), C", 1, 8, load, MEM_HL, C),
        (0x72, "LD (HL), D", 1, 8, load, MEM_HL, D),
        (0x73, "LD (HL), E", 1, 8, load, MEM_HL, E),
        (0x74, "LD (HL), H", 1, 8, load, MEM_HL, H),
        (0x75, "LD (HL), L", 1, 8, load, MEM_HL, L),
        (0x76, "HALT", 1, 4, misc, HALT, 0),
        (0x77, "LD (HL), A", 1, 8, load, MEM_HL, A),
        (0x78, "LD A, B", 1, 4, load, A, B),
        (0x79, "LD A, C", 1, 4, load, A, C),
        (0x7A, "LD A, D", 1, 4, load, A, D),
        (0x7B, "LD A, E", 1, 4, load, A, E),
        (0x7C, "LD A, H", 1, 4, load, A, H),
        (0x7D, "LD A, L", 1, 4, load, A, L),
        (0x7E, "LD A, (HL)", 1, 8, load, A, MEM_HL),
        (0x7F, "LD A, A", 1, 4, nop, 0, 0),
        (0x80, "ADD A, B", 1, 4, alu, ADD, B),
        (0x81, "ADD A, C", 1, 4, alu, ADD, C),
        (0x82, "ADD A, D", 1, 4, alu, ADD, D),
        (0x83, "ADD A, E", 1, 4, alu, ADD, E),
        (0x84, "ADD A, H", 1, 4, alu, ADD, H),
        (0x85, "ADD A, L", 1, 4, alu, ADD, L),
        (0x86, "ADD A, (HL)", 1, 8, alu, ADD, MEM_HL),
        (0x87, "ADD A, A", 1, 4, alu, ADD, A),
        (0x88, "ADC A, B", 1, 4, alu, ADC, B),
        (0x89, "ADC A, C", 1, 4, alu, ADC, C),
        (0x8A, "ADC A, D", 1, 4, alu, ADC, D),
        (0x8B, "ADC A, E", 1, 4, alu, ADC, E),
        (0x8C, "ADC A, H", 1, 4, alu, ADC, H),
        (0x8D, "ADC A, L", 1, 4, alu, ADC, L),
        (0x8E, "ADC A, (HL)", 1, 8, alu, ADC, MEM_HL),
        (0x8F, "ADC A, A", 1, 4, alu, ADC, A),
        (0x90, "SUB B", 1, 4, alu, SUB, B),
        (0x91, "SUB C", 1, 4, alu, SUB, C),
        (0x92, "SUB D", 1, 4, alu, SUB, D),
        (0x93, "SUB E", 1, 4, alu, SUB, E),
        (0x94, "SUB H", 1, 4, alu, SUB, H),
        (0x95, "SUB L", 1, 4, alu, SUB, L),
        (0x96, "SUB (HL)", 1, 8, alu, SUB, MEM_HL),
        (0x97, "SUB A", 1, 4, alu, SUB, A),
        (0x98, "SBC B", 1, 4, alu, SBC, B),
        (0x99, "SBC C", 1, 4, alu, SBC, C),
        (0x9A, "SBC D", 1, 4, alu, SBC, D),
        (0x9B, "SBC E", 1, 4, alu, SBC, E),
        (0x9C, "SBC H", 1, 4, alu, SBC, H),
        (0x9D, "SBC L", 1, 4, alu, SBC, L),
        (0x9E, "SBC (HL)", 1, 8, alu, SBC, MEM_HL),
        (0x9F, "SUB A", 1, 4, alu, SBC, A),
        (0xA0, "AND B", 1, 4, alu, AND, B),
        (0xA1, "AND C", 1, 4, alu, AND, C),
        (0xA2, "AND D", 1, 4, alu, AND, D),
        (0xA3, "AND E", 1, 4, alu, AND, E),
        (0xA4, "AND H", 1, 4, alu, AND, H),
        (0xA5, "AND L", 1, 4, alu, AND, L),
        (0xA6, "AND (HL)", 1, 8, alu, AND, MEM_HL),
        (0xA7, "AND A", 1, 4, alu, AND, A),
        (0xA8, "XOR B", 1, 4, alu, XOR, B),
        (0xA9, "XOR C", 1, 4, alu, XOR, C),
        (0xAA, "XOR D", 1, 4, alu, XOR, D),
        (0xAB, "XOR E", 1, 4, alu, XOR, E),
        (0xAC, "XOR H", 1, 4, alu, XOR, H),
        (0xAD, "XOR L", 1, 4, alu, XOR, L),
        (0xAE, "XOR (HL)", 1, 8, alu, XOR, MEM_HL),
        (0xAF, "XOR A", 1, 4, alu, XOR, A),
        (0xB0, "OR B", 1, 4, alu, OR, B),
        (0xB1, "OR C", 1, 4, alu, OR, C),
        (0xB2, "OR D", 1, 4, alu, OR, D),
        (0xB3, "OR E", 1, 4, alu, OR, E),
        (0xB4, "OR H", 1, 4, alu, OR, H),
        (0xB5, "OR L", 1, 4, alu, OR, L),
        (0xB6, "OR (HL)", 1, 8, alu, OR, MEM_HL),
        (0xB7, "OR A", 1, 4, alu, OR, A),
        (0xB8, "CP B", 1, 4, alu, CP, B),
        (0xB9, "CP C", 1, 4, alu, CP, C),
        (0xBA, "CP D", 1, 4, alu, CP, D),
        (0xBB, "CP E", 1, 4, alu, CP, E),
        (0xBC, "CP H", 1, 4, alu, CP, H),
        (0xBD, "CP L", 1, 4, alu, CP, L),
        (0xBE, "CP (HL)", 1, 8, alu, CP, MEM_HL),
        (0xBF, "CP A", 1, 4, alu, CP, A),
        (0xC0, "RET NZ", 1, 8, cfu, RET, CONDNZ),
        (0xC1, "POP BC", 1, 12, load16, POP, BC),
        (0xC2, "JP NZ, a16", 3, 12, cfu, JP, CONDNZ),
        (0xC3, "JP a16", 3, 12, cfu, JP, UNCOND),
        (0xC4, "CALL NZ, a16", 3, 12, cfu, CALL, CONDNZ),
        (0xC5, "PUSH BC", 1, 16, load16, PUSH, BC),
        (0xC6, "ADD A, d8", 2, 8, alu, ADD, D8),
        (0xC7, "RST 00H", 1, 16, cfu, RST, 0x00),
        (0xC8, "RET Z", 1, 8, cfu, RET, CONDZ),
        (0xC9, "RET", 1, 4, cfu, RET, UNCOND),
        (0xCA, "JP Z, a16", 3, 12, cfu, JP, CONDZ),
        (0xCB, "PREFIX CB", 0, 0, trap, UNREACHABLE, 0),
        (0xCC, "CALL Z, a16", 3, 12, cfu, CALL, CONDZ),
        (0xCD, "CALL a16", 3, 12, cfu, CALL, UNCOND),
        (0xCE, "ADC A, d8,", 2, 8, alu, ADC, D8),
        (0xCF, "RST 08H", 1, 16, cfu, RST, 0x08),
        (0xD0, "RET NC", 1, 8, cfu, RET, CONDNC),
        (0xD1, "POP DE", 1, 12, load16, POP, DE),
        (0xD2, "JP NC, a16", 3, 12, cfu, JP, CONDNC),
        (0xD3, "0xD3", 1, 0, trap, NOT_EXISTS, 0),
        (0xD4, "CALL NC, a16", 3, 12, cfu, CALL, CONDNC),
        (0xD5, "PUSH DE", 1, 16, load16, PUSH, DE),
        (0xD6, "SUB A, d8", 2, 8, alu, SUB, D8),
        (0xD7, "RST 10H", 1, 16, cfu, RST, 0x10),
        (0xD8, "RET C", 1, 8, cfu, RET, CONDC),
        (0xD9, "RETI", 1, 4, cfu, RETI, 0),
        (0xDA, "JP C, a16", 3, 12, cfu, JP, CONDC),
        (0xDB, "0xDB", 1, 0, trap, NOT_EXISTS, 0),
        (0xDC, "CALL C, a16", 3, 12, cfu, CALL, CONDC),
        (0xDD, "0xDD", 1, 0, trap, NOT_EXISTS, 0),
        (0xDE, "SBC A, d8", 2, 8, alu, SBC, D8),
        (0xDF, "RST 18H", 1, 16, cfu, RST, 0x18),
        (0xE0, "LDH (a8), A", 2, 12, load, A8, A),
        (0xE1, "POP HL", 1, 12, load16, POP, HL),
        (0xE2, "LD (C),A", 1, 8, load, MEMH_C, A),
        (0xE3, "0xE3", 1, 0, trap, NOT_EXISTS, 0),
        (0xE4, "0xE4", 1, 0, trap, NOT_EXISTS, 0),
        (0xE5, "PUSH HL", 1, 16, load16, PUSH, HL),
        (0xE6, "AND d8", 2, 8, alu, AND, D8),
        (0xE7, "RST 20H", 1, 16, cfu, RST, 0x20),
        (0xE8, "ADD SP, r8", 2, 16, alu16, ADDSP, 0),
        (0xE9, "JP HL", 1, 4, cfu, JP_HL, 0),
        (0xEA, "LD (a16), A", 3, 16, load, MEM_IMM16, A),
        (0xEB, "0xEB", 1, 0, trap, NOT_EXISTS, 0),
        (0xEC, "0xEC", 1, 0, trap, NOT_EXISTS, 0),
        (0xED, "0xED", 1, 0, trap, NOT_EXISTS, 0),
        (0xEE, "XOR d8,", 2, 8, alu, XOR, D8),
        (0xEF, "RST 28H", 1, 16, cfu, RST, 0x28),
        (0xF0, "LDH A, (a8)", 2, 12, load, A, A8),
        (0xF1, "POP AF", 1, 12, load16, POP, AF),
        (0xF2, "LD A, (C)", 1, 8, load, A, MEMH_C),
        (0xF3, "DI", 1, 4, misc, DI, 0),
        (0xF4, "0xF4", 1, 0, trap, NOT_EXISTS, 0),
        (0xF5, "PUSH AF", 1, 16, load16, PUSH, AF),
        (0xF6, "OR d8", 2, 8, alu, OR, D8),
        (0xF7, "RST 30H", 1, 16, cfu, RST, 0x30),
        (0xF8, "LD HL, SP + r8", 2, 12, load16, LDHLSPr8, 0),
        (0xF9, "LD SP, HL", 1, 8, load16, LDSPHL, 0),
        (0xFA, "LD A, (a16)", 3, 16, load, A, MEM_IMM16),
        (0xFB, "EI", 1, 4, misc, EI, 0),
        (0xFC, "0xFC", 1, 0, trap, NOT_EXISTS, 0),
        (0xFD, "0xFD", 1, 0, trap, NOT_EXISTS, 0),
        (0xFE, "CP d8", 2, 8, alu, CP, D8),
        (0xFF, "RST 38H", 1, 16, cfu, RST, 0x38),
    ]
    /* prefix ops */ [
        (0x00, "RLC B", 2, 8, rot, RLC, B),
        (0x01, "RLC C", 2, 8, rot, RLC, C),
        (0x02, "RLC D", 2, 8, rot, RLC, D),
        (0x03, "RLC E", 2, 8, rot, RLC, E),
        (0x04, "RLC H", 2, 8, rot, RLC, H),
        (0x05, "RLC L", 2, 8, rot, RLC, L),
        (0x06, "RLC (HL)", 2, 16, rot, RLC, MEM_HL),
        (0x07, "RLC A", 2, 8, rot, RLC, A),
        (0x08, "RRC B", 2, 8, rot, RRC, B),
        (0x09, "RRC C", 2, 8, rot, RRC, C),
        (0x0A, "RRC D", 2, 8, rot, RRC, D),
        (0x0B, "RRC E", 2, 8, rot, RRC, E),
        (0x0C, "RRC H", 2, 8, rot, RRC, H),
        (0x0D, "RRC L", 2, 8, rot, RRC, L),
        (0x0E, "RRC (HL)", 2, 16, rot, RRC, MEM_HL),
        (0x0F, "RRC A", 2, 8, rot, RRC, A),
        (0x10, "RL B", 2, 8, rot, RL, B),
        (0x11, "RL C", 2, 8, rot, RL, C),
        (0x12, "RL D", 2, 8, rot, RL, D),
        (0x13, "RL E", 2, 8, rot, RL, E),
        (0x14, "RL H", 2, 8, rot, RL, H),
        (0x15, "RL L", 2, 8, rot, RL, L),
        (0x16, "RL (HL)", 2, 16, rot, RL, MEM_HL),
        (0x17, "RL A", 2, 8, rot, RL, A),
        (0x18, "RR B", 2, 8, rot, RR, B),
        (0x19, "RR C", 2, 8, rot, RR, C),
        (0x1A, "RR D", 2, 8, rot, RR, D),
        (0x1B, "RR E", 2, 8, rot, RR, E),
        (0x1C, "RR H", 2, 8, rot, RR, H),
        (0x1D, "RR L", 2, 8, rot, RR, L),
        (0x1E, "RR (HL)", 2, 16, rot, RR, MEM_HL),
        (0x1F, "RR A", 2, 8, rot, RR, A),
        (0x20, "SLA B", 2, 8, rot, SLA, B),
        (0x21, "SLA C", 2, 8, rot, SLA, C),
        (0x22, "SLA D", 2, 8, rot, SLA, D),
        (0x23, "SLA E", 2, 8, rot, SLA, E),
        (0x24, "SLA H", 2, 8, rot, SLA, H),
        (0x25, "SLA L", 2, 8, rot, SLA, L),
        (0x26, "SLA (HL)", 2, 16, rot, SLA, MEM_HL),
        (0x27, "SLA A", 2, 8, rot, SLA, A),
        (0x28, "SRA B", 2, 8, rot, SRA, B),
        (0x29, "SRA C", 2, 8, rot, SRA, C),
        (0x2A, "SRA D", 2, 8, rot, SRA, D),
        (0x2B, "SRA E", 2, 8, rot, SRA, E),
        (0x2C, "SRA H", 2, 8, rot, SRA, H),
        (0x2D, "SRA L", 2, 8, rot, SRA, L),
        (0x2E, "SRA (HL)", 2, 16, rot, SRA, MEM_HL),
        (0x2F, "SRA A", 2, 8, rot, SRA, A),
        (0x30, "SWAP B", 2, 8, rot, SWAP, B),
        (0x31, "SWAP C", 2, 8, rot, SWAP, C),
        (0x32, "SWAP D", 2, 8, rot, SWAP, D),
        (0x33, "SWAP E", 2, 8, rot, SWAP, E),
        (0x34, "SWAP H", 2, 8, rot, SWAP, H),
        (0x35, "SWAP L", 2, 8, rot, SWAP, L),
        (0x36, "SWAP (HL)", 2, 16, rot, SWAP, MEM_HL),
        (0x37, "SWAP A", 2, 8, rot, SWAP, A),
        (0x38, "SRL B", 2, 8, rot, SRL, B),
        (0x39, "SRL C", 2, 8, rot, SRL, C),
        (0x3A, "SRL D", 2, 8, rot, SRL, D),
        (0x3B, "SRL E", 2, 8, rot, SRL, E),
        (0x3C, "SRL H", 2, 8, rot, SRL, H),
        (0x3D, "SRL L", 2, 8, rot, SRL, L),
        (0x3E, "SRL (HL)", 2, 16, rot, SRL, MEM_HL),
        (0x3F, "SRL A", 2, 8, rot, SRL, A),
        (0x40, "BIT 0, B", 2, 8, bit, 0, B),
        (0x41, "BIT 0, C", 2, 8, bit, 0, C),
        (0x42, "BIT 0, D", 2, 8, bit, 0, D),
        (0x43, "BIT 0, E", 2, 8, bit, 0, E),
        (0x44, "BIT 0, H", 2, 8, bit, 0, H),
        (0x45, "BIT 0, L", 2, 8, bit, 0, L),
        (0x46, "BIT 0, (HL)", 2, 12, bit, 0, MEM_HL),
        (0x47, "BIT 0, A", 2, 8, bit, 0, A),
        (0x48, "BIT 1, B", 2, 8, bit, 1, B),
        (0x49, "BIT 1, C", 2, 8, bit, 1, C),
        (0x4A, "BIT 1, D", 2, 8, bit, 1, D),
        (0x4B, "BIT 1, E", 2, 8, bit, 1, E),
        (0x4C, "BIT 1, H", 2, 8, bit, 1, H),
        (0x4D, "BIT 1, L", 2, 8, bit, 1, L),
        (0x4E, "BIT 1, (HL)", 2, 12, bit, 1, MEM_HL),
        (0x4F, "BIT 1, A", 2, 8, bit, 1, A),
        (0x50, "BIT 2, B", 2, 8, bit, 2, B),
        (0x51, "BIT 2, C", 2, 8, bit, 2, C),
        (0x52, "BIT 2, D", 2, 8, bit, 2, D),
        (0x53, "BIT 2, E", 2, 8, bit, 2, E),
        (0x54, "BIT 2, H", 2, 8, bit, 2, H),
        (0x55, "BIT 2, L", 2, 8, bit, 2, L),
        (0x56, "BIT 2, (HL)", 2, 12, bit, 2, MEM_HL),
        (0x57, "BIT 2, A", 2, 8, bit, 2, A),
        (0x58, "BIT 3, B", 2, 8, bit, 3, B),
        (0x59, "BIT 3, C", 2, 8, bit, 3, C),
        (0x5A, "BIT 3, D", 2, 8, bit, 3, D),
        (0x5B, "BIT 3, E", 2, 8, bit, 3, E),
        (0x5C, "BIT 3, H", 2, 8, bit, 3, H),
        (0x5D, "BIT 3, L", 2, 8, bit, 3, L),
        (0x5E, "BIT 3, (HL)", 2, 12, bit, 3, MEM_HL),
        (0x5F, "BIT 3, A", 2, 8, bit, 3, A),
        (0x60, "BIT 4, B", 2, 8, bit, 4, B),
        (0x61, "BIT 4, C", 2, 8, bit, 4, C),
        (0x62, "BIT 4, D", 2, 8, bit, 4, D),
        (0x63, "BIT 4, E", 2, 8, bit, 4, E),
        (0x64, "BIT 4, H", 2, 8, bit, 4, H),
        (0x65, "BIT 4, L", 2, 8, bit, 4, L),
        (0x66, "BIT 4, (HL)", 2, 12, bit, 4, MEM_HL),
        (0x67, "BIT 4, A", 2, 8, bit, 4, A),
        (0x68, "BIT 5, B", 2, 8, bit, 5, B),
        (0x69, "BIT 5, C", 2, 8, bit, 5, C),
        (0x6A, "BIT 5, D", 2, 8, bit, 5, D),
        (0x6B, "BIT 5, E", 2, 8, bit, 5, E),
        (0x6C, "BIT 5, H", 2, 8, bit, 5, H),
        (0x6D, "BIT 5, L", 2, 8, bit, 5, L),
        (0x6E, "BIT 5, (HL)", 2, 12, bit, 5, MEM_HL),
        (0x6F, "BIT 5, A", 2, 8, bit, 5, A),
        (0x70, "BIT 6, B", 2, 8, bit, 6, B),
        (0x71, "BIT 6, C", 2, 8, bit, 6, C),
        (0x72, "BIT 6, D", 2, 8, bit, 6, D),
        (0x73, "BIT 6, E", 2, 8, bit, 6, E),
        (0x74, "BIT 6, H", 2, 8, bit, 6, H),
        (0x75, "BIT 6, L", 2, 8, bit, 6, L),
        (0x76, "BIT 6, (HL)", 2, 12, bit, 6, MEM_HL),
        (0x77, "BIT 6, A", 2, 8, bit, 6, A),
        (0x78, "BIT 7, B", 2, 8, bit, 7, B),
        (0x79, "BIT 7, C", 2, 8, bit, 7, C),
        (0x7A, "BIT 7, D", 2, 8, bit, 7, D),
        (0x7B, "BIT 7, E", 2, 8, bit, 7, E),
        (0x7C, "BIT 7, H", 2, 8, bit, 7, H),
        (0x7D, "BIT 7, L", 2, 8, bit, 7, L),
        (0x7E, "BIT 7, (HL)", 2, 12, bit, 7, MEM_HL),
        (0x7F, "BIT 7, A", 2, 8, bit, 7, A),
        (0x80, "RES 0, B", 2, 8, resb, 0, B),
        (0x81, "RES 0, C", 2, 8, resb, 0, C),
        (0x82, "RES 0, D", 2, 8, resb, 0, D),
        (0x83, "RES 0, E", 2, 8, resb, 0, E),
        (0x84, "RES 0, H", 2, 8, resb, 0, H),
        (0x85, "RES 0, L", 2, 8, resb, 0, L),
        (0x86, "RES 0, (HL)", 2, 16, resb, 0, MEM_HL),
        (0x87, "RES 0, A", 2, 8, resb, 0, A),
        (0x88, "RES 1, B", 2, 8, resb, 1, B),
        (0x89, "RES 1, C", 2, 8, resb, 1, C),
        (0x8A, "RES 1, D", 2, 8, resb, 1, D),
        (0x8B, "RES 1, E", 2, 8, resb, 1, E),
        (0x8C, "RES 1, H", 2, 8, resb, 1, H),
        (0x8D, "RES 1, L", 2, 8, resb, 1, L),
        (0x8E, "RES 1, (HL)", 2, 16, resb, 1, MEM_HL),
        (0x8F, "RES 1, A", 2, 8, resb, 1, A),
        (0x90, "RES 2, B", 2, 8, resb, 2, B),
        (0x91, "RES 2, C", 2, 8, resb, 2, C),
        (0x92, "RES 2, D", 2, 8, resb, 2, D),
        (0x93, "RES 2, E", 2, 8, resb, 2, E),
        (0x94, "RES 2, H", 2, 8, resb, 2, H),
        (0x95, "RES 2, L", 2, 8, resb, 2, L),
        (0x96, "RES 2, (HL)", 2, 16, resb, 2, MEM_HL),
        (0x97, "RES 2, A", 2, 8, resb, 2, A),
        (0x98, "RES 3, B", 2, 8, resb, 3, B),
        (0x99, "RES 3, C", 2, 8, resb, 3, C),
        (0x9A, "RES 3, D", 2, 8, resb, 3, D),
        (0x9B, "RES 3, E", 2, 8, resb, 3, E),
        (0x9C, "RES 3, H", 2, 8, resb, 3, H),
        (0x9D, "RES 3, L", 2, 8, resb, 3, L),
        (0x9E, "RES 3, (HL)", 2, 16, resb, 3, MEM_HL),
        (0x9F, "RES 3, A", 2, 8, resb, 3, A),
        (0xA0, "RES 4, B", 2, 8, resb, 4, B),
        (0xA1, "RES 4, C", 2, 8, resb, 4, C),
        (0xA2, "RES 4, D", 2, 8, resb, 4, D),
        (0xA3, "RES 4, E", 2, 8, resb, 4, E),
        (0xA4, "RES 4, H", 2, 8, resb, 4, H),
        (0xA5, "RES 4, L", 2, 8, resb, 4, L),
        (0xA6, "RES 4, (HL)", 2, 16, resb, 4, MEM_HL),
        (0xA7, "RES 4, A", 2, 8, resb, 4, A),
        (0xA8, "RES 5, B", 2, 8, resb, 5, B),
        (0xA9, "RES 5, C", 2, 8, resb, 5, C),
        (0xAA, "RES 5, D", 2, 8, resb, 5, D),
        (0xAB, "RES 5, E", 2, 8, resb, 5, E),
        (0xAC, "RES 5, H", 2, 8, resb, 5, H),
        (0xAD, "RES 5, L", 2, 8, resb, 5, L),
        (0xAE, "RES 5, (HL)", 2, 16, resb, 5, MEM_HL),
        (0xAF, "RES 5, A", 2, 8, resb, 5, A),
        (0xB0, "RES 6, B", 2, 8, resb, 6, B),
        (0xB1, "RES 6, C", 2, 8, resb, 6, C),
        (0xB2, "RES 6, D", 2, 8, resb, 6, D),
        (0xB3, "RES 6, E", 2, 8, resb, 6, E),
        (0xB4, "RES 6, H", 2, 8, resb, 6, H),
        (0xB5, "RES 6, L", 2, 8, resb, 6, L),
        (0xB6, "RES 6, (HL)", 2, 16, resb, 6, MEM_HL),
        (0xB7, "RES 6, A", 2, 8, resb, 6, A),
        (0xB8, "RES 7, B", 2, 8, resb, 7, B),
        (0xB9, "RES 7, C", 2, 8, resb, 7, C),
        (0xBA, "RES 7, D", 2, 8, resb, 7, D),
        (0xBB, "RES 7, E", 2, 8, resb, 7, E),
        (0xBC, "RES 7, H", 2, 8, resb, 7, H),
        (0xBD, "RES 7, L", 2, 8, resb, 7, L),
        (0xBE, "RES 7, (HL)", 2, 16, resb, 7, MEM_HL),
        (0xBF, "RES 7, A", 2, 8, resb, 7, A),
        (0xC0, "SET 0, B", 2, 8, setb, 0, B),
        (0xC1, "SET 0, C", 2, 8, setb, 0, C),
        (0xC2, "SET 0, D", 2, 8, setb, 0, D),
        (0xC3, "SET 0, E", 2, 8, setb, 0, E),
        (0xC4, "SET 0, H", 2, 8, setb, 0, H),
        (0xC5, "SET 0, L", 2, 8, setb, 0, L),
        (0xC6, "SET 0, (HL)", 2, 16, setb, 0, MEM_HL),
        (0xC7, "SET 0, A", 2, 8, setb, 0, A),
        (0xC8, "SET 1, B", 2, 8, setb, 1, B),
        (0xC9, "SET 1, C", 2, 8, setb, 1, C),
        (0xCA, "SET 1, D", 2, 8, setb, 1, D),
        (0xCB, "SET 1, E", 2, 8, setb, 1, E),
        (0xCC, "SET 1, H", 2, 8, setb, 1, H),
        (0xCD, "SET 1, L", 2, 8, setb, 1, L),
        (0xCE, "SET 1, (HL)", 2, 16, setb, 1, MEM_HL),
        (0xCF, "SET 1, A", 2, 8, setb, 1, A),
        (0xD0, "SET 2, B", 2, 8, setb, 2, B),
        (0xD1, "SET 2, C", 2, 8, setb, 2, C),
        (0xD2, "SET 2, D", 2, 8, setb, 2, D),
        (0xD3, "SET 2, E", 2, 8, setb, 2, E),
        (0xD4, "SET 2, H", 2, 8, setb, 2, H),
        (0xD5, "SET 2, L", 2, 8, setb, 2, L),
        (0xD6, "SET 2, (HL)", 2, 16, setb, 2, MEM_HL),
        (0xD7, "SET 2, A", 2, 8, setb, 2, A),
        (0xD8, "SET 3, B", 2, 8, setb, 3, B),
        (0xD9, "SET 3, C", 2, 8, setb, 3, C),
        (0xDA, "SET 3, D", 2, 8, setb, 3, D),
        (0xDB, "SET 3, E", 2, 8, setb, 3, E),
        (0xDC, "SET 3, H", 2, 8, setb, 3, H),
        (0xDD, "SET 3, L", 2, 8, setb, 3, L),
        (0xDE, "SET 3, (HL)", 2, 16, setb, 3, MEM_HL),
        (0xDF, "SET 3, A", 2, 8, setb, 3, A),
        (0xE0, "SET 4, B", 2, 8, setb, 4, B),
        (0xE1, "SET 4, C", 2, 8, setb, 4, C),
        (0xE2, "SET 4, D", 2, 8, setb, 4, D),
        (0xE3, "SET 4, E", 2, 8, setb, 4, E),
        (0xE4, "SET 4, H", 2, 8, setb, 4, H),
        (0xE5, "SET 4, L", 2, 8, setb, 4, L),
        (0xE6, "SET 4, (HL)", 2, 16, setb, 4, MEM_HL),
        (0xE7, "SET 4, A", 2, 8, setb, 4, A),
        (0xE8, "SET 5, B", 2, 8, setb, 5, B),
        (0xE9, "SET 5, C", 2, 8, setb, 5, C),
        (0xEA, "SET 5, D", 2, 8, setb, 5, D),
        (0xEB, "SET 5, E", 2, 8, setb, 5, E),
        (0xEC, "SET 5, H", 2, 8, setb, 5, H),
        (0xED, "SET 5, L", 2, 8, setb, 5, L),
        (0xEE, "SET 5, (HL)", 2, 16, setb, 5, MEM_HL),
        (0xEF, "SET 5, A", 2, 8, setb, 5, A),
        (0xF0, "SET 6, B", 2, 8, setb, 6, B),
        (0xF1, "SET 6, C", 2, 8, setb, 6, C),
        (0xF2, "SET 6, D", 2, 8, setb, 6, D),
        (0xF3, "SET 6, E", 2, 8, setb, 6, E),
        (0xF4, "SET 6, H", 2, 8, setb, 6, H),
        (0xF5, "SET 6, L", 2, 8, setb, 6, L),
        (0xF6, "SET 6, (HL)", 2, 16, setb, 6, MEM_HL),
        (0xF7, "SET 6, A", 2, 8, setb, 6, A),
        (0xF8, "SET 7, B", 2, 8, setb, 7, B),
        (0xF9, "SET 7, C", 2, 8, setb, 7, C),
        (0xFA, "SET 7, D", 2, 8, setb, 7, D),
        (0xFB, "SET 7, E", 2, 8, setb, 7, E),
        (0xFC, "SET 7, H", 2, 8, setb, 7, H),
        (0xFD, "SET 7, L", 2, 8, setb, 7, L),
        (0xFE, "SET 7, (HL)", 2, 16, setb, 7, MEM_HL),
        (0xFF, "SET 7, A", 2, 8, setb, 7, A),
    ]
);

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
    let result = utils::set_bit(val, idx as usize, 1);
    writeback(cpu, reg, result);
    OpStatus::Normal
}

fn resb(cpu: &mut CPU, args: [u8; 2], _: u8) -> OpStatus {
    let [idx, reg] = args;
    let val = readsrc(cpu, reg);
    let result = utils::set_bit(val, idx as usize, 0);
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

#[cold]
fn trap(cpu: &mut CPU, args: [u8; 2], op_bytes: u8) -> OpStatus {
    let [action, _] = args;
    let bytes = cpu.mmu.block_load(cpu.regs.pc, op_bytes as usize);
    match action {
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

    #[test]
    fn serialize_deserialize_roundtrip() {
        let mut bytes = Vec::with_capacity(std::mem::size_of::<u16>());
        for code in 0x00..=0xFF {
            ciborium::into_writer(&op_from_code(code), &mut bytes).unwrap();
            let op: &'static Op =
                ciborium::from_reader(bytes.as_slice()).unwrap();
            assert_eq!(op_from_code(code), op);
            bytes.clear();
        }
        for code in 0x00..=0xFF {
            ciborium::into_writer(&op_from_prefixed_code(code), &mut bytes)
                .unwrap();
            let op: &'static Op =
                ciborium::from_reader(bytes.as_slice()).unwrap();
            assert_eq!(op_from_prefixed_code(code), op);
            bytes.clear();
        }
    }
}
