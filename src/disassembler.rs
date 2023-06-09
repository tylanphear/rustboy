use crate::opcodes::{self, op_from_code, op_from_prefixed_code, Op};

#[derive(Clone, Copy)]
pub struct Inst {
    pub op: &'static Op,
    pub bytes: [u8; 3],
    pub offset: usize,
}

impl Inst {
    pub fn to_string(&self) -> String {
        self.op.to_string(&self.bytes)
    }

    pub fn code(&self) -> u8 {
        self.bytes[0]
    }
}

pub struct InstIter<'a> {
    buffer: &'a [u8],
    position: usize,
}

impl<'a> InstIter<'a> {
    pub fn new(buffer: &'a [u8]) -> Self {
        InstIter {
            buffer,
            position: 0,
        }
    }

    fn at(&self, offset: usize) -> u8 {
        self.buffer[self.position + offset]
    }
}

pub fn iterate_insts(buffer: &[u8]) -> InstIter<'_> {
    InstIter::new(buffer)
}

pub fn insts_til_unconditional_jump(
    buffer: &[u8],
) -> impl Iterator<Item = Inst> + '_ {
    let mut iter = InstIter::new(buffer);
    let mut saw_unconditional_jump = false;
    std::iter::from_fn(move || {
        if saw_unconditional_jump {
            return None;
        }
        let inst = iter.next()?;
        let code = inst.code();
        if opcodes::is_unconditional_jump(code) {
            saw_unconditional_jump = true;
        }
        Some(inst)
    })
}

impl<'a> Iterator for InstIter<'a> {
    type Item = Inst;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position >= self.buffer.len() {
            return None;
        }
        let op = if self.at(0) == 0xCB {
            op_from_prefixed_code(self.at(1))
        } else {
            op_from_code(self.at(0))
        };
        let mut bytes = [0; 3];
        for i in 0..op.num_bytes {
            bytes[i as usize] = self.at(i as usize);
        }
        let offset = self.position;
        self.position += op.num_bytes as usize;
        Some(Inst { op, bytes, offset })
    }
}

pub fn disassemble(buffer: &[u8]) -> Vec<Inst> {
    InstIter::new(buffer).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn some_insts() {
        let insts = disassemble(&[
            0x00,             // NOP
            0x80,             // ADD A, B
            0x21, 0xAB, 0xCD, // LD HL, CDAB
            0x38, 0xFB,       // JR C, -5
        ]);
        assert_eq!(insts.len(), 4);
        assert_eq!(&insts[0].to_string(), "NOP");
        assert_eq!(insts[0].bytes, [0x00u8, 0x00u8, 0x00u8]);
        assert_eq!(&insts[1].to_string(), "ADD A, B");
        assert_eq!(insts[1].bytes, [0x80u8, 0x00u8, 0x00u8]);
        assert_eq!(&insts[2].to_string(), "LD HL, CDAB");
        assert_eq!(insts[2].bytes, [0x21u8, 0xABu8, 0xCDu8]);
        assert_eq!(&insts[3].to_string(), "JR C, -5");
        assert_eq!(insts[3].bytes, [0x38u8, 0xFBu8, 0x00u8]);
    }

    #[test]
    #[rustfmt::skip]
    fn til_jump() {
        let insts: Vec<_> = insts_til_unconditional_jump(&[
            0x41,              // LD B, C
            0x16, 0x10,        // LD D, 10
            0xB8,              // CP B
            0x90,              // SUB B
            0xC3, 0x30, 0x02,  // JP 0230
            0x10, 0x00, 0x20,
        ])
        .collect();
        assert_eq!(insts.len(), 5);
        assert_eq!(&insts[0].to_string(), "LD B, C");
        assert_eq!(&insts[1].to_string(), "LD D, 10");
        assert_eq!(&insts[2].to_string(), "CP B");
        assert_eq!(&insts[3].to_string(), "SUB B");
        assert_eq!(&insts[4].to_string(), "JP 0230");
    }
}
