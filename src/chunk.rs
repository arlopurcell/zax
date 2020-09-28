use std::convert::TryFrom;
use std::convert::TryInto;

use crate::value::Value;

pub enum ByteCode {
    Return,
    PrintInt,
    PrintFloat,
    Constant(u8),
    NegateInt,
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    NegateFloat,
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
}

pub struct Chunk {
    code: Vec<ByteCode>,
    constants: Vec<u8>,
    // TODO save memory by using a run length encoding
    lines: Vec<u32>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn get_code(&self, offset: usize) -> &ByteCode {
        self.code.get(offset).unwrap()
    }

    pub fn get_constant(&self, constant: &u8, length: usize) -> &[u8] {
        let idx = *constant as usize;
        &self.constants[idx..idx+length]
    }

    pub fn append(&mut self, code: ByteCode, line: u32) -> () {
        self.code.push(code);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: &[u8]) -> u8 {
        let next_start = self.constants.len();
        self.constants.extend_from_slice(value);
        u8::try_from(next_start)
            .ok()
            .expect("Too many constants")
    }

    #[cfg(feature = "debug-logging")]
    pub fn disassemble(&self, name: &str) -> () {
        println!("== {} ==", name);

        for offset in 0..self.code.len() {
            self.disassemble_instruction(offset);
        }
    }

    #[cfg(feature = "debug-logging")]
    pub fn disassemble_instruction(&self, offset: usize) -> () {
        print!("{offset:>width$} ", offset = offset, width = 4);
        if offset > 0 && self.lines.get(offset).unwrap() == self.lines.get(offset - 1).unwrap() {
            print!("   | ")
        } else {
            print!(
                "{line:>width$} ",
                line = self.lines.get(offset).unwrap(),
                width = 4
            );
        }

        match self.get_code(offset) {
            ByteCode::Return => Chunk::simple_instruction("Return"),
            ByteCode::PrintInt => Chunk::simple_instruction("PrintInt"),
            ByteCode::PrintFloat => Chunk::simple_instruction("PrintFloat"),
            ByteCode::Constant(constant) => self.constant_instruction(constant),
            ByteCode::NegateInt => Chunk::simple_instruction("Negate"),
            ByteCode::AddInt => Chunk::simple_instruction("AddInt"),
            ByteCode::SubInt => Chunk::simple_instruction("SubInt"),
            ByteCode::MulInt => Chunk::simple_instruction("MulInt"),
            ByteCode::DivInt => Chunk::simple_instruction("DivInt"),
            ByteCode::NegateFloat => Chunk::simple_instruction("NegateFloat"),
            ByteCode::AddFloat => Chunk::simple_instruction("AddFloat"),
            ByteCode::SubFloat => Chunk::simple_instruction("SubFloat"),
            ByteCode::MulFloat => Chunk::simple_instruction("MulFloat"),
            ByteCode::DivFloat => Chunk::simple_instruction("DivFloat"),
        }
    }

    #[cfg(feature = "debug-logging")]
    fn simple_instruction(name: &str) -> () {
        println!("{}", name);
    }

    #[cfg(feature = "debug-logging")]
    fn constant_instruction(&self, constant: &u8) -> () {
        println!(
            "Constant {:>6} {:?}",
            constant,
            self.get_constant(constant, 8),
        );
    }
}
