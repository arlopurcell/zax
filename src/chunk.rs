use std::convert::TryFrom;

use crate::value::Value;

pub enum ByteCode {
    Return,
    Constant(u8),
    Negate,
    Add,
    Sub,
    Mul,
    Div,
}

pub struct Chunk {
    code: Vec<ByteCode>,
    lines: Vec<u32>,
    constants: Vec<Value>,
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

    pub fn get_constant(&self, constant: &u8) -> &Value {
        self.constants.get(*constant as usize).unwrap()
    }

    pub fn disassemble(&self, name: &str) -> () {
        println!("== {} ==", name);

        for offset in 0..self.code.len() {
            self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> () {
        print!("{offset:>width$} ", offset=offset, width=4);
        if offset > 0 && self.lines.get(offset).unwrap() == self.lines.get(offset - 1).unwrap() {
            print!("   | ")
        } else {
            print!("{line:>width$} ", line=self.lines.get(offset).unwrap(), width=4);
        }

        match self.get_code(offset) {
            ByteCode::Return => Chunk::simple_instruction("Return"),
            ByteCode::Constant(constant) => self.constant_instruction(constant),
            ByteCode::Negate => Chunk::simple_instruction("Negate"),
            ByteCode::Add => Chunk::simple_instruction("Add"),
            ByteCode::Sub => Chunk::simple_instruction("Sub"),
            ByteCode::Mul => Chunk::simple_instruction("Mul"),
            ByteCode::Div => Chunk::simple_instruction("Div"),
        }
    }

    pub fn append(&mut self, code: ByteCode, line: u32) -> () {
        self.code.push(code);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.push(value);
        u8::try_from(self.constants.len() - 1).ok().expect("Too many constants")
    }

    fn simple_instruction(name: &str) -> () {
        println!("{}", name);
    }

    fn constant_instruction(&self, constant: &u8) -> () {
        print!("Constant {constant:>width$} ", constant=constant, width=6);
        self.get_constant(constant).print();
        println!();
    }
}



