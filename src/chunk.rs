use std::convert::TryFrom;

#[derive(Debug)]
pub enum ByteCode {
    Return,
    PrintInt,
    PrintFloat,
    PrintBool,
    PrintStr,
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
    Not,
    GreaterInt,
    LessInt,
    GreaterEqualInt,
    LessEqualInt,
    GreaterFloat,
    LessFloat,
    GreaterEqualFloat,
    LessEqualFloat,
    Equal8, //each arg is 8 bytes
    NotEqual8, //each arg is 8 bytes
    Equal1,
    NotEqual1,
    EqualHeap,
    NotEqualHeap,
    Concat,
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

        let code = self.get_code(offset);
        match code {
            ByteCode::Constant(constant) => self.constant_instruction(constant),
            _ => println!("{:?}", code),
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
