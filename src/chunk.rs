use std::convert::{TryFrom, TryInto};

use crate::common::ByteSerialize;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ByteCode {
    Return,
    PrintInt,
    PrintFloat,
    PrintBool,
    PrintStr,
    Constant1(u8),
    Constant8(u8),
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
    Equal8,
    NotEqual8,
    Equal1,
    NotEqual1,
    EqualHeap,
    NotEqualHeap,
    Concat,
    Pop8,
    Pop1,
    DefineGlobal1(u8),
    DefineGlobal8(u8),
    GetGlobal1(u8),
    GetGlobal8(u8),
    SetGlobal1(u8),
    SetGlobal8(u8),
    GetLocal1(usize),
    GetLocal8(usize),
    SetLocal1(usize),
    SetLocal8(usize),
    JumpIfFalse(u16),
    Jump(u16),
    Loop(u16),
}

impl ByteCode {
    pub fn size(&self) -> u8 {
        match self {
            Self::Return
            | Self::PrintInt
            | Self::PrintFloat
            | Self::PrintBool
            | Self::PrintStr
            | Self::NegateInt
            | Self::AddInt
            | Self::SubInt
            | Self::MulInt
            | Self::DivInt
            | Self::NegateFloat
            | Self::AddFloat
            | Self::SubFloat
            | Self::MulFloat
            | Self::DivFloat
            | Self::Not
            | Self::GreaterInt
            | Self::LessInt
            | Self::GreaterEqualInt
            | Self::LessEqualInt
            | Self::GreaterFloat
            | Self::LessFloat
            | Self::GreaterEqualFloat
            | Self::LessEqualFloat
            | Self::Equal8
            | Self::NotEqual8
            | Self::Equal1
            | Self::NotEqual1
            | Self::EqualHeap
            | Self::NotEqualHeap
            | Self::Concat
            | Self::Pop8
            | Self::Pop1 => 1,
            Self::Constant1(_)
            | Self::Constant8(_)
            | Self::DefineGlobal1(_)
            | Self::DefineGlobal8(_)
            | Self::GetGlobal1(_)
            | Self::GetGlobal8(_)
            | Self::SetGlobal1(_)
            | Self::SetGlobal8(_) => 2,
            Self::JumpIfFalse(_)
            | Self::Jump(_)
            | Self::Loop(_) => 3,
            Self::GetLocal1(_)
            | Self::GetLocal8(_)
            | Self::SetLocal1(_)
            | Self::SetLocal8(_) => 9,
        }
    }
}

pub struct ChunkBuilder {
    code: Vec<u8>,
    constants: Vec<u8>,
    // TODO save memory by using a run length encoding
    lines: Vec<u32>,
}

#[derive(PartialEq, Eq)]
pub struct Chunk {
    data: Vec<u8>,
    constant_idx: usize,
    line_idx: usize,
}

impl Chunk {
    pub fn new(builder: ChunkBuilder) -> Self {
        let constant_idx = builder.code.len();
        let line_idx = constant_idx + builder.constants.len();
        let mut data = builder.code;
        data.extend_from_slice(&builder.constants);
        for line in builder.lines.into_iter() {
            data.extend_from_slice(&line.to_be_bytes());
        }

        Self {
            data,
            constant_idx,
            line_idx,
        }
    }

    fn code(&self) -> &[u8] {
        &self.data[0..self.constant_idx]
    }

    fn constants(&self) -> &[u8] {
        &self.data[self.constant_idx..self.line_idx]
    }

    fn lines(&self) -> &[u8] {
        &self.data[self.line_idx..]
    }

    fn get_u8(&self, offset: usize) -> u8 {
        *self.code().get(offset).unwrap()
    }

    fn get_u16(&self, offset: usize) -> u16 {
        let bytes: &[u8] = &self.code()[offset..offset + 2];
        u16::from_be_bytes(bytes.try_into().unwrap())
    }

    fn get_usize(&self, offset: usize) -> usize {
        let bytes: &[u8] = &self.code()[offset..offset + 8];
        usize::from_be_bytes(bytes.try_into().unwrap())
    }

    pub fn get_code(&self, offset: usize) -> ByteCode {
        let byte = self.code().get(offset).unwrap();
        match byte {
            0x0 => ByteCode::Return,
            0x1 => ByteCode::PrintInt,
            0x2 => ByteCode::PrintFloat,
            0x3 => ByteCode::PrintBool,
            0x4 => ByteCode::PrintStr,
            0x5 => ByteCode::Constant1(self.get_u8(offset + 1)),
            0x6 => ByteCode::Constant8(self.get_u8(offset + 1)),
            0x7 => ByteCode::NegateInt,
            0x8 => ByteCode::AddInt,
            0x9 => ByteCode::SubInt,
            0xa => ByteCode::MulInt,
            0xb => ByteCode::DivInt,
            0xc => ByteCode::NegateFloat,
            0xd => ByteCode::AddFloat,
            0xe => ByteCode::SubFloat,
            0xf => ByteCode::MulFloat,
            0x10 => ByteCode::DivFloat,
            0x11 => ByteCode::Not,
            0x12 => ByteCode::GreaterInt,
            0x13 => ByteCode::LessInt,
            0x14 => ByteCode::GreaterEqualInt,
            0x15 => ByteCode::LessEqualInt,
            0x16 => ByteCode::GreaterFloat,
            0x17 => ByteCode::LessFloat,
            0x18 => ByteCode::GreaterEqualFloat,
            0x19 => ByteCode::LessEqualFloat,
            0x1a => ByteCode::Equal8,    //each arg is 8 bytes
            0x1b => ByteCode::NotEqual8, //each arg is 8 bytes
            0x1c => ByteCode::Equal1,
            0x1d => ByteCode::NotEqual1,
            0x1e => ByteCode::EqualHeap,
            0x1f => ByteCode::NotEqualHeap,
            0x20 => ByteCode::Concat,
            0x21 => ByteCode::Pop8,
            0x22 => ByteCode::Pop1,
            0x23 => ByteCode::DefineGlobal1(self.get_u8(offset + 1)),
            0x24 => ByteCode::DefineGlobal8(self.get_u8(offset + 1)),
            0x25 => ByteCode::GetGlobal1(self.get_u8(offset + 1)),
            0x26 => ByteCode::GetGlobal8(self.get_u8(offset + 1)),
            0x27 => ByteCode::SetGlobal1(self.get_u8(offset + 1)),
            0x28 => ByteCode::SetGlobal8(self.get_u8(offset + 1)),
            0x29 => ByteCode::GetLocal1(self.get_usize(offset + 1)),
            0x2a => ByteCode::GetLocal8(self.get_usize(offset + 1)),
            0x2b => ByteCode::SetLocal1(self.get_usize(offset + 1)),
            0x2c => ByteCode::SetLocal8(self.get_usize(offset + 1)),
            0x2d => ByteCode::JumpIfFalse(self.get_u16(offset + 1)),
            0x2e => ByteCode::Jump(self.get_u16(offset + 1)),
            0x2f => ByteCode::Loop(self.get_u16(offset + 1)),
            _ => panic!("Invalid byte code: {:x}", byte),
        }
    }

    pub fn get_line(&self, offset: usize) -> u32 {
        let index = self.line_idx + offset * 4;
        u32::from_be_bytes(self.data[index..index+4].try_into().unwrap())
    }

    pub fn get_constant(&self, constant: &u8, length: usize) -> &[u8] {
        let idx = *constant as usize;
        &self.constants()[idx..idx + length]
    }

    pub fn code_len(&self) -> usize {
        self.code().len()
    }


    #[cfg(feature = "debug-logging")]
    pub fn disassemble(&self, name: &str) -> () {
        println!("== {} ==", name);

        let mut offset = 0;
        while offset < self.code().len() {
        //for offset in 0..self.code.len() {
            self.disassemble_instruction(offset);
            offset += self.get_code(offset).size() as usize;
        }
    }

    #[cfg(feature = "debug-logging")]
    pub fn disassemble_instruction(&self, offset: usize) -> () {
        print!("{offset:>width$} ", offset = offset, width = 4);
        if offset > 0 && self.get_line(offset) == self.get_line(offset - 1) {
            print!("   | ")
        } else {
            print!(
                "{line:>width$} ",
                line = self.get_line(offset),
                width = 4
            );
        }

        let code = self.get_code(offset);

        print!("{:?}", self.get_code(offset));
        let slice = &self.code()[offset..offset+(code.size() as usize)];
        println!(" ({:x?})", slice);
    }

}

impl ChunkBuilder {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn code_len(&self) -> usize {
        self.code.len()
    }

    pub fn append(&mut self, code: ByteCode, line: u32) -> () {
        match code {
            ByteCode::Return => {
                self.code.push(0x0);
                self.lines.push(line);
            },
            ByteCode::PrintInt => {
                self.code.push(0x1);
                self.lines.push(line);
            },
            ByteCode::PrintFloat => {
                self.code.push(0x2);
                self.lines.push(line);
            },
            ByteCode::PrintBool => {
                self.code.push(0x3);
                self.lines.push(line);
            },
            ByteCode::PrintStr => {
                self.code.push(0x4);
                self.lines.push(line);
            },
            ByteCode::Constant1(arg) => {
                self.code.push(0x5);
                self.code.push(arg);
                self.lines.push(line);
                self.lines.push(line);
            },
            ByteCode::Constant8(arg) => {
                self.code.push(0x6);
                self.code.push(arg);
                self.lines.push(line);
                self.lines.push(line);
            },
            ByteCode::NegateInt => {
                self.code.push(0x7);
                self.lines.push(line);
            },
            ByteCode::AddInt => {
                self.code.push(0x8);
                self.lines.push(line);
            },
            ByteCode::SubInt => {
                self.code.push(0x9);
                self.lines.push(line);
            },
            ByteCode::MulInt => {
                self.code.push(0xa);
                self.lines.push(line);
            },
            ByteCode::DivInt => {
                self.code.push(0xb);
                self.lines.push(line);
            },
            ByteCode::NegateFloat => {
                self.code.push(0xc);
                self.lines.push(line);
            },
            ByteCode::AddFloat => {
                self.code.push(0xd);
                self.lines.push(line);
            },
            ByteCode::SubFloat => {
                self.code.push(0xe);
                self.lines.push(line);
            },
            ByteCode::MulFloat => {
                self.code.push(0xf);
                self.lines.push(line);
            },
            ByteCode::DivFloat => {
                self.code.push(0x10);
                self.lines.push(line);
            },
            ByteCode::Not => {
                self.code.push(0x11);
                self.lines.push(line);
            },
            ByteCode::GreaterInt => {
                self.code.push(0x12);
                self.lines.push(line);
            },
            ByteCode::LessInt => {
                self.code.push(0x13);
                self.lines.push(line);
            },
            ByteCode::GreaterEqualInt => {
                self.code.push(0x14);
                self.lines.push(line);
            },
            ByteCode::LessEqualInt => {
                self.code.push(0x15);
                self.lines.push(line);
            },
            ByteCode::GreaterFloat => {
                self.code.push(0x16);
                self.lines.push(line);
            },
            ByteCode::LessFloat => {
                self.code.push(0x17);
                self.lines.push(line);
            },
            ByteCode::GreaterEqualFloat => {
                self.code.push(0x18);
                self.lines.push(line);
            },
            ByteCode::LessEqualFloat => {
                self.code.push(0x19);
                self.lines.push(line);
            },
            ByteCode::Equal8 => {
                self.code.push(0x1a);
                self.lines.push(line);
            },
            ByteCode::NotEqual8 => {
                self.code.push(0x1b);
                self.lines.push(line);
            },
            ByteCode::Equal1 => {
                self.code.push(0x1c);
                self.lines.push(line);
            },
            ByteCode::NotEqual1 => {
                self.code.push(0x1d);
                self.lines.push(line);
            },
            ByteCode::EqualHeap => {
                self.code.push(0x1e);
                self.lines.push(line);
            },
            ByteCode::NotEqualHeap => {
                self.code.push(0x1f);
                self.lines.push(line);
            },
            ByteCode::Concat => {
                self.code.push(0x20);
                self.lines.push(line);
            },
            ByteCode::Pop8 => {
                self.code.push(0x21);
                self.lines.push(line);
            },
            ByteCode::Pop1 => {
                self.code.push(0x22);
                self.lines.push(line);
            },
            ByteCode::DefineGlobal1(arg) => {
                self.code.push(0x23);
                self.code.push(arg);
                self.lines.push(line);
                self.lines.push(line);
            },
            ByteCode::DefineGlobal8(arg) => {
                self.code.push(0x24);
                self.code.push(arg);
                self.lines.push(line);
                self.lines.push(line);
            },
            ByteCode::GetGlobal1(arg) => {
                self.code.push(0x25);
                self.code.push(arg);
                self.lines.push(line);
                self.lines.push(line);
            },
            ByteCode::GetGlobal8(arg) => {
                self.code.push(0x26);
                self.code.push(arg);
                self.lines.push(line);
                self.lines.push(line);
            },
            ByteCode::SetGlobal1(arg) => {
                self.code.push(0x27);
                self.code.push(arg);
                self.lines.push(line);
                self.lines.push(line);
            },
            ByteCode::SetGlobal8(arg) => {
                self.code.push(0x28);
                self.code.push(arg);
                self.lines.push(line);
                self.lines.push(line);
            },
            ByteCode::GetLocal1(arg) => {
                self.code.push(0x29);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 9]);
            },
            ByteCode::GetLocal8(arg) => {
                self.code.push(0x2a);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 9]);
            },
            ByteCode::SetLocal1(arg) => {
                self.code.push(0x2b);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 9]);
            },
            ByteCode::SetLocal8(arg) => {
                self.code.push(0x2c);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 9]);
            },
            ByteCode::JumpIfFalse(arg) => {
                self.code.push(0x2d);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 3]);
            },
            ByteCode::Jump(arg) => {
                self.code.push(0x2e);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 3]);
            },
            ByteCode::Loop(arg) => {
                self.code.push(0x2f);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 3]);
            },
        }
    }

    pub fn add_constant(&mut self, value: &[u8]) -> u8 {
        let next_start = self.constants.len();
        self.constants.extend_from_slice(value);
        u8::try_from(next_start).ok().expect("Too many constants")
    }

    pub fn patch_jump(&mut self, index: usize, offset: u16) -> () {
        let offset_bytes = offset.to_be_bytes();
        self.code[index - 1] = offset_bytes[0];
        self.code[index] = offset_bytes[1];
    }

}
