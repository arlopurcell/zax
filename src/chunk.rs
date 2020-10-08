use std::convert::{TryFrom, TryInto};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ByteCode {
    Return(u8),
    PrintInt,
    PrintFloat,
    PrintBool,
    PrintObject,
    Constant(u8, u8),
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
    Equal(u8),
    NotEqual(u8),
    EqualHeap,
    NotEqualHeap,
    Concat,
    Pop(usize),
    DefineGlobal(u8, u8),
    GetGlobal(u8, u8),
    SetGlobal(u8, u8),
    GetLocal(usize, u8),
    SetLocal(usize, u8),
    GetUpvalue(usize, u8),
    SetUpvalue(usize, u8),
    JumpIfFalse(u16),
    Jump(u16),
    Loop(u16),
    Call(usize),
    NoOp,
    ToHeap(u8),
    GetHeap,
}

impl ByteCode {
    pub fn size(&self) -> u8 {
        match self {
            Self::PrintInt
            | Self::PrintFloat
            | Self::PrintBool
            | Self::PrintObject
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
            | Self::EqualHeap
            | Self::NotEqualHeap
            | Self::Concat
            | Self::GetHeap
            | Self::NoOp => 1,
            Self::Return(_)
            | Self::Equal(_)
            | Self::NotEqual(_)
            | Self::ToHeap(_)
            => 2,
            Self::JumpIfFalse(_) | Self::Jump(_) | Self::Loop(_) 
            | Self::Constant(_, _)
            | Self::DefineGlobal(_, _)
            | Self::GetGlobal(_, _)
            | Self::SetGlobal(_, _)
                => 3,
            | Self::Pop(_)
            | Self::Call(_) => 9,
            Self::GetLocal(_, _) 
            | Self::SetLocal(_, _)
            | Self::GetUpvalue(_, _)
            | Self::SetUpvalue(_, _)
                => 10,
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
            0x0 => ByteCode::Return(self.get_u8(offset + 1)),
            0x1 => ByteCode::PrintInt,
            0x2 => ByteCode::PrintFloat,
            0x3 => ByteCode::PrintBool,
            0x4 => ByteCode::PrintObject,
            0x5 => ByteCode::Constant(self.get_u8(offset + 1), self.get_u8(offset + 2)),
            0x6 => ByteCode::NegateInt,
            0x7 => ByteCode::AddInt,
            0x8 => ByteCode::SubInt,
            0x9 => ByteCode::MulInt,
            0xa => ByteCode::DivInt,
            0xb => ByteCode::NegateFloat,
            0xc => ByteCode::AddFloat,
            0xd => ByteCode::SubFloat,
            0xe => ByteCode::MulFloat,
            0xf => ByteCode::DivFloat,
            0x10 => ByteCode::Not,
            0x11 => ByteCode::GreaterInt,
            0x12 => ByteCode::LessInt,
            0x13 => ByteCode::GreaterEqualInt,
            0x14 => ByteCode::LessEqualInt,
            0x15 => ByteCode::GreaterFloat,
            0x16 => ByteCode::LessFloat,
            0x17 => ByteCode::GreaterEqualFloat,
            0x18 => ByteCode::LessEqualFloat,
            0x19 => ByteCode::Equal(self.get_u8(offset + 1)),
            0x1a => ByteCode::NotEqual(self.get_u8(offset + 1)),
            0x1b => ByteCode::EqualHeap,
            0x1c => ByteCode::NotEqualHeap,
            0x1d => ByteCode::Concat,
            0x1e => ByteCode::Pop(self.get_usize(offset + 1)),
            0x1f => ByteCode::DefineGlobal(self.get_u8(offset + 1), self.get_u8(offset + 2)),
            0x20 => ByteCode::GetGlobal(self.get_u8(offset + 1), self.get_u8(offset + 2)),
            0x21 => ByteCode::SetGlobal(self.get_u8(offset + 1), self.get_u8(offset + 2)),
            0x22 => ByteCode::GetLocal(self.get_usize(offset +  1), self.get_u8(offset + 9)),
            0x23 => ByteCode::SetLocal(self.get_usize(offset +  1), self.get_u8(offset + 9)),
            0x24 => ByteCode::GetUpvalue(self.get_usize(offset +  1), self.get_u8(offset + 9)),
            0x25 => ByteCode::SetUpvalue(self.get_usize(offset +  1), self.get_u8(offset + 9)),
            0x26 => ByteCode::JumpIfFalse(self.get_u16(offset + 1)),
            0x27 => ByteCode::Jump(self.get_u16(offset + 1)),
            0x28 => ByteCode::Loop(self.get_u16(offset + 1)),
            0x29 => ByteCode::Call(self.get_usize(offset + 1)),
            0x2a => ByteCode::NoOp,
            0x2b => ByteCode::ToHeap(self.get_u8(offset + 1)),
            0x2c => ByteCode::GetHeap,
            _ => panic!("Invalid byte code: {}", byte),
        }
    }

    pub fn get_line(&self, offset: usize) -> u32 {
        let index = self.line_idx + offset * 4;
        u32::from_be_bytes(self.data[index..index + 4].try_into().unwrap())
    }

    pub fn get_constant(&self, constant: &u8, length: usize) -> &[u8] {
        let idx = *constant as usize;
        &self.constants()[idx..idx + length]
    }

    #[cfg(feature = "debug-logging")]
    pub fn disassemble(&self, name: &str) -> () {
        eprintln!("constants: {:?}", self.constants());
        eprintln!("== {} ==", name);

        let mut offset = 0;
        while offset < self.code().len() {
            //for offset in 0..self.code.len() {
            self.disassemble_instruction(offset);
            offset += self.get_code(offset).size() as usize;
        }
    }

    #[cfg(feature = "debug-logging")]
    pub fn disassemble_instruction(&self, offset: usize) -> () {
        eprint!("{offset:>width$} ", offset = offset, width = 4);
        if offset > 0 && self.get_line(offset) == self.get_line(offset - 1) {
            eprint!("   | ")
        } else {
            eprint!("{line:>width$} ", line = self.get_line(offset), width = 4);
        }

        let code = self.get_code(offset);

        eprint!("{:?}", self.get_code(offset));
        let slice = &self.code()[offset..offset + (code.size() as usize)];
        eprintln!(" ({:x?})", slice);
    }

    fn to_bytes(mut self) -> Vec<u8> {
        self.data
            .extend_from_slice(&self.constant_idx.to_be_bytes());
        self.data.extend_from_slice(&self.line_idx.to_be_bytes());
        self.data
    }

    fn from_bytes(bytes: &[u8]) -> Self {
        let mut data = bytes.to_vec();
        let constant_idx_bytes = [
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
        ];
        let constant_idx = usize::from_le_bytes(constant_idx_bytes);

        let line_idx_bytes = [
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
            data.pop().unwrap(),
        ];
        let line_idx = usize::from_le_bytes(line_idx_bytes);
        Self {
            data,
            constant_idx,
            line_idx,
        }
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
            ByteCode::Return(arg) => {
                self.code.push(0x0);
                self.code.push(arg);
                self.lines.push(line);
                self.lines.push(line);
            }
            ByteCode::PrintInt => {
                self.code.push(0x1);
                self.lines.push(line);
            }
            ByteCode::PrintFloat => {
                self.code.push(0x2);
                self.lines.push(line);
            }
            ByteCode::PrintBool => {
                self.code.push(0x3);
                self.lines.push(line);
            }
            ByteCode::PrintObject => {
                self.code.push(0x4);
                self.lines.push(line);
            }
            ByteCode::Constant(arg, length) => {
                self.code.push(0x5);
                self.code.push(arg);
                self.code.push(length);
                self.lines.push(line);
                self.lines.push(line);
                self.lines.push(line);
            }
            ByteCode::NegateInt => {
                self.code.push(0x6);
                self.lines.push(line);
            }
            ByteCode::AddInt => {
                self.code.push(0x7);
                self.lines.push(line);
            }
            ByteCode::SubInt => {
                self.code.push(0x8);
                self.lines.push(line);
            }
            ByteCode::MulInt => {
                self.code.push(0x9);
                self.lines.push(line);
            }
            ByteCode::DivInt => {
                self.code.push(0xa);
                self.lines.push(line);
            }
            ByteCode::NegateFloat => {
                self.code.push(0xb);
                self.lines.push(line);
            }
            ByteCode::AddFloat => {
                self.code.push(0xc);
                self.lines.push(line);
            }
            ByteCode::SubFloat => {
                self.code.push(0xd);
                self.lines.push(line);
            }
            ByteCode::MulFloat => {
                self.code.push(0xe);
                self.lines.push(line);
            }
            ByteCode::DivFloat => {
                self.code.push(0xf);
                self.lines.push(line);
            }
            ByteCode::Not => {
                self.code.push(0x10);
                self.lines.push(line);
            }
            ByteCode::GreaterInt => {
                self.code.push(0x11);
                self.lines.push(line);
            }
            ByteCode::LessInt => {
                self.code.push(0x12);
                self.lines.push(line);
            }
            ByteCode::GreaterEqualInt => {
                self.code.push(0x13);
                self.lines.push(line);
            }
            ByteCode::LessEqualInt => {
                self.code.push(0x14);
                self.lines.push(line);
            }
            ByteCode::GreaterFloat => {
                self.code.push(0x15);
                self.lines.push(line);
            }
            ByteCode::LessFloat => {
                self.code.push(0x16);
                self.lines.push(line);
            }
            ByteCode::GreaterEqualFloat => {
                self.code.push(0x17);
                self.lines.push(line);
            }
            ByteCode::LessEqualFloat => {
                self.code.push(0x18);
                self.lines.push(line);
            }
            ByteCode::Equal(length) => {
                self.code.push(0x19);
                self.code.push(length);
                self.lines.push(line);
                self.lines.push(line);
            }
            ByteCode::NotEqual(length) => {
                self.code.push(0x1a);
                self.code.push(length);
                self.lines.push(line);
                self.lines.push(line);
            }
            ByteCode::EqualHeap => {
                self.code.push(0x1b);
                self.lines.push(line);
            }
            ByteCode::NotEqualHeap => {
                self.code.push(0x1c);
                self.lines.push(line);
            }
            ByteCode::Concat => {
                self.code.push(0x1d);
                self.lines.push(line);
            }
            ByteCode::Pop(n) => {
                self.code.push(0x1e);
                self.code.extend_from_slice(&n.to_be_bytes());
                self.lines.extend_from_slice(&[line; 9]);
            }
            ByteCode::DefineGlobal(arg, n) => {
                self.code.push(0x1f);
                self.code.push(arg);
                self.code.push(n);
                self.lines.push(line);
                self.lines.push(line);
                self.lines.push(line);
            }
            ByteCode::GetGlobal(arg, n) => {
                self.code.push(0x20);
                self.code.push(arg);
                self.code.push(n);
                self.lines.push(line);
                self.lines.push(line);
                self.lines.push(line);
            }
            ByteCode::SetGlobal(arg, n) => {
                self.code.push(0x21);
                self.code.push(arg);
                self.code.push(n);
                self.lines.push(line);
                self.lines.push(line);
                self.lines.push(line);
            }
            ByteCode::GetLocal(arg, n) => {
                self.code.push(0x22);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.code.push(n);
                self.lines.extend_from_slice(&[line; 10]);
            }
            ByteCode::SetLocal(arg, n) => {
                self.code.push(0x23);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.code.push(n);
                self.lines.extend_from_slice(&[line; 10]);
            }
            ByteCode::GetUpvalue(arg, n) => {
                self.code.push(0x24);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.code.push(n);
                self.lines.extend_from_slice(&[line; 10]);
            }
            ByteCode::SetUpvalue(arg, n) => {
                self.code.push(0x25);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.code.push(n);
                self.lines.extend_from_slice(&[line; 10]);
            }
            ByteCode::JumpIfFalse(arg) => {
                self.code.push(0x26);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 3]);
            }
            ByteCode::Jump(arg) => {
                self.code.push(0x27);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 3]);
            }
            ByteCode::Loop(arg) => {
                self.code.push(0x28);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 3]);
            }
            ByteCode::Call(arg) => {
                self.code.push(0x29);
                self.code.extend_from_slice(&arg.to_be_bytes());
                self.lines.extend_from_slice(&[line; 9]);
            }
            ByteCode::NoOp => {
                self.code.push(0x2a);
                self.lines.push(line);
            }
            ByteCode::ToHeap(length) => {
                self.code.push(0x2b);
                self.code.push(length);
                self.lines.push(line);
                self.lines.push(line);
            }
            ByteCode::GetHeap => {
                self.code.push(0x2c);
                self.lines.push(line);
            }
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
