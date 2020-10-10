use crate::chunk::{ByteCode, Chunk};

pub struct ChunkGenerator {
    pub chunk: Chunk,
    pub func_type: FunctionType,
}

#[derive(PartialEq)]
pub enum FunctionType {
    Function,
    Script,
}

impl ChunkGenerator {
    pub fn new(func_type: FunctionType) -> Self {
        // TODO accept function name as argument for debugging
        Self {
            chunk: Chunk::new(),
            func_type,
        }
    }

    pub fn emit_byte(&mut self, code: ByteCode, line: u32) -> () {
        self.chunk.append(code, line)
    }

    pub fn emit_constant(&mut self, value: i64, line: u32) -> () {
        let constant = self.chunk.add_constant(value);
        self.emit_byte(ByteCode::Constant(constant), line)
    }

    pub fn add_constant(&mut self, value: i64) -> u8 {
        self.chunk.add_constant(value)
    }

    pub fn emit_jump_if_false(&mut self, line: u32) -> usize {
        self.emit_jump_code(ByteCode::JumpIfFalse(0xffff), line)
    }

    pub fn emit_jump(&mut self, line: u32) -> usize {
        self.emit_jump_code(ByteCode::Jump(0xffff), line)
    }

    fn emit_jump_code(&mut self, code: ByteCode, line: u32) -> usize {
        self.emit_byte(code, line);
        // -1 to get the index of the last element
        self.chunk.code_len() - 1
    }

    pub fn patch_jump(&mut self, index: usize) -> () {
        // calculate offset from absolute index
        let jump = (self.chunk.code_len() - 1 - index) as u16;

        if jump > u16::MAX {
            // TODO make compiler error
            panic!("too much code to jump over");
        }

        self.chunk.patch_jump(index, jump)
    }

    pub fn loop_start(&self) -> usize {
        self.chunk.code_len() - 1
    }

    pub fn emit_loop(&mut self, loop_start: usize, line: u32) -> () {
        let offset = (self.chunk.code_len() - loop_start + 2) as u16;

        if offset > u16::MAX {
            // TODO make compiler error
            panic!("too much code to loop over");
        }

        self.emit_byte(ByteCode::Loop(offset), line);
    }

    pub fn end(mut self) -> Chunk {
        &mut self.emit_byte(ByteCode::Return(0), 0);
        self.chunk
    }
}
