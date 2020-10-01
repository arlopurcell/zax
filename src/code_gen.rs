use crate::chunk::{ByteCode, Chunk, ChunkBuilder};
use crate::object::FunctionObj;

pub struct Generator {
    //function: FunctionObj,
    chunk_builder: ChunkBuilder,
}

enum FunctionType {
    Function,
    Script,
}


impl Generator {
    pub fn new() -> Self {
        // TODO accept function name as argument for debugging
        Self { 
            //function: FunctionObj::new(), 
            chunk_builder: ChunkBuilder::new(),
        }
    }

    /*
    pub fn current_chunk(&self) -> &Chunk {
        &self.function.chunk
    }

    pub fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }
    */

    pub fn emit_byte(&mut self, code: ByteCode, line: u32) -> () {
        self.chunk_builder.append(code, line)
    }

    pub fn emit_constant_1(&mut self, value: u8, line: u32) -> () {
        let constant = self.chunk_builder.add_constant(&[value]);
        self.emit_byte(ByteCode::Constant1(constant), line)
    }

    pub fn emit_constant_8(&mut self, value: &[u8], line: u32) -> () {
        let constant = self.chunk_builder.add_constant(value);
        self.emit_byte(ByteCode::Constant8(constant), line)
    }

    pub fn add_constant(&mut self, value: &[u8]) -> u8 {
        self.chunk_builder.add_constant(value)
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
        self.chunk_builder.code_len() - 1
    }

    pub fn patch_jump(&mut self, index: usize) -> () {
        // calculate offset from absolute index
        let jump = (self.chunk_builder.code_len() - 1 - index) as u16;

        if jump > u16::MAX {
            // TODO make compiler error
            panic!("too much code to jump over");
        }

        self.chunk_builder.patch_jump(index, jump)
    }

    pub fn loop_start(&self) -> usize {
        self.chunk_builder.code_len() - 1
    }

    pub fn emit_loop(&mut self, loop_start: usize, line: u32) -> () {
        let offset = (self.chunk_builder.code_len() - loop_start + 2) as u16;

        if offset > u16::MAX {
            // TODO make compiler error
            panic!("too much code to loop over");
        }

        self.emit_byte(ByteCode::Loop(offset), line);
    }

    pub fn end(mut self) -> FunctionObj {
        &mut self.emit_byte(ByteCode::Return, 0);
        FunctionObj::new(Chunk::new(self.chunk_builder))
    }
}
