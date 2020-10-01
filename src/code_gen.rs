use crate::chunk::{ByteCode, Chunk};
use crate::object::FunctionObj;

pub struct Generator {
    function: FunctionObj,
}

enum FunctionType {
    Function,
    Script,
}


impl Generator {
    pub fn new() -> Self {
        // TODO accept function name as argument for debugging
        Self { 
            function: FunctionObj::new(), 
        }
    }

    pub fn current_chunk(&self) -> &Chunk {
        &self.function.chunk
    }

    pub fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }

    pub fn emit_byte(&mut self, code: ByteCode, line: u32) -> () {
        self.current_chunk_mut().append(code, line)
    }

    pub fn emit_constant_1(&mut self, value: u8, line: u32) -> () {
        let constant = self.current_chunk_mut().add_constant(&[value]);
        self.emit_byte(ByteCode::Constant1(constant), line)
    }

    pub fn emit_constant_8(&mut self, value: &[u8], line: u32) -> () {
        let constant = self.current_chunk_mut().add_constant(value);
        self.emit_byte(ByteCode::Constant8(constant), line)
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
        self.current_chunk().len() - 1
    }

    pub fn patch_jump(&mut self, index: usize) -> () {
        let chunk = self.current_chunk_mut();
        // calculate offset from absolute index
        let jump = (chunk.len() - 1 - index) as u16;

        if jump > u16::MAX {
            // TODO make compiler error
            panic!("too much code to jump over");
        }

        chunk.patch_jump(index, jump)
    }

    pub fn loop_start(&self) -> usize {
        self.current_chunk().len() - 1
    }

    pub fn emit_loop(&mut self, loop_start: usize, line: u32) -> () {
        let offset = (self.current_chunk().len() - loop_start + 2) as u16;

        if offset > u16::MAX {
            // TODO make compiler error
            panic!("too much code to loop over");
        }

        self.emit_byte(ByteCode::Loop(offset), line);
    }

    pub fn end(mut self) -> FunctionObj {
        &mut self.emit_byte(ByteCode::Return, 0);
        self.function
    }
}
