use std::collections::HashMap;

use crate::ast::{NodeId, VarLocation};
use crate::chunk::{ByteCode, Chunk, ChunkBuilder};
use crate::heap::Heap;

pub struct GlobalGenerator<'a> {
    pub heap: &'a mut Heap,
    pub upvalue_allocations: HashMap<NodeId, usize>,
    pub var_locations: HashMap<NodeId, VarLocation>,
}

impl<'a> GlobalGenerator<'a> {
    pub fn new(heap: &'a mut Heap) -> Self {
        Self {
            heap,
            upvalue_allocations: HashMap::new(),
            var_locations: HashMap::new(),
        }
    }
}

pub struct ChunkGenerator {
    chunk_builder: ChunkBuilder,
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
            chunk_builder: ChunkBuilder::new(),
            func_type,
        }
    }

    pub fn emit_byte(&mut self, code: ByteCode, line: u32) -> () {
        self.chunk_builder.append(code, line)
    }

    pub fn emit_constant_1(&mut self, value: u8, line: u32) -> () {
        let constant = self.chunk_builder.add_constant(&[value]);
        self.emit_byte(ByteCode::Constant(constant, 1), line)
    }

    pub fn emit_constant_8(&mut self, value: &[u8], line: u32) -> () {
        let constant = self.chunk_builder.add_constant(value);
        self.emit_byte(ByteCode::Constant(constant, 8), line)
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

    pub fn end(mut self) -> Chunk {
        &mut self.emit_byte(ByteCode::Return(0), 0);
        Chunk::new(self.chunk_builder)
    }
}
