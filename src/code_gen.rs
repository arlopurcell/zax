use crate::chunk::{ByteCode, Chunk, ChunkBuilder};
use crate::object::FunctionObj;

pub struct Generator {
    chunk_builder: ChunkBuilder,
    locals: Vec<Local>,
    pub scope_depth: usize,
}

enum FunctionType {
    Function,
    Script,
}

#[derive(Debug)]
struct Local {
    name: String,
    depth: usize,
    index: usize,
    size: u8,
}

impl Generator {
    pub fn new() -> Self {
        // TODO accept function name as argument for debugging
        Self {
            //function: FunctionObj::new(),
            chunk_builder: ChunkBuilder::new(),
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    pub fn add_local(&mut self, name: &str, size: u8) -> () {
        let index = if self.locals.is_empty() {
            0
        } else {
            self.locals
                .get(self.locals.len() - 1)
                .map(|l| l.index + l.size as usize)
                .unwrap_or(0)
        };
        self.locals.push(Local {
            name: name.to_string(),
            depth: self.scope_depth,
            index,
            size,
        })
    }

    pub fn resolve_local(&self, name: &str) -> Option<usize> {
        self.locals
            .iter()
            .rev()
            .find(|local| local.name == name)
            .map(|local| local.index)
    }

    pub fn begin_scope(&mut self) -> () {
        self.scope_depth += 1;
    }

    pub fn end_scope(&mut self) -> usize {
        self.scope_depth -= 1;

        let mut pop_n = 0;
        let mut truncate_size = 0;
        for (index, local) in self.locals.iter().enumerate().rev() {
            if local.depth <= self.scope_depth {
                truncate_size = index + 1;
                pop_n = local.index + local.size as usize;
            }
        }

        self.locals.truncate(truncate_size);
        pop_n
        //self.locals.retain(|l| l.depth <= scope_depth);
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
