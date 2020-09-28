use crate::chunk::{ByteCode, Chunk};
use crate::value::Value;

pub struct Generator {
    compiling_chunk: Chunk,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            compiling_chunk: Chunk::new(),
        }
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.compiling_chunk
    }

    pub fn emit_byte(&mut self, code: ByteCode, line: u32) -> () {
        self.current_chunk_mut().append(code, line)
    }

    pub fn emit_constant(&mut self, value: Value, line: u32) -> () {
        let constant = self.current_chunk_mut().add_constant(value);
        self.emit_byte(ByteCode::Constant(constant), line)
    }

    pub fn end(mut self) -> Chunk {
        &mut self.emit_byte(ByteCode::Return, 0);

        /*
        #[cfg(feature = "debug-print-code")]
        self.compiling_chunk.disassemble("code");
        */

        self.compiling_chunk
    }
}
