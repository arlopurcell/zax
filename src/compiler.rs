use std::collections::HashMap;

use crate::code_gen::{FunctionType, ChunkGenerator, GlobalGenerator};
use crate::common::InterpretError;
use crate::heap::Heap;
use crate::lexer::Lexer;
use crate::object::ClosureObj;
use crate::parser::Parser;
use crate::type_check::{final_type_check, generate_substitutions};
use crate::ast::analyze;

pub fn compile(source: &str, heap: &mut Heap) -> Result<ClosureObj, InterpretError> {
    let bytes: Vec<_> = source.bytes().collect();
    let lexer = Lexer::new(&bytes);
    let mut parser = Parser::new(lexer);
    parser.advance();
    let mut ast = parser.program();
    let mut global_generator = GlobalGenerator::new(heap);
    if parser.had_error {
        Err(InterpretError::Compile)
    } else {
        analyze(&mut ast, &mut global_generator)?;
        let mut generator = ChunkGenerator::new(FunctionType::Script);
        ast.generate(&mut generator, &mut global_generator)
            .map(|()| {
                let chunk = generator.end();
                ClosureObj::new(chunk, "main", 0, Vec::new(), heap)
            })
    }
}
