use crate::ast::analyze;
use crate::code_gen::{ChunkGenerator, FunctionType, GlobalGenerator};
use crate::common::InterpretError;
use crate::heap::Heap;
use crate::lexer::Lexer;
use crate::object::FunctionObj;
use crate::parser::Parser;

pub fn compile(source: &str, heap: &mut Heap) -> Result<FunctionObj, InterpretError> {
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
                FunctionObj::new("main", chunk, 0, heap)
            })
    }
}
