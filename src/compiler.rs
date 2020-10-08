use crate::code_gen::{FunctionType, Generator};
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
    if parser.had_error {
        Err(InterpretError::Compile)
    } else {
        analyze(&mut ast)?;
        let mut generator = Generator::new(FunctionType::Script);
        ast.generate(&mut generator, heap)
            .map(|()| {
                let chunk = generator.end();
                ClosureObj::new(chunk, 0, 0, heap)
            })
    }
}
