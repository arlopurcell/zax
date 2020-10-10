use crate::ast::analyze;
use crate::code_gen::{ChunkGenerator, FunctionType};
use crate::common::{InterpretError, InterpretResult};
use crate::lexer::Lexer;
use crate::object::FunctionObj;
use crate::parser::Parser;
use crate::vm::VM;

pub fn compile(source: &str, vm: &mut VM) -> InterpretResult {
    let bytes: Vec<_> = source.bytes().collect();
    let lexer = Lexer::new(&bytes);
    let mut parser = Parser::new(lexer);
    parser.advance();
    let mut ast = parser.program();
    if parser.had_error {
        Err(InterpretError::Compile)
    } else {
        analyze(&mut ast, vm)?;
        ast.generate(vm)?;
        Ok(())
    }
}
