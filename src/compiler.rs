use crate::code_gen::Generator;
use crate::common::InterpretError;
use crate::heap::Heap;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::type_check::{generate_substitutions, Scope};
use crate::object::FunctionObj;

pub fn compile(
    source: &str,
    heap: &mut Heap,
    mut scope: &mut Scope,
) -> Result<FunctionObj, InterpretError> {
    let bytes: Vec<_> = source.bytes().collect();
    let lexer = Lexer::new(&bytes);
    let mut parser = Parser::new(lexer);
    parser.advance();
    let ast = parser.program();
    let func = parser.function; // TODO return from parser.program()?
    if parser.had_error {
        Err(InterpretError::Compile)
    } else {
        let substitutions = generate_substitutions(&ast, &mut scope);
        match substitutions {
            Ok(substitutions) => {
                let ast = (&ast).resolve_types(&substitutions, &mut scope)?;

                #[cfg(feature = "debug-logging")]
                eprintln!("{:?}", ast);

                let mut generator = Generator::new(func);
                ast.generate(&mut generator, heap);
                Ok(generator.end())
            }
            Err(e) => {
                eprintln!("{:?}", e);
                Err(InterpretError::Compile)
            }
        }
    }
}

