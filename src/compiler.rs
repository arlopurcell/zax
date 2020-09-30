use crate::chunk::Chunk;
use crate::code_gen::Generator;
use crate::common::InterpretError;
use crate::heap::Heap;
use crate::lexer::{Lexer, Token};
use crate::parser::Parser;
use crate::type_check::{generate_substitutions, Scope};

pub fn compile(
    source: &str,
    heap: &mut Heap,
    mut scope: &mut Scope,
) -> Result<Chunk, InterpretError> {
    let bytes: Vec<_> = source.bytes().collect();
    let lexer = Lexer::new(&bytes);
    let mut parser = Parser::new(lexer);
    parser.advance();
    let ast = parser.program();
    if parser.had_error {
        Err(InterpretError::Compile)
    } else {
        let substitutions = generate_substitutions(&ast, &mut scope);
        match substitutions {
            Ok(substitutions) => {
                let ast = (&ast).resolve_types(&substitutions, &mut scope)?;

                #[cfg(feature = "debug-logging")]
                eprintln!("{:?}", ast);

                let mut generator = Generator::new();
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

pub struct Compiler<'a> {
    locals: Vec<Local<'a>>,
    scope_depth: usize,
}

struct Local<'a> {
    name: Token<'a>,
    depth: usize,
}
