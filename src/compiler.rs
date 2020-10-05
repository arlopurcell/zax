use crate::code_gen::Generator;
use crate::common::InterpretError;
use crate::heap::Heap;
use crate::lexer::Lexer;
use crate::object::FunctionObj;
use crate::parser::Parser;
use crate::type_check::generate_substitutions;

pub fn compile(
    source: &str,
    heap: &mut Heap,
) -> Result<FunctionObj, InterpretError> {
    let bytes: Vec<_> = source.bytes().collect();
    let lexer = Lexer::new(&bytes);
    let mut parser = Parser::new(lexer);
    parser.advance();
    let mut ast = parser.program();
    if parser.had_error {
        Err(InterpretError::Compile)
    } else {
        let substitutions = generate_substitutions(&ast);
        match substitutions {
            Ok(substitutions) => {
                ast.resolve_types(&substitutions)?;

                #[cfg(feature = "debug-logging")]
                eprintln!("{:#?}", ast);

                let mut generator = Generator::new();
                ast.generate(&mut generator, heap);
                let func_obj = generator.end(0);
                Ok(func_obj)
            }
            Err(e) => {
                eprintln!("{:?}", e);
                Err(InterpretError::Compile)
            }
        }
    }
}
