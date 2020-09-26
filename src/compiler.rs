use crate::lexer::{Lexer, TokenType};

pub fn compile(source: &str) -> () {
    let bytes: Vec<_> = source.bytes().collect();
    let mut lexer = Lexer::new(&bytes);
    let mut line = u32::MAX;
    loop {
        let token = lexer.next();
        if token.line != line {
            print!("{line:>width$} ", line=line, width=4);
            line = token.line;
        } else {
            print!("   | ");
        }
        print!("{:?} '{}'", token.tok_type, token.source);
        
        if token.tok_type == TokenType::Eof {
            break;
        }
    }
}
