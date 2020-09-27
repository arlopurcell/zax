use std::mem::swap;

use crate::lexer::{Lexer, TokenType, Token};
use crate::chunk::Chunk;
use crate::common::InterpretError;
use crate::ast::{AstNode, Operator};
use crate::code_gen::Generator;

pub fn compile(source: &str) -> Result<Chunk, InterpretError> {
    let bytes: Vec<_> = source.bytes().collect();
    let lexer = Lexer::new(&bytes);
    let mut parser = Parser::new(lexer);
    parser.advance();
    let ast = parser.expression();
    parser.consume(TokenType::Eof, "Expect end of expression");
    if parser.had_error {
        Err(InterpretError::Compile)
    } else {
        let mut generator = Generator::new();
        ast.generate(&mut generator);
        Ok(generator.end())
    }
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token<'a>,
    previous: Token<'a>,
    had_error: bool,
    panic_mode: bool,
}

impl <'a> Parser<'a> {
    fn new(lexer: Lexer<'a>) -> Self {
        let current = lexer.error_token("Before start");
        let previous = lexer.error_token("Before start");
        Self {
            lexer,
            current,
            previous,
            had_error: false,
            panic_mode: false,
        }
    }

    fn advance(&mut self) -> () {
        swap(&mut self.current, &mut self.previous);
        loop {
            self.current = self.lexer.next();

            #[cfg(feature = "debug-logging")]
            {
                if (self.current.line != self.previous.line) {
                    print!("{:>4} ", self.current.line);
                } else {
                    print!("   | ");
                }
                println!("{:?} '{}'", self.current.tok_type, self.current.source); 
            }


            if self.current.tok_type != TokenType::Error {
                break;
            }
            self.error_at_current(self.current.source);
        }
    }

    fn consume(&mut self, tok_type: TokenType, message: &str) -> () {
        if self.current.tok_type == tok_type {
            self.advance();
        } else {
            self.error_at_current(message);
        }
    }

    fn error_at_current(&mut self, message: &str) -> () {
        if self.panic_mode { return } // Silence errors until sync point
        self.panic_mode = true;
        eprint!("[line {}] Error", self.current.line);
        match self.current.tok_type {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => (),
            _ => eprint!(" at '{}'", self.current.source)
        }

        eprintln!(": {}", message);
        self.had_error = true;
    }

    fn error(&mut self, message: &str) -> () {
        if self.panic_mode { return } // Silence errors until sync point
        self.panic_mode = true;
        eprint!("[line {}] Error", self.previous.line);
        match self.previous.tok_type {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => (),
            _ => eprint!(" at '{}'", self.previous.source)
        }

        eprintln!(": {}", message);
        self.had_error = true;
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> AstNode<'a> {
        self.advance();
        let mut node = match self.previous.tok_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus => self.unary(),
            TokenType::Integer => self.integer(),
            TokenType::Float => self.float(),
            _ => {
                self.error("Expect expression.");
                AstNode::Error
            },
        };

        while precedence.value() <= infix_left_precedence(&self.current.tok_type).value() {
            self.advance();
            node = match self.previous.tok_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Star
                | TokenType::Slash => self.binary(node),
                _ => {
                    self.error("Unreachable no infix parse function");
                    AstNode::Error
                }
            };
        }
        node
    }
    
    fn expression(&mut self) -> AstNode<'a> {
        self.parse_precedence(Precedence::Base)
    }

    fn integer(&self) -> AstNode<'a> {
        AstNode::IntLiteral(self.previous.line, self.previous.source.parse::<u32>().unwrap())
    }

    fn float(&self) -> AstNode<'a> {
        AstNode::FloatLiteral(self.previous.line, self.previous.source.parse::<f32>().unwrap())
    }

    fn grouping(&mut self) -> AstNode<'a> {
        let e = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
        e
    }

    fn unary(&mut self) -> AstNode<'a> {
        let operator = match self.previous.tok_type {
            TokenType::Minus => Operator::Neg,
            _ => panic!("Unreachable unary operator"),
        };
        let line = self.previous.line;
        let operand = self.parse_precedence(prefix_precedence(&self.previous.tok_type));
        AstNode::Unary(line, operator, Box::new(operand))
    }

    fn binary(&mut self, lhs: AstNode<'a>) -> AstNode<'a> {
        let operator = match self.previous.tok_type {
            TokenType::Plus => Operator::Add,
            TokenType::Minus => Operator::Sub,
            TokenType::Slash => Operator::Div,
            TokenType::Star => Operator::Mul,

            _ => panic!("Unreachable binary operator"),
        };
        let line = self.previous.line;
        let precedence = infix_right_precedence(&self.previous.tok_type);
        let rhs = self.parse_precedence(precedence);
        AstNode::Binary(line, operator, Box::new(lhs), Box::new(rhs))
    }


}

fn prefix_precedence(tok_type: &TokenType) -> Precedence {
    match tok_type {
        TokenType::LeftParen => Precedence::Nothing,
        TokenType::Minus => Precedence::UnaryRight,
        _ => Precedence::Nothing,
    }
}

fn infix_left_precedence(tok_type: &TokenType) -> Precedence {
    match tok_type {
        TokenType::Minus => Precedence::TermLeft,
        TokenType::Plus => Precedence::TermLeft,
        TokenType::Slash => Precedence::FactorLeft,
        TokenType::Star => Precedence::FactorLeft,
        _ => Precedence::Nothing,
    }
}

fn infix_right_precedence(tok_type: &TokenType) -> Precedence {
    match tok_type {
        TokenType::Minus => Precedence::TermRight,
        TokenType::Plus => Precedence::TermRight,
        TokenType::Slash => Precedence::FactorRight,
        TokenType::Star => Precedence::FactorRight,
        _ => Precedence::Nothing,
    }
}

enum Precedence {
    Nothing,
    Base,
    AssignmentLeft,
    AssignmentRight,
    OrLeft,
    OrRight,
    AndLeft,
    AndRight,
    EqualityLeft,
    EqualityRight,
    ComparisonLeft,
    ComparisonRight,
    TermLeft,
    TermRight,
    FactorLeft,
    FactorRight,
    UnaryLeft,
    UnaryRight,
    CallLeft,
    CallRight,
    PrimaryLeft,
    PrimaryRight,
}

impl Precedence {
    fn value(&self) -> u8 {
        match self {
            Self::Nothing => 0,
            Self::Base => 1,
            Self::AssignmentLeft => 2,
            Self::AssignmentRight => 3,
            Self::OrLeft => 4,
            Self::OrRight => 5,
            Self::AndLeft => 6,
            Self::AndRight => 7,
            Self::EqualityLeft => 8,
            Self::EqualityRight => 9,
            Self::ComparisonLeft => 10,
            Self::ComparisonRight => 11,
            Self::TermLeft => 12,
            Self::TermRight => 13,
            Self::FactorLeft => 14,
            Self::FactorRight => 15,
            Self::UnaryLeft => 16,
            Self::UnaryRight => 17,
            Self::CallLeft => 18,
            Self::CallRight => 19,
            Self::PrimaryLeft => 20,
            Self::PrimaryRight => 21,
        }
    }
}


