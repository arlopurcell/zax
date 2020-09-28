use std::mem::swap;

use crate::ast::{AstNode, AstNodeType, Operator};
use crate::chunk::{Chunk, ByteCode};
use crate::code_gen::Generator;
use crate::common::InterpretError;
use crate::lexer::{Lexer, Token, TokenType};
use crate::type_check::{generate_substitutions, DataType};
use crate::heap::Heap;

pub fn compile(source: &str, heap: &mut Heap) -> Result<Chunk, InterpretError> {
    let bytes: Vec<_> = source.bytes().collect();
    let lexer = Lexer::new(&bytes);
    let mut parser = Parser::new(lexer);
    parser.advance();
    let ast = parser.program();
    if parser.had_error {
        Err(InterpretError::Compile)
    } else {
        let substitutions = generate_substitutions(&ast);
        match substitutions {
            Ok(substitutions) => {
                let ast = (&ast).resolve_types(&substitutions)?;

                #[cfg(feature = "debug-logging")]
                eprintln!("{:?}", ast);

                let mut generator = Generator::new();
                let dt = ast.data_type.clone();
                ast.generate(&mut generator, heap);
                /*
                match dt {
                    Some(DataType::Int) => generator.emit_byte(ByteCode::PrintInt, 0),
                    Some(DataType::Float) => generator.emit_byte(ByteCode::PrintFloat, 0),
                    Some(DataType::Str) => generator.emit_byte(ByteCode::PrintStr, 0),
                    Some(DataType::Bool) => generator.emit_byte(ByteCode::PrintBool, 0),
                    _ => (),
                }
                */
                Ok(generator.end())
            }
            Err(e) => {
                eprintln!("{:?}", e);
                Err(InterpretError::Compile)
            }
        }
    }
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token<'a>,
    previous: Token<'a>,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
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
        if self.panic_mode {
            return;
        } // Silence errors until sync point
        self.panic_mode = true;
        eprint!("[line {}] Error", self.current.line);
        match self.current.tok_type {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => (),
            _ => eprint!(" at '{}'", self.current.source),
        }

        eprintln!(": {}", message);
        self.had_error = true;
    }

    fn error(&mut self, message: &str) -> () {
        if self.panic_mode {
            return;
        } // Silence errors until sync point
        self.panic_mode = true;
        eprint!("[line {}] Error", self.previous.line);
        match self.previous.tok_type {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => (),
            _ => eprint!(" at '{}'", self.previous.source),
        }

        eprintln!(": {}", message);
        self.had_error = true;
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> AstNode<'a> {
        self.advance();
        let mut node = match self.previous.tok_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus => self.unary(),
            TokenType::Bang => self.unary(),
            TokenType::Integer => self.integer(),
            TokenType::Float => self.float(),
            TokenType::True => AstNode::new(self.previous.line, AstNodeType::BoolLiteral(true)),
            TokenType::False => AstNode::new(self.previous.line, AstNodeType::BoolLiteral(false)),
            TokenType::Str => self.string(),
            _ => {
                self.error("Expect expression.");
                AstNode::new(self.previous.line, AstNodeType::Error)
            }
        };

        while precedence <= infix_left_precedence(&self.current.tok_type) {
            self.advance();
            node = match self.previous.tok_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Star
                | TokenType::Slash
                | TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual
                | TokenType::EqualEqual
                | TokenType::BangEqual => self.binary(node),
                _ => {
                    self.error("Unreachable no infix parse function");
                    AstNode::new(self.previous.line, AstNodeType::Error)
                }
            };
        }
        node
    }

    fn program(&mut self) -> AstNode<'a> {
        let mut statements = Vec::new();
        while !self.match_tok(TokenType::Eof) {
            statements.push(self.declaration());
        }
        AstNode::new(1, AstNodeType::Program(statements))
    }

    fn match_tok(&mut self, tok_type: TokenType) -> bool {
        if !self.check(tok_type) { 
            false
        } else {
            self.advance();
            true
        }
    }

    fn check(&mut self, tok_type: TokenType) -> bool {
        self.current.tok_type == tok_type
    }

    fn declaration(&mut self) -> AstNode<'a> {
        self.statement()
    }

    fn statement(&mut self) -> AstNode<'a> {
        if self.match_tok(TokenType::Print) {
            self.print_statement()
        } else {
            self.error("Unknown statement type");
            AstNode::new(self.previous.line, AstNodeType::Error)
        }
    }

    fn print_statement(&mut self) -> AstNode<'a> {
        let line = self.previous.line;
        let e = self.expression();
        self.consume(TokenType::SemiColon, "Expect ';' after print statement.");
        AstNode::new(line, AstNodeType::PrintStatement(Box::new(e)))
    }

    fn expression(&mut self) -> AstNode<'a> {
        self.parse_precedence(Precedence::Base)
    }

    fn integer(&self) -> AstNode<'a> {
        AstNode::new(
            self.previous.line,
            AstNodeType::IntLiteral(self.previous.source.parse::<i64>().unwrap()),
        )
    }

    fn float(&self) -> AstNode<'a> {
        AstNode::new(
            self.previous.line,
            AstNodeType::FloatLiteral(self.previous.source.parse::<f64>().unwrap()),
        )
    }

    fn string(&self) -> AstNode<'a> {
        AstNode::new(self.previous.line, AstNodeType::StrLiteral(&self.previous.source[1..self.previous.source.len()-1]))
    }

    fn grouping(&mut self) -> AstNode<'a> {
        let e = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
        e
    }

    fn unary(&mut self) -> AstNode<'a> {
        let operator = match self.previous.tok_type {
            TokenType::Minus => Operator::Neg,
            TokenType::Bang => Operator::Not,
            _ => panic!("Unreachable unary operator"),
        };
        let line = self.previous.line;
        let operand = self.parse_precedence(prefix_precedence(&self.previous.tok_type));
        AstNode::new(line, AstNodeType::Unary(operator, Box::new(operand)))
    }

    fn binary(&mut self, lhs: AstNode<'a>) -> AstNode<'a> {
        let operator = match self.previous.tok_type {
            TokenType::Plus => Operator::Add,
            TokenType::Minus => Operator::Sub,
            TokenType::Slash => Operator::Div,
            TokenType::Star => Operator::Mul,
            TokenType::Greater => Operator::Greater,
            TokenType::Less => Operator::Less,
            TokenType::GreaterEqual => Operator::GreaterEqual,
            TokenType::LessEqual => Operator::LessEqual,
            TokenType::EqualEqual => Operator::Equal,
            TokenType::BangEqual => Operator::NotEqual,

            _ => panic!("Unreachable binary operator"),
        };
        let line = self.previous.line;
        let precedence = infix_right_precedence(&self.previous.tok_type);
        let rhs = self.parse_precedence(precedence);
        AstNode::new(
            line,
            AstNodeType::Binary(operator, Box::new(lhs), Box::new(rhs)),
        )
    }
}

fn prefix_precedence(tok_type: &TokenType) -> Precedence {
    match tok_type {
        TokenType::LeftParen => Precedence::Nothing,
        TokenType::Minus => Precedence::UnaryRight,
        TokenType::Bang => Precedence::UnaryRight,
        _ => Precedence::Nothing,
    }
}

fn infix_left_precedence(tok_type: &TokenType) -> Precedence {
    match tok_type {
        TokenType::Minus => Precedence::TermLeft,
        TokenType::Plus => Precedence::TermLeft,
        TokenType::Slash => Precedence::FactorLeft,
        TokenType::Star => Precedence::FactorLeft,
        TokenType::Greater => Precedence::ComparisonLeft,
        TokenType::Less => Precedence::ComparisonLeft,
        TokenType::GreaterEqual => Precedence::ComparisonLeft,
        TokenType::LessEqual => Precedence::ComparisonLeft,
        TokenType::EqualEqual => Precedence::EqualityLeft,
        TokenType::BangEqual => Precedence::EqualityLeft,
        _ => Precedence::Nothing,
    }
}

fn infix_right_precedence(tok_type: &TokenType) -> Precedence {
    match tok_type {
        TokenType::Minus => Precedence::TermRight,
        TokenType::Plus => Precedence::TermRight,
        TokenType::Slash => Precedence::FactorRight,
        TokenType::Star => Precedence::FactorRight,
        TokenType::Greater => Precedence::ComparisonRight,
        TokenType::Less => Precedence::ComparisonRight,
        TokenType::GreaterEqual => Precedence::ComparisonRight,
        TokenType::LessEqual => Precedence::ComparisonRight,
        TokenType::EqualEqual => Precedence::EqualityRight,
        TokenType::BangEqual => Precedence::EqualityRight,
        _ => Precedence::Nothing,
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
