use std::mem::swap;

use crate::ast::{AstNode, AstNodeType, Operator};
use crate::lexer::{Lexer, Token, TokenType};
use crate::type_check::DataType;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token<'a>,
    previous: Token<'a>,
    pub had_error: bool,
    panic_mode: bool,
    counter: u64,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let current = lexer.error_token("Before start");
        let previous = lexer.error_token("Before start");
        Self {
            lexer,
            current,
            previous,
            had_error: false,
            panic_mode: false,
            counter: 0,
            //locals: vec![Local { name: "", depth: 0 }], // TODO idk why i'm doing this yet. it's
            //for the script's funtion name: ""
        }
    }

    fn id(&mut self) -> u64 {
        self.counter += 1;
        self.counter
    }

    pub fn advance(&mut self) -> () {
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

    fn parse_precedence(&mut self, precedence: Precedence) -> AstNode {
        self.advance();
        let mut node = match self.previous.tok_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus => self.unary(),
            TokenType::Bang => self.unary(),
            TokenType::Integer => self.integer(),
            TokenType::Float => self.float(),
            TokenType::True => AstNode::new(
                self.id(),
                self.previous.line,
                AstNodeType::BoolLiteral(true),
            ),
            TokenType::False => AstNode::new(
                self.id(),
                self.previous.line,
                AstNodeType::BoolLiteral(false),
            ),
            TokenType::Str => self.string(),
            TokenType::Identifier => self.variable(),
            _ => {
                self.error("Expect expression.");
                AstNode::new(self.id(), self.previous.line, AstNodeType::Error)
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
                | TokenType::BangEqual
                | TokenType::And
                | TokenType::Or
                | TokenType::Equal => self.binary(node),
                TokenType::LeftParen => self.call(node),
                _ => {
                    self.error("Unreachable no infix parse function");
                    AstNode::new(self.id(), self.previous.line, AstNodeType::Error)
                }
            };
        }
        node
    }

    pub fn program(&mut self) -> AstNode {
        let mut statements = Vec::new();
        while !self.match_tok(TokenType::Eof) {
            statements.push(self.declaration());
        }
        AstNode::new(self.id(), 1, AstNodeType::Program(statements))
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

    fn synchronize(&mut self) -> () {
        self.panic_mode = false;

        // Consume tokens until either after a semicolon or before a statement starting token
        while self.current.tok_type != TokenType::Eof {
            if self.previous.tok_type == TokenType::SemiColon {
                break;
            }

            match self.current.tok_type {
                TokenType::Fun
                | TokenType::Let
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => break,
                _ => (),
            }
            self.advance();
        }
    }

    fn declaration(&mut self) -> AstNode {
        let result = if self.match_tok(TokenType::Let) {
            self.let_declaration()
        } else if self.match_tok(TokenType::Fun) {
            self.fun_declaration()
        } else {
            self.statement()
        };
        if self.panic_mode {
            self.synchronize();
        }
        result
    }

    fn let_declaration(&mut self) -> AstNode {
        let variable = self.variable_declaration();
        let line = self.previous.line;
        self.consume(
            TokenType::Equal,
            "Variables must be assigned at declaration",
        );
        let expression = self.expression();
        self.consume(TokenType::SemiColon, "Expect ';' after let statement.");
        AstNode::new(
            self.id(),
            line,
            //AstNodeType::LetStatement(var_name, Box::new(variable), Box::new(expression)),
            AstNodeType::DeclareStatement(Box::new(variable), Box::new(expression)),
        )
    }

    fn variable_declaration(&mut self) -> AstNode {
        self.consume(TokenType::Identifier, "Expect variable name");
        let var_name = self.previous.source;
        let type_annotation = if self.match_tok(TokenType::Colon) {
            self.type_annotation()
        } else {
            AstNode::new(self.id(), self.previous.line, AstNodeType::TypeAnnotation)
        };
        AstNode::new(
            self.id(),
            self.previous.line,
            AstNodeType::Variable {
                name: var_name.to_string(),
                type_annotation: Box::new(type_annotation),
            },
        )
    }

    fn type_annotation(&mut self) -> AstNode {
        if self.match_tok(TokenType::Fun) {
            self.consume(
                TokenType::LeftParen,
                "Expect '(' before function parameter types",
            );
            let line = self.previous.line;
            let parameters = self
                .comma_separated(
                    InnerParser::TypeAnnotation,
                    TokenType::RightParen,
                    "after parameter types.",
                )
                .into_iter()
                .map(|node| node.data_type.unwrap())
                .collect();
            self.consume(
                TokenType::Arrow,
                "Expect '->' after function parameter types",
            );
            let return_type = self.type_annotation().data_type.unwrap();
            AstNode::type_annotation(
                self.id(),
                line,
                DataType::Function {
                    return_type: Box::new(return_type),
                    parameters,
                },
            )
        } else {
            self.consume(TokenType::Identifier, "Invalid type annotation");
            // TODO user types
            let data_type = match self.previous.source {
                "int" => DataType::Int,
                "float" => DataType::Float,
                "bool" => DataType::Bool,
                "str" => DataType::Str,
                "nil" => DataType::Nil,
                _ => {
                    self.error("Invalid type");
                    DataType::Nil
                }
            };
            AstNode::type_annotation(self.id(), self.previous.line, data_type)
        }
    }

    fn fun_declaration(&mut self) -> AstNode {
        self.consume(TokenType::Identifier, "Expect function name");
        let name = self.previous.source;
        let var = self.variable();
        let line = self.previous.line;
        let func = self.function(name);

        AstNode::new(
            self.id(),
            line,
            AstNodeType::DeclareStatement(Box::new(var), Box::new(func)),
        )
    }

    fn function(&mut self, name: &str) -> AstNode {
        self.consume(
            TokenType::LeftParen,
            "Expect '(' before function parameters.",
        );
        let line = self.previous.line;
        let params = self.comma_separated(
            InnerParser::Variable,
            TokenType::RightParen,
            "after parameters.",
        );

        self.consume(TokenType::Arrow, "Expect '->' after parameters.");
        let return_type = self.type_annotation();

        self.consume(TokenType::LeftBrace, "Expect '{' after return type");
        let body = self.block();

        AstNode::new(
            self.id(),
            line,
            AstNodeType::FunctionDef {
                name: name.to_string(),
                return_type: Box::new(return_type),
                params,
                body: Box::new(body),
            },
        )
    }

    fn statement(&mut self) -> AstNode {
        if self.match_tok(TokenType::Print) {
            self.print_statement()
        } else if self.match_tok(TokenType::If) {
            self.if_statement()
        } else if self.match_tok(TokenType::While) {
            self.while_statement()
        } else if self.match_tok(TokenType::LeftBrace) {
            self.block()
        } else if self.match_tok(TokenType::Return) {
            self.return_statement()
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> AstNode {
        let mut statements = Vec::new();
        let line = self.previous.line;
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            statements.push(self.declaration());
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
        AstNode::new(self.id(), line, AstNodeType::Block(statements, 0))
    }

    fn if_statement(&mut self) -> AstNode {
        let condition = self.expression();
        let line = self.previous.line;
        self.consume(TokenType::LeftBrace, "Expect '{' after if condition.");
        let then_block = self.block();
        let else_block = if self.match_tok(TokenType::Else) {
            if self.match_tok(TokenType::If) {
                self.if_statement()
            } else {
                self.consume(TokenType::LeftBrace, "Expect '{' or 'if' after else");
                self.block()
            }
        } else {
            AstNode::new(
                self.id(),
                self.previous.line,
                AstNodeType::Block(Vec::new(), 0),
            )
        };
        AstNode::new(
            self.id(),
            line,
            AstNodeType::IfStatement(
                Box::new(condition),
                Box::new(then_block),
                Box::new(else_block),
            ),
        )
    }

    fn while_statement(&mut self) -> AstNode {
        let condition = self.expression();
        let line = self.previous.line;
        self.consume(TokenType::LeftBrace, "Expect '{' after if condition.");
        let loop_block = self.block();
        AstNode::new(
            self.id(),
            line,
            AstNodeType::WhileStatement(Box::new(condition), Box::new(loop_block)),
        )
    }

    fn print_statement(&mut self) -> AstNode {
        let line = self.previous.line;
        let e = self.expression();
        self.consume(TokenType::SemiColon, "Expect ';' after print statement.");
        AstNode::new(self.id(), line, AstNodeType::PrintStatement(Box::new(e)))
    }

    fn expression_statement(&mut self) -> AstNode {
        let line = self.current.line;
        let e = self.expression();
        self.consume(
            TokenType::SemiColon,
            "Expect ';' after expression statement.",
        );
        AstNode::new(
            self.id(),
            line,
            AstNodeType::ExpressionStatement(Box::new(e)),
        )
    }

    fn return_statement(&mut self) -> AstNode {
        let line = self.previous.line;
        let value = if self.check(TokenType::SemiColon) {
            None
        } else {
            Some(Box::new(self.expression()))
        };
        self.consume(TokenType::SemiColon, "Expect ';' after return statement.");
        AstNode::new(self.id(), line, AstNodeType::ReturnStatement(value))
    }

    fn expression(&mut self) -> AstNode {
        self.parse_precedence(Precedence::Base)
    }

    fn integer(&mut self) -> AstNode {
        AstNode::new(
            self.id(),
            self.previous.line,
            AstNodeType::IntLiteral(self.previous.source.parse::<i64>().unwrap()),
        )
    }

    fn float(&mut self) -> AstNode {
        AstNode::new(
            self.id(),
            self.previous.line,
            AstNodeType::FloatLiteral(self.previous.source.parse::<f64>().unwrap()),
        )
    }

    fn string(&mut self) -> AstNode {
        AstNode::new(
            self.id(),
            self.previous.line,
            AstNodeType::StrLiteral(
                self.previous.source[1..self.previous.source.len() - 1].to_string(),
            ),
        )
    }

    fn variable(&mut self) -> AstNode {
        AstNode::new(
            self.id(),
            self.previous.line,
            AstNodeType::Variable {
                name: self.previous.source.to_string(),
                type_annotation: Box::new(AstNode::new(
                    self.id(),
                    self.previous.line,
                    AstNodeType::TypeAnnotation,
                )),
            },
        )
    }

    fn grouping(&mut self) -> AstNode {
        let e = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
        e
    }

    fn unary(&mut self) -> AstNode {
        let operator = match self.previous.tok_type {
            TokenType::Minus => Operator::Neg,
            TokenType::Bang => Operator::Not,
            _ => panic!("Unreachable unary operator"),
        };
        let line = self.previous.line;
        let operand = self.parse_precedence(prefix_precedence(&self.previous.tok_type));
        AstNode::new(
            self.id(),
            line,
            AstNodeType::Unary(operator, Box::new(operand)),
        )
    }

    fn binary(&mut self, lhs: AstNode) -> AstNode {
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
            TokenType::Equal => Operator::Assign,
            TokenType::And => Operator::And,
            TokenType::Or => Operator::Or,
            _ => panic!("Unreachable binary operator"),
        };
        let line = self.previous.line;
        let precedence = infix_right_precedence(&self.previous.tok_type);
        let rhs = self.parse_precedence(precedence);
        AstNode::new(
            self.id(),
            line,
            AstNodeType::Binary(operator, Box::new(lhs), Box::new(rhs)),
        )
    }

    fn call(&mut self, target: AstNode) -> AstNode {
        let line = self.previous.line;
        let args = self.comma_separated(
            InnerParser::Expression,
            TokenType::RightParen,
            "after arguments.",
        );
        AstNode::new(
            self.id(),
            line,
            AstNodeType::Call {
                target: Box::new(target),
                args,
            },
        )
    }

    fn comma_separated(
        &mut self,
        inner: InnerParser,
        end_tok: TokenType,
        context: &'static str,
    ) -> Vec<AstNode> {
        let mut args = Vec::new();
        // This loop looks weird, but it should parse arg lists with an optional trailing comma
        loop {
            if self.match_tok(end_tok) {
                break;
            }
            args.push(match inner {
                InnerParser::Expression => self.expression(),
                InnerParser::Variable => self.variable_declaration(),
                InnerParser::TypeAnnotation => self.type_annotation(),
            });
            if !self.match_tok(TokenType::Comma) {
                self.consume(end_tok, &format!("Expect {:?} {}", end_tok, context));
                break;
            }
        }

        if args.len() >= 255 {
            self.error("Cannot have more than 255 arguments");
        }

        args
    }
}

enum InnerParser {
    Expression,
    Variable,
    TypeAnnotation,
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
        TokenType::Equal => Precedence::AssignmentLeft,
        TokenType::And => Precedence::AndLeft,
        TokenType::Or => Precedence::OrLeft,
        TokenType::LeftParen => Precedence::CallLeft,
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
        TokenType::Equal => Precedence::AssignmentRight,
        TokenType::And => Precedence::AndRight,
        TokenType::Or => Precedence::OrRight,
        TokenType::LeftParen => Precedence::CallRight,
        _ => Precedence::Nothing,
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Nothing,
    Base,

    // Assignment is right associative, so right comes first
    AssignmentRight,
    AssignmentLeft,

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
    //UnaryLeft,
    UnaryRight,
    CallLeft,
    CallRight,
    //PrimaryLeft,
    //PrimaryRight,
}
