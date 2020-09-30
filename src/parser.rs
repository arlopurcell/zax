use std::mem::swap;

use crate::ast::{AstNode, AstNodeType, Operator};
use crate::lexer::{Lexer, Token, TokenType};
use crate::object::FunctionObj;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token<'a>,
    previous: Token<'a>,
    pub had_error: bool,
    panic_mode: bool,
    locals: Vec<Local<'a>>,
    scope_depth: usize,
    pub function: FunctionObj,
    func_type: FunctionType,
}

struct Local<'a> {
    name: &'a str,
    depth: usize,
}

enum FunctionType {
    Function,
    Script,
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
            locals: vec![Local{name: "", depth: 0}], // TODO idk why i'm doing this yet
            scope_depth: 0,
            function: FunctionObj::new(),
            func_type: FunctionType::Script,
        }
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
            TokenType::Identifier => self.variable(),
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
                | TokenType::BangEqual
                | TokenType::And
                | TokenType::Or
                | TokenType::Equal => self.binary(node),
                _ => {
                    self.error("Unreachable no infix parse function");
                    AstNode::new(self.previous.line, AstNodeType::Error)
                }
            };
        }
        node
    }

    pub fn program(&mut self) -> AstNode<'a> {
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

    fn declaration(&mut self) -> AstNode<'a> {
        let result = if self.match_tok(TokenType::Let) {
            self.let_declaration()
        } else {
            self.statement()
        };
        if self.panic_mode {
            self.synchronize();
        }
        result
    }

    fn let_declaration(&mut self) -> AstNode<'a> {
        self.consume(TokenType::Identifier, "Expect variable name");
        let var_name = self.previous.source;
        let variable = if self.scope_depth > 0 {
            self.add_local();
            AstNode::new(
                self.previous.line,
                AstNodeType::LocalVariable(self.locals.len() - 1, var_name),
            )
        } else {
            AstNode::new(self.previous.line, AstNodeType::GlobalVariable(var_name))
        };
        let line = self.previous.line;
        self.consume(
            TokenType::Equal,
            "Variables must be assigned at declaration",
        );
        let expression = self.expression();
        self.consume(TokenType::SemiColon, "Expect ';' after let statement.");
        AstNode::new(
            line,
            //AstNodeType::LetStatement(var_name, Box::new(variable), Box::new(expression)),
            AstNodeType::DeclareStatement(Box::new(variable), Box::new(expression)),
        )
    }

    fn add_local(&mut self) -> () {
        self.locals.push(Local {
            name: self.previous.source,
            depth: self.scope_depth,
        })
    }

    fn statement(&mut self) -> AstNode<'a> {
        if self.match_tok(TokenType::Print) {
            self.print_statement()
        } else if self.match_tok(TokenType::If) {
            self.if_statement()
        } else if self.match_tok(TokenType::While) {
            self.while_statement()
        } else if self.match_tok(TokenType::LeftBrace) {
            self.begin_scope();
            let block = self.block();
            self.end_scope();
            block
        } else {
            self.expression_statement()
        }
    }

    fn begin_scope(&mut self) -> () {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) -> () {
        self.scope_depth -= 1;
        let scope_depth = self.scope_depth;
        self.locals.retain(|l| l.depth <= scope_depth);
    }

    fn block(&mut self) -> AstNode<'a> {
        let mut statements = Vec::new();
        let line = self.previous.line;
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            statements.push(self.declaration());
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
        AstNode::new(line, AstNodeType::Block(statements))
    }

    fn if_statement(&mut self) -> AstNode<'a> {
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
            AstNode::new(self.previous.line, AstNodeType::Block(Vec::new()))
        };
        AstNode::new(line, AstNodeType::IfStatement(Box::new(condition), Box::new(then_block), Box::new(else_block)))
    }

    fn while_statement(&mut self) -> AstNode<'a> {
        let condition = self.expression();
        let line = self.previous.line;
        self.consume(TokenType::LeftBrace, "Expect '{' after if condition.");
        let loop_block = self.block();
        AstNode::new(line, AstNodeType::WhileStatement(Box::new(condition), Box::new(loop_block)))
    }

    fn print_statement(&mut self) -> AstNode<'a> {
        let line = self.previous.line;
        let e = self.expression();
        self.consume(TokenType::SemiColon, "Expect ';' after print statement.");
        AstNode::new(line, AstNodeType::PrintStatement(Box::new(e)))
    }

    fn expression_statement(&mut self) -> AstNode<'a> {
        let line = self.previous.line;
        let e = self.expression();
        self.consume(
            TokenType::SemiColon,
            "Expect ';' after expression statement.",
        );
        AstNode::new(line, AstNodeType::ExpressionStatement(Box::new(e)))
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
        AstNode::new(
            self.previous.line,
            AstNodeType::StrLiteral(&self.previous.source[1..self.previous.source.len() - 1]),
        )
    }

    fn variable(&self) -> AstNode<'a> {
        let name = &self.previous.source;
        if let Some(local_index) = self.resolve_local(name) {
            AstNode::new(
                self.previous.line,
                AstNodeType::LocalVariable(local_index, name),
            )
        } else {
            AstNode::new(self.previous.line, AstNodeType::GlobalVariable(name))
        }
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(index, local)| local.name == name)
            .map(|(index, _)| index)
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
            TokenType::Equal => Operator::Assign,
            TokenType::And => Operator::And,
            TokenType::Or => Operator::Or,
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
        TokenType::Equal => Precedence::AssignmentLeft,
        TokenType::And => Precedence::AndLeft,
        TokenType::Or => Precedence::OrLeft,
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
    UnaryLeft,
    UnaryRight,
    CallLeft,
    CallRight,
    PrimaryLeft,
    PrimaryRight,
}
