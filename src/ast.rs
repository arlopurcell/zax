use crate::code_gen::Generator;
use crate::value::Value;
use crate::chunk::ByteCode;
use crate::type_check::{find_type, TypeConstraint, DataType};
use crate::common::InterpretError;

#[derive(Debug, Clone, PartialEq)]
pub struct AstNode<'a> {
    pub line: u32,
    pub data_type: Option<DataType>, 
    pub node_type: AstNodeType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstNodeType<'a> {
    Error,
    IntLiteral(u32),
    FloatLiteral(f32),
    BoolLiteral(bool),
    StrLiteral(&'a str),
    Unary(Operator, Box<AstNode<'a>>),
    Binary(Operator, Box<AstNode<'a>>, Box<AstNode<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add, Sub, Mul, Div,
    And, Or,
    Equal, NotEqual,
    Greater, GreaterEqual, Less, LessEqual,

    Neg, Not
}

impl <'a> AstNode<'a> {
    pub fn new(line: u32, node_type: AstNodeType<'a>) -> Self {
        Self {
            line,
            data_type: match &node_type {
                AstNodeType::IntLiteral(_) => Some(DataType::Int),
                AstNodeType::FloatLiteral(_) => Some(DataType::Float),
                AstNodeType::BoolLiteral(_) => Some(DataType::Bool),
                AstNodeType::StrLiteral(_) => Some(DataType::Str),
                _ => None,
            },
            node_type,
        }
    }

    pub fn generate(self, generator: &mut Generator) -> () {
        match self.node_type {
            AstNodeType::IntLiteral(value) => generator.emit_constant(Value::Integer(value as i64), self.line),
            AstNodeType::FloatLiteral(value) => generator.emit_constant(Value::Float(value as f64), self.line),
            AstNodeType::Unary(operator, operand) => {
                operand.generate(generator);
                match operator {
                    Operator::Neg => generator.emit_byte(ByteCode::Negate, self.line),
                    _ => panic!("Unexpected unary code gen"),
                }
            },
            AstNodeType::Binary(operator, lhs, rhs) => {
                lhs.generate(generator);
                rhs.generate(generator);
                match operator {
                    Operator::Add => generator.emit_byte(ByteCode::Add, self.line),
                    Operator::Sub => generator.emit_byte(ByteCode::Sub, self.line),
                    Operator::Mul => generator.emit_byte(ByteCode::Mul, self.line),
                    Operator::Div => generator.emit_byte(ByteCode::Div, self.line),
                    _ => panic!("Unexpected binary code gen"),
                }
            },

            _ => panic!("in implemented code gen"),
        }
    }

    pub fn resolve_types(&self, substitutions: &'a Vec<TypeConstraint<'a>>) -> Result<Self, InterpretError> {
        let data_type = find_type(self, substitutions).ok_or(InterpretError::Compile)?;
        let mut result = self.clone();
        result.data_type = Some(data_type);
        Ok(result)
    }
}

