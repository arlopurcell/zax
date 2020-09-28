use crate::chunk::ByteCode;
use crate::code_gen::Generator;
use crate::common::InterpretError;
use crate::type_check::{find_type, DataType, TypeConstraint};
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct AstNode<'a> {
    pub line: u32,
    pub data_type: Option<DataType>,
    pub node_type: AstNodeType<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstNodeType<'a> {
    Error,
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StrLiteral(&'a str),
    Unary(Operator, Box<AstNode<'a>>),
    Binary(Operator, Box<AstNode<'a>>, Box<AstNode<'a>>),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Neg,
    Not,
}

impl<'a> AstNode<'a> {
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
            AstNodeType::IntLiteral(value) => {
                generator.emit_constant(&value.to_be_bytes(), self.line)
            }
            AstNodeType::FloatLiteral(value) => {
                generator.emit_constant(&value.to_be_bytes(), self.line)
            }
            AstNodeType::Unary(operator, operand) => {
                let code = match operator {
                    Operator::Neg => match &operand.data_type {
                        Some(DataType::Int) => ByteCode::NegateInt,
                        Some(DataType::Float) => ByteCode::NegateFloat,
                        _ => panic!("Invalid type for negate")
                    }
                    Operator::Not => ByteCode::Not,
                    _ => panic!("Unexpected unary code gen"),
                };
                operand.generate(generator);
                generator.emit_byte(code, self.line)
            }
            AstNodeType::Binary(operator, lhs, rhs) => {
                let code = match operator {
                    Operator::Add => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::AddInt,
                        Some(DataType::Float) => ByteCode::AddFloat,
                        _ => panic!("Invalid type for add"),
                    }
                    Operator::Sub => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::SubInt,
                        Some(DataType::Float) => ByteCode::SubFloat,
                        _ => panic!("Invalid type for sub"),
                    }
                    Operator::Mul => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::MulInt,
                        Some(DataType::Float) => ByteCode::MulFloat,
                        _ => panic!("Invalid type for mul"),
                    }
                    Operator::Div => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::DivInt,
                        Some(DataType::Float) => ByteCode::DivFloat,
                        _ => panic!("Invalid type for div"),
                    }
                    Operator::Greater => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::GreaterInt,
                        Some(DataType::Float) => ByteCode::GreaterFloat,
                        _ => panic!("Invalid type for div"),
                    }
                    Operator::Less => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::LessInt,
                        Some(DataType::Float) => ByteCode::LessFloat,
                        _ => panic!("Invalid type for div"),
                    }
                    Operator::GreaterEqual => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::GreaterEqualInt,
                        Some(DataType::Float) => ByteCode::GreaterEqualFloat,
                        _ => panic!("Invalid type for div"),
                    }
                    Operator::LessEqual => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::LessEqualInt,
                        Some(DataType::Float) => ByteCode::LessEqualFloat,
                        _ => panic!("Invalid type for div"),
                    }
                    Operator::Equal => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::Equal8,
                        Some(DataType::Float) => ByteCode::Equal8,
                        Some(DataType::Bool) => ByteCode::Equal1,
                        _ => panic!("Invalid type for equal"),
                    }
                    Operator::NotEqual => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::NotEqual8,
                        Some(DataType::Float) => ByteCode::NotEqual8,
                        Some(DataType::Bool) => ByteCode::NotEqual1,
                        _ => panic!("Invalid type for equal"),
                    }
                    _ => panic!("Unexpected binary code gen"),
                };
                lhs.generate(generator);
                rhs.generate(generator);
                generator.emit_byte(code, self.line)
            }

            _ => panic!("in implemented code gen"),
        }
    }

    pub fn resolve_types(
        &self,
        substitutions: &'a Vec<TypeConstraint<'a>>,
    ) -> Result<Self, InterpretError> {
        let data_type = find_type(self, substitutions).ok_or(InterpretError::Compile)?;
        let mut result = self.clone();
        result.data_type = Some(data_type);
        result.node_type = match &self.node_type {
            AstNodeType::Unary(operator, operand) => {
                let operand = operand.resolve_types(substitutions)?;
                AstNodeType::Unary(*operator, Box::new(operand))
            }
            AstNodeType::Binary(operator, lhs, rhs) => {
                let lhs = lhs.resolve_types(substitutions)?;
                let rhs = rhs.resolve_types(substitutions)?;
                AstNodeType::Binary(*operator, Box::new(lhs), Box::new(rhs))
            }
            _ => self.node_type.clone(),
        };
        Ok(result)
    }
}
