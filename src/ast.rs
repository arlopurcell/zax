use crate::chunk::ByteCode;
use crate::code_gen::Generator;
use crate::common::InterpretError;
use crate::type_check::{find_type, DataType, TypeConstraint};
use crate::heap::{Heap, Object, ObjType};

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

    PrintStatement(Box<AstNode<'a>>),
    Program(Vec<AstNode<'a>>),
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

    pub fn generate(self, generator: &mut Generator, heap: &mut Heap) -> () {
        match self.node_type {
            AstNodeType::Program(statements) => for statement in statements {
                statement.generate(generator, heap);
            }
            AstNodeType::PrintStatement(e) => {
                let code = match e.data_type {
                    Some(DataType::Int) => ByteCode::PrintInt,
                    Some(DataType::Float) => ByteCode::PrintFloat,
                    Some(DataType::Bool) => ByteCode::PrintBool,
                    Some(DataType::Str) => ByteCode::PrintStr,
                    _ => panic!("Invalid arg type for print statement: {:?}", e.data_type),
                };
                e.generate(generator, heap);
                generator.emit_byte(code, self.line)
            }
            AstNodeType::IntLiteral(value) => {
                generator.emit_constant(&value.to_be_bytes(), self.line)
            }
            AstNodeType::FloatLiteral(value) => {
                generator.emit_constant(&value.to_be_bytes(), self.line)
            }
            AstNodeType::StrLiteral(value) => {
                let heap_index = heap.allocate(Object::new(ObjType::Str(value.to_string())));
                generator.emit_constant(&heap_index.to_be_bytes(), self.line)
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
                operand.generate(generator, heap);
                generator.emit_byte(code, self.line)
            }
            AstNodeType::Binary(operator, lhs, rhs) => {
                let code = match operator {
                    Operator::Add => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::AddInt,
                        Some(DataType::Float) => ByteCode::AddFloat,
                        Some(DataType::Str) => ByteCode::Concat,
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
                        Some(DataType::Str) => ByteCode::EqualHeap,
                        _ => panic!("Invalid type for equal"),
                    }
                    Operator::NotEqual => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::NotEqual8,
                        Some(DataType::Float) => ByteCode::NotEqual8,
                        Some(DataType::Bool) => ByteCode::NotEqual1,
                        Some(DataType::Str) => ByteCode::NotEqualHeap,
                        _ => panic!("Invalid type for equal"),
                    }
                    _ => panic!("Unexpected binary code gen"),
                };
                lhs.generate(generator, heap);
                rhs.generate(generator, heap);
                generator.emit_byte(code, self.line)
            }

            _ => panic!("unimplemented code gen"),
        }
    }

    pub fn resolve_types(
        &self,
        substitutions: &'a Vec<TypeConstraint<'a>>,
    ) -> Result<Self, InterpretError> {
        let data_type = find_type(self, substitutions);
        let mut result = self.clone();
        result.data_type = match self.node_type {
            AstNodeType::Program(_) 
            | AstNodeType::PrintStatement(_)
                => data_type,
            _ => Some(data_type.ok_or(InterpretError::Compile)?)
        };
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
            AstNodeType::Program(statements) => {
                let statements: Result<Vec<_>, _> = statements.iter().map(|s| s.resolve_types(substitutions)).collect();
                AstNodeType::Program(statements?)
            }
            AstNodeType::PrintStatement(e) => {
                AstNodeType::PrintStatement(Box::new(e.resolve_types(substitutions)?))
            }
            _ => self.node_type.clone(),
        };
        Ok(result)
    }
}
