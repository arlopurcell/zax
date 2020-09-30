use crate::chunk::ByteCode;
use crate::code_gen::Generator;
use crate::common::InterpretError;
use crate::heap::{Heap, ObjType, Object};
use crate::type_check::{find_type, DataType, Scope, TCNodeType, TypeConstraint};

#[derive(Debug, Clone)]
pub struct AstNode<'a> {
    pub line: u32,
    pub data_type: Option<DataType>,
    pub node_type: AstNodeType<'a>,
}

impl<'a> PartialEq for AstNode<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.data_type == other.data_type && self.node_type == other.node_type
    }
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

    GlobalVariable(&'a str),
    LocalVariable(usize, &'a str),

    //LetStatement(&'a str, Box<AstNode<'a>>, Box<AstNode<'a>>),
    DeclareStatement(Box<AstNode<'a>>, Box<AstNode<'a>>),
    PrintStatement(Box<AstNode<'a>>),
    ExpressionStatement(Box<AstNode<'a>>),
    Block(Vec<AstNode<'a>>),
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

    Assign,
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

    // TODO static analysis to check lvalues
    pub fn generate(self, generator: &mut Generator, heap: &mut Heap) -> () {
        self.generate_rec(generator, heap, false)
    }

    fn generate_rec(self, generator: &mut Generator, heap: &mut Heap, lvalue: bool) -> () {
        let block_bytes = if let AstNodeType::Block(_) = &self.node_type {
            self.local_var_bytes(false)
        } else {
            0
        };
        match self.node_type {
            AstNodeType::Program(statements) => {
                for statement in statements {
                    statement.generate_rec(generator, heap, false);
                }
            }
            AstNodeType::PrintStatement(e) => {
                let code = match e.data_type {
                    Some(DataType::Int) => ByteCode::PrintInt,
                    Some(DataType::Float) => ByteCode::PrintFloat,
                    Some(DataType::Bool) => ByteCode::PrintBool,
                    Some(DataType::Str) => ByteCode::PrintStr,
                    _ => panic!("Invalid arg type for print statement: {:?}", e.data_type),
                };
                e.generate_rec(generator, heap, false);
                generator.emit_byte(code, self.line)
            }
            AstNodeType::ExpressionStatement(e) => {
                let code = match e.data_type {
                    Some(DataType::Int) | Some(DataType::Str) | Some(DataType::Float) => {
                        ByteCode::Pop8
                    }
                    Some(DataType::Bool) => ByteCode::Pop1,
                    _ => panic!("Invalid type for expression statement: {:?}", e.data_type),
                };
                e.generate_rec(generator, heap, false);
                generator.emit_byte(code, self.line)
            }
            AstNodeType::DeclareStatement(lhs, rhs) => match lhs.node_type {
                AstNodeType::GlobalVariable(name) => {
                    let constant = identifier_constant(name, heap, generator);
                    let code = match rhs.data_type {
                        Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Str) => {
                            ByteCode::DefineGlobal8(constant)
                        }
                        Some(DataType::Bool) => ByteCode::DefineGlobal1(constant),
                        _ => panic!("Unexpected data type for let statement"),
                    };
                    rhs.generate_rec(generator, heap, false);
                    generator.emit_byte(code, self.line)
                }
                AstNodeType::LocalVariable(_, _) => {
                    rhs.generate_rec(generator, heap, false);
                    // simply allow result of rhs to remain on the stack
                }
                _ => panic!("Invalid lhs for let"),
            },
            AstNodeType::Block(statements) => {
                for s in statements {
                    s.generate_rec(generator, heap, false);
                }
                // TODO implement pop n
                for _ in 0..block_bytes {
                    generator.emit_byte(ByteCode::Pop1, self.line)
                }
            }
            /*
            AstNodeType::LetStatement(name, _, e) => {
                let constant = identifier_constant(name, heap, generator);
                let code = match e.data_type {
                    Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Str) => {
                        ByteCode::DefineGlobal8(constant)
                    }
                    Some(DataType::Bool) => ByteCode::DefineGlobal1(constant),
                    _ => panic!("Unexpected data type for let statement"),
                };
                e.generate_rec(generator, heap, false);
                generator.emit_byte(code, self.line)
            }
            */
            AstNodeType::LocalVariable(index, _) => {
                if !lvalue {
                    let code = match &self.data_type {
                        Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Str) => {
                            ByteCode::GetLocal8(index)
                        }
                        Some(DataType::Bool) => ByteCode::GetLocal1(index),
                        _ => panic!("Unexpected data type for variable"),
                    };
                    generator.emit_byte(code, self.line)
                }
            }
            AstNodeType::GlobalVariable(name) => {
                if !lvalue {
                    let constant = identifier_constant(name, heap, generator);
                    let code = match &self.data_type {
                        Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Str) => {
                            ByteCode::GetGlobal8(constant)
                        }
                        Some(DataType::Bool) => ByteCode::GetGlobal1(constant),
                        _ => panic!("Unexpected data type for variable"),
                    };
                    generator.emit_byte(code, self.line)
                }
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
                        _ => panic!("Invalid type for negate"),
                    },
                    Operator::Not => ByteCode::Not,
                    _ => panic!("Unexpected unary code gen"),
                };
                operand.generate_rec(generator, heap, false);
                generator.emit_byte(code, self.line)
            }
            AstNodeType::Binary(operator, lhs, rhs) => {
                let code = match operator {
                    Operator::Add => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::AddInt,
                        Some(DataType::Float) => ByteCode::AddFloat,
                        Some(DataType::Str) => ByteCode::Concat,
                        _ => panic!("Invalid type for add"),
                    },
                    Operator::Sub => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::SubInt,
                        Some(DataType::Float) => ByteCode::SubFloat,
                        _ => panic!("Invalid type for sub"),
                    },
                    Operator::Mul => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::MulInt,
                        Some(DataType::Float) => ByteCode::MulFloat,
                        _ => panic!("Invalid type for mul"),
                    },
                    Operator::Div => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::DivInt,
                        Some(DataType::Float) => ByteCode::DivFloat,
                        _ => panic!("Invalid type for div"),
                    },
                    Operator::Greater => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::GreaterInt,
                        Some(DataType::Float) => ByteCode::GreaterFloat,
                        _ => panic!("Invalid type for div"),
                    },
                    Operator::Less => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::LessInt,
                        Some(DataType::Float) => ByteCode::LessFloat,
                        _ => panic!("Invalid type for div"),
                    },
                    Operator::GreaterEqual => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::GreaterEqualInt,
                        Some(DataType::Float) => ByteCode::GreaterEqualFloat,
                        _ => panic!("Invalid type for div"),
                    },
                    Operator::LessEqual => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::LessEqualInt,
                        Some(DataType::Float) => ByteCode::LessEqualFloat,
                        _ => panic!("Invalid type for div"),
                    },
                    Operator::Equal => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::Equal8,
                        Some(DataType::Float) => ByteCode::Equal8,
                        Some(DataType::Bool) => ByteCode::Equal1,
                        Some(DataType::Str) => ByteCode::EqualHeap,
                        _ => panic!("Invalid type for equal"),
                    },
                    Operator::NotEqual => match &lhs.data_type {
                        Some(DataType::Int) => ByteCode::NotEqual8,
                        Some(DataType::Float) => ByteCode::NotEqual8,
                        Some(DataType::Bool) => ByteCode::NotEqual1,
                        Some(DataType::Str) => ByteCode::NotEqualHeap,
                        _ => panic!("Invalid type for not equal"),
                    },
                    Operator::Assign => {
                        let one_byte = match &lhs.data_type {
                            Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Str) => {
                                false
                            }
                            Some(DataType::Bool) => true,
                            _ => panic!("Unexpected data type for let statement"),
                        };
                        let code = lhs.get_lvalue_code(heap, generator, one_byte);
                        lhs.generate_rec(generator, heap, true);
                        rhs.generate_rec(generator, heap, false);
                        generator.emit_byte(code, self.line);
                        return;
                    }
                    _ => panic!("Unexpected binary code gen"),
                };
                lhs.generate_rec(generator, heap, false);
                rhs.generate_rec(generator, heap, false);
                generator.emit_byte(code, self.line)
            }

            _ => panic!("unimplemented code gen"),
        }
    }

    fn local_var_bytes(&self, lvalue: bool) -> usize {
        match &self.node_type {
            AstNodeType::DeclareStatement(lhs, rhs) => lhs.local_var_bytes(true),
            AstNodeType::Program(statements) | AstNodeType::Block(statements) => {
                statements.iter().map(|s| s.local_var_bytes(false)).sum()
            }
            AstNodeType::LocalVariable(_, _) => {
                if lvalue {
                    match self.data_type {
                        Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Str) => 8,
                        Some(DataType::Bool) => 1,
                        _ => panic!("unknown type in local var bytes: {:?}", self.data_type),
                    }
                } else {
                    0
                }
            }
            _ => 0,
        }
    }

    // TODO clean up the typing here
    fn get_lvalue_code(
        &self,
        heap: &mut Heap,
        generator: &mut Generator,
        one_byte: bool,
    ) -> ByteCode {
        match self.node_type {
            AstNodeType::GlobalVariable(name) => {
                let constant = identifier_constant(name, heap, generator);
                if one_byte {
                    ByteCode::SetGlobal1(constant)
                } else {
                    ByteCode::SetGlobal8(constant)
                }
            }
            AstNodeType::LocalVariable(index, _) => {
                if one_byte {
                    ByteCode::SetLocal1(index)
                } else {
                    ByteCode::SetLocal8(index)
                }
            }
            // TODO for dotted stuff, recurse
            _ => panic!("Invalid lvalue"),
        }
    }

    pub fn resolve_types<'b>(
        &self,
        substitutions: &'a Vec<TypeConstraint<'a>>,
        scope: &'b mut Scope,
    ) -> Result<Self, InterpretError> {
        let tc_type = find_type(self, substitutions);
        if let Some(tc_type) = &tc_type {
            match &self.node_type {
                AstNodeType::LocalVariable(_, name) | AstNodeType::GlobalVariable(name) => {
                    scope.insert(name, tc_type.clone())
                }
                _ => (),
            }
        }
        let data_type = tc_type.map(|tc_type| match tc_type {
            TCNodeType::Int => DataType::Int,
            TCNodeType::Float => DataType::Float,
            TCNodeType::Bool => DataType::Bool,
            TCNodeType::Str => DataType::Str,
            /* TODO
               TCNodeType::Array => {
               let child = children.get(0).unwrap();
               let inner_type = find_type_helper(&child, substitutions, &mut Vec::new());
               inner_type.map(|inner_type| NodeType::Array(Box::new(inner_type)))
               }
            */
        });
        let mut result = self.clone();
        result.data_type = match self.node_type {
            AstNodeType::Program(_)
            | AstNodeType::PrintStatement(_)
            | AstNodeType::ExpressionStatement(_)
            | AstNodeType::Block(_)
            | AstNodeType::DeclareStatement(_, _) => data_type,
            _ => Some(data_type.ok_or(InterpretError::Compile)?),
        };
        result.node_type = match &self.node_type {
            AstNodeType::Unary(operator, operand) => {
                let operand = operand.resolve_types(substitutions, scope)?;
                AstNodeType::Unary(*operator, Box::new(operand))
            }
            AstNodeType::Binary(operator, lhs, rhs) => {
                let lhs = lhs.resolve_types(substitutions, scope)?;
                let rhs = rhs.resolve_types(substitutions, scope)?;
                AstNodeType::Binary(*operator, Box::new(lhs), Box::new(rhs))
            }
            AstNodeType::Block(statements) => {
                let statements: Result<Vec<_>, _> = statements
                    .iter()
                    .map(|s| s.resolve_types(substitutions, scope))
                    .collect();
                AstNodeType::Block(statements?)
            }
            AstNodeType::Program(statements) => {
                let statements: Result<Vec<_>, _> = statements
                    .iter()
                    .map(|s| s.resolve_types(substitutions, scope))
                    .collect();
                AstNodeType::Program(statements?)
            }
            AstNodeType::PrintStatement(e) => {
                AstNodeType::PrintStatement(Box::new(e.resolve_types(substitutions, scope)?))
            }
            AstNodeType::ExpressionStatement(e) => {
                AstNodeType::ExpressionStatement(Box::new(e.resolve_types(substitutions, scope)?))
            }
            AstNodeType::DeclareStatement(lhs, rhs) => AstNodeType::DeclareStatement(
                Box::new(lhs.resolve_types(substitutions, scope)?),
                Box::new(rhs.resolve_types(substitutions, scope)?),
            ),
            _ => self.node_type.clone(),
        };
        Ok(result)
    }
}

fn identifier_constant(name: &str, heap: &mut Heap, generator: &mut Generator) -> u8 {
    let heap_index = heap.allocate(Object::new(ObjType::Str(name.to_string())));
    generator
        .current_chunk_mut()
        .add_constant(&heap_index.to_be_bytes())
}
