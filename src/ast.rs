use crate::chunk::ByteCode;
use crate::code_gen::Generator;
use crate::common::InterpretError;
use crate::heap::Heap;
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

    WhileStatement(Box<AstNode<'a>>, Box<AstNode<'a>>),
    DeclareStatement(Box<AstNode<'a>>, Box<AstNode<'a>>),
    FunctionStatement{return_type: &'a str, args: Vec<AstNode<'a>>, body: Box<AstNode<'a>>},
    PrintStatement(Box<AstNode<'a>>),
    ExpressionStatement(Box<AstNode<'a>>),
    Block(Vec<AstNode<'a>>),
    IfStatement(Box<AstNode<'a>>, Box<AstNode<'a>>, Box<AstNode<'a>>),
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
        self.generate_with_lvalue(generator, heap, false)
    }

    fn generate_with_lvalue(self, generator: &mut Generator, heap: &mut Heap, lvalue: bool) -> () {
        let block_bytes = if let AstNodeType::Block(_) = &self.node_type {
            self.local_var_bytes(false)
        } else {
            0
        };
        match self.node_type {
            AstNodeType::Program(statements) => {
                for statement in statements {
                    statement.generate(generator, heap);
                }
            }
            AstNodeType::FunctionStatement{return_type: _, args: _, body} => {
                // TODO pass in function name for debugging
                let mut child_generator = Generator::new();
                // TODO args
                body.generate(&mut child_generator, heap);
                let func_obj = child_generator.end();
                //generator.emit_constant_8(, self.line);
            },
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
            AstNodeType::ExpressionStatement(e) => {
                let code = match e.data_type {
                    Some(DataType::Int) | Some(DataType::Str) | Some(DataType::Float) => {
                        ByteCode::Pop8
                    }
                    Some(DataType::Bool) => ByteCode::Pop1,
                    _ => panic!("Invalid type for expression statement: {:?}", e.data_type),
                };
                e.generate(generator, heap);
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
                    rhs.generate(generator, heap);
                    generator.emit_byte(code, self.line)
                }
                AstNodeType::LocalVariable(_, _) => {
                    rhs.generate(generator, heap);
                    // simply allow result of rhs to remain on the stack
                }
                _ => panic!("Invalid lhs for let"),
            },
            AstNodeType::Block(statements) => {
                for s in statements {
                    s.generate(generator, heap);
                }
                // TODO implement pop n
                for _ in 0..block_bytes {
                    generator.emit_byte(ByteCode::Pop1, self.line)
                }
            }
            AstNodeType::IfStatement(condition, then_block, else_block) => {
                condition.generate(generator, heap);
                let then_jump = generator.emit_jump_if_false(self.line);

                generator.emit_byte(ByteCode::Pop1, self.line); // pop condition
                then_block.generate(generator, heap);

                let else_jump = generator.emit_jump(self.line);
                generator.patch_jump(then_jump);

                generator.emit_byte(ByteCode::Pop1, self.line); // pop condition
                else_block.generate(generator, heap);
                generator.patch_jump(else_jump);
            }
            AstNodeType::WhileStatement(condition, loop_block) => {
                let loop_start = generator.loop_start();
                condition.generate(generator, heap);
                let exit_jump = generator.emit_jump_if_false(self.line);
                generator.emit_byte(ByteCode::Pop1, self.line); // pop condition
                loop_block.generate(generator, heap);
                generator.emit_loop(loop_start, self.line);
                generator.patch_jump(exit_jump);
                generator.emit_byte(ByteCode::Pop1, self.line); // pop condition
            }
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
                generator.emit_constant_8(&value.to_be_bytes(), self.line)
            }
            AstNodeType::FloatLiteral(value) => {
                generator.emit_constant_8(&value.to_be_bytes(), self.line)
            }
            AstNodeType::BoolLiteral(value) => {
                generator.emit_constant_1(if value { 1 } else { 0 }, self.line)
            }
            AstNodeType::StrLiteral(value) => {
                let heap_index = heap.allocate_string(value.to_string());
                generator.emit_constant_8(&heap_index.to_be_bytes(), self.line)
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
                        lhs.generate_with_lvalue(generator, heap, true);
                        rhs.generate(generator, heap);
                        generator.emit_byte(code, self.line);
                        return;
                    }
                    Operator::And => {
                        lhs.generate(generator, heap);
                        let end_jump = generator.emit_jump_if_false(self.line);
                        // If lhs was true, pop lhs value and use value of rhs
                        generator.emit_byte(ByteCode::Pop1, self.line);
                        rhs.generate(generator, heap);
                        generator.patch_jump(end_jump);
                        return;
                    }
                    Operator::Or => {
                        lhs.generate(generator, heap);
                        // If lhs was false jump over the unconditional jump
                        let else_jump = generator.emit_jump_if_false(self.line);
                        // else lhs was true, so jump to the end
                        let end_jump = generator.emit_jump(self.line);
                        generator.patch_jump(else_jump);

                        // If lhs was false, pop lhs value and use rhs value
                        generator.emit_byte(ByteCode::Pop1, self.line);
                        rhs.generate(generator, heap);
                        generator.patch_jump(end_jump);
                        return;
                    }
                    _ => panic!("Unexpected binary code gen"),
                };
                lhs.generate(generator, heap);
                rhs.generate(generator, heap);
                generator.emit_byte(code, self.line)
            }
            AstNodeType::Error => panic!("Shouldn't try to generate code for Error node"),
        }
    }

    fn local_var_bytes(&self, lvalue: bool) -> usize {
        match &self.node_type {
            AstNodeType::DeclareStatement(lhs, _) => lhs.local_var_bytes(true),
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
            | AstNodeType::IfStatement(_, _, _)
            | AstNodeType::WhileStatement(_, _)
            | AstNodeType::DeclareStatement(_, _) => data_type,
            _ => {
                if data_type.is_some() {
                    data_type
                } else {
                    eprintln!("Unable to infer type for node {:?}", self.node_type);
                    return Err(InterpretError::Compile);
                }
            }
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
            AstNodeType::IfStatement(condition, then_block, else_block) => {
                AstNodeType::IfStatement(
                    Box::new(condition.resolve_types(substitutions, scope)?),
                    Box::new(then_block.resolve_types(substitutions, scope)?),
                    Box::new(else_block.resolve_types(substitutions, scope)?),
                )
            }
            AstNodeType::WhileStatement(condition, loop_block) => AstNodeType::WhileStatement(
                Box::new(condition.resolve_types(substitutions, scope)?),
                Box::new(loop_block.resolve_types(substitutions, scope)?),
            ),
            _ => self.node_type.clone(),
        };
        Ok(result)
    }
}

fn identifier_constant(name: &str, heap: &mut Heap, generator: &mut Generator) -> u8 {
    let heap_index = heap.allocate_string(name.to_string());
    generator.add_constant(&heap_index.to_be_bytes())
}
