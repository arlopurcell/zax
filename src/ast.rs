use crate::chunk::ByteCode;
use crate::code_gen::Generator;
use crate::common::{InterpretError, InterpretResult};
use crate::heap::Heap;
use crate::type_check::{find_type, DataType, Scope, TCNodeType, TypeConstraint};
use crate::object::{Object, ObjType};

#[derive(Debug, Clone)]
pub struct AstNode {
    pub id: u64,
    pub line: u32,
    pub data_type: Option<DataType>,
    pub node_type: AstNodeType,
}

impl PartialEq for AstNode {
    fn eq(&self, other: &Self) -> bool {
        self.data_type == other.data_type && self.node_type == other.node_type
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstNodeType {
    Error,
    IntLiteral(i64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StrLiteral(String),
    Unary(Operator, Box<AstNode>),
    Binary(Operator, Box<AstNode>, Box<AstNode>),

    Variable(String),

    Call{target: Box<AstNode>, args: Vec<AstNode>},

    WhileStatement(Box<AstNode>, Box<AstNode>),
    DeclareStatement(Box<AstNode>, Box<AstNode>),
    FunctionDef{return_type: String, params: Vec<AstNode>, body: Box<AstNode>},
    PrintStatement(Box<AstNode>),
    ExpressionStatement(Box<AstNode>),
    Block(Vec<AstNode>),
    IfStatement(Box<AstNode>, Box<AstNode>, Box<AstNode>),
    Program(Vec<AstNode>),
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

impl AstNode {
    pub fn new(id: u64, line: u32, node_type: AstNodeType) -> Self {
        Self {
            id,
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

    /*
    pub fn debug_print(&self) -> () {
        self.debug_print_indent(0);
    }

    fn debug_print_indent(&self, indent: usize) -> () {
        let increment = 2;
        eprint!("{:>width$}", "", width=indent);
        match self.node_type {
            AstNodeType::Error => eprintln!("Error"),
            AstNodeType::IntLiteral(val) => eprintln!("IntLiteral({})", val),
            AstNodeType::FloatLiteral(val) => eprintln!("FloatLiteral({})", val),
            AstNodeType::BoolLiteral(val) => eprintln!("BoolLiteral({})", val),
            AstNodeType::StrLiteral(val) => eprintln!("StrLiteral({})", val),

            AstNodeType::Unary(operator, operand) => {
                eprintln!("Unary {:?} (", operator);
                operand.debug_print_indent(indent + increment);
                eprintln!("{:>width$})", "", width=indent);
            },
            AstNodeType::Binary(op, a, b) => {
                eprintln!("Binary {:?} (", op);
                a.debug_print_indent(indent + increment);
                b.debug_print_indent(indent + increment);
                eprintln!("{:>width$})", "", width=indent);
            },
            AstNodeType::GlobalVariable(val) => eprintln!("GlobalVariable({})", val),
            AstNodeType::LocalVariable(_, val) => eprintln!("LocalVariable({})", val),

            AstNodeType::Call{target, args} => {
                eprintln!("Call (");
                eprint!("{:>width$}target:", "", width=indent+increment);
                target.debug_print_indent(indent + increment);
                eprint!("{:>width$}args:", "", width=indent+increment);
                for arg in args.iter() {
                    arg.debug_print_indent(indent + increment);
                }
                eprintln!(")");
            }

            AstNodeType::WhileStatement(Box<AstNode<'a>>, Box<AstNode<'a>>),
            AstNodeType::DeclareStatement(Box<AstNode<'a>>, Box<AstNode<'a>>),
            AstNodeType::FunctionStatement{return_type: &'a str, args: Vec<AstNode<'a>>, body: Box<AstNode<'a>>},
            AstNodeType::PrintStatement(Box<AstNode<'a>>),
            AstNodeType::ExpressionStatement(Box<AstNode<'a>>),
            AstNodeType::Block(Vec<AstNode<'a>>),
            AstNodeType::IfStatement(Box<AstNode<'a>>, Box<AstNode<'a>>, Box<AstNode<'a>>),
            AstNodeType::Program(Vec<AstNode<'a>>),
        }
    }
    */

    // TODO static analysis to check lvalues
    pub fn generate(self, generator: &mut Generator, heap: &mut Heap) -> () {
        self.generate_with_lvalue(generator, heap, false)
    }

    fn generate_with_lvalue(self, generator: &mut Generator, heap: &mut Heap, lvalue: bool) -> () {
        /*
        let block_bytes = match &self.node_type {
            AstNodeType::Block(_) | AstNodeType::Call{target:_, args:_} => self.local_var_bytes(false),
            _ => 0,
        };
        */
        match self.node_type {
            AstNodeType::Program(statements) => {
                for statement in statements {
                    statement.generate(generator, heap);
                }
            }
            AstNodeType::FunctionDef{return_type: _, params: _, body} => {
                // TODO pass in function name for debugging
                let mut child_generator = Generator::new();

                generator.begin_scope();
                // TODO args

                if let AstNodeType::Block(statements) = body.node_type {
                    for s in statements {
                        s.generate(&mut child_generator, heap);
                    }
                } else {
                    panic!("Function body must be a block");
                }
                let block_bytes = generator.end_scope();
                // TODO implement pop n
                for _ in 0..block_bytes {
                    generator.emit_byte(ByteCode::Pop1, self.line)
                }

                // TODO function name
                let func_obj = child_generator.end();
                //generator.emit_constant_8(, self.line);
                let heap_index = heap.allocate(Object::new(ObjType::Function(Box::new(func_obj))));
                generator.emit_constant_8(&heap_index.to_be_bytes(), self.line)
            },
            AstNodeType::Call{target, args} => {
                target.generate(generator, heap);
                let size = args.iter().map(|a| a.data_type.expect("function argument must have type").size() as usize).sum();
                for arg in args {
                    arg.generate(generator, heap);
                }
                generator.emit_byte(ByteCode::Call(size), self.line);
            },
            AstNodeType::PrintStatement(e) => {
                let code = match e.data_type {
                    Some(DataType::Int) => ByteCode::PrintInt,
                    Some(DataType::Float) => ByteCode::PrintFloat,
                    Some(DataType::Bool) => ByteCode::PrintBool,
                    Some(DataType::Str) => ByteCode::PrintObject,
                    Some(DataType::Function) => ByteCode::PrintObject,
                    Some(DataType::Nil) => panic!("Can't print nil"),
                    None => panic!("None arg type for print statement: {:?}", e.data_type),
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
                    Some(DataType::Nil) => ByteCode::NoOp,
                    _ => panic!("Invalid type for expression statement: {:?}", e.data_type),
                };
                e.generate(generator, heap);
                generator.emit_byte(code, self.line)
            }
            AstNodeType::DeclareStatement(lhs, rhs) => match lhs.node_type {
                AstNodeType::Variable(name) => {
                    if generator.scope_depth == 0 {
                        let constant = identifier_constant(&name, heap, generator);
                        let code = match rhs.data_type {
                            Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Str) | Some(DataType::Function)=> {
                                ByteCode::DefineGlobal8(constant)
                            }
                            Some(DataType::Bool) => ByteCode::DefineGlobal1(constant),
                            Some(DataType::Nil) | None => panic!("No data type for global variable")
                        };
                        rhs.generate(generator, heap);
                        generator.emit_byte(code, self.line)
                    } else {
                        generator.add_local(&name, lhs.data_type.expect("Variable must have data type").size());

                        rhs.generate(generator, heap);
                        // simply allow result of rhs to remain on the stack
                    }
                }
                _ => panic!("Invalid lhs for let"),
            },
            AstNodeType::Block(statements) => {
                generator.begin_scope();
                for s in statements {
                    s.generate(generator, heap);
                }
                let block_bytes = generator.end_scope();
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
            AstNodeType::Variable(name) => if !lvalue {
                if let Some(local_index) = generator.resolve_local(&name) {
                    let code = match &self.data_type {
                        Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Str) => {
                            ByteCode::GetLocal8(local_index)
                        }
                        Some(DataType::Bool) => ByteCode::GetLocal1(local_index),
                        _ => panic!("Unexpected data type for variable"),
                    };
                    generator.emit_byte(code, self.line)
                } else {
                    let constant = identifier_constant(&name, heap, generator);
                    let code = match &self.data_type {
                        Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Str) | Some(DataType::Function) => {
                            ByteCode::GetGlobal8(constant)
                        }
                        Some(DataType::Bool) => ByteCode::GetGlobal1(constant),
                        Some(DataType::Nil) | None => panic!("None data type for variable"),
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

    /*
    fn local_var_bytes(&self, lvalue: bool) -> usize {
        match &self.node_type {
            AstNodeType::DeclareStatement(lhs, _) => lhs.local_var_bytes(true),
            AstNodeType::Program(statements) | AstNodeType::Block(statements) => {
                statements.iter().map(|s| s.local_var_bytes(false)).sum()
            },
            AstNodeType::Call{target: _, args} => {
                args.iter().map(|s| s.local_var_bytes(false)).sum()
            },
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
    */

    // TODO clean up the typing here
    fn get_lvalue_code(
        &self,
        heap: &mut Heap,
        generator: &mut Generator,
        one_byte: bool,
    ) -> ByteCode {
        match &self.node_type {
            AstNodeType::Variable(name) => {
                if let Some(local_index) = generator.resolve_local(&name) {
                    if one_byte {
                        ByteCode::SetLocal1(local_index)
                    } else {
                        ByteCode::SetLocal8(local_index)
                    }
                } else {
                    let constant = identifier_constant(&name, heap, generator);
                    if one_byte {
                        ByteCode::SetGlobal1(constant)
                    } else {
                        ByteCode::SetGlobal8(constant)
                    }
                }
            }
            // TODO for dotted stuff, recurse
            _ => panic!("Invalid lvalue"),
        }
    }

    pub fn resolve_types(
        &mut self,
        substitutions: &Vec<TypeConstraint>,
        scope: &mut Scope,
    ) -> InterpretResult {
        let tc_type = find_type(&self, substitutions);
        let data_type = tc_type.map(|tc_type| match tc_type {
            TCNodeType::Int => DataType::Int,
            TCNodeType::Float => DataType::Float,
            TCNodeType::Bool => DataType::Bool,
            TCNodeType::Str => DataType::Str,
            TCNodeType::Function => DataType::Function,
            TCNodeType::Nil => DataType::Nil,
            /* TODO
               TCNodeType::Array => {
               let child = children.get(0).unwrap();
               let inner_type = find_type_helper(&child, substitutions, &mut Vec::new());
               inner_type.map(|inner_type| NodeType::Array(Box::new(inner_type)))
               }
            */
        });
        self.data_type = data_type;
        /*
        result.data_type = match self.node_type {
            AstNodeType::Program(_)
            | AstNodeType::PrintStatement(_)
            | AstNodeType::ExpressionStatement(_)
            | AstNodeType::Block(_)
            | AstNodeType::IfStatement(_, _, _)
            | AstNodeType::WhileStatement(_, _)
            | AstNodeType::FunctionStatement{return_type:_, args:_, body:_}
            | AstNodeType::Call{target: _, args: _} // TODO remove
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
        */
        Ok(match &mut self.node_type {
            AstNodeType::Unary(_, operand) => {
                operand.resolve_types(substitutions, scope)?;
            }
            AstNodeType::Binary(_, lhs, rhs) => {
                lhs.resolve_types(substitutions, scope)?;
                rhs.resolve_types(substitutions, scope)?;
            }
            AstNodeType::Block(statements) => {
                let statements: Result<Vec<_>, _> = statements
                    .into_iter()
                    .map(|s| s.resolve_types(substitutions, scope))
                    .collect();
                statements?;
            }
            AstNodeType::Program(statements) => {
                let statements: Result<Vec<_>, _> = statements
                    .into_iter()
                    .map(|s| s.resolve_types(substitutions, scope))
                    .collect();
                statements?;
            }
            AstNodeType::PrintStatement(e)
            | AstNodeType::ExpressionStatement(e) => {
                e.resolve_types(substitutions, scope)?;
            }
            AstNodeType::DeclareStatement(lhs, rhs) => {
                lhs.resolve_types(substitutions, scope)?;
                rhs.resolve_types(substitutions, scope)?;
            },
            AstNodeType::IfStatement(condition, then_block, else_block) => {
                condition.resolve_types(substitutions, scope)?;
                then_block.resolve_types(substitutions, scope)?;
                else_block.resolve_types(substitutions, scope)?;
            }
            AstNodeType::WhileStatement(condition, loop_block) => {
                condition.resolve_types(substitutions, scope)?;
                loop_block.resolve_types(substitutions, scope)?;
            }
            AstNodeType::Call{target, args} => {
                target.resolve_types(substitutions, scope)?;
                let args: Result<Vec<_>, _> = args
                    .into_iter()
                    .map(|arg| arg.resolve_types(substitutions, scope))
                    .collect();
                args?;
            }
            AstNodeType::FunctionDef{return_type, params, body} => {
                let params: Result<Vec<_>, _> = params
                    .into_iter()
                    .map(|param| param.resolve_types(substitutions, scope))
                    .collect();
                params?;
                body.resolve_types(substitutions, scope)?;
            }
            AstNodeType::IntLiteral(_)
            | AstNodeType::FloatLiteral(_)
            | AstNodeType::StrLiteral(_)
            | AstNodeType::BoolLiteral(_)
            | AstNodeType::Variable(_)
            | AstNodeType::Error => (),
        })
    }
}

fn identifier_constant(name: &str, heap: &mut Heap, generator: &mut Generator) -> u8 {
    let heap_index = heap.allocate_string(name.to_string());
    generator.add_constant(&heap_index.to_be_bytes())
}
