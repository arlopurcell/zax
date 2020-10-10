use std::collections::HashMap;

use crate::chunk::ByteCode;
use crate::code_gen::{ChunkGenerator, FunctionType};
use crate::common::{InterpretError, InterpretResult};
use crate::object::{FunctionObj, ObjType, Object};
use crate::type_check::{
    final_type_check, find_type, generate_substitutions, DataType, TypeConstraint,
};
use crate::vm::VM;

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

    Variable {
        name: String,
        type_annotation: Option<String>,
    },

    Call {
        target: Box<AstNode>,
        args: Vec<AstNode>,
    },

    WhileStatement(Box<AstNode>, Box<AstNode>),
    DeclareStatement(Box<AstNode>, Box<AstNode>),
    FunctionDef {
        name: String,
        return_type: String,
        params: Vec<AstNode>,
        body: Box<AstNode>,
    },
    PrintStatement(Box<AstNode>),
    ExpressionStatement(Box<AstNode>),
    ReturnStatement(Option<Box<AstNode>>),
    Block(Vec<AstNode>, usize),
    IfStatement(Box<AstNode>, Box<AstNode>, Box<AstNode>),
    Program(Vec<AstNode>),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum VarLocation {
    Global,
    Local(usize),
    Upvalue(NodeId),
}

pub type NodeId = u64;

struct Scope {
    enclosing: Option<Box<Scope>>,
    locals: Vec<Local>,
    hoisted: Vec<(NodeId, String)>,
    upvalues: Vec<(NodeId, String)>,
    scope_depth: usize,
}

#[derive(Debug)]
struct Local {
    node: Option<NodeId>,
    name: String,
    depth: usize,
    index: usize,
    size: u8,
}

impl Scope {
    fn top() -> Self {
        Self {
            enclosing: None,
            locals: vec![
                // Represents main function
                Local {
                    node: None,
                    name: "".to_string(),
                    depth: 0,
                    index: 0,
                    size: 1,
                },
            ],
            hoisted: Vec::new(),
            upvalues: Vec::new(),
            scope_depth: 0,
        }
    }

    fn child(scope: Scope) -> Self {
        Self {
            enclosing: Some(Box::new(scope)),
            locals: vec![
                // Represents main function
                Local {
                    node: None,
                    name: "".to_string(),
                    depth: 0,
                    index: 0,
                    size: 1,
                },
            ],
            hoisted: Vec::new(),
            upvalues: Vec::new(),
            scope_depth: 0,
        }
    }

    fn add_local(
        &mut self,
        node: &AstNode,
        size: u8,
        locations: &mut HashMap<NodeId, VarLocation>,
    ) -> () {
        // Top level vars should stay globals
        if self.scope_depth > 0 || self.enclosing.is_some() {
            let index = self
                .locals
                .iter()
                .rev()
                .next()
                .map(|l| l.index + l.size as usize)
                .unwrap_or(0);
            locations.insert(node.id, VarLocation::Local(index));
            self.locals.push(Local {
                node: Some(node.id),
                name: node.var_node_name().to_string(),
                depth: self.scope_depth,
                index,
                size,
            });
        }
    }

    fn resolve(&mut self, node: &AstNode, locations: &mut HashMap<NodeId, VarLocation>) -> () {
        if let Some((_, index)) = find_local_location(&self.locals, node.var_node_name()) {
            locations.insert(node.id, VarLocation::Local(index));
        } else if let Some(node_id) = find_node(&self.upvalues, node.var_node_name()) {
            let loc = *locations.get(&node_id).expect("should be in locations map");
            locations.insert(node.id, loc);
        } else if let Some(node_id) = find_node(&self.hoisted, node.var_node_name()) {
            let loc = *locations.get(&node_id).expect("should be in locations map");
            locations.insert(node.id, loc);
        } else if let Some(parent) = &self.enclosing {
            parent.hoist(node, locations);
        }
    }

    fn hoist(&self, node: &AstNode, locations: &mut HashMap<NodeId, VarLocation>) -> () {
        if let Some((local_node_id, _)) = find_local_location(&self.locals, node.var_node_name()) {
            locations.insert(local_node_id, VarLocation::Upvalue(local_node_id));
            locations.insert(node.id, VarLocation::Upvalue(local_node_id));
        } else if let Some(node_id) = find_node(&self.upvalues, node.var_node_name()) {
            let loc = *locations.get(&node_id).expect("should be in locations map");
            locations.insert(node.id, loc);
        } else if let Some(node_id) = find_node(&self.hoisted, node.var_node_name()) {
            let loc = *locations.get(&node_id).expect("should be in locations map");
            locations.insert(node.id, loc);
        } else if let Some(parent) = &self.enclosing {
            parent.hoist(node, locations);
        }
    }

    fn begin_scope(&mut self) -> () {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) -> usize {
        self.scope_depth -= 1;

        let mut pop_n = 0;
        let mut truncate_size = 0;
        for (index, local) in self.locals.iter().enumerate().rev() {
            if local.depth > self.scope_depth {
                truncate_size = index + 1;
                //pop_n = local.index + local.size as usize;
                pop_n += local.size as usize;
            } else {
                break;
            }
        }

        self.locals.truncate(self.locals.len() - truncate_size);
        pop_n
    }

    fn close(self) -> Scope {
        self.enclosing
            .map(|enclose_box| *enclose_box)
            .unwrap_or(Scope::top())
    }
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

    fn var_node_name(&self) -> &str {
        if let AstNodeType::Variable {
            name,
            type_annotation: _,
        } = &self.node_type
        {
            &name
        } else {
            panic!("expected variable node")
        }
    }

    // TODO static analysis to check lvalues
    pub fn generate(self, vm: &mut VM) -> InterpretResult {
        self.generate_with_lvalue(vm, false)
    }

    fn generate_with_lvalue(self, vm: &mut VM, lvalue: bool) -> InterpretResult {
        match self.node_type {
            AstNodeType::Program(statements) => {
                for statement in statements {
                    statement.generate(vm)?;
                }
            }
            AstNodeType::FunctionDef {
                name,
                return_type: _,
                params,
                body,
            } => {
                vm.chunk_generators
                    .push(ChunkGenerator::new(FunctionType::Function));

                let arity = params.len() as u8;
                // TODO maybe just recurse for body. there's an extra popn but whatever
                match body.node_type {
                    AstNodeType::Block(statements, _) => {
                        for s in statements {
                            s.generate(vm)?;
                        }
                    }
                    _ => panic!("Invalid node type for function body"),
                }

                // create empty function without chunk so that chunk isn't GC'd before it's time
                let name_index = vm.allocate_string(&name);
                // put on stack to avoid GCing
                vm.stack.push(name_index);
                let func = FunctionObj::empty(name_index, arity, vm);

                let heap_index = vm.allocate(Object::new(ObjType::Function(Box::new(func))));

                // Add to second to last chunk generator (not the one for this function)
                let chunk_gen_index = vm.chunk_generators.len() - 2;
                vm.chunk_generators[chunk_gen_index].emit_constant(heap_index, self.line);
                //vm.gen().emit_constant(heap_index, self.line);

                let chunk = vm.chunk_generators.pop().unwrap().end();
                if let ObjType::Function(func) = &mut vm.heap.get_mut(&heap_index).value {
                    func.chunk = chunk;
                }
                // pop name index off stack
                vm.stack.pop();
            }
            AstNodeType::Call { target, args } => {
                target.generate(vm)?;
                let size = args
                    .iter()
                    .map(|a| {
                        a.data_type
                            .as_ref()
                            .expect("function argument must have type")
                            .size() as usize
                    })
                    .sum();
                for arg in args {
                    arg.generate(vm)?;
                }
                vm.gen().emit_byte(ByteCode::Call(size), self.line);
            }
            AstNodeType::PrintStatement(e) => {
                let code = match e.data_type {
                    Some(DataType::Int) => ByteCode::PrintInt,
                    Some(DataType::Float) => ByteCode::PrintFloat,
                    Some(DataType::Bool) => ByteCode::PrintBool,
                    Some(DataType::Str) => ByteCode::PrintObject,
                    Some(DataType::Function {
                        return_type: _,
                        parameters: _,
                    }) => ByteCode::PrintObject,
                    Some(DataType::Nil) => return error(self.line, "Can't print nil"),
                    None => panic!("None arg type for print statement: {:?}", e.data_type),
                };
                e.generate(vm)?;
                vm.gen().emit_byte(code, self.line)
            }
            AstNodeType::ExpressionStatement(e) => {
                let size = e
                    .data_type
                    .as_ref()
                    .map(|dt| dt.size())
                    .expect("No data type for expression statement");
                let size = size;
                e.generate(vm)?;
                vm.gen().emit_byte(ByteCode::Pop(size as usize), self.line)
            }
            AstNodeType::ReturnStatement(e) => {
                if vm.gen().func_type == FunctionType::Script {
                    return error(self.line, "Can't return from top-level code.");
                }
                let code = if let Some(e) = e {
                    let size = e
                        .data_type
                        .as_ref()
                        .expect("Return value must be typed")
                        .size();
                    e.generate(vm)?;
                    ByteCode::Return(size)
                } else {
                    ByteCode::Return(0)
                };
                vm.gen().emit_byte(code, self.line)
            }
            AstNodeType::DeclareStatement(lhs, rhs) => match lhs.node_type {
                AstNodeType::Variable {
                    name,
                    type_annotation: _,
                } => {
                    rhs.generate(vm)?;
                    match *vm
                        .var_locations
                        .get(&lhs.id)
                        .unwrap_or(&VarLocation::Global)
                    {
                        VarLocation::Local(_index) => {
                            // TODO?
                            // simply allow result of rhs to remain on the stack
                        }
                        VarLocation::Upvalue(node_id) => {
                            let heap_index = vm.allocate(Object::new(ObjType::Upvalue(0)));
                            vm.upvalue_allocations.insert(node_id, heap_index);
                            vm.gen().emit_byte(ByteCode::SetHeap(heap_index), self.line);
                        }
                        VarLocation::Global => {
                            let constant = identifier_constant(&name, vm);
                            vm.gen()
                                .emit_byte(ByteCode::DefineGlobal(constant), self.line)
                        }
                    }
                }
                _ => panic!("Invalid lhs for let"),
            },
            AstNodeType::Block(statements, block_bytes) => {
                for s in statements {
                    s.generate(vm)?;
                }
                vm.gen().emit_byte(ByteCode::Pop(block_bytes), self.line)
            }
            AstNodeType::IfStatement(condition, then_block, else_block) => {
                condition.generate(vm)?;
                let then_jump = vm.gen().emit_jump_if_false(self.line);

                vm.gen().emit_byte(ByteCode::Pop(1), self.line); // pop condition
                then_block.generate(vm)?;

                let else_jump = vm.gen().emit_jump(self.line);
                vm.gen().patch_jump(then_jump);

                vm.gen().emit_byte(ByteCode::Pop(1), self.line); // pop condition
                else_block.generate(vm)?;
                vm.gen().patch_jump(else_jump);
            }
            AstNodeType::WhileStatement(condition, loop_block) => {
                let loop_start = vm.gen().loop_start();
                condition.generate(vm)?;
                let exit_jump = vm.gen().emit_jump_if_false(self.line);
                vm.gen().emit_byte(ByteCode::Pop(1), self.line); // pop condition
                loop_block.generate(vm)?;
                vm.gen().emit_loop(loop_start, self.line);
                vm.gen().patch_jump(exit_jump);
            }
            AstNodeType::Variable {
                name,
                type_annotation: _,
            } => {
                if !lvalue {
                    let var_locations = &vm.var_locations;
                    match var_locations.get(&self.id).unwrap_or(&VarLocation::Global) {
                        VarLocation::Local(index) => {
                            let gen = vm.chunk_generators.iter_mut().rev().nth(0).unwrap();
                            gen.emit_byte(ByteCode::GetLocal(*index), self.line)
                        }
                        VarLocation::Upvalue(node_id) => {
                            let gen = vm.chunk_generators.iter_mut().rev().nth(0).unwrap();
                            let upvalue_allocations = &mut vm.upvalue_allocations;
                            let heap_index = upvalue_allocations
                                .get(&node_id)
                                .expect("Unallocated upvalue");
                            gen.emit_byte(ByteCode::GetHeap(*heap_index), self.line);
                        }
                        VarLocation::Global => {
                            let constant = identifier_constant(&name, vm);
                            let gen = vm.chunk_generators.iter_mut().rev().nth(0).unwrap();
                            gen.emit_byte(ByteCode::GetGlobal(constant), self.line)
                        }
                    }
                }
            }
            AstNodeType::IntLiteral(value) => vm.gen().emit_constant(value, self.line),
            AstNodeType::FloatLiteral(value) => vm.gen().emit_constant(value as i64, self.line),
            AstNodeType::BoolLiteral(value) => {
                vm.gen().emit_constant(if value { 1 } else { 0 }, self.line)
            }
            AstNodeType::StrLiteral(value) => {
                let heap_index = vm.allocate_string(&value);
                vm.gen().emit_constant(heap_index, self.line)
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
                operand.generate(vm)?;
                vm.gen().emit_byte(code, self.line)
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
                        Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Bool) => {
                            ByteCode::Equal
                        }
                        Some(DataType::Function {
                            return_type: _,
                            parameters: _,
                        })
                        | Some(DataType::Str) => ByteCode::EqualHeap,
                        _ => panic!("Invalid type for equal"),
                    },
                    Operator::NotEqual => match &lhs.data_type {
                        Some(DataType::Int) | Some(DataType::Float) | Some(DataType::Bool) => {
                            ByteCode::NotEqual
                        }
                        Some(DataType::Function {
                            return_type: _,
                            parameters: _,
                        })
                        | Some(DataType::Str) => ByteCode::NotEqualHeap,
                        _ => panic!("Invalid type for equal"),
                    },
                    Operator::Assign => {
                        let code = match &lhs.node_type {
                            AstNodeType::Variable {
                                name,
                                type_annotation: _,
                            } => match vm
                                .var_locations
                                .get(&lhs.id)
                                .unwrap_or(&VarLocation::Global)
                            {
                                VarLocation::Local(index) => ByteCode::SetLocal(*index),
                                VarLocation::Upvalue(node_id) => {
                                    let heap_index = vm
                                        .upvalue_allocations
                                        .get(&node_id)
                                        .expect("Unallocated upvalue");
                                    ByteCode::SetHeap(*heap_index)
                                }
                                VarLocation::Global => {
                                    let constant = identifier_constant(&name, vm);
                                    ByteCode::SetGlobal(constant)
                                }
                            },
                            // TODO for dotted stuff, recurse
                            _ => panic!("Invalid lvalue"),
                        };
                        lhs.generate_with_lvalue(vm, true)?;
                        rhs.generate(vm)?;

                        vm.gen().emit_byte(code, self.line);
                        return Ok(());
                    }
                    Operator::And => {
                        lhs.generate(vm)?;
                        let end_jump = vm.gen().emit_jump_if_false(self.line);
                        // If lhs was true, pop lhs value and use value of rhs
                        vm.gen().emit_byte(ByteCode::Pop(1), self.line);
                        rhs.generate(vm)?;
                        vm.gen().patch_jump(end_jump);
                        return Ok(());
                    }
                    Operator::Or => {
                        lhs.generate(vm)?;
                        // If lhs was false jump over the unconditional jump
                        let else_jump = vm.gen().emit_jump_if_false(self.line);
                        // else lhs was true, so jump to the end
                        let end_jump = vm.gen().emit_jump(self.line);
                        vm.gen().patch_jump(else_jump);

                        // If lhs was false, pop lhs value and use rhs value
                        vm.gen().emit_byte(ByteCode::Pop(1), self.line);
                        rhs.generate(vm)?;
                        vm.gen().patch_jump(end_jump);
                        return Ok(());
                    }
                    _ => panic!("Unexpected binary code gen"),
                };
                lhs.generate(vm)?;
                rhs.generate(vm)?;
                vm.gen().emit_byte(code, self.line)
            }
            AstNodeType::Error => panic!("Shouldn't try to generate code for Error node"),
        }
        Ok(())
    }

    fn resolve_variables<'a>(&'a mut self, mut scope: Scope, vm: &mut VM) -> Scope {
        match &mut self.node_type {
            AstNodeType::IntLiteral(_)
            | AstNodeType::FloatLiteral(_)
            | AstNodeType::StrLiteral(_)
            | AstNodeType::BoolLiteral(_)
            | AstNodeType::Error
            | AstNodeType::ReturnStatement(None) => scope,
            AstNodeType::Unary(_, e)
            | AstNodeType::PrintStatement(e)
            | AstNodeType::ExpressionStatement(e)
            | AstNodeType::ReturnStatement(Some(e)) => e.resolve_variables(scope, vm),
            AstNodeType::Program(stmts) => {
                for s in stmts {
                    scope = s.resolve_variables(scope, vm);
                }
                scope
            }
            AstNodeType::Binary(_, lhs, rhs) | AstNodeType::WhileStatement(lhs, rhs) => {
                scope = lhs.resolve_variables(scope, vm);
                rhs.resolve_variables(scope, vm)
            }
            AstNodeType::IfStatement(cond, t_block, f_block) => {
                scope = cond.resolve_variables(scope, vm);
                scope = t_block.resolve_variables(scope, vm);
                f_block.resolve_variables(scope, vm)
            }
            AstNodeType::Call { target, args } => {
                scope = target.resolve_variables(scope, vm);
                for a in args {
                    scope = a.resolve_variables(scope, vm);
                }
                scope
            }
            AstNodeType::Block(stmts, block_bytes) => {
                scope.begin_scope();
                for s in stmts {
                    scope = s.resolve_variables(scope, vm);
                }
                *block_bytes = scope.end_scope();
                scope
            }
            AstNodeType::DeclareStatement(lhs, rhs) => {
                //scope.add_local(lhs, lhs.data_type.as_ref().expect("variable needs data type").size());
                scope.add_local(
                    lhs,
                    lhs.data_type
                        .as_ref()
                        .expect("variable needs data type")
                        .size(),
                    &mut vm.var_locations,
                );
                rhs.resolve_variables(scope, vm)
            }
            AstNodeType::FunctionDef {
                name: _,
                return_type: _,
                params,
                body,
            } => {
                let mut inner_scope = Scope::child(scope);

                for param in params {
                    inner_scope.add_local(
                        param,
                        param
                            .data_type
                            .as_ref()
                            .expect("variable needs data type")
                            .size(),
                        &mut vm.var_locations,
                    );
                }

                inner_scope = body.resolve_variables(inner_scope, vm);
                inner_scope.close()
            }
            AstNodeType::Variable {
                name: _,
                type_annotation: _,
            } => {
                scope.resolve(self, &mut vm.var_locations);
                scope
            }
        }
    }

    pub fn resolve_types(&mut self, substitutions: &Vec<TypeConstraint>) -> InterpretResult {
        let data_type = find_type(&self, substitutions)?;
        self.data_type = Some(data_type);

        match &mut self.node_type {
            AstNodeType::Unary(_, operand) => {
                operand.resolve_types(substitutions)?;
            }
            AstNodeType::Binary(_, lhs, rhs) => {
                lhs.resolve_types(substitutions)?;
                rhs.resolve_types(substitutions)?;
            }
            AstNodeType::Block(statements, _) => {
                let statements: Result<Vec<_>, _> = statements
                    .into_iter()
                    .map(|s| s.resolve_types(substitutions))
                    .collect();
                statements?;
            }
            AstNodeType::Program(statements) => {
                let statements: Result<Vec<_>, _> = statements
                    .into_iter()
                    .map(|s| s.resolve_types(substitutions))
                    .collect();
                statements?;
            }
            AstNodeType::PrintStatement(e) | AstNodeType::ExpressionStatement(e) => {
                e.resolve_types(substitutions)?;
            }
            AstNodeType::ReturnStatement(e) => {
                if let Some(e) = e {
                    e.resolve_types(substitutions)?;
                }
            }
            AstNodeType::DeclareStatement(lhs, rhs) => {
                lhs.resolve_types(substitutions)?;
                rhs.resolve_types(substitutions)?;
            }
            AstNodeType::IfStatement(condition, then_block, else_block) => {
                condition.resolve_types(substitutions)?;
                then_block.resolve_types(substitutions)?;
                else_block.resolve_types(substitutions)?;
            }
            AstNodeType::WhileStatement(condition, loop_block) => {
                condition.resolve_types(substitutions)?;
                loop_block.resolve_types(substitutions)?;
            }
            AstNodeType::Call { target, args } => {
                target.resolve_types(substitutions)?;
                let args: Result<Vec<_>, _> = args
                    .into_iter()
                    .map(|arg| arg.resolve_types(substitutions))
                    .collect();
                args?;
            }
            AstNodeType::FunctionDef {
                name: _,
                return_type: _,
                params,
                body,
            } => {
                let params: Result<Vec<_>, _> = params
                    .into_iter()
                    .map(|param| param.resolve_types(substitutions))
                    .collect();
                params?;
                body.resolve_types(substitutions)?;
            }
            AstNodeType::Variable {
                name: _,
                type_annotation: _,
            }
            | AstNodeType::IntLiteral(_)
            | AstNodeType::FloatLiteral(_)
            | AstNodeType::StrLiteral(_)
            | AstNodeType::BoolLiteral(_)
            | AstNodeType::Error => (),
        }
        Ok(())
    }
}

fn error(line: u32, message: &str) -> InterpretResult {
    eprintln!("[line {}] Error: {}", line, message);
    Err(InterpretError::Compile)
}

fn identifier_constant(name: &str, vm: &mut VM) -> u8 {
    let heap_index = vm.allocate_string(name);
    vm.gen().add_constant(heap_index)
}

pub fn analyze(ast: &mut AstNode, vm: &mut VM) -> InterpretResult {
    let substitutions = generate_substitutions(ast);
    match substitutions {
        Ok(substitutions) => {
            ast.resolve_types(&substitutions)?;
            final_type_check(&ast)?;

            ast.resolve_variables(Scope::top(), vm);

            #[cfg(feature = "debug-logging")]
            eprintln!("{:#?}", ast);
            Ok(())
        }
        Err(e) => {
            eprintln!("{:?}", e);
            Err(InterpretError::Compile)
        }
    }
}

fn find_local_location(locals: &[Local], name: &str) -> Option<(NodeId, usize)> {
    locals
        .iter()
        .rev()
        .find(|local| local.name == name)
        .and_then(|local| local.node.map(|id| (id, local.index)))
}

fn find_node(nodes: &[(NodeId, String)], name: &str) -> Option<NodeId> {
    nodes
        .iter()
        .rev()
        .find(|(_, node_name)| node_name == name)
        .map(|(node_id, _)| *node_id)
}
