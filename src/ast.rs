use crate::chunk::ByteCode;
use crate::code_gen::{FunctionType, Generator};
use crate::common::{InterpretError, InterpretResult};
use crate::heap::Heap;
use crate::object::{ObjType, Object, FunctionObj, ClosureObj};
use crate::type_check::{find_type, DataType, TypeConstraint, final_type_check, generate_substitutions};

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

    Variable{name: String, type_annotation: Option<String>, location: VarLocation},

    Call {
        target: Box<AstNode>,
        args: Vec<AstNode>,
    },

    WhileStatement(Box<AstNode>, Box<AstNode>),
    DeclareStatement(Box<AstNode>, Box<AstNode>),
    FunctionDef {
        return_type: String,
        params: Vec<AstNode>,
        body: Box<AstNode>,
        hoisted: u8,
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
    Heap(usize),
}

struct Scope<'a> {
    enclosing: Option<Box<Scope<'a>>>,
    locals: Vec<Local<'a>>,
    scope_depth: usize,
    hoisted: u8,
}

#[derive(Debug)]
struct Local<'a> {
    node: Option<&'a mut AstNode>,
    depth: usize,
    index: usize,
    size: u8,
}

impl <'a> Scope<'a> {
    fn top() -> Self {
        Self {
            enclosing: None,
            locals: vec![
                // Represents main function
                Local{
                    node: None,
                    depth: 0,
                    index: 0,
                    size: 8,
                }
            ],
            hoisted: 0,
            scope_depth: 0,
        }
    }

    fn child(scope: Scope<'a>) -> Self {
        Self {
            enclosing: Some(Box::new(scope)),
            locals: vec![
                // Represents main function
                Local{
                    node: None,
                    depth: 0,
                    index: 0,
                    size: 8,
                }
            ],
            hoisted: 0,
            scope_depth: 0,
        }
    } 

    fn add_local(&mut self, node: &'a mut AstNode, size: u8) -> () {
        // Top level vars should stay globals
        if self.scope_depth > 0 || self.enclosing.is_some() {
            let index = self.locals
                .iter()
                .rev()
                .next()
                .map(|l| l.index + l.size as usize)
                .unwrap_or(0);
            if let AstNodeType::Variable{name: _, type_annotation: _, location} = &mut node.node_type {
                *location = VarLocation::Local(index);
            } else {panic!("locals should be variables")}
            self.locals.push(Local {
                node: Some(node),
                depth: self.scope_depth,
                index,
                size,
            });
        }
    }

    fn resolve(&mut self, name: &str, location: &'a mut VarLocation) -> () {
        for local in self.locals.iter_mut().rev() {
            if let Some(node) = &mut local.node {
                if let AstNodeType::Variable{name: local_name, type_annotation: _, location: _} = &mut node.node_type {
                    if name == local_name {
                        *location = VarLocation::Local(local.index);
                        return;
                    }
                } else {panic!("locals should be variables")}
            }
        }

        if let Some(parent) = &mut self.enclosing {
            parent.hoist(name, location);
        }
    }

    fn hoist(&mut self, name: &str, location: &'a mut VarLocation) -> () {
        for local in self.locals.iter_mut().rev() {
            if let Some(node) = &mut local.node {
                if let AstNodeType::Variable{name: local_name, type_annotation: _, location: local_location} = &mut node.node_type {
                    if name == local_name {
                        // TODO change type of VarLocation::Heap content
                        // TODO check not too many hoisted and do a compiler error
                        *local_location = VarLocation::Heap(self.hoisted as usize);
                        *location = VarLocation::Heap(self.hoisted as usize);
                        self.hoisted += 1;
                        return;
                    }
                } else {panic!("locals should be variables")}
            }
        }

        if let Some(parent) = &mut self.enclosing {
            parent.hoist(name, location);
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


    fn close(self) -> (u8, Scope<'a>) {
        (self.hoisted, self.enclosing.map(|enclose_box| *enclose_box).unwrap_or(Scope::top()))
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

    // TODO static analysis to check lvalues
    pub fn generate(self, generator: &mut Generator, heap: &mut Heap) -> InterpretResult {
        self.generate_with_lvalue(generator, heap, false)
    }

    fn generate_with_lvalue(
        self,
        generator: &mut Generator,
        heap: &mut Heap,
        lvalue: bool,
    ) -> InterpretResult {
        match self.node_type {
            AstNodeType::Program(statements) => {
                for statement in statements {
                    statement.generate(generator, heap)?;
                }
            }
            AstNodeType::FunctionDef {
                return_type: _,
                params,
                body,
                hoisted,
            } => {
                // TODO pass in function name for debugging
                let mut child_generator = Generator::new(FunctionType::Function);

                let arity = params.len() as u8;
                // TODO maybe just recurse for body. there's an extra popn but whatever
                match body.node_type {
                    AstNodeType::Block(statements, _) => {
                        for s in statements {
                            s.generate(&mut child_generator, heap)?;
                        }
                    }
                    _ => panic!("Invalid node type for function body"),
                }

                // TODO function name
                let chunk = child_generator.end();
                let closure = ClosureObj::new(chunk, arity, hoisted, heap);
                let closure_heap_index = heap.allocate(Object::new(ObjType::Closure(Box::new(closure))));
                generator.emit_constant_8(&closure_heap_index.to_be_bytes(), self.line)
            }
            AstNodeType::Call { target, args } => {
                target.generate(generator, heap)?;
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
                    arg.generate(generator, heap)?;
                }
                generator.emit_byte(ByteCode::Call(size), self.line);
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
                e.generate(generator, heap)?;
                generator.emit_byte(code, self.line)
            }
            AstNodeType::ExpressionStatement(e) => {
                let size = e.data_type.as_ref().map(|dt| dt.size()).expect("No data type for expression statement");
                let size = size;
                e.generate(generator, heap)?;
                generator.emit_byte(ByteCode::Pop(size as usize), self.line)
            }
            AstNodeType::ReturnStatement(e) => {
                if generator.func_type == FunctionType::Script {
                    return error(self.line, "Can't return from top-level code.");
                }
                let code = if let Some(e) = e {
                    let size = e
                        .data_type
                        .as_ref()
                        .expect("Return value must be typed")
                        .size();
                    e.generate(generator, heap)?;
                    ByteCode::Return(size)
                } else {
                    ByteCode::Return(0)
                };
                generator.emit_byte(code, self.line)
            }
            AstNodeType::DeclareStatement(lhs, rhs) => match lhs.node_type {
                AstNodeType::Variable{name, type_annotation: _, location} => {
                    let size = rhs.data_type.as_ref().map(|dt| dt.size()).expect("No data type for variable");
                    let is_prim = 
                        match rhs.data_type.as_ref() {
                            Some(DataType::Int)
                                | Some(DataType::Float)
                                | Some(DataType::Bool) => true,
                            _ => false,
                        };
                    match location {
                        VarLocation::Local(_index) => {
                            // TODO?
                            rhs.generate(generator, heap)?;
                            // simply allow result of rhs to remain on the stack
                        },
                        VarLocation::Heap(index) => {
                            rhs.generate(generator, heap)?;
                            // note that this keeps it on the stack too so that other locals' stack
                            // indexes don't get thrown off
                            if is_prim {
                                generator.emit_byte(ByteCode::ToHeap(size), self.line);
                            }
                            generator.emit_byte(ByteCode::SetUpvalue(index, size), self.line);
                        },
                        VarLocation::Global => {
                            let constant = identifier_constant(&name, heap, generator);
                            rhs.generate(generator, heap)?;
                            generator.emit_byte(ByteCode::DefineGlobal(constant, size), self.line)
                        },
                    }
                }
                _ => panic!("Invalid lhs for let"),
            },
            AstNodeType::Block(statements, block_bytes) => {
                for s in statements {
                    s.generate(generator, heap)?;
                }
                generator.emit_byte(ByteCode::Pop(block_bytes), self.line)
            }
            AstNodeType::IfStatement(condition, then_block, else_block) => {
                condition.generate(generator, heap)?;
                let then_jump = generator.emit_jump_if_false(self.line);

                generator.emit_byte(ByteCode::Pop(1), self.line); // pop condition
                then_block.generate(generator, heap)?;

                let else_jump = generator.emit_jump(self.line);
                generator.patch_jump(then_jump);

                generator.emit_byte(ByteCode::Pop(1), self.line); // pop condition
                else_block.generate(generator, heap)?;
                generator.patch_jump(else_jump);
            }
            AstNodeType::WhileStatement(condition, loop_block) => {
                let loop_start = generator.loop_start();
                condition.generate(generator, heap)?;
                let exit_jump = generator.emit_jump_if_false(self.line);
                generator.emit_byte(ByteCode::Pop(1), self.line); // pop condition
                loop_block.generate(generator, heap)?;
                generator.emit_loop(loop_start, self.line);
                generator.patch_jump(exit_jump);
                generator.emit_byte(ByteCode::Pop(1), self.line); // pop condition
            }
            AstNodeType::Variable{name, type_annotation: _, location} => {
                if !lvalue {
                    let is_prim = 
                        match &self.data_type {
                            Some(DataType::Int)
                                | Some(DataType::Float)
                                | Some(DataType::Bool) => true,
                            _ => false,
                        };
                    let size = self.data_type.map(|dt| dt.size()).expect("No data type for variable");
                    match location {
                        VarLocation::Local(index) => {
                            generator.emit_byte(ByteCode::GetLocal(index, size), self.line)
                        },
                        VarLocation::Heap(index) => {
                            generator.emit_byte(ByteCode::GetUpvalue(index, size), self.line);
                            if is_prim {
                                generator.emit_byte(ByteCode::GetHeap, self.line);
                            }
                        },
                        VarLocation::Global => {
                            let constant = identifier_constant(&name, heap, generator);
                            generator.emit_byte(ByteCode::GetGlobal(constant, size), self.line)
                        }
                    }
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
                operand.generate(generator, heap)?;
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
                        Some(DataType::Int) | Some(DataType::Float) => ByteCode::Equal(8),
                        Some(DataType::Bool) => ByteCode::Equal(1),
                        Some(DataType::Function{return_type: _, parameters: _}) | Some(DataType::Str) => ByteCode::EqualHeap,
                        _ => panic!("Invalid type for equal"),
                    },
                    Operator::NotEqual => match &lhs.data_type {
                        Some(DataType::Int) | Some(DataType::Float) => ByteCode::NotEqual(8),
                        Some(DataType::Bool) => ByteCode::NotEqual(1),
                        Some(DataType::Function{return_type: _, parameters: _}) | Some(DataType::Str) => ByteCode::NotEqualHeap,
                        _ => panic!("Invalid type for equal"),
                    },
                    Operator::Assign => {
                        let size = lhs.data_type.as_ref().map(|dt| dt.size()).expect("No data type for variable");
                        let is_prim = 
                            match rhs.data_type.as_ref() {
                                Some(DataType::Int)
                                    | Some(DataType::Float)
                                    | Some(DataType::Bool) => true,
                                    _ => false,
                            };
                        let code = match &lhs.node_type {
                            AstNodeType::Variable{name, type_annotation: _, location} => match location {
                                VarLocation::Local(index) => ByteCode::SetLocal(*index, size),
                                VarLocation::Heap(index) => ByteCode::SetUpvalue(*index, size),
                                VarLocation::Global => {
                                    let constant = identifier_constant(&name, heap, generator);
                                    ByteCode::SetGlobal(constant, size)
                                },
                            },
                            // TODO for dotted stuff, recurse
                            _ => panic!("Invalid lvalue"),
                        };
                        lhs.generate_with_lvalue(generator, heap, true);
                        rhs.generate(generator, heap)?;

                        if is_prim {
                            generator.emit_byte(ByteCode::ToHeap(size), self.line);
                        }
                        generator.emit_byte(code, self.line);
                        return Ok(());
                    }
                    Operator::And => {
                        lhs.generate(generator, heap)?;
                        let end_jump = generator.emit_jump_if_false(self.line);
                        // If lhs was true, pop lhs value and use value of rhs
                        generator.emit_byte(ByteCode::Pop(1), self.line);
                        rhs.generate(generator, heap)?;
                        generator.patch_jump(end_jump);
                        return Ok(());
                    }
                    Operator::Or => {
                        lhs.generate(generator, heap)?;
                        // If lhs was false jump over the unconditional jump
                        let else_jump = generator.emit_jump_if_false(self.line);
                        // else lhs was true, so jump to the end
                        let end_jump = generator.emit_jump(self.line);
                        generator.patch_jump(else_jump);

                        // If lhs was false, pop lhs value and use rhs value
                        generator.emit_byte(ByteCode::Pop(1), self.line);
                        rhs.generate(generator, heap)?;
                        generator.patch_jump(end_jump);
                        return Ok(());
                    }
                    _ => panic!("Unexpected binary code gen"),
                };
                lhs.generate(generator, heap)?;
                rhs.generate(generator, heap)?;
                generator.emit_byte(code, self.line)
            }
            AstNodeType::Error => panic!("Shouldn't try to generate code for Error node"),
        }
        Ok(())
    }

    fn resolve_variables<'a>(&'a mut self, mut scope: Scope<'a>) -> Scope<'a> {
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
            | AstNodeType::ReturnStatement(Some(e)) => e.resolve_variables(scope),
            AstNodeType::Program(stmts) => {
                for s in stmts {
                    scope = s.resolve_variables(scope);
                }
                scope
            }
            AstNodeType::Binary(_, lhs, rhs)
            | AstNodeType::WhileStatement(lhs, rhs) => {
                scope = lhs.resolve_variables(scope);
                rhs.resolve_variables(scope)
            }
            AstNodeType::IfStatement(cond, t_block, f_block) => {
                scope = cond.resolve_variables(scope);
                scope = t_block.resolve_variables(scope);
                f_block.resolve_variables(scope)
            }
            AstNodeType::Call{ target, args } => {
                scope = target.resolve_variables(scope);
                for a in args {
                    scope = a.resolve_variables(scope);
                }
                scope
            }
            AstNodeType::Block(stmts, block_bytes) => {
                //let mut inner_scope = Scope::child(scope);
                scope.begin_scope();
                for s in stmts {
                    scope = s.resolve_variables(scope);
                }
                *block_bytes = scope.end_scope();
                //inner_scope.close()
                scope
            }
            AstNodeType::DeclareStatement(lhs, rhs) => {
                scope.add_local(lhs, lhs.data_type.as_ref().expect("variable needs data type").size());
                rhs.resolve_variables(scope)
            }
            /*
                match &mut lhs.node_type {
                AstNodeType::Variable{name, type_annotation: _, location} => {
                    if let ScopeEnclosure::Top = scope.enclosing {
                        // Leave global if at top level
                        // *location = VarLocation::Local(index);
                    }
                    rhs.resolve_variables(scope)
                }
                _ => panic!("lhs of declare must be variable"),
            }
                */
            AstNodeType::FunctionDef{ return_type: _, params, body, hoisted} => {
                let mut inner_scope = Scope::child(scope);

                for param in params {
                    inner_scope.add_local(param, param.data_type.as_ref().expect("variable needs data type").size());
                }

                inner_scope = body.resolve_variables(inner_scope);
                /*
                match &mut body.node_type {
                    AstNodeType::Block(stmts) => {
                        for s in stmts {
                            inner_scope = s.resolve_variables(inner_scope);
                        }
                    }
                    _ => panic!("Function body must be a Block"),
                }
                */
                let (inner_hoisted, scope) = inner_scope.close();
                *hoisted = inner_hoisted;
                scope
            }
            AstNodeType::Variable{name, type_annotation: _, location} => {
                scope.resolve(name, location);
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
                return_type: _,
                params,
                body,
                hoisted: _,
            } => {
                let params: Result<Vec<_>, _> = params
                    .into_iter()
                    .map(|param| param.resolve_types(substitutions))
                    .collect();
                params?;
                body.resolve_types(substitutions)?;
            }
            AstNodeType::Variable{name, type_annotation: _, location} => {
                // TODO location resolution
            },
            AstNodeType::IntLiteral(_)
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

fn identifier_constant(name: &str, heap: &mut Heap, generator: &mut Generator) -> u8 {
    let heap_index = heap.allocate_string(name.to_string());
    generator.add_constant(&heap_index.to_be_bytes())
}

pub fn analyze(ast: &mut AstNode) -> InterpretResult {
    let substitutions = generate_substitutions(ast);
    match substitutions {
        Ok(substitutions) => {
            ast.resolve_types(&substitutions)?;
            final_type_check(&ast)?;

            ast.resolve_variables(Scope::top());

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
