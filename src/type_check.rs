use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::ast::{AstNode, AstNodeType, Operator};
use crate::common::{InterpretError, InterpretResult};
use crate::object::NativeFunctionObj;

#[derive(Debug)]
pub struct TypeConstraint {
    left: TCSide,
    right: TCSide,
}

impl TypeConstraint {
    fn new(left: TCSide, right: TCSide) -> Self {
        TypeConstraint { left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TCSide {
    Expr(u64),
    Constraint {
        base_type: TCNodeType,
        return_type: Option<Box<TCSide>>,
        parameters: Vec<TCSide>,
    },
}

impl TCSide {
    fn basic(node_type: TCNodeType) -> Self {
        TCSide::Constraint {
            base_type: node_type,
            return_type: None,
            parameters: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TCNodeType {
    Int,
    Float,
    Bool,
    Str,
    Function,
    Nil,
    // TOOD Array,
}

impl TCNodeType {
    fn try_from(s: &str) -> Result<Self, TypeError> {
        match s {
            "int" => Ok(Self::Int),
            "float" => Ok(Self::Float),
            "bool" => Ok(Self::Bool),
            "str" => Ok(Self::Str),
            "nil" => Ok(Self::Nil),
            _ => {
                // TODO print something useful
                Err(TypeError::InvalidTypeAnnonation(s.to_string()))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Str,
    Function {
        return_type: Box<DataType>,
        parameters: Vec<DataType>,
    },
    Nil,
    // TODO Array(Box<NodeType>),
}

impl DataType {
    pub fn size(&self) -> u8 {
        match self {
            DataType::Int
            | DataType::Float
            | DataType::Str
            | DataType::Function {
                return_type: _,
                parameters: _,
            }
            | DataType::Bool => 1,
            DataType::Nil => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    Mismatched, // TODO add type params
    InvalidTypeAnnonation(String),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::Mismatched => write!(f, "Mismatched types.",),
            TypeError::InvalidTypeAnnonation(name) => {
                write!(f, "Invalid type annotation: {}", name,)
            }
        }
    }
}

impl error::Error for TypeError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

#[derive(Debug)]
pub struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    variables: HashMap<String, TCSide>,
}

impl<'a> Scope<'a> {
    pub fn new(parent: Option<&'a Scope<'a>>) -> Self {
        let mut s = Self {
            parent,
            variables: HashMap::new(),
        };

        // include native functions in global scope
        if parent.is_none() {
            for f in NativeFunctionObj::all() {
                s.variables.insert(
                    f.name().to_string(),
                    match f {
                        NativeFunctionObj::Clock => TCSide::Constraint {
                            base_type: TCNodeType::Function,
                            return_type: Some(Box::new(TCSide::Constraint {
                                base_type: TCNodeType::Float,
                                return_type: None,
                                parameters: Vec::new(),
                            })),
                            parameters: Vec::new(),
                        },
                    },
                );
            }
        }

        s
    }

    fn get(&self, name: &str) -> Option<TCSide> {
        self.variables
            .get(name)
            .map(|s| s.clone())
            .or_else(|| self.parent.and_then(|p| p.get(name)))
    }

    fn insert(&mut self, name: &str, value: TCSide) -> () {
        self.variables.insert(name.to_string(), value);
    }
}

pub fn find_type(
    node: &AstNode,
    substitutions: &[TypeConstraint],
) -> Result<DataType, InterpretError> {
    side_to_data_type(
        find_type_helper(&TCSide::Expr(node.id), substitutions, &mut Vec::new()).as_ref(),
        substitutions,
    )
}

fn side_to_data_type(
    side: Option<&TCSide>,
    substitutions: &[TypeConstraint],
) -> Result<DataType, InterpretError> {
    match side {
        None => Err(error("Failed to infer type")),
        Some(TCSide::Expr(_)) => Err(error("failed to infer type")),
        Some(TCSide::Constraint {
            base_type,
            return_type,
            parameters,
        }) => Ok(match base_type {
            TCNodeType::Int => DataType::Int,
            TCNodeType::Float => DataType::Float,
            TCNodeType::Bool => DataType::Bool,
            TCNodeType::Str => DataType::Str,
            TCNodeType::Nil => DataType::Nil,
            TCNodeType::Function => {
                let return_type = if let Some(return_type) = return_type {
                    find_side_data_type(return_type, substitutions)
                } else {
                    Err(error("Failed to find return type"))
                }?;
                let parameters: Result<Vec<_>, _> = parameters
                    .iter()
                    .map(|p| find_side_data_type(p, substitutions))
                    .collect();
                DataType::Function {
                    return_type: Box::new(return_type),
                    parameters: parameters?,
                }
            }
        }),
    }
}

fn find_side_data_type(
    target: &TCSide,
    substitutions: &[TypeConstraint],
) -> Result<DataType, InterpretError> {
    side_to_data_type(
        find_type_helper(target, substitutions, &mut Vec::new()).as_ref(),
        substitutions,
    )
}

fn error(message: &str) -> InterpretError {
    eprintln!("Error: {}", message);
    InterpretError::Compile
}

fn find_type_helper<'a>(
    target: &'a TCSide,
    substitutions: &'a [TypeConstraint],
    visited: &mut Vec<&'a TCSide>,
) -> Option<TCSide> {
    if let TCSide::Expr(id) = target {
        substitutions.iter().find_map(|sub| {
            let mut maybe_result = None;
            if !visited.contains(&target) {
                visited.push(target);
                maybe_result = match &sub {
                    TypeConstraint {
                        left: TCSide::Expr(sub_id),
                        right,
                    } if sub_id == id => find_type_helper(right, substitutions, visited),
                    TypeConstraint {
                        left,
                        right: TCSide::Expr(sub_id),
                    } if sub_id == id => find_type_helper(left, substitutions, visited),
                    _ => None,
                };
                visited.retain(|item| *item != target);
            }
            maybe_result
        })
    } else {
        Some(target.clone())
    }
}

pub fn generate_substitutions(node: &AstNode) -> Result<Vec<TypeConstraint>, TypeError> {
    let mut scope = Scope::new(None);
    let constraints = generate_constraints(node, &mut scope)?;
    unify(constraints)
}

fn check_type(node: &AstNode, dt: &DataType) -> Result<(), TypeError> {
    check_types(node, &[&dt])
}

fn check_numeric(node: &AstNode) -> Result<(), TypeError> {
    check_types(node, &[&DataType::Int, &DataType::Float])
}

fn check_plus_types(node: &AstNode) -> Result<(), TypeError> {
    check_types(node, &[&DataType::Int, &DataType::Float, &DataType::Str])
}

fn check_types(node: &AstNode, types: &[&DataType]) -> Result<(), TypeError> {
    if let Some(dt) = &node.data_type {
        if types.contains(&dt) {
            Ok(())
        } else {
            Err(TypeError::Mismatched)
        }
    } else {
        Err(TypeError::Mismatched)
    }
}

fn check_node_has_type(node: &AstNode) -> Result<(), TypeError> {
    if node.data_type.is_some() {
        Ok(())
    } else {
        Err(TypeError::Mismatched)
    }
}

pub fn final_type_check(node: &AstNode) -> InterpretResult {
    check_operator_constraints(node).map_err(|e| {
        error(&format!("{}", e));
        InterpretError::Compile
    })
}

fn check_operator_constraints(node: &AstNode) -> Result<(), TypeError> {
    match &node.node_type {
        AstNodeType::IntLiteral(_) => Ok(()),
        AstNodeType::FloatLiteral(_) => Ok(()),
        AstNodeType::BoolLiteral(_) => Ok(()),
        AstNodeType::StrLiteral(_) => Ok(()),
        AstNodeType::Unary(operator, operand) => {
            check_operator_constraints(operand)?;
            match operator {
                Operator::Neg => {
                    check_numeric(node)?;
                    check_numeric(operand)
                }
                Operator::Not => {
                    check_type(node, &DataType::Bool)?;
                    check_type(operand, &DataType::Bool)
                }
                _ => panic!("Invalid unary operator"),
            }
        }
        AstNodeType::Binary(operator, a, b) => {
            check_operator_constraints(a)?;
            check_operator_constraints(b)?;
            match operator {
                Operator::And | Operator::Or => {
                    check_type(node, &DataType::Bool)?;
                    check_type(a, &DataType::Bool)?;
                    check_type(b, &DataType::Bool)
                }
                Operator::Add => {
                    check_plus_types(a)?;
                    check_plus_types(b)?;
                    check_plus_types(node)
                }
                Operator::Sub | Operator::Mul | Operator::Div => {
                    check_numeric(a)?;
                    check_numeric(b)?;
                    check_numeric(node)
                }
                Operator::Equal | Operator::NotEqual => check_type(node, &DataType::Bool),
                Operator::Greater
                | Operator::GreaterEqual
                | Operator::Less
                | Operator::LessEqual => {
                    check_numeric(a)?;
                    check_numeric(b)?;
                    check_type(node, &DataType::Bool)
                }
                Operator::Assign => Ok(()),
                _ => panic!("Invalid binary operator"),
            }
        }
        AstNodeType::Program(statements) | AstNodeType::Block(statements, _) => {
            for statement in statements.iter() {
                check_operator_constraints(statement)?;
            }
            Ok(())
        }
        AstNodeType::PrintStatement(e) | AstNodeType::ExpressionStatement(e) => {
            check_node_has_type(e)?;
            check_operator_constraints(e)
        }
        AstNodeType::ReturnStatement(e) => {
            // TODO check against declared return type
            if let Some(e) = e {
                check_node_has_type(e)?;
                check_operator_constraints(e)
            } else {
                Ok(())
            }
        }
        AstNodeType::DeclareStatement(lhs, rhs) => {
            check_operator_constraints(lhs)?;
            check_operator_constraints(rhs)
        }
        AstNodeType::IfStatement(condition, then_block, else_block) => {
            check_operator_constraints(condition)?;
            check_operator_constraints(then_block)?;
            check_operator_constraints(else_block)?;
            check_type(condition, &DataType::Bool)
        }
        AstNodeType::WhileStatement(condition, loop_block) => {
            check_operator_constraints(condition)?;
            check_operator_constraints(loop_block)?;
            check_type(condition, &DataType::Bool)
        }
        AstNodeType::Variable {
            name: _,
            type_annotation: _,
        } => {
            // TODO?
            Ok(())
        }
        AstNodeType::FunctionDef {
            name: _,
            return_type: _,
            params,
            body,
        } => {
            // TODO stuff with scope and return type?
            for param in params.iter() {
                check_operator_constraints(param)?;
            }
            check_operator_constraints(body)
        }
        AstNodeType::Call { target, args } => {
            // TODO stuff with scope and return type?
            check_operator_constraints(target)?;
            for arg in args.iter() {
                check_operator_constraints(arg)?;
            }
            Ok(())
        }
        AstNodeType::Error => panic!("Unreachable error node in type check"),
    }
}

fn generate_constraints<'a>(
    node: &AstNode,
    scope: &'a mut Scope,
) -> Result<Vec<TypeConstraint>, TypeError> {
    match &node.node_type {
        AstNodeType::IntLiteral(_) => Ok(vec![TypeConstraint::new(
            TCSide::Expr(node.id),
            TCSide::basic(TCNodeType::Int),
        )]),
        AstNodeType::FloatLiteral(_) => Ok(vec![TypeConstraint::new(
            TCSide::Expr(node.id),
            TCSide::basic(TCNodeType::Float),
        )]),
        AstNodeType::BoolLiteral(_) => Ok(vec![TypeConstraint::new(
            TCSide::Expr(node.id),
            TCSide::basic(TCNodeType::Bool),
        )]),
        AstNodeType::StrLiteral(_) => Ok(vec![TypeConstraint::new(
            TCSide::Expr(node.id),
            TCSide::basic(TCNodeType::Str),
        )]),
        AstNodeType::Unary(operator, operand) => {
            let mut constraints = match operator {
                Operator::Neg => vec![TypeConstraint::new(
                    TCSide::Expr(node.id),
                    TCSide::Expr(operand.id),
                )],
                Operator::Not => vec![
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(operand.id), TCSide::basic(TCNodeType::Bool)),
                ],
                _ => panic!("Invalid unary operator"),
            };
            constraints.append(&mut generate_constraints(operand, scope)?);
            Ok(constraints)
        }
        AstNodeType::Binary(operator, a, b) => {
            let mut constraints = match operator {
                Operator::And | Operator::Or => vec![
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(b.id), TCSide::basic(TCNodeType::Bool)),
                ],
                Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => vec![
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::Expr(b.id)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(a.id)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(b.id)),
                ],
                Operator::Equal
                | Operator::NotEqual
                | Operator::Greater
                | Operator::Less
                | Operator::GreaterEqual
                | Operator::LessEqual => vec![
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::Expr(b.id)),
                ],
                Operator::Assign => vec![
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::Expr(b.id)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(a.id)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(b.id)),
                ],
                _ => panic!("Invalid infix operator"),
            };

            constraints.append(&mut generate_constraints(a, scope)?);
            constraints.append(&mut generate_constraints(b, scope)?);
            Ok(constraints)
        }
        AstNodeType::Program(statements) => {
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::basic(TCNodeType::Nil),
            )];
            for statement in statements.iter() {
                constraints.append(&mut generate_constraints(statement, scope)?);
            }
            Ok(constraints)
        }
        AstNodeType::PrintStatement(e) => {
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::basic(TCNodeType::Nil),
            )];
            constraints.append(&mut generate_constraints(e, scope)?);
            Ok(constraints)
        }
        AstNodeType::ExpressionStatement(e) => {
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::basic(TCNodeType::Nil),
            )];
            constraints.append(&mut generate_constraints(e, scope)?);
            Ok(constraints)
        }
        AstNodeType::ReturnStatement(e) => {
            // TODO check against declared return type
            if let Some(e) = e {
                let mut constraints = vec![TypeConstraint::new(
                    TCSide::Expr(node.id),
                    TCSide::basic(TCNodeType::Nil),
                )];
                constraints.append(&mut generate_constraints(e, scope)?);
                Ok(constraints)
            } else {
                Ok(Vec::new())
            }
        }
        AstNodeType::DeclareStatement(lhs, rhs) => {
            let mut constraints = vec![
                TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Nil)),
                TypeConstraint::new(TCSide::Expr(lhs.id), TCSide::Expr(rhs.id)),
            ];
            match &lhs.node_type {
                AstNodeType::Variable {
                    name,
                    type_annotation,
                } => {
                    scope.insert(&name, TCSide::Expr(rhs.id));
                    if let Some(type_annotation) = type_annotation {
                        let var_type = TCNodeType::try_from(&type_annotation)?;
                        constraints.push(TypeConstraint::new(
                            TCSide::Expr(lhs.id),
                            TCSide::basic(var_type),
                        ));
                    }
                }
                _ => panic!("Invalid lhs for let"),
            };
            //constraints.append(&mut generate_constraints(lhs, scope)?);
            constraints.append(&mut generate_constraints(rhs, scope)?);
            Ok(constraints)
        }
        AstNodeType::Block(statements, _) => {
            let mut block_scope = Scope::new(Some(scope));
            let mut constraints = vec![
                // TODO make this node have type and value of last statement like rust
                TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Nil)),
            ];
            for statement in statements.iter() {
                constraints.append(&mut generate_constraints(statement, &mut block_scope)?);
            }
            Ok(constraints)
        }
        AstNodeType::IfStatement(condition, then_block, else_block) => {
            let mut constraints = vec![
                TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Nil)),
                TypeConstraint::new(TCSide::Expr(condition.id), TCSide::basic(TCNodeType::Bool)),
            ];
            constraints.append(&mut generate_constraints(condition, scope)?);
            constraints.append(&mut generate_constraints(then_block, scope)?);
            constraints.append(&mut generate_constraints(else_block, scope)?);
            Ok(constraints)
        }
        AstNodeType::WhileStatement(condition, loop_block) => {
            let mut constraints = vec![
                TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Nil)),
                TypeConstraint::new(TCSide::Expr(condition.id), TCSide::basic(TCNodeType::Bool)),
            ];
            constraints.append(&mut generate_constraints(condition, scope)?);
            constraints.append(&mut generate_constraints(loop_block, scope)?);
            Ok(constraints)
        }
        AstNodeType::Variable {
            name,
            type_annotation,
        } => {
            let mut constraints = if let Some(tc_side) = scope.get(name) {
                vec![TypeConstraint::new(TCSide::Expr(node.id), tc_side)]
            } else {
                // TODO for recursive stuff, it's not here yet. but how do i get this to happen
                // later?
                Vec::new()
            };

            if let Some(type_annotation) = type_annotation {
                let var_type = TCNodeType::try_from(&type_annotation)?;
                constraints.push(TypeConstraint::new(
                    TCSide::Expr(node.id),
                    TCSide::basic(var_type),
                ));
            }
            Ok(constraints)
        }
        AstNodeType::FunctionDef {
            name: _,
            return_type,
            params,
            body,
        } => {
            let mut func_scope = Scope::new(Some(scope));
            let func_scope = &mut func_scope;

            let mut constraints = Vec::new();
            for param in params.iter() {
                constraints.append(&mut generate_constraints(param, func_scope)?);
                match &param.node_type {
                    AstNodeType::Variable {
                        name,
                        type_annotation,
                    } => {
                        func_scope.insert(&name, TCSide::Expr(param.id));
                        if let Some(type_annotation) = type_annotation {
                            let param_type = TCNodeType::try_from(&type_annotation)?;
                            constraints.push(TypeConstraint::new(
                                TCSide::Expr(param.id),
                                TCSide::basic(param_type),
                            ));
                        }
                        // TODO else fail? i.e. require type annotations on parameters
                    }
                    _ => panic!("Invalid node type for func param"),
                }
            }

            // TODO support more complex (function) return types
            let return_type = TCNodeType::try_from(return_type)?;
            constraints.push(TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::Constraint {
                    base_type: TCNodeType::Function,
                    return_type: Some(Box::new(TCSide::basic(return_type))),
                    parameters: params.iter().map(|p| TCSide::Expr(p.id)).collect(),
                },
            ));

            constraints.append(&mut generate_constraints(body, func_scope)?);
            Ok(constraints)
        }
        AstNodeType::Call { target, args } => {
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(target.id),
                TCSide::Constraint {
                    base_type: TCNodeType::Function,
                    return_type: Some(Box::new(TCSide::Expr(node.id))),
                    parameters: args.iter().map(|arg| TCSide::Expr(arg.id)).collect(),
                },
            )];

            constraints.append(&mut generate_constraints(target, scope)?);
            for arg in args.iter() {
                constraints.append(&mut generate_constraints(arg, scope)?);
            }

            Ok(constraints)
        }
        AstNodeType::Error => panic!("Unreachable error node in type check"),
    }
}

fn unify<'a>(mut constraints: Vec<TypeConstraint>) -> Result<Vec<TypeConstraint>, TypeError> {
    let mut substitutions = Vec::new();
    unify_helper(&mut constraints, &mut substitutions)?;
    Ok(substitutions)
}

fn unify_helper(
    constraints: &mut Vec<TypeConstraint>,
    substitutions: &mut Vec<TypeConstraint>,
) -> Result<(), TypeError> {
    while let Some(c) = constraints.pop() {
        match &c.left {
            TCSide::Expr(_) => {
                substitute(&c.left, &c.right, constraints);
                substitute(&c.left, &c.right, substitutions);
                substitutions.push(c);
            }
            TCSide::Constraint {
                base_type: left_type,
                return_type: left_return,
                parameters: left_children,
            } => match &c.right {
                TCSide::Expr(_) => {
                    constraints.push(TypeConstraint::new(c.right, c.left));
                }
                TCSide::Constraint {
                    base_type: right_type,
                    return_type: right_return,
                    parameters: right_children,
                } => {
                    if left_type != right_type {
                        return Err(TypeError::Mismatched);
                    }
                    match (left_return, right_return) {
                        (Some(left_return), Some(right_return)) => {
                            constraints.push(TypeConstraint::new(
                                *left_return.clone(),
                                *right_return.clone(),
                            ));
                        }
                        (None, None) => (),
                        _ => return Err(TypeError::Mismatched),
                    }

                    if left_children.len() != right_children.len() {
                        return Err(TypeError::Mismatched);
                    }
                    for child_constr in left_children
                        .into_iter()
                        .zip(right_children.into_iter())
                        .map(|(left, right)| TypeConstraint::new(left.clone(), right.clone()))
                    {
                        constraints.push(child_constr);
                    }
                }
            },
        }
    }
    Ok(())
}

fn substitute(from: &TCSide, to: &TCSide, list: &mut Vec<TypeConstraint>) {
    for c in list.iter_mut() {
        if c.left == *from {
            c.left = to.clone();
        } else if c.right == *from {
            c.right = to.clone();
        }
    }
}
