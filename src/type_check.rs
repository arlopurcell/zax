use std::collections::HashMap;
use std::error;
use std::fmt;

use crate::ast::{AstNode, AstNodeType, Operator};
use crate::common::InterpretError;

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
pub enum TCSide {
    Expr(u64),
    Constraint(Vec<TCNodeType>, Vec<TCSide>),
}

impl TCSide {
    fn basic(node_type: TCNodeType) -> Self {
        TCSide::Constraint(vec![node_type], Vec::new())
    }

    fn add() -> Self {
        TCSide::Constraint(
            vec![TCNodeType::Int, TCNodeType::Float, TCNodeType::Str],
            Vec::new(),
        )
    }

    fn numeric() -> Self {
        TCSide::Constraint(vec![TCNodeType::Int, TCNodeType::Float], Vec::new())
    }

    fn any() -> Self {
        TCSide::Constraint(
            vec![
                TCNodeType::Str,
                TCNodeType::Int,
                TCNodeType::Float,
                TCNodeType::Bool,
            ],
            Vec::new(),
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TCNodeType {
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Str,
    Function,
    Nil,
    // TODO Array(Box<NodeType>),
}

impl DataType {
    pub fn size(&self) -> u8 {
        match self {
            DataType::Int | DataType::Float | DataType::Str | DataType::Function => 8,
            DataType::Bool => 1,
            DataType::Nil => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    Mismatched, // TODO add type params
    Unknown,
    UnknownFunction(String),
    ArgNumber(usize, usize),
    InvalidTypeAnnonation(String),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::Mismatched => write!(f, "Mismatched types.",),
            TypeError::Unknown => write!(f, "Unknown error occured"),
            TypeError::UnknownFunction(func_name) => {
                write!(f, "Unknown function name: {}", func_name)
            }
            TypeError::ArgNumber(expected, found) => write!(
                f,
                "Called function with wrong number of arguments. Expected {}, Found {}",
                expected, found
            ),
            TypeError::InvalidTypeAnnonation(name) => write!(f, "Invalid type annotation: {}", name,),
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
        Self {
            parent,
            variables: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Option<TCSide> {
        self.variables
            .get(name)
            .map(|s| s.clone())
            .or_else(|| self.parent.and_then(|p| p.get(name)))
    }

    pub fn insert(&mut self, name: &str, value: TCSide) -> () {
        self.variables.insert(name.to_string(), value);
    }
}

pub fn find_type(node: &AstNode, substitutions: &Vec<TypeConstraint>) -> Option<TCNodeType> {
    find_type_helper(&TCSide::Expr(node.id), substitutions, &mut Vec::new())
}

fn find_type_helper<'a>(
    target: &'a TCSide,
    substitutions: &'a Vec<TypeConstraint>,
    visited: &mut Vec<&'a TCSide>,
) -> Option<TCNodeType> {
    match target {
        TCSide::Expr(id) => substitutions.iter().find_map(|sub| {
            let mut maybe_result = None;
            if !visited.contains(&target) {
                visited.push(target);
                if let TCSide::Expr(left_id) = sub.left {
                    if left_id == *id {
                        maybe_result = find_type_helper(&sub.right, substitutions, visited)
                    }
                } else if let TCSide::Expr(right_id) = sub.right {
                    if right_id == *id {
                        maybe_result = find_type_helper(&sub.left, substitutions, visited)
                    }
                }
                visited.retain(|item| *item != target);
            }
            maybe_result
        }),
        TCSide::Constraint(types, _children) => Some(types.get(0).unwrap().clone()),
    }
}

pub fn generate_substitutions(
    node: &AstNode,
) -> Result<Vec<TypeConstraint>, TypeError> {
    let mut scope = Scope::new(None);
    let constraints = generate_constraints(node, &mut scope)?;
    unify(constraints)
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
        /* TODO
        AstNodeType::ArrayLiteral(values) => {
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::Constraint(
                    vec![TCNodeType::Array],
                    values.iter().take(1).map(|v| TCSide::Expr(v.id)).collect(),
                ),
            )];
            let mut prev = None;
            for v in values.iter() {
                if let Some(prev) = prev {
                    constraints.push(TypeConstraint::new(TCSide::Expr(prev.id), TCSide::Expr(v)))
                }
                constraints.append(&mut generate_constraints(v, function_map)?);
                prev = Some(v);
            }
            Ok(constraints)
        }
        SyntaxTreeNode::FieldRef(_) => Ok(Vec::new()),
        */
        AstNodeType::Unary(operator, operand) => {
            let mut constraints = match operator {
                Operator::Neg => vec![
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(operand.id), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(operand.id)),
                ],
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
                Operator::Add => vec![
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::add()),
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::add()),
                    TypeConstraint::new(TCSide::Expr(b.id), TCSide::add()),
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::Expr(b.id)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(a.id)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(b.id)),
                ],
                Operator::Sub | Operator::Mul | Operator::Div => vec![
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(b.id), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::Expr(b.id)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(a.id)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(b.id)),
                ],
                Operator::Equal | Operator::NotEqual => vec![
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::Expr(b.id)),
                ],
                Operator::Greater
                | Operator::Less
                | Operator::GreaterEqual
                | Operator::LessEqual => vec![
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(b.id), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::Expr(b.id)),
                ],
                Operator::Assign => vec![
                    TypeConstraint::new(TCSide::Expr(a.id), TCSide::Expr(b.id)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(a.id)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(b.id)),
                ],
                /* TODO
                Operator::Lookup => vec![
                    TypeConstraint::new(
                        TCSide::Expr(a.id),
                        TCSide::Constraint(vec![TCNodeType::Array], vec![TCSide::Expr(node.id)]),
                    ),
                    TypeConstraint::new(TCSide::Expr(b.id), TCSide::basic(TCNodeType::Int)),
                    TypeConstraint::new(TCSide::Expr(node.id), TCSide::any()),
                ],
                */
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
        AstNodeType::DeclareStatement(lhs, rhs) => {
            let mut constraints = vec![
                TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Nil)),
                TypeConstraint::new(TCSide::Expr(lhs.id), TCSide::Expr(rhs.id)),
            ];
            match &lhs.node_type {
                AstNodeType::Variable(name, type_annotation) => {
                    scope.insert(&name, TCSide::Expr(rhs.id));
                    if let Some(type_annotation) = type_annotation {
                        let var_type = TCNodeType::try_from(&type_annotation)?;
                        constraints.push(TypeConstraint::new(TCSide::Expr(lhs.id), TCSide::basic(var_type)));
                    }
                }
                _ => panic!("Invalid lhs for let"),
            };
            //constraints.append(&mut generate_constraints(lhs, scope)?);
            constraints.append(&mut generate_constraints(rhs, scope)?);
            Ok(constraints)
        }
        AstNodeType::Block(statements) => {
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
        AstNodeType::Variable(name, type_annotation) => {
            let mut constraints = if let Some(tc_side) = scope.get(name) {
                vec![TypeConstraint::new(TCSide::Expr(node.id), tc_side)]
            } else {
                Vec::new()
            };

            if let Some(type_annotation) = type_annotation {
                let var_type = TCNodeType::try_from(&type_annotation)?;
                constraints.push(TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(var_type)));
            }
            Ok(constraints)
        }
        AstNodeType::FunctionDef {
            return_type,
            params,
            body,
        } => {
            let mut scope = Scope::new(None);
            let scope = &mut scope;

            let mut constraints = Vec::new();
            for param in params.iter() {
                constraints.append(&mut generate_constraints(param, scope)?);
                match &param.node_type {
                    AstNodeType::Variable(name, type_annotation) => {
                        scope.insert(&name, TCSide::Expr(param.id));
                        if let Some(type_annotation) = type_annotation {
                            let param_type = TCNodeType::try_from(&type_annotation)?;
                            constraints.push(TypeConstraint::new(TCSide::Expr(param.id), TCSide::basic(param_type)));
                        }
                        // TODO else fail? i.e. require type annotations on parameters
                    }
                    _ => panic!("Invalid node type for func param"),
                }
            }

            let return_type = TCNodeType::try_from(return_type)?;
            constraints.push(
                TypeConstraint::new(
                    TCSide::Expr(node.id),
                    TCSide::Constraint(vec![TCNodeType::Function], params.iter().map(|p| TCSide::Expr(p.id)).collect())
                ));

            constraints.append(&mut generate_constraints(body, scope)?);
            Ok(constraints)
        }
        AstNodeType::Call { target, args } => {
            let mut constraints = Vec::new();
            // TODO get signature from scope and type check return type and arg types
            constraints.push(TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::basic(TCNodeType::Nil),
            ));
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
            TCSide::Constraint(left_types, left_children) => match &c.right {
                TCSide::Expr(_) => {
                    constraints.push(TypeConstraint::new(c.right, c.left));
                }
                TCSide::Constraint(right_types, right_children) => {
                    let mut new_types = Vec::new();
                    for t in left_types.iter() {
                        if right_types.contains(&t) {
                            new_types.push(t.clone());
                        }
                    }
                    if new_types.is_empty() {
                        return Err(TypeError::Mismatched);
                    }
                    let new_constraint =
                        TCSide::Constraint(new_types.clone(), left_children.clone());
                    substitute(&c.left, &new_constraint, constraints);
                    substitute(
                        &TCSide::Constraint(left_types.clone(), left_children.clone()),
                        &TCSide::Constraint(new_types.clone(), left_children.clone()),
                        substitutions,
                    );
                    substitute(
                        &TCSide::Constraint(right_types.clone(), right_children.clone()),
                        &TCSide::Constraint(new_types.clone(), right_children.clone()),
                        constraints,
                    );
                    substitute(
                        &TCSide::Constraint(right_types.clone(), right_children.clone()),
                        &TCSide::Constraint(new_types.clone(), right_children.clone()),
                        substitutions,
                    );

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
