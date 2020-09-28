use std::error;
use std::fmt;

use crate::ast::{AstNode, AstNodeType, Operator};

#[derive(Debug)]
pub struct TypeConstraint<'a> {
    left: TCSide<'a>,
    right: TCSide<'a>,
}

impl<'a> TypeConstraint<'a> {
    fn new(left: TCSide<'a>, right: TCSide<'a>) -> Self {
        TypeConstraint { left, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TCSide<'a> {
    Expr(&'a AstNode<'a>),
    Constraint(Vec<TCNodeType>, Vec<TCSide<'a>>),
}

impl<'a> TCSide<'a> {
    fn basic(node_type: TCNodeType) -> Self {
        TCSide::Constraint(vec![node_type], Vec::new())
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

impl<'a> From<&TypeParameter> for TCSide<'a> {
    fn from(type_param: &TypeParameter) -> Self {
        match type_param {
            TypeParameter::Any => TCSide::any(),
            TypeParameter::Number => TCSide::numeric(),
            TypeParameter::Int => TCSide::basic(TCNodeType::Int),
            TypeParameter::Float => TCSide::basic(TCNodeType::Float),
            TypeParameter::Bool => TCSide::basic(TCNodeType::Bool),
            TypeParameter::Str => TCSide::basic(TCNodeType::Str),
            /* TODO
            TypeParameter::Array(inner_type) => TCSide::Constraint(
                vec![TCNodeType::Array],
                vec![TCSide::from(inner_type.as_ref())],
            ),
            */
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TCNodeType {
    Int,
    Float,
    Bool,
    Str,
    // TOOD Array,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
    Int,
    Float,
    Bool,
    Str,
    // TODO Array(Box<NodeType>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    Mismatched, // TODO add type params
    Unknown,
    UnknownFunction(String),
    ArgNumber(usize, usize),
    InvalidFieldRef(String),
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
            TypeError::InvalidFieldRef(name) => write!(f, "Invalid field name: {}", name,),
        }
    }
}

impl error::Error for TypeError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionSignature {
    pub name: String,
    pub return_type: TypeParameter,
    pub argument_types: Vec<TypeParameter>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeParameter {
    Any,
    Number,
    Int,
    Float,
    Str,
    Bool,
    // TODO Array(Box<TypeParameter>),
}

/*
pub fn generate_symbol_table(substitutions: &Vec<TypeConstraint>) -> HashMap<String, NodeType> {
    let mut symbols = HashMap::new();
    for sub in substitutions.iter() {
        let TypeConstraint { left, right } = sub;
        if let Some((k, v)) = get_field_ref_type(left, substitutions) {
            symbols.insert(k, v);
        }
        if let Some((k, v)) = get_field_ref_type(right, substitutions) {
            symbols.insert(k, v);
        }
    }

    symbols
}

fn get_field_ref_type(
    side: &TCSide,
    substitutions: &Vec<TypeConstraint>,
) -> Option<(String, NodeType)> {
    if let TCSide::Expr(node) = side {
        if let SyntaxTreeNode::FieldRef(name) = node {
            Some((name.to_string(), find_type(node, substitutions)))
        } else {
            None
        }
    } else {
        None
    }
}
*/

pub fn find_type<'a>(
    node: &'a AstNode<'a>,
    substitutions: &'a Vec<TypeConstraint<'a>>,
) -> Option<DataType> {
    find_type_helper(&TCSide::Expr(node), substitutions, &mut Vec::new())
}

fn find_type_helper<'a>(
    target: &'a TCSide<'a>,
    substitutions: &'a Vec<TypeConstraint<'a>>,
    visited: &mut Vec<&'a TCSide<'a>>,
) -> Option<DataType> {
    match target {
        TCSide::Expr(node) => substitutions.iter().find_map(|sub| {
            let mut maybe_result = None;
            if !visited.contains(&target) {
                visited.push(target);
                if let TCSide::Expr(left_expr) = sub.left {
                    if &left_expr == node {
                        maybe_result = find_type_helper(&sub.right, substitutions, visited)
                    }
                } else if let TCSide::Expr(right_expr) = sub.right {
                    if &right_expr == node {
                        maybe_result = find_type_helper(&sub.left, substitutions, visited)
                    }
                }
                visited.retain(|item| *item != target);
            }
            maybe_result
        }),
        TCSide::Constraint(types, _children) => {
            let t = types.get(0).unwrap();
            match t {
                TCNodeType::Int => Some(DataType::Int),
                TCNodeType::Float => Some(DataType::Float),
                TCNodeType::Bool => Some(DataType::Bool),
                TCNodeType::Str => Some(DataType::Str),
                /* TODO
                TCNodeType::Array => {
                    let child = children.get(0).unwrap();
                    let inner_type = find_type_helper(&child, substitutions, &mut Vec::new());
                    inner_type.map(|inner_type| NodeType::Array(Box::new(inner_type)))
                }
                */
            }
        }
    }
}

pub fn generate_substitutions<'a, 'b>(
    node: &'a AstNode<'a>,
) -> Result<Vec<TypeConstraint<'a>>, TypeError> {
    let constraints = generate_constraints(node)?;
    unify(constraints)
}

fn generate_constraints<'a, 'b>(node: &'a AstNode) -> Result<Vec<TypeConstraint<'a>>, TypeError> {
    match &node.node_type {
        AstNodeType::IntLiteral(_) => Ok(vec![TypeConstraint::new(
            TCSide::Expr(node),
            TCSide::basic(TCNodeType::Int),
        )]),
        AstNodeType::FloatLiteral(_) => Ok(vec![TypeConstraint::new(
            TCSide::Expr(node),
            TCSide::basic(TCNodeType::Float),
        )]),
        AstNodeType::BoolLiteral(_) => Ok(vec![TypeConstraint::new(
            TCSide::Expr(node),
            TCSide::basic(TCNodeType::Bool),
        )]),
        AstNodeType::StrLiteral(_) => Ok(vec![TypeConstraint::new(
            TCSide::Expr(node),
            TCSide::basic(TCNodeType::Str),
        )]),
        /* TODO
        AstNodeType::ArrayLiteral(values) => {
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(node),
                TCSide::Constraint(
                    vec![TCNodeType::Array],
                    values.iter().take(1).map(|v| TCSide::Expr(v)).collect(),
                ),
            )];
            let mut prev = None;
            for v in values.iter() {
                if let Some(prev) = prev {
                    constraints.push(TypeConstraint::new(TCSide::Expr(prev), TCSide::Expr(v)))
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
                    TypeConstraint::new(TCSide::Expr(node), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(operand), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(node), TCSide::Expr(operand)),
                ],
                Operator::Not => vec![
                    TypeConstraint::new(TCSide::Expr(node), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(operand), TCSide::basic(TCNodeType::Bool)),
                ],
                _ => panic!("Invalid unary operator"),
            };
            constraints.append(&mut generate_constraints(operand)?);
            Ok(constraints)
        }
        AstNodeType::Binary(operator, a, b) => {
            let mut constraints = match operator {
                Operator::And | Operator::Or => vec![
                    TypeConstraint::new(TCSide::Expr(node), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(a), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(b), TCSide::basic(TCNodeType::Bool)),
                ],
                Operator::Add | Operator::Sub | Operator::Mul | Operator::Div => vec![
                    TypeConstraint::new(TCSide::Expr(node), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(a), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(b), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(a), TCSide::Expr(b)),
                    TypeConstraint::new(TCSide::Expr(node), TCSide::Expr(a)),
                    TypeConstraint::new(TCSide::Expr(node), TCSide::Expr(b)),
                ],
                Operator::Equal | Operator::NotEqual => vec![
                    TypeConstraint::new(TCSide::Expr(node), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(a), TCSide::Expr(b)),
                ],
                Operator::Greater
                | Operator::Less
                | Operator::GreaterEqual
                | Operator::LessEqual => vec![
                    TypeConstraint::new(TCSide::Expr(node), TCSide::basic(TCNodeType::Bool)),
                    TypeConstraint::new(TCSide::Expr(a), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(b), TCSide::numeric()),
                    TypeConstraint::new(TCSide::Expr(a), TCSide::Expr(b)),
                ],
                /* TODO
                Operator::Lookup => vec![
                    TypeConstraint::new(
                        TCSide::Expr(a),
                        TCSide::Constraint(vec![TCNodeType::Array], vec![TCSide::Expr(node)]),
                    ),
                    TypeConstraint::new(TCSide::Expr(b), TCSide::basic(TCNodeType::Int)),
                    TypeConstraint::new(TCSide::Expr(node), TCSide::any()),
                ],
                */
                _ => panic!("Invalid infix operator"),
            };

            constraints.append(&mut generate_constraints(a)?);
            constraints.append(&mut generate_constraints(b)?);
            Ok(constraints)
        }
        /* TODO
        SyntaxTreeNode::FunctionCall(name, arguments) => {
            let name: &str = name.as_ref();
            let (return_side, arg_sides) = function_map
                .get(name)
                .ok_or(TypeError::UnknownFunction(name.to_string()))?;
            if arguments.len() != arg_sides.len() {
                Err(TypeError::ArgNumber(arg_sides.len(), arguments.len()))
            } else {
                let mut constraints =
                    vec![TypeConstraint::new(TCSide::Expr(node), return_side.clone())];
                for (arg, arg_side) in arguments.iter().zip(arg_sides.iter()) {
                    constraints.push(TypeConstraint::new(TCSide::Expr(arg), arg_side.clone()));
                    constraints.append(&mut generate_constraints(arg, function_map)?);
                }
                Ok(constraints)
            }
        }
        */
        AstNodeType::Error => panic!("Unreachable error node in type check"),
    }
}

fn unify<'a>(
    mut constraints: Vec<TypeConstraint<'a>>,
) -> Result<Vec<TypeConstraint<'a>>, TypeError> {
    let mut substitutions = Vec::new();
    unify_helper(&mut constraints, &mut substitutions)?;
    Ok(substitutions)
}

fn unify_helper<'a>(
    constraints: &mut Vec<TypeConstraint<'a>>,
    substitutions: &mut Vec<TypeConstraint<'a>>,
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

fn substitute<'a>(from: &TCSide<'a>, to: &TCSide<'a>, list: &mut Vec<TypeConstraint<'a>>) {
    for c in list.iter_mut() {
        if c.left == *from {
            c.left = to.clone();
        } else if c.right == *from {
            c.right = to.clone();
        }
    }
}
