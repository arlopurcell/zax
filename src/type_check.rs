use std::collections::{HashMap, VecDeque};

use crate::ast::{AstNode, AstNodeType, NodeId, Operator};
use crate::common::{InterpretError, InterpretResult};
use crate::object::NativeFunctionObj;

#[derive(Debug, PartialEq)]
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

    fn from(data_type: &DataType) -> Self {
        match data_type {
            DataType::Int => TCSide::basic(TCNodeType::Int),
            DataType::Float => TCSide::basic(TCNodeType::Float),
            DataType::Bool => TCSide::basic(TCNodeType::Bool),
            DataType::Str => TCSide::basic(TCNodeType::Str),
            DataType::Nil => TCSide::basic(TCNodeType::Nil),
            DataType::Function {
                return_type,
                parameters,
            } => TCSide::Constraint {
                base_type: TCNodeType::Function,
                return_type: Some(Box::new(TCSide::from(return_type))),
                parameters: parameters.iter().map(|p| TCSide::from(p)).collect(),
            },
        }
    }

    fn constrained_type(&self, type_map: &mut HashMap<NodeId, DataType>) -> Option<DataType> {
        match self {
            Self::Expr(id) => type_map.get(id).cloned(),
            Self::Constraint {
                base_type,
                return_type,
                parameters,
            } => match base_type {
                TCNodeType::Int => Some(DataType::Int),
                TCNodeType::Float => Some(DataType::Float),
                TCNodeType::Bool => Some(DataType::Bool),
                TCNodeType::Str => Some(DataType::Str),
                TCNodeType::Nil => Some(DataType::Nil),
                TCNodeType::Function => {
                    let cons_rt = return_type
                        .as_ref()
                        .and_then(|rt| rt.constrained_type(type_map));
                    let cons_params: Option<Vec<_>> = parameters
                        .iter()
                        .map(|p| p.constrained_type(type_map))
                        .collect();
                    if let (Some(rt), Some(params)) = (cons_rt, cons_params) {
                        Some(DataType::Function {
                            return_type: Box::new(rt),
                            parameters: params,
                        })
                    } else {
                        None
                    }
                }
            },
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum TCNodeType {
    Int,
    Float,
    Bool,
    Str,
    Function,
    Nil,
    // TOOD Array,
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

fn error(message: &str) -> InterpretError {
    eprintln!("Error: {}", message);
    InterpretError::Compile
}

fn error_line(line: u32, message: &str) -> InterpretError {
    eprintln!("[line {}] Error: {}", line, message);
    InterpretError::Compile
}

pub fn create_type_map(
    substitutions: Vec<TypeConstraint>,
) -> Result<HashMap<NodeId, DataType>, InterpretError> {
    let mut type_map = HashMap::new();
    let mut sub_deque = VecDeque::new();
    sub_deque.extend(substitutions);

    populate_type_map(sub_deque, &mut type_map).map(|_| type_map)
}

fn populate_type_map(
    mut substitutions: VecDeque<TypeConstraint>,
    type_map: &mut HashMap<NodeId, DataType>,
) -> InterpretResult {
    let max_iterations = substitutions.len() * 50;
    let mut iterations = 0;
    while let Some(sub) = substitutions.pop_front() {
        substitutions.extend(insert_if_constrained(sub.left, sub.right, type_map));

        let mut resoved_node_ids: Vec<_> = type_map.keys().collect();
        resoved_node_ids.sort();

        iterations += 1;
        if iterations > max_iterations {
            return Err(error("Failed to substitute types."));
        }
    }
    Ok(())
}

fn insert_if_constrained(
    left: TCSide,
    right: TCSide,
    type_map: &mut HashMap<NodeId, DataType>,
) -> Vec<TypeConstraint> {
    // make sure that if one is an Expr, it's the left
    let (left, right) = if let TCSide::Expr(_) = left {
        (left, right)
    } else {
        (right, left)
    };
    match (left, right) {
        (TCSide::Expr(node_id), TCSide::Expr(other_id)) => {
            if type_map.contains_key(&node_id) {
                type_map.insert(other_id, type_map.get(&node_id).unwrap().clone());
                Vec::new()
            } else if type_map.contains_key(&other_id) {
                type_map.insert(node_id, type_map.get(&other_id).unwrap().clone());
                Vec::new()
            } else {
                vec![TypeConstraint::new(
                    TCSide::Expr(node_id),
                    TCSide::Expr(other_id),
                )]
            }
        }
        (TCSide::Expr(node_id), other_side) => {
            let constrained_type = other_side.constrained_type(type_map);

            if let Some(constrained_type) = constrained_type {
                type_map.insert(node_id, constrained_type);
                Vec::new()
            } else if type_map.contains_key(&node_id) {
                // Something unconstrained in right, but we can fill in with fully constrained left
                let node_dt = type_map.get(&node_id).unwrap().clone();
                if let TCSide::Constraint {
                    base_type: _,
                    return_type: other_rt,
                    parameters: other_params,
                } = other_side
                {
                    if let DataType::Function {
                        return_type: node_rt,
                        parameters: node_params,
                    } = node_dt
                    {
                        if let Some(TCSide::Expr(rt_id)) = other_rt.as_deref() {
                            type_map.insert(*rt_id, *node_rt);
                        }

                        for (other_p, node_p) in other_params.iter().zip(node_params.iter()) {
                            if let TCSide::Expr(p_id) = other_p {
                                type_map.insert(*p_id, node_p.clone());
                            }
                        }
                    }

                    Vec::new()
                } else {
                    vec![TypeConstraint::new(TCSide::Expr(node_id), other_side)]
                }
            } else {
                vec![TypeConstraint::new(TCSide::Expr(node_id), other_side)]
            }
        }
        (
            TCSide::Constraint {
                base_type: left_bt,
                return_type: left_rt,
                parameters: left_params,
            },
            TCSide::Constraint {
                base_type: right_bt,
                return_type: right_rt,
                parameters: right_params,
            },
        ) => {
            if left_bt == right_bt {
                let mut new_subs = Vec::new();
                if let (Some(left_rt), Some(right_rt)) = (left_rt, right_rt) {
                    new_subs.append(&mut insert_if_constrained(*left_rt, *right_rt, type_map));
                }
                for (left_p, right_p) in left_params.into_iter().zip(right_params.into_iter()) {
                    new_subs.append(&mut insert_if_constrained(left_p, right_p, type_map));
                }
                new_subs
            } else {
                unreachable!("Substitutions should not have mismatched base types")
            }
        }
        _ => unreachable!("either one is an expression or both are constraints"),
    }
}

pub fn generate_substitutions(node: &AstNode) -> Result<Vec<TypeConstraint>, InterpretError> {
    let mut scope = Scope::new(None);
    let constraints = generate_constraints(node, &mut scope, None)?;
    unify(constraints)
}

fn check_type(node: &AstNode, dt: &DataType) -> Result<(), InterpretError> {
    check_types(node, &[&dt])
}

fn check_numeric(node: &AstNode) -> Result<(), InterpretError> {
    check_types(node, &[&DataType::Int, &DataType::Float])
}

fn check_plus_types(node: &AstNode) -> Result<(), InterpretError> {
    check_types(node, &[&DataType::Int, &DataType::Float, &DataType::Str])
}

fn check_types(node: &AstNode, types: &[&DataType]) -> Result<(), InterpretError> {
    if let Some(dt) = &node.data_type {
        if types.contains(&dt) {
            Ok(())
        } else {
            Err(error("Mismatched types."))
        }
    } else {
        Err(error("Mismatched types."))
    }
}

fn check_node_has_type(node: &AstNode) -> Result<(), InterpretError> {
    if node.data_type.is_some() {
        Ok(())
    } else {
        Err(error("Mismatched types."))
    }
}

pub fn check_operator_constraints(node: &AstNode) -> Result<(), InterpretError> {
    match &node.node_type {
        AstNodeType::IntLiteral(_)
        | AstNodeType::FloatLiteral(_)
        | AstNodeType::BoolLiteral(_)
        | AstNodeType::StrLiteral(_)
        | AstNodeType::TypeAnnotation => Ok(()),
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
        AstNodeType::Program(statements) => {
            for statement in statements.iter() {
                check_operator_constraints(statement)?;
            }
            Ok(())
        }
        AstNodeType::Block {
            statements,
            expression,
            scope_words: _,
        } => {
            for statement in statements.iter() {
                check_operator_constraints(statement)?;
            }
            check_operator_constraints(expression)
        }
        AstNodeType::PrintStatement(e) | AstNodeType::ExpressionStatement(e) => {
            check_node_has_type(e)?;
            check_operator_constraints(e)
        }
        AstNodeType::ReturnStatement(e) => {
            // TODO check against declared return type
            check_node_has_type(e)?;
            check_operator_constraints(e)
        }
        AstNodeType::DeclareStatement(lhs, rhs) => {
            check_operator_constraints(lhs)?;
            check_operator_constraints(rhs)
        }
        AstNodeType::IfExpression(condition, then_block, else_block) => {
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
    enclosing_return_type_id: Option<NodeId>,
) -> Result<Vec<TypeConstraint>, InterpretError> {
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
            constraints.append(&mut generate_constraints(
                operand,
                scope,
                enclosing_return_type_id,
            )?);
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

            constraints.append(&mut generate_constraints(
                a,
                scope,
                enclosing_return_type_id,
            )?);
            constraints.append(&mut generate_constraints(
                b,
                scope,
                enclosing_return_type_id,
            )?);
            Ok(constraints)
        }
        AstNodeType::Program(statements) => {
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::basic(TCNodeType::Nil),
            )];
            for statement in statements.iter() {
                constraints.append(&mut generate_constraints(
                    statement,
                    scope,
                    enclosing_return_type_id,
                )?);
            }
            Ok(constraints)
        }
        AstNodeType::PrintStatement(e) => {
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::basic(TCNodeType::Nil),
            )];
            constraints.append(&mut generate_constraints(
                e,
                scope,
                enclosing_return_type_id,
            )?);
            Ok(constraints)
        }
        AstNodeType::ExpressionStatement(e) => {
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::basic(TCNodeType::Nil),
            )];
            constraints.append(&mut generate_constraints(
                e,
                scope,
                enclosing_return_type_id,
            )?);
            Ok(constraints)
        }
        AstNodeType::ReturnStatement(e) => {
            // TODO check against declared return type
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::basic(TCNodeType::Nil),
            )];
            if let Some(enclosing_return_type_id) = enclosing_return_type_id {
                // Ensure return statements match enclosing function return type
                constraints.push(TypeConstraint::new(
                    TCSide::Expr(enclosing_return_type_id),
                    TCSide::Expr(e.id),
                ));

                constraints.append(&mut generate_constraints(
                    e,
                    scope,
                    Some(enclosing_return_type_id),
                )?);
                Ok(constraints)
            } else {
                Err(error_line(node.line, "Can't return from top-level code."))
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
                    constraints.push(TypeConstraint::new(
                        TCSide::Expr(lhs.id),
                        TCSide::Expr(type_annotation.id),
                    ));
                }
                _ => panic!("Invalid lhs for let"),
            };
            constraints.append(&mut generate_constraints(
                rhs,
                scope,
                enclosing_return_type_id,
            )?);
            Ok(constraints)
        }
        AstNodeType::Block {
            statements,
            expression,
            scope_words: _,
        } => {
            let mut block_scope = Scope::new(Some(scope));
            let mut constraints = vec![TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::Expr(expression.id),
            )];
            for statement in statements.iter() {
                constraints.append(&mut generate_constraints(
                    statement,
                    &mut block_scope,
                    enclosing_return_type_id,
                )?);
            }
            constraints.append(&mut generate_constraints(
                expression,
                &mut block_scope,
                enclosing_return_type_id,
            )?);
            Ok(constraints)
        }
        AstNodeType::IfExpression(condition, then_block, else_block) => {
            let mut constraints = vec![
                TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(then_block.id)),
                TypeConstraint::new(TCSide::Expr(node.id), TCSide::Expr(else_block.id)),
                TypeConstraint::new(TCSide::Expr(condition.id), TCSide::basic(TCNodeType::Bool)),
            ];
            constraints.append(&mut generate_constraints(
                condition,
                scope,
                enclosing_return_type_id,
            )?);
            constraints.append(&mut generate_constraints(
                then_block,
                scope,
                enclosing_return_type_id,
            )?);
            constraints.append(&mut generate_constraints(
                else_block,
                scope,
                enclosing_return_type_id,
            )?);
            Ok(constraints)
        }
        AstNodeType::WhileStatement(condition, loop_block) => {
            let mut constraints = vec![
                TypeConstraint::new(TCSide::Expr(node.id), TCSide::basic(TCNodeType::Nil)),
                TypeConstraint::new(TCSide::Expr(condition.id), TCSide::basic(TCNodeType::Bool)),
            ];
            constraints.append(&mut generate_constraints(
                condition,
                scope,
                enclosing_return_type_id,
            )?);
            constraints.append(&mut generate_constraints(
                loop_block,
                scope,
                enclosing_return_type_id,
            )?);
            Ok(constraints)
        }
        AstNodeType::Variable {
            name,
            type_annotation,
        } => {
            let mut constraints = if let Some(tc_side) = scope.get(name) {
                vec![TypeConstraint::new(TCSide::Expr(node.id), tc_side)]
            } else {
                Vec::new()
            };

            constraints.append(&mut generate_constraints(
                type_annotation,
                scope,
                enclosing_return_type_id,
            )?);
            constraints.push(TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::Expr(type_annotation.id),
            ));
            Ok(constraints)
        }
        AstNodeType::TypeAnnotation => Ok(if let Some(data_type) = node.data_type.as_ref() {
            vec![TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::from(data_type),
            )]
        } else {
            Vec::new()
        }),
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
                constraints.append(&mut generate_constraints(
                    param,
                    func_scope,
                    enclosing_return_type_id,
                )?);
                match &param.node_type {
                    AstNodeType::Variable {
                        name,
                        type_annotation: _,
                    } => {
                        func_scope.insert(&name, TCSide::Expr(param.id));
                        /*
                        constraints.push(TypeConstraint::new(
                            TCSide::Expr(param.id),
                            TCSide::Expr(type_annotation.id),
                        ));
                        */
                        // TODO require data type on parameter type annotation node?
                    }
                    _ => panic!("Invalid node type for func param"),
                }
            }

            //constraints.push(TypeConstraint::new(TCSide::Expr(return_type.id), TCSide::from(return_type.data_type.as_ref().unwrap())));
            constraints.append(&mut generate_constraints(
                return_type,
                func_scope,
                enclosing_return_type_id,
            )?);

            constraints.push(TypeConstraint::new(
                TCSide::Expr(node.id),
                TCSide::Constraint {
                    base_type: TCNodeType::Function,
                    //return_type: Some(Box::new(TCSide::from(return_type.data_type.as_ref().unwrap()))),
                    return_type: Some(Box::new(TCSide::Expr(return_type.id))),
                    parameters: params.iter().map(|p| TCSide::Expr(p.id)).collect(),
                    //parameters: params.iter().map(|p| TCSide::from(p.data_type.as_ref().unwrap())).collect(),
                },
            ));

            constraints.append(&mut generate_constraints(
                body,
                func_scope,
                Some(return_type.id),
            )?);

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

            constraints.append(&mut generate_constraints(
                target,
                scope,
                enclosing_return_type_id,
            )?);
            for arg in args.iter() {
                constraints.append(&mut generate_constraints(
                    arg,
                    scope,
                    enclosing_return_type_id,
                )?);
            }

            Ok(constraints)
        }
        AstNodeType::Error => panic!("Unreachable error node in type check"),
    }
}

fn unify<'a>(mut constraints: Vec<TypeConstraint>) -> Result<Vec<TypeConstraint>, InterpretError> {
    let mut substitutions = Vec::new();
    unify_helper(&mut constraints, &mut substitutions)?;
    Ok(substitutions)
}

fn unify_helper(
    constraints: &mut Vec<TypeConstraint>,
    substitutions: &mut Vec<TypeConstraint>,
) -> Result<(), InterpretError> {
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
                        return Err(error("Mismatched types."));
                    }
                    match (left_return, right_return) {
                        (Some(left_return), Some(right_return)) => {
                            constraints.push(TypeConstraint::new(
                                *left_return.clone(),
                                *right_return.clone(),
                            ));
                        }
                        (None, None) => (),
                        _ => return Err(error("Mismatched types.")),
                    }

                    if left_children.len() != right_children.len() {
                        return Err(error("Mismatched types."));
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
