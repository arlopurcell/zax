use crate::code_gen::Generator;
use crate::value::Value;
use crate::chunk::ByteCode;

pub enum AstNode<'a> {
    Error,
    IntLiteral{line: u32, value: u32},
    FloatLiteral{line: u32, value: f32},
    BoolLiteral{line: u32, value: bool},
    StrLiteral{line: u32, value: &'a str},

    Unary{line: u32, operator: Operator, operand: Box<AstNode<'a>>},
    Binary{line: u32, operator: Operator, lhs: Box<AstNode<'a>>, rhs: Box<AstNode<'a>>},
}

pub enum Operator {
    Add, Sub, Mul, Div,
    And, Or,
    Equal, NotEqual,
    Greater, GreaterEqual, Less, LessEqual,

    Neg, Not
}

impl <'a> AstNode<'a> {
    pub fn generate(self, generator: &mut Generator) -> () {
        match self {
            Self::IntLiteral{line, value} => generator.emit_constant(Value::Integer(value as i64), line),
            Self::FloatLiteral{line, value} => generator.emit_constant(Value::Float(value as f64), line),
            Self::Unary{line, operator, operand} => {
                operand.generate(generator);
                match operator {
                    Operator::Neg => generator.emit_byte(ByteCode::Negate, line),
                    _ => panic!("Unexpected unary code gen"),
                }
            },
            Self::Binary{line, operator, lhs, rhs} => {
                lhs.generate(generator);
                rhs.generate(generator);
                match operator {
                    Operator::Add => generator.emit_byte(ByteCode::Add, line),
                    Operator::Sub => generator.emit_byte(ByteCode::Sub, line),
                    Operator::Mul => generator.emit_byte(ByteCode::Mul, line),
                    Operator::Div => generator.emit_byte(ByteCode::Div, line),
                    _ => panic!("Unexpected binary code gen"),
                }
            },

            _ => panic!("in implemented code gen"),
        }
    }
}

