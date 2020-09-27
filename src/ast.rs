use crate::code_gen::Generator;
use crate::value::Value;
use crate::chunk::ByteCode;

pub enum AstNode<'a> {
    Error,
    IntLiteral(u32, u32),
    FloatLiteral(u32, f32),
    BoolLiteral(u32, bool),
    StrLiteral(u32, &'a str),

    Unary(u32, Operator, Box<AstNode<'a>>),
    Binary(u32, Operator, Box<AstNode<'a>>, Box<AstNode<'a>>),
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
            Self::IntLiteral(line, val) => generator.emit_constant(Value::Integer(val as i64), line),
            Self::FloatLiteral(line, val) => generator.emit_constant(Value::Float(val as f64), line),
            Self::Unary(line, operator, operand) => {
                operand.generate(generator);
                match operator {
                    Operator::Neg => generator.emit_byte(ByteCode::Negate, line),
                    _ => panic!("Unexpected unary code gen"),
                }
            },
            Self::Binary(line, operator, a, b) => {
                a.generate(generator);
                b.generate(generator);
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

