use crate::code_gen::Generator;
use crate::value::Value;
use crate::chunk::ByteCode;

pub struct AstNode<'a> {
    line: u32,
    data_type: DataType, 
    node_type: AstNodeType<'a>,
}

pub enum AstNodeType<'a> {
    Error,
    IntLiteral(u32),
    FloatLiteral(f32),
    BoolLiteral(bool),
    StrLiteral(&'a str),
    Unary(Operator, Box<AstNode<'a>>),
    Binary(Operator, Box<AstNode<'a>>, Box<AstNode<'a>>),
}

pub enum DataType {
    None,
    Int,
    Float,
    Bool,
    Str,
    // TODO Array(Box<NodeType>)
}

pub enum Operator {
    Add, Sub, Mul, Div,
    And, Or,
    Equal, NotEqual,
    Greater, GreaterEqual, Less, LessEqual,

    Neg, Not
}

impl <'a> AstNode<'a> {
    pub fn new(line: u32, node_type: AstNodeType<'a>) -> Self {
        Self {
            line,
            data_type: DataType::None,
            node_type,
        }
    }

    pub fn generate(self, generator: &mut Generator) -> () {
        match self.node_type {
            AstNodeType::IntLiteral(value) => generator.emit_constant(Value::Integer(value as i64), self.line),
            AstNodeType::FloatLiteral(value) => generator.emit_constant(Value::Float(value as f64), self.line),
            AstNodeType::Unary(operator, operand) => {
                operand.generate(generator);
                match operator {
                    Operator::Neg => generator.emit_byte(ByteCode::Negate, self.line),
                    _ => panic!("Unexpected unary code gen"),
                }
            },
            AstNodeType::Binary(operator, lhs, rhs) => {
                lhs.generate(generator);
                rhs.generate(generator);
                match operator {
                    Operator::Add => generator.emit_byte(ByteCode::Add, self.line),
                    Operator::Sub => generator.emit_byte(ByteCode::Sub, self.line),
                    Operator::Mul => generator.emit_byte(ByteCode::Mul, self.line),
                    Operator::Div => generator.emit_byte(ByteCode::Div, self.line),
                    _ => panic!("Unexpected binary code gen"),
                }
            },

            _ => panic!("in implemented code gen"),
        }
    }
}

