use crate::chunk::{ByteCode, Chunk};
use crate::common::{InterpretError, InterpretResult};
use crate::compiler::compile;
use crate::value::Value;

pub struct VM {
    pub chunk: Chunk,
    ip: usize,
    stack: Vec<u8>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        // TODO rearrange calls so we don't allocate a chunk in new
        self.chunk = compile(source)?;
        self.ip = 0;
        self.run()
    }

    fn pop_bytes_8(&mut self) -> [u8; 8] {
        [
            self.stack.pop().unwrap(),
            self.stack.pop().unwrap(),
            self.stack.pop().unwrap(),
            self.stack.pop().unwrap(),
            self.stack.pop().unwrap(),
            self.stack.pop().unwrap(),
            self.stack.pop().unwrap(),
            self.stack.pop().unwrap(),
        ]
    }

    fn pop_byte(&mut self) -> u8 {
        self.stack.pop().unwrap()
    }

    fn pop_bool(&mut self) -> bool {
        self.pop_byte() != 0
    }

    fn pop_int(&mut self) -> i64 {
        i64::from_le_bytes(self.pop_bytes_8())
    }

    fn pop_float(&mut self) -> f64 {
        f64::from_le_bytes(self.pop_bytes_8())
    }

    fn push(&mut self, bytes: &[u8]) -> () {
        self.stack.extend_from_slice(bytes)
    }

    fn push_bool(&mut self, val: bool) -> () {
        self.push(&[if val {1} else {0}])
    }

    fn push_int(&mut self, val: i64) -> () {
        self.push(&val.to_be_bytes())
    }

    fn push_float(&mut self, val: f64) -> () {
        self.push(&val.to_be_bytes())
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let bc = self.chunk.get_code(self.ip);

            #[cfg(feature = "debug-logging")]
            {
                print!(" stack: ");
                for slot in self.stack.iter() {
                    print!("[ {} ]", slot);
                }
                println!();
                self.chunk.disassemble_instruction(self.ip);
            }

            self.ip += 1;
            match bc {
                ByteCode::Return => {
                    // TODO 
                    println!("{:?}", self.stack);
                    self.stack.clear();
                    return Ok(()) 
                }
                ByteCode::PrintInt => {
                    println!("{}", self.pop_int())
                }
                ByteCode::PrintFloat => {
                    println!("{}", self.pop_float())
                }
                ByteCode::Constant(constant) => {
                    let constant = self.chunk.get_constant(constant, 8);
                    self.stack.extend_from_slice(constant)
                }
                ByteCode::NegateInt => {
                    let arg = self.pop_int();
                    self.push_int(-arg)
                }
                ByteCode::NegateFloat => {
                    let arg = self.pop_float();
                    self.push_float(-arg)
                }
                ByteCode::Not => {
                    let arg = self.pop_bool();
                    self.push_bool(!arg)
                }
                ByteCode::AddInt => {
                    let b = self.pop_int();
                    let a = self.pop_int();
                    self.push_int(a + b)
                }
                ByteCode::AddFloat => {
                    let b = self.pop_float();
                    let a = self.pop_float();
                    self.push_float(a + b)
                }
                ByteCode::SubInt => {
                    let b = self.pop_int();
                    let a = self.pop_int();
                    self.push_int(a - b)
                }
                ByteCode::SubFloat => {
                    let b = self.pop_float();
                    let a = self.pop_float();
                    self.push_float(a - b)
                }
                ByteCode::MulInt => {
                    let b = self.pop_int();
                    let a = self.pop_int();
                    self.push_int(a * b)
                }
                ByteCode::MulFloat => {
                    let b = self.pop_float();
                    let a = self.pop_float();
                    self.push_float(a * b)
                }
                ByteCode::DivInt => {
                    let b = self.pop_int();
                    let a = self.pop_int();
                    self.push_int(a / b)
                }
                ByteCode::DivFloat => {
                    let b = self.pop_float();
                    let a = self.pop_float();
                    self.push_float(a / b)
                }
                ByteCode::GreaterInt => {
                    let b = self.pop_int();
                    let a = self.pop_int();
                    self.push_bool(a > b)
                }
                ByteCode::GreaterFloat => {
                    let b = self.pop_float();
                    let a = self.pop_float();
                    self.push_bool(a > b)
                }
                ByteCode::LessInt => {
                    let b = self.pop_int();
                    let a = self.pop_int();
                    self.push_bool(a < b)
                }
                ByteCode::LessFloat => {
                    let b = self.pop_float();
                    let a = self.pop_float();
                    self.push_bool(a < b)
                }
                ByteCode::GreaterEqualInt => {
                    let b = self.pop_int();
                    let a = self.pop_int();
                    self.push_bool(a >= b)
                }
                ByteCode::GreaterEqualFloat => {
                    let b = self.pop_float();
                    let a = self.pop_float();
                    self.push_bool(a >= b)
                }
                ByteCode::LessEqualInt => {
                    let b = self.pop_int();
                    let a = self.pop_int();
                    self.push_bool(a <= b)
                }
                ByteCode::LessEqualFloat => {
                    let b = self.pop_float();
                    let a = self.pop_float();
                    self.push_bool(a <= b)
                }
                ByteCode::Equal8 => {
                    let b = self.pop_bytes_8();
                    let a = self.pop_bytes_8();
                    self.push_bool(a == b);
                }
                ByteCode::NotEqual8 => {
                    let b = self.pop_bytes_8();
                    let a = self.pop_bytes_8();
                    self.push_bool(a != b);
                }
                ByteCode::Equal1 => {
                    let b = self.pop_byte();
                    let a = self.pop_byte();
                    self.push_bool(a == b);
                }
                ByteCode::NotEqual1 => {
                    let b = self.pop_byte();
                    let a = self.pop_byte();
                    self.push_bool(a != b);
                }
            }
        }
    }
}
