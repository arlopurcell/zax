use crate::chunk::{Chunk, ByteCode};
use crate::value::Value;

pub struct VM {
    pub chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

pub enum InterpretError {
    Compile,
    Runtime,
}

type InterpretResult = Result<(), InterpretError>;

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        loop {
            let bc = self.chunk.get_code(self.ip);

            #[cfg(feature = "debug-logging")]
            {
                print!("        ");
                for slot in self.stack.iter() {
                    print!("[ ");
                    slot.print();
                    print!(" ]");
                }
                println!();
                self.chunk.disassemble_instruction(self.ip);
            }

            self.ip += 1;
            match bc {
                ByteCode::Return => {
                    self.stack.pop().unwrap().print();
                    println!();
                    return Ok(());
                },
                ByteCode::Constant(constant) => {
                       self.stack.push(self.chunk.get_constant(constant).clone());
                },
                ByteCode::Negate => {
                    let result = match self.stack.pop().unwrap() {
                        Value::Float(v) => Value::Float(-v)
                    };
                    self.stack.push(result);
                },
                ByteCode::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (Value::Float(a), Value::Float(b)) => Value::Float(a + b)
                    };
                    self.stack.push(result);
                },
                ByteCode::Sub => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (Value::Float(a), Value::Float(b)) => Value::Float(a - b)
                    };
                    self.stack.push(result);
                },
                ByteCode::Mul => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (Value::Float(a), Value::Float(b)) => Value::Float(a * b)
                    };
                    self.stack.push(result);
                },
                ByteCode::Div => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (Value::Float(a), Value::Float(b)) => Value::Float(a / b)
                    };
                    self.stack.push(result);
                },
            }
        }
    }
}
