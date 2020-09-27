use crate::chunk::{Chunk, ByteCode};
use crate::value::Value;
use crate::compiler::compile;
use crate::common::{InterpretResult, InterpretError};

pub struct VM {
    pub chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
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

    fn run(&mut self) -> InterpretResult {
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
                        Value::Float(v) => Value::Float(-v),
                        Value::Integer(v) => Value::Integer(-v),
                    };
                    self.stack.push(result);
                },
                ByteCode::Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                        (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
                        _ => return Err(InterpretError::Bug),
                    };
                    self.stack.push(result);
                },
                ByteCode::Sub => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                        (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
                        _ => return Err(InterpretError::Bug),
                    };
                    self.stack.push(result);
                },
                ByteCode::Mul => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                        (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
                        _ => return Err(InterpretError::Bug),
                    };
                    self.stack.push(result);
                },
                ByteCode::Div => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    let result = match (a, b) {
                        (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                        (Value::Integer(a), Value::Integer(b)) => Value::Integer(a / b),
                        _ => return Err(InterpretError::Bug),
                    };
                    self.stack.push(result);
                },
            }
        }
    }
}
