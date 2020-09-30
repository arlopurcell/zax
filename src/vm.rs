use std::collections::HashMap;

use crate::chunk::{ByteCode, Chunk};
use crate::common::{InterpretError, InterpretResult};
use crate::compiler::compile;
use crate::heap::{Heap, ObjType, Object};
use crate::type_check::Scope;

pub struct VM<'a> {
    pub chunk: Chunk,
    ip: usize,
    stack: Stack,
    heap: Heap,
    globals: HashMap<String, Vec<u8>>,
    type_scope: Scope<'a>,
}

struct Stack(Vec<u8>);

impl Stack {
    fn pop_bytes_8(&mut self) -> [u8; 8] {
        let (a, b, c, d, e, f, g, h) = (
            self.0.pop().unwrap(),
            self.0.pop().unwrap(),
            self.0.pop().unwrap(),
            self.0.pop().unwrap(),
            self.0.pop().unwrap(),
            self.0.pop().unwrap(),
            self.0.pop().unwrap(),
            self.0.pop().unwrap(),
        );
        [h, g, f, e, d, c, b, a]
    }

    fn pop_byte(&mut self) -> u8 {
        self.0.pop().unwrap()
    }

    fn peek_byte(&self) -> u8 {
        self.0[self.0.len() - 1]
    }

    fn peek_bytes_8(&self) -> &[u8] {
        &self.0[self.0.len() - 8..]
    }

    fn pop_bool(&mut self) -> bool {
        self.pop_byte() != 0
    }

    fn pop_int(&mut self) -> i64 {
        i64::from_be_bytes(self.pop_bytes_8())
    }

    fn pop_float(&mut self) -> f64 {
        f64::from_be_bytes(self.pop_bytes_8())
    }

    fn push(&mut self, bytes: &[u8]) -> () {
        self.0.extend_from_slice(bytes)
    }

    fn push_bool(&mut self, val: bool) -> () {
        self.push(&[if val { 1 } else { 0 }])
    }

    fn push_int(&mut self, val: i64) -> () {
        self.push(&val.to_be_bytes())
    }

    fn push_float(&mut self, val: f64) -> () {
        self.push(&val.to_be_bytes())
    }

    fn read_at(&self, index: usize, size: usize) -> &[u8] {
        &self.0[index..index + size]
    }

    fn write_at(&mut self, index: usize, size: usize, value: &[u8]) -> () {
        self.0 = self
            .0
            .splice(index..index + size, value.into_iter().cloned())
            .collect();
    }
}

impl<'a> VM<'a> {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::new(),
            ip: 0,
            stack: Stack(Vec::new()),
            heap: Heap::new(),
            globals: HashMap::new(),
            type_scope: Scope::new(None),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        // TODO rearrange calls so we don't allocate a chunk in new
        self.chunk = compile(source, &mut self.heap, &mut self.type_scope)?;
        self.ip = 0;
        self.run()
    }

    fn runtime_error(&self, message: &str) -> () {
        eprintln!("{}", message);
        let line = self.chunk.get_line(self.ip - 1);
        eprintln!("[line {:>4}] in script", line);
    }

    fn pop_heap(&mut self) -> &Object {
        let bytes = &(self.stack.pop_bytes_8());
        self.heap.get_with_bytes(bytes)
    }

    fn pop_heap_2(&mut self) -> (&Object, &Object) {
        let bytes_2 = &self.stack.pop_bytes_8();
        let bytes_1 = &self.stack.pop_bytes_8();
        let o2 = self.heap.get_with_bytes(bytes_2);
        let o1 = self.heap.get_with_bytes(bytes_1);
        (o1, o2)
    }

    fn push_string(&mut self, val: String) -> () {
        let heap_index = self.heap.allocate(Object::new(ObjType::Str(val)));
        self.stack.push(&heap_index.to_be_bytes())
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let bc = self.chunk.get_code(self.ip);

            #[cfg(feature = "debug-logging")]
            {
                print!(" stack: ");
                for slot in self.stack.0.iter() {
                    print!("[ {} ]", slot);
                }
                println!();
                self.heap.print();
                self.chunk.disassemble_instruction(self.ip);
            }

            self.ip += 1;
            match bc {
                ByteCode::Return => return Ok(()),
                ByteCode::PrintInt => println!("{}", self.stack.pop_int()),
                ByteCode::PrintFloat => println!("{}", self.stack.pop_float()),
                ByteCode::PrintBool => println!("{}", self.stack.pop_bool()),
                ByteCode::PrintStr => println!("{}", self.pop_heap().to_string()),
                ByteCode::Constant(constant) => {
                    let constant = self.chunk.get_constant(constant, 8);
                    self.stack.push(constant)
                }
                ByteCode::NegateInt => {
                    let arg = self.stack.pop_int();
                    self.stack.push_int(-arg)
                }
                ByteCode::NegateFloat => {
                    let arg = self.stack.pop_float();
                    self.stack.push_float(-arg)
                }
                ByteCode::Not => {
                    let arg = self.stack.pop_bool();
                    self.stack.push_bool(!arg)
                }
                ByteCode::AddInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    self.stack.push_int(a + b)
                }
                ByteCode::AddFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    self.stack.push_float(a + b)
                }
                ByteCode::SubInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    self.stack.push_int(a - b)
                }
                ByteCode::SubFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    self.stack.push_float(a - b)
                }
                ByteCode::MulInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    self.stack.push_int(a * b)
                }
                ByteCode::MulFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    self.stack.push_float(a * b)
                }
                ByteCode::DivInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    self.stack.push_int(a / b)
                }
                ByteCode::DivFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    self.stack.push_float(a / b)
                }
                ByteCode::GreaterInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    self.stack.push_bool(a > b)
                }
                ByteCode::GreaterFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    self.stack.push_bool(a > b)
                }
                ByteCode::LessInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    self.stack.push_bool(a < b)
                }
                ByteCode::LessFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    self.stack.push_bool(a < b)
                }
                ByteCode::GreaterEqualInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    self.stack.push_bool(a >= b)
                }
                ByteCode::GreaterEqualFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    self.stack.push_bool(a >= b)
                }
                ByteCode::LessEqualInt => {
                    let b = self.stack.pop_int();
                    let a = self.stack.pop_int();
                    self.stack.push_bool(a <= b)
                }
                ByteCode::LessEqualFloat => {
                    let b = self.stack.pop_float();
                    let a = self.stack.pop_float();
                    self.stack.push_bool(a <= b)
                }
                ByteCode::Equal8 => {
                    let b = self.stack.pop_bytes_8();
                    let a = self.stack.pop_bytes_8();
                    self.stack.push_bool(a == b);
                }
                ByteCode::NotEqual8 => {
                    let b = self.stack.pop_bytes_8();
                    let a = self.stack.pop_bytes_8();
                    self.stack.push_bool(a != b);
                }
                ByteCode::Equal1 => {
                    let b = self.stack.pop_byte();
                    let a = self.stack.pop_byte();
                    self.stack.push_bool(a == b);
                }
                ByteCode::NotEqual1 => {
                    let b = self.stack.pop_byte();
                    let a = self.stack.pop_byte();
                    self.stack.push_bool(a != b);
                }
                ByteCode::EqualHeap => {
                    let (a, b) = self.pop_heap_2();
                    let result = a == b;
                    self.stack.push_bool(result);
                }
                ByteCode::NotEqualHeap => {
                    let (a, b) = self.pop_heap_2();
                    let result = a != b;
                    self.stack.push_bool(result);
                }
                ByteCode::Concat => {
                    let (a, b) = self.pop_heap_2();
                    let mut result = a.to_string().to_string();
                    result.push_str(b.to_string());
                    self.push_string(result);
                }
                ByteCode::Pop8 => {
                    self.stack.pop_bytes_8();
                }
                ByteCode::Pop1 => {
                    self.stack.pop_byte();
                }
                ByteCode::DefineGlobal1(constant) => {
                    let constant = self.chunk.get_constant(constant, 8);
                    let name = self.heap.get_with_bytes(constant).to_string().to_string();
                    let value = vec![self.stack.pop_byte()];
                    self.globals.insert(name, value);
                }
                ByteCode::DefineGlobal8(constant) => {
                    let constant = self.chunk.get_constant(constant, 8);
                    let name = self.heap.get_with_bytes(constant).to_string().to_string();
                    let value = self.stack.pop_bytes_8().to_vec();
                    self.globals.insert(name, value);
                }
                ByteCode::GetGlobal1(constant) => {
                    let constant = self.chunk.get_constant(constant, 8);
                    let name = self.heap.get_with_bytes(constant).to_string().to_string();
                    if let Some(value) = self.globals.get(&name) {
                        self.stack.push(value)
                    } else {
                        // TODO static analysis for variable usage
                        self.runtime_error(&format!("Undefined variable {}", name));
                        return Err(InterpretError::Runtime);
                    }
                }
                ByteCode::GetGlobal8(constant) => {
                    let constant = self.chunk.get_constant(constant, 8);
                    let name = self.heap.get_with_bytes(constant).to_string();
                    if let Some(value) = self.globals.get(name) {
                        self.stack.push(value)
                    } else {
                        self.runtime_error(&format!("Undefined variable {}", name));
                        return Err(InterpretError::Runtime);
                    }
                }
                ByteCode::SetGlobal1(constant) => {
                    let constant = self.chunk.get_constant(constant, 8);
                    let name = self.heap.get_with_bytes(constant).to_string();
                    let value = vec![self.stack.peek_byte()];
                    let already_defined = if let None = self.globals.insert(name.to_string(), value)
                    {
                        true
                    } else {
                        false
                    };
                    if already_defined {
                        // It was already defined
                        self.globals.remove(name);
                        self.runtime_error(&format!("Undefined variable {}", name));
                        return Err(InterpretError::Runtime);
                    }
                }
                ByteCode::SetGlobal8(constant) => {
                    let constant = self.chunk.get_constant(constant, 8);
                    let name = self.heap.get_with_bytes(constant).to_string();
                    let value = self.stack.peek_bytes_8().to_vec();
                    let already_defined = if let None = self.globals.insert(name.to_string(), value)
                    {
                        true
                    } else {
                        false
                    };
                    if already_defined {
                        // It was already defined
                        self.globals.remove(name);
                        self.runtime_error(&format!("Undefined variable {}", name));
                        return Err(InterpretError::Runtime);
                    }
                }
                ByteCode::GetLocal1(index) => {
                    let value = self.stack.read_at(*index, 1).to_vec();
                    self.stack.push(&value)
                }
                ByteCode::GetLocal8(index) => {
                    let value = self.stack.read_at(*index, 8).to_vec();
                    self.stack.push(&value)
                }
                ByteCode::SetLocal1(index) => {
                    self.stack.write_at(*index, 1, &[self.stack.peek_byte()])
                }
                ByteCode::SetLocal8(index) => {
                    let value = self.stack.peek_bytes_8().to_vec();
                    self.stack.write_at(*index, 8, &value)
                }
            }
        }
    }
}
