use std::collections::HashMap;
use std::convert::TryInto;

use crate::chunk::{ByteCode, Chunk};
use crate::common::{InterpretError, InterpretResult};
use crate::compiler::compile;
use crate::heap::Heap;
use crate::object::{FunctionObj, ObjType, Object};

pub struct VM {
    stack: Stack,
    heap: Heap,
    globals: HashMap<String, Vec<u8>>,
    frames: Vec<CallFrame>,
}

struct Stack(Vec<u8>);

struct CallFrame {
    function_heap_index: usize,
    ip: usize,
    stack_index: usize,
}

impl CallFrame {
    fn new(function_heap_index: usize, stack_index: usize) -> Self {
        Self {
            function_heap_index,
            stack_index,
            ip: 0,
        }
    }

    fn func_obj<'a>(&self, heap: &'a Heap) -> &'a FunctionObj {
        let object = heap.get(self.function_heap_index);
        if let ObjType::Function(func_obj) = &object.value {
            &func_obj
        } else {
            panic!("FunctionObj of CallFrame wasn't a function")
        }
    }

    fn chunk<'a>(&self, heap: &'a Heap) -> &'a Chunk {
        &self.func_obj(heap).chunk
    }

    fn get_code<'a>(&self, heap: &'a Heap) -> ByteCode {
        self.func_obj(heap).chunk.get_code(self.ip)
    }

    fn get_last_line<'a>(&self, heap: &'a Heap) -> u32 {
        self.func_obj(heap).chunk.get_line(self.ip - 1)
    }

    fn jump(&mut self, offset: u16) -> () {
        self.ip += offset as usize;
    }

    fn back_jump(&mut self, offset: u16) -> () {
        self.ip -= offset as usize;
    }
}

impl Stack {
    fn len(&self) -> usize {
        self.0.len()
    }

    fn skip_peek_bytes_8(&self, skip: usize) -> &[u8] {
        let start_index = self.0.len() - skip - 8;
        &self.0[start_index..start_index + 8]
    }

    fn pop_bulk(&mut self, bytes: usize) -> Vec<u8> {
        self.0.drain(self.0.len() - bytes..).collect()
    }

    fn truncate(&mut self, size: usize) -> () {
        self.0.truncate(size);
    }

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

    fn peek_bool(&self) -> bool {
        self.peek_byte() != 0
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
        self.0
            .splice(index..index + size, value.into_iter().cloned());
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack(vec![0,0,0,0,0,0,0,0]), // 8 bytes to represent the root "function"
            heap: Heap::new(),
            globals: HashMap::new(),
            frames: Vec::with_capacity(u8::MAX as usize),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let main_func = compile(source, &mut self.heap)?;

        #[cfg(feature = "debug-logging")]
        main_func.chunk.disassemble("main");

        let heap_index = self
            .heap
            .allocate(Object::new(ObjType::Function(Box::new(main_func))));
        let frame = CallFrame::new(heap_index, 0);
        self.frames.push(frame);

        self.run()
    }

    fn current_frame(&self) -> &CallFrame {
        &self.frames[self.frames.len() - 1]
    }

    fn runtime_error(&self, message: &str) -> () {
        eprintln!("{}", message);

        for frame in self.frames.iter().rev() {
            eprintln!("[line {:>4}] in script", frame.get_last_line(&self.heap));
            // TODO add function name to trace
        }
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
        let heap_index = self.heap.allocate_string(val);
        self.stack.push(&heap_index.to_be_bytes())
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let VM {
                stack,
                heap: _,
                globals: _,
                frames,
            } = self;
            let last_index = frames.len() - 1;
            let current_frame = &mut frames[last_index];

            #[cfg(feature = "debug-logging")]
            {
                eprintln!();
                eprint!(" stack: ");
                for (index, slot) in stack.0.iter().enumerate() {
                    if index == current_frame.stack_index {
                        eprint!(" | ");
                    }
                    eprint!("[ {} ]", slot);
                }
                eprintln!();
                self.heap.print();
                current_frame
                    .chunk(&self.heap)
                    .disassemble_instruction(current_frame.ip);
            }

            let bc = current_frame.get_code(&self.heap);
            current_frame.ip += bc.size() as usize;
            match bc {
                ByteCode::Return(size) => {
                    let result = self.stack.pop_bulk(size as usize);
                    let old_frame = self.frames.pop().unwrap();
                    if self.frames.len() == 0 {
                        return Ok(());
                    }

                    self.stack.truncate(old_frame.stack_index);
                    self.stack.push(&result);
                }
                ByteCode::PrintInt => println!("{}", self.stack.pop_int()),
                ByteCode::PrintFloat => println!("{}", self.stack.pop_float()),
                ByteCode::PrintBool => println!("{}", self.stack.pop_bool()),
                ByteCode::PrintObject => println!("{}", self.pop_heap()),
                ByteCode::Constant1(constant) => {
                    let constant = current_frame.chunk(&self.heap).get_constant(&constant, 1);
                    stack.push(constant)
                }
                ByteCode::Constant8(constant) => {
                    let constant = current_frame.chunk(&self.heap).get_constant(&constant, 8);
                    stack.push(constant)
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
                    let mut result = a.as_string().to_string();
                    result.push_str(b.as_string());
                    self.push_string(result);
                }
                ByteCode::Pop8 => {
                    self.stack.pop_bytes_8();
                }
                ByteCode::Pop1 => {
                    self.stack.pop_byte();
                }
                ByteCode::DefineGlobal1(constant) => {
                    let constant = self
                        .current_frame()
                        .chunk(&self.heap)
                        .get_constant(&constant, 8);
                    let name = self.heap.get_with_bytes(constant).as_string().to_string();
                    let value = vec![self.stack.pop_byte()];
                    self.globals.insert(name, value);
                }
                ByteCode::DefineGlobal8(constant) => {
                    let constant = self
                        .current_frame()
                        .chunk(&self.heap)
                        .get_constant(&constant, 8);
                    let name = self.heap.get_with_bytes(constant).as_string().to_string();
                    let value = self.stack.pop_bytes_8().to_vec();
                    self.globals.insert(name, value);
                }
                ByteCode::GetGlobal1(constant) => {
                    let constant = self
                        .current_frame()
                        .chunk(&self.heap)
                        .get_constant(&constant, 8);
                    let name = self.heap.get_with_bytes(constant).as_string().to_string();
                    if let Some(value) = self.globals.get(&name) {
                        self.stack.push(value)
                    } else {
                        // TODO static analysis for variable usage
                        self.runtime_error(&format!("Undefined variable {}", name));
                        return Err(InterpretError::Runtime);
                    }
                }
                ByteCode::GetGlobal8(constant) => {
                    let constant = self
                        .current_frame()
                        .chunk(&self.heap)
                        .get_constant(&constant, 8);
                    let name = self.heap.get_with_bytes(constant).as_string();
                    if let Some(value) = self.globals.get(name) {
                        self.stack.push(value)
                    } else {
                        // TODO static analysis for variable usage
                        self.runtime_error(&format!("Undefined variable {}", name));
                        return Err(InterpretError::Runtime);
                    }
                }
                ByteCode::SetGlobal1(constant) => {
                    let constant = self
                        .current_frame()
                        .chunk(&self.heap)
                        .get_constant(&constant, 8);
                    let name = self.heap.get_with_bytes(constant).as_string();
                    let value = vec![self.stack.peek_byte()];
                    let already_defined = if let None = self.globals.insert(name.to_string(), value)
                    {
                        true
                    } else {
                        false
                    };
                    if already_defined {
                        // TODO static analysis for variable usage
                        // It was already defined
                        self.globals.remove(name);
                        self.runtime_error(&format!("Undefined variable {}", name));
                        return Err(InterpretError::Runtime);
                    }
                }
                ByteCode::SetGlobal8(constant) => {
                    let constant = self
                        .current_frame()
                        .chunk(&self.heap)
                        .get_constant(&constant, 8);
                    let name = self.heap.get_with_bytes(constant).as_string();
                    let value = self.stack.peek_bytes_8().to_vec();
                    let already_defined = if let None = self.globals.insert(name.to_string(), value)
                    {
                        true
                    } else {
                        false
                    };
                    if already_defined {
                        // TODO static analysis for variable usage
                        // It was already defined
                        self.globals.remove(name);
                        self.runtime_error(&format!("Undefined variable {}", name));
                        return Err(InterpretError::Runtime);
                    }
                }
                ByteCode::GetLocal1(index) => {
                    let value = self
                        .stack
                        .read_at(index + current_frame.stack_index, 1)
                        .to_vec();
                    self.stack.push(&value)
                }
                ByteCode::GetLocal8(index) => {
                    let value = self
                        .stack
                        .read_at(index + current_frame.stack_index, 8)
                        .to_vec();
                    self.stack.push(&value)
                }
                ByteCode::SetLocal1(index) => self.stack.write_at(
                    index + current_frame.stack_index,
                    1,
                    &[self.stack.peek_byte()],
                ),
                ByteCode::SetLocal8(index) => {
                    let value = self.stack.peek_bytes_8().to_vec();
                    self.stack
                        .write_at(index + current_frame.stack_index, 8, &value)
                }
                ByteCode::JumpIfFalse(offset) => {
                    if !self.stack.peek_bool() {
                        current_frame.jump(offset);
                    }
                }
                ByteCode::Jump(offset) => {
                    current_frame.jump(offset);
                }
                ByteCode::Loop(offset) => {
                    current_frame.back_jump(offset);
                }
                ByteCode::Call(args_bytes) => {
                    let heap_index = stack.skip_peek_bytes_8(args_bytes);
                    let heap_index = usize::from_be_bytes(heap_index.try_into().unwrap());
                    let frame = CallFrame::new(heap_index, self.stack.len() - (args_bytes + 8)); // To account for function object

                    #[cfg(feature = "debug-logging")]
                    frame.chunk(&self.heap).disassemble("function"); // TODO get/use function name

                    self.frames.push(frame);

                }
                ByteCode::NoOp => (),
            }
        }
    }
}
