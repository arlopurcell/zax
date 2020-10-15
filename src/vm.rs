use std::collections::HashMap;

use crate::ast::{NodeId, VarLocation};
use crate::chunk::{ByteCode, Chunk};
use crate::code_gen::ChunkGenerator;
use crate::code_gen::FunctionType;
use crate::common::{InterpretError, InterpretResult};
use crate::compiler::compile;
use crate::gc::collect_garbage;
use crate::heap::Heap;
use crate::object::{FunctionObj, NativeFunctionObj, ObjType, Object};

pub struct VM {
    pub stack: Stack,
    pub heap: Heap,
    pub globals: HashMap<String, Vec<i64>>,
    pub frames: Vec<CallFrame>,

    // Compile time stuff
    pub upvalue_allocations: HashMap<NodeId, i64>,
    pub var_locations: HashMap<NodeId, VarLocation>,
    pub chunk_generators: Vec<ChunkGenerator>,

    // GC bookkeeping
    pub bytes_allocated: usize,
    pub next_gc: usize,
}

pub struct Stack(pub Vec<i64>);

#[derive(Debug)]
pub struct CallFrame {
    pub function_heap_index: i64,
    ip: usize,
    stack_index: usize,
}

impl CallFrame {
    fn new(function_heap_index: i64, stack_index: usize) -> Self {
        Self {
            function_heap_index,
            stack_index,
            ip: 0,
        }
    }

    fn func_obj<'a>(&self, heap: &'a Heap) -> &'a FunctionObj {
        let object = heap.get(&self.function_heap_index);
        if let ObjType::Function(func) = &object.value {
            &func
        } else {
            panic!("func of CallFrame wasn't a FunctionObj")
        }
    }

    fn func_obj_mut<'a>(&mut self, heap: &'a mut Heap) -> &'a mut FunctionObj {
        let object = heap.get_mut(&self.function_heap_index);
        if let ObjType::Function(func) = &mut object.value {
            func
        } else {
            panic!("func of CallFrame wasn't a FunctionObj")
        }
    }

    fn chunk<'a>(&self, heap: &'a Heap) -> &'a Chunk {
        &self.func_obj(heap).chunk
    }

    fn get_code(&self, heap: &Heap) -> ByteCode {
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

    fn skip_peek(&self, skip: usize) -> i64 {
        self.0[self.0.len() - skip - 1]
    }

    fn pop_bulk(&mut self, words: usize) -> Vec<i64> {
        self.0.drain(self.0.len() - words..).collect()
    }

    fn truncate(&mut self, size: usize) -> () {
        self.0.truncate(size);
    }

    pub fn pop(&mut self) -> i64 {
        self.0.pop().unwrap()
    }

    fn peek(&self) -> i64 {
        self.skip_peek(0)
    }

    fn peek_bulk(&self, size: usize) -> &[i64] {
        &self.0[self.0.len() - size - 1..]
    }

    fn pop_bool(&mut self) -> bool {
        self.pop() != 0
    }

    fn peek_bool(&self) -> bool {
        self.peek() != 0
    }

    pub fn push(&mut self, value: i64) -> () {
        self.0.push(value)
    }

    fn push_bulk(&mut self, values: &[i64]) -> () {
        self.0.extend_from_slice(values)
    }

    fn copy_to_top(&mut self, index: usize, size: usize) -> () {
        //TODO optimize?
        for i in 0..size {
            self.0.push(self.0[index + i])
        }
    }

    fn copy_from_top(&mut self, index: usize, size: usize) -> () {
        //TODO optimize?
        for i in 0..size {
            self.0[index + i] = self.0[self.0.len() - size + i]
        }
    }

    fn push_bool(&mut self, val: bool) -> () {
        self.push(if val { 1 } else { 0 })
    }
}

impl VM {
    pub fn new() -> Self {
        let mut vm = Self {
            stack: Stack(vec![0]), // 1 word to represent the root "function"
            heap: Heap::new(),
            globals: HashMap::new(),
            frames: Vec::with_capacity(u8::MAX as usize),

            upvalue_allocations: HashMap::new(),
            var_locations: HashMap::new(),
            chunk_generators: Vec::new(),

            bytes_allocated: 0,
            next_gc: 1024 * 1024, // arbitrary
        };

        for f in NativeFunctionObj::all() {
            vm.define_native(f);
        }

        vm
    }

    pub fn gen(&mut self) -> &mut ChunkGenerator {
        // This is a crazy way to get a mutable reference to the last element
        self.chunk_generators.iter_mut().rev().nth(0).unwrap()
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        self.chunk_generators
            .push(ChunkGenerator::new(FunctionType::Script));
        compile(source, self)?;

        // create empty function and set chunk after to avoid GCing chunk before main
        // function allocation
        let empty_str_index = self.allocate_string("");
        // put on stack to avoid GCing
        self.stack.push(empty_str_index);
        let main_func = FunctionObj::empty(empty_str_index, 0);

        let heap_index = self.allocate(Object::new(ObjType::Function(Box::new(main_func))));
        let frame = CallFrame::new(heap_index, 0);
        self.frames.push(frame);

        let chunk = self.chunk_generators.pop().unwrap().end();
        self.frames[0].func_obj_mut(&mut self.heap).chunk = chunk;

        self.upvalue_allocations.clear();
        self.chunk_generators.clear();
        self.var_locations.clear();
        // pop name index off stack
        self.stack.pop();

        #[cfg(feature = "debug-logging")]
        self.current_frame().chunk(&self.heap).disassemble("main");

        self.run()
    }

    fn current_frame(&self) -> &CallFrame {
        &self.frames[self.frames.len() - 1]
    }

    fn runtime_error(&self, message: &str) -> () {
        eprintln!("{}", message);

        for frame in self.frames.iter().rev() {
            eprintln!(
                "{}[line {:>4}] in script",
                frame.func_obj(&self.heap).name(&self.heap),
                frame.get_last_line(&self.heap)
            );
        }
    }

    fn define_native(&mut self, func_obj: NativeFunctionObj) -> () {
        let name = func_obj.name().to_string();
        let heap_index = self.allocate(Object::new(ObjType::NativeFunction(Box::new(func_obj))));
        self.globals.insert(name, vec![heap_index]);
    }

    fn pop_heap_2(&mut self) -> (&Object, &Object) {
        let index_2 = self.stack.pop();
        let index_1 = self.stack.pop();
        let o2 = self.heap.get(&index_2);
        let o1 = self.heap.get(&index_1);
        (o1, o2)
    }

    fn push_string(&mut self, val: &str) -> () {
        let heap_index = self.allocate_string(val);
        self.stack.push(heap_index)
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let VM {
                stack,
                heap,
                globals: _,
                frames,

                upvalue_allocations: _,
                var_locations: _,
                chunk_generators: _,

                bytes_allocated: _,
                next_gc: _,
            } = self;
            let last_index = frames.len() - 1;
            let current_frame = &mut frames[last_index];

            #[cfg(feature = "debug-logging")]
            {
                eprintln!();
                current_frame
                    .chunk(&heap)
                    .disassemble_instruction(current_frame.ip);
                eprintln!("{:?}", current_frame);
                eprint!(" stack: ");
                for (index, slot) in stack.0.iter().enumerate() {
                    if index == current_frame.stack_index {
                        eprint!(" | ");
                    }
                    eprint!("[ {} ]", slot);
                }
                eprintln!();
                heap.print();
            }

            let bc = current_frame.get_code(&heap);
            current_frame.ip += bc.size() as usize;
            match bc {
                ByteCode::Return(size) => {
                    let result = self.stack.pop_bulk(size as usize);
                    let old_frame = self.frames.pop().unwrap();
                    if self.frames.len() == 0 {
                        return Ok(());
                    }

                    self.stack.truncate(old_frame.stack_index);
                    self.stack.push_bulk(&result);
                }
                ByteCode::PrintInt => println!("{}", self.stack.pop()),
                ByteCode::PrintFloat => println!("{}", self.stack.pop() as f64),
                ByteCode::PrintBool => println!("{}", self.stack.pop_bool()),
                ByteCode::PrintObject => {
                    let heap_ref = self.stack.pop();
                    let value = heap.get(&heap_ref);
                    println!("{}", value.print(&heap))
                }
                ByteCode::Constant(constant, size) => {
                    let constant = current_frame
                        .chunk(&self.heap)
                        .get_constant(&constant, &size);
                    stack.push_bulk(constant)
                }
                ByteCode::NegateInt => {
                    let arg = self.stack.pop();
                    self.stack.push(-arg)
                }
                ByteCode::NegateFloat => {
                    let arg = self.stack.pop() as f64;
                    self.stack.push((-arg) as i64)
                }
                ByteCode::Not => {
                    let arg = self.stack.pop_bool();
                    self.stack.push_bool(!arg)
                }
                ByteCode::AddInt => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a + b)
                }
                ByteCode::AddFloat => {
                    let b = self.stack.pop() as f64;
                    let a = self.stack.pop() as f64;
                    self.stack.push((a + b) as i64)
                }
                ByteCode::SubInt => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a - b)
                }
                ByteCode::SubFloat => {
                    let b = self.stack.pop() as f64;
                    let a = self.stack.pop() as f64;
                    self.stack.push((a - b) as i64)
                }
                ByteCode::MulInt => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a * b)
                }
                ByteCode::MulFloat => {
                    let b = self.stack.pop() as f64;
                    let a = self.stack.pop() as f64;
                    self.stack.push((a * b) as i64)
                }
                ByteCode::DivInt => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push(a / b)
                }
                ByteCode::DivFloat => {
                    let b = self.stack.pop() as f64;
                    let a = self.stack.pop() as f64;
                    self.stack.push((a / b) as i64)
                }
                ByteCode::GreaterInt => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push_bool(a > b)
                }
                ByteCode::GreaterFloat => {
                    let b = self.stack.pop() as f64;
                    let a = self.stack.pop() as f64;
                    self.stack.push_bool(a > b)
                }
                ByteCode::LessInt => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push_bool(a < b)
                }
                ByteCode::LessFloat => {
                    let b = self.stack.pop() as f64;
                    let a = self.stack.pop() as f64;
                    self.stack.push_bool(a < b)
                }
                ByteCode::GreaterEqualInt => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push_bool(a >= b)
                }
                ByteCode::GreaterEqualFloat => {
                    let b = self.stack.pop() as f64;
                    let a = self.stack.pop() as f64;
                    self.stack.push_bool(a >= b)
                }
                ByteCode::LessEqualInt => {
                    let b = self.stack.pop();
                    let a = self.stack.pop();
                    self.stack.push_bool(a <= b)
                }
                ByteCode::LessEqualFloat => {
                    let b = self.stack.pop() as f64;
                    let a = self.stack.pop() as f64;
                    self.stack.push_bool(a <= b)
                }
                ByteCode::Equal(size) => {
                    let b = self.stack.pop_bulk(size as usize);
                    let a = self.stack.pop_bulk(size as usize);
                    self.stack.push_bool(a == b);
                }
                ByteCode::NotEqual(size) => {
                    let b = self.stack.pop_bulk(size as usize);
                    let a = self.stack.pop_bulk(size as usize);
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
                    self.push_string(&result);
                }
                ByteCode::Pop(n) => {
                    self.stack.pop_bulk(n);
                }
                ByteCode::DefineGlobal(constant, size) => {
                    let constant = self
                        .current_frame()
                        .chunk(&self.heap)
                        .get_constant(&constant, &1);
                    let name = self.heap.get(&constant[0]).as_string().to_string();
                    let value = self.stack.pop_bulk(size as usize);
                    self.globals.insert(name, value.to_vec());
                }
                ByteCode::GetGlobal(constant) => {
                    let constant = self
                        .current_frame()
                        .chunk(&self.heap)
                        .get_constant(&constant, &1);
                    let name = self.heap.get(&constant[0]).as_string().to_string();
                    if let Some(value) = self.globals.get(&name) {
                        self.stack.push_bulk(value)
                    } else {
                        // TODO static analysis for variable usage
                        self.runtime_error(&format!("Undefined variable {}", name));
                        return Err(InterpretError::Runtime);
                    }
                }
                ByteCode::SetGlobal(constant, size) => {
                    let constant = self
                        .current_frame()
                        .chunk(&self.heap)
                        .get_constant(&constant, &1);
                    let name = self.heap.get(&constant[0]).as_string();
                    let value = self.stack.peek_bulk(size as usize);
                    let already_defined = self
                        .globals
                        .insert(name.to_string(), value.to_vec())
                        .is_none();
                    if already_defined {
                        // TODO static analysis for variable usage
                        // It was already defined
                        self.globals.remove(name);
                        self.runtime_error(&format!("Undefined variable {}", name));
                        return Err(InterpretError::Runtime);
                    }
                }
                ByteCode::GetLocal(index, size) => self
                    .stack
                    .copy_to_top(index + current_frame.stack_index, size as usize),
                ByteCode::SetLocal(index, size) => self
                    .stack
                    .copy_from_top(index + current_frame.stack_index, size as usize),
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
                    let heap_index = stack.skip_peek(args_bytes);
                    let object = self.heap.get(&heap_index);
                    if let ObjType::NativeFunction(func_obj) = &object.value {
                        let args_bytes = func_obj.arg_bytes();
                        let args = self.stack.pop_bulk(args_bytes);
                        self.stack.push_bulk(&func_obj.exec(&args));
                    } else {
                        let frame = CallFrame::new(heap_index, self.stack.len() - (args_bytes + 1)); // To account for function object

                        #[cfg(feature = "debug-logging")]
                        frame.chunk(&self.heap).disassemble("function"); // TODO get/use function name

                        self.frames.push(frame);
                    }
                }
                ByteCode::NoOp => (),
                ByteCode::SetHeap(index, size) => {
                    let value = self.stack.peek_bulk(size as usize);
                    if let ObjType::Upvalue(words) = &mut heap.get_mut(&index).value {
                        words.clear();
                        words.extend_from_slice(value);
                    } else {
                        panic!("should be upvalue")
                    }
                }
                ByteCode::GetHeap(index) => {
                    if let ObjType::Upvalue(value) = &heap.get(&index).value {
                        self.stack.push_bulk(value);
                    } else {
                        panic!("should be upvalue")
                    }
                }
                ByteCode::PopSkip(n, size) => {
                    let skipped = self.stack.pop_bulk(size as usize);
                    self.stack.pop_bulk(n);
                    self.stack.push_bulk(&skipped);
                }
            }
        }
    }

    pub fn allocate_string(&mut self, s: &str) -> i64 {
        if let Some(key) = self.heap.interned_strings.get(s) {
            *key
        } else {
            let key = self.allocate(Object::new(ObjType::Str(Box::new(s.to_string()))));
            self.heap.interned_strings.insert(s.to_string(), key);
            key
        }
    }

    pub fn allocate(&mut self, o: Object) -> i64 {
        #[cfg(feature = "debug-stress-gc")]
        collect_garbage(self);

        self.bytes_allocated += o.size();
        if self.bytes_allocated > self.next_gc {
            collect_garbage(self);
        }

        self.heap.allocate(o)
    }
}
