use std::convert::TryInto;
use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::chunk::Chunk;
use crate::heap::Heap;
use crate::common::InterpretError;

#[derive(PartialEq, Eq)]
pub struct Object {
    pub value: ObjType,
}

#[derive(PartialEq, Eq)]
pub enum ObjType {
    Str(Box<String>),
    Function(Box<FunctionObj>),
    NativeFunction(Box<NativeFunctionObj>),
    Closure(Box<ClosureObj>),
    Primitive(Vec<u8>),
    Nil,
}

#[derive(PartialEq, Eq)]
pub struct FunctionObj {
    pub arity: u8,
    pub chunk: Chunk,
    //pub name: String,
}

#[derive(PartialEq, Eq)]
pub struct ClosureObj {
    pub func_index: usize,
    pub upvalue_indexes: Vec<usize>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum NativeFunctionObj {
    Clock,
}

impl Object {
    pub fn new(obj_type: ObjType) -> Self {
        Self { value: obj_type }
    }

    pub fn as_string(&self) -> &str {
        match &self.value {
            ObjType::Str(string) => &string,
            _ => panic!("object at heap index non-string"),
        }
    }

    pub fn print(&self, heap: &Heap) -> String {
        self.value.print(heap)
    }
}


impl ObjType {
    pub fn print(&self, heap: &Heap) -> String {
        match self {
            Self::Str(s) => format!("\"{}\"", s),
            Self::Function(func) => format!("Function {}", func),
            Self::NativeFunction(func) => format!("Native Function {:?}", func),
            Self::Closure(closure) => format!("Closure({}, upvalue_indexes: {:?})", heap.get(closure.func_index).print(heap), closure.upvalue_indexes),
            Self::Primitive(value) => format!("Primitive {:?}", value),
            Self::Nil => "nil".to_string(),
        }
    }
}

impl FunctionObj {
    // TODO fix this
    pub fn new(chunk: Chunk, arity: u8) -> Self {
        Self {
            arity,
            chunk,
            //name: "".to_string(),
        }
    }
}

impl fmt::Display for FunctionObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //write!(f, "{}({})", self.name, self.arity)
        write!(f, "function({})", self.arity)
    }
}

impl ClosureObj {
    // TODO function name
    pub fn new(chunk:Chunk, arity: u8, upvalue_count: u8, heap: &mut Heap) -> Self {
        let func_index = heap.allocate(Object::new(ObjType::Function(Box::new(FunctionObj::new(chunk, arity)))));
        Self {
            func_index,
            upvalue_indexes: (0..upvalue_count).map(|_| heap.allocate(Object::new(ObjType::Nil))).collect(),
        }
    }
}

impl NativeFunctionObj {
    pub fn exec(&self, args: &[u8]) -> Vec<u8> {
        match self {
            Self::Clock => match SystemTime::now().duration_since(UNIX_EPOCH) {
                Ok(n) => n.as_secs_f64().to_be_bytes().to_vec(),
                Err(_) => panic!("SystemTime before UNIX EPOCH!"),
            },
        }
    }

    pub fn arg_bytes(&self) -> usize {
        match self {
            Self::Clock => 0,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::Clock => "clock",
        }
    }

    pub fn all() -> Vec<NativeFunctionObj> {
        vec![Self::Clock]
    }
}
