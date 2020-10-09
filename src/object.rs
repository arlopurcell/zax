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
    Upvalue(Vec<u8>), // TODO keep on stack until hoist is required?
    Nil,
}

#[derive(PartialEq, Eq)]
pub struct FunctionObj {
    name_index: usize,
    pub arity: u8,
    pub chunk: Chunk,
    //pub name: String,
}

#[derive(PartialEq, Eq)]
pub struct ClosureObj {
    pub func_index: usize,
    pub upvalues: Vec<usize>,
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
            Self::Function(func) => format!("fun {} ({})", heap.get(func.name_index).print(heap), func.arity),
            Self::NativeFunction(func) => format!("Native Function {:?}", func),
            Self::Closure(closure) => format!("Closure({}, upvalue_indexes: {:?})", heap.get(closure.func_index).print(heap), closure.upvalues),
            Self::Upvalue(value) => format!("Upvalue {:?}", value),
            Self::Nil => "nil".to_string(),
        }
    }
}

impl FunctionObj {
    pub fn new(name: &str, chunk: Chunk, arity: u8, heap: &mut Heap) -> Self {
        let name_index = heap.allocate_string(&name);
        Self {
            name_index,
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
    pub fn new(chunk:Chunk, name: &str, arity: u8, upvalues: Vec<usize>, heap: &mut Heap) -> Self {
        let func = Object::new(ObjType::Function(Box::new(FunctionObj::new(name, chunk, arity, heap))));
        let func_index = heap.allocate(func);
        Self {
            func_index,
            upvalues,
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
