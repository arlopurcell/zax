use std::fmt;
use std::time::{SystemTime, UNIX_EPOCH};
use std::mem::size_of;

use crate::chunk::Chunk;
use crate::heap::Heap;
use crate::vm::VM;

#[derive(PartialEq, Eq)]
pub struct Object {
    pub value: ObjType,
}

#[derive(PartialEq, Eq)]
pub enum ObjType {
    Str(Box<String>),
    Function(Box<FunctionObj>),
    NativeFunction(Box<NativeFunctionObj>),
    Upvalue(i64), // TODO keep on stack until hoist is required?
    Nil,
}

#[derive(PartialEq, Eq)]
pub struct FunctionObj {
    name_index: i64,
    pub arity: u8,
    pub chunk: Chunk,
    //pub name: String,
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

    pub fn size(&self) -> usize {
        self.value.size()
    }
}

impl Drop for Object {
    fn drop(&mut self) -> () {
        #[cfg(feature = "debug-log-gc")]
        eprintln!("Freeing {}", self.print())
    }
}

impl ObjType {
    pub fn print(&self, heap: &Heap) -> String {
        match self {
            Self::Str(s) => format!("\"{}\"", s),
            Self::Function(func) => format!(
                "fun {} ({})",
                heap.get(&func.name_index).print(heap),
                func.arity
            ),
            Self::NativeFunction(func) => format!("Native Function {:?}", func),
            Self::Upvalue(value) => format!("Upvalue {:?}", value),
            Self::Nil => "nil".to_string(),
        }
    }

    fn size(&self) -> usize {
        match self {
            Self::Str(s) => s.bytes().len(),
            Self::Function(_) => size_of::<FunctionObj>(),
            Self::NativeFunction(_) => size_of::<FunctionObj>(),
            Self::Upvalue(bytes) => size_of::<i64>(),
            Self::Nil => 0,
        }
    }
}

impl FunctionObj {
    pub fn new(name: &str, chunk: Chunk, arity: u8, vm: &mut VM) -> Self {
        let name_index = vm.allocate_string(&name);
        Self {
            name_index,
            arity,
            chunk,
            //name: "".to_string(),
        }
    }

    pub fn name(&self, heap: &Heap) -> String {
        heap.get(&self.name_index).print(heap)
    }
}

impl fmt::Display for FunctionObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        //write!(f, "{}({})", self.name, self.arity)
        write!(f, "function({})", self.arity)
    }
}

impl NativeFunctionObj {
    pub fn exec(&self, _args: &[i64]) -> Vec<i64> {
        match self {
            Self::Clock => match SystemTime::now().duration_since(UNIX_EPOCH) {
                Ok(n) => vec![n.as_secs_f64() as i64],
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
