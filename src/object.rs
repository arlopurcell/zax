use std::fmt;

use crate::chunk::Chunk;

#[derive(PartialEq, Eq)]
pub struct Object {
    pub value: ObjType,
}

#[derive(PartialEq, Eq)]
pub enum ObjType {
    Str(Box<String>),
    Function(Box<FunctionObj>),
}

#[derive(PartialEq, Eq)]
pub struct FunctionObj {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: String,
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
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Object({})", self.value)
    }
}

impl fmt::Display for ObjType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Function(func) => write!(f, "Function {}", func),
        }
    }
}

impl FunctionObj {
    // TODO fix this
    pub fn new(chunk: Chunk) -> Self {
        Self {
            arity: 0,
            chunk,
            name: "".to_string(),
        }
    }
}

impl fmt::Display for FunctionObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, self.arity)
    }
}
