use std::fmt;

use crate::chunk::Chunk;
use crate::common::ByteSerialize;

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
    //pub name: String,
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

/*
impl ByteSerialize for Object {
    fn to_bytes(self) -> Vec<u8> {
    }

    fn from_bytes(bytes: &[u8]) -> Self {
    }
}
*/

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.fmt(f)
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

/*
impl ByteSerialize for ObjType {
    fn to_bytes(self) -> Vec<u8> {
    }

    fn from_bytes(bytes: &[u8]) -> Self {
    }
}
*/

impl FunctionObj {
    // TODO fix this
    pub fn new(chunk: Chunk) -> Self {
        Self {
            arity: 0,
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

impl ByteSerialize for FunctionObj {
    fn to_bytes(self) -> Vec<u8> {
        let mut data = self.chunk.to_bytes();
        data.push(self.arity);
        //let name_bytes: Vec<_> = self.name.bytes().collect();
        //data.extend_from_slice(&name_bytes);
        data
    }

    fn from_bytes(bytes: &[u8]) -> Self {
        let arity = bytes[bytes.len() - 1];
        let chunk = Chunk::from_bytes(&bytes[0..bytes.len() - 2]);
        Self {arity, chunk}
    }
}
