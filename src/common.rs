#[derive(Debug)]
pub enum InterpretError {
    Compile,
    Runtime,
    Bug,
    File,
}

pub type InterpretResult = Result<(), InterpretError>;

pub trait ByteSerialize {
    fn to_bytes(&self) -> &[u8];
    fn from_bytes(bytes: &[u8]) -> Self;
}
