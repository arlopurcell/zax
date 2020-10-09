#[derive(Debug)]
pub enum InterpretError {
    Compile,
    Runtime,
//    Bug,
    File,
}

pub type InterpretResult = Result<(), InterpretError>;
