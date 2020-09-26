use std::io;
use std::mem::size_of;

mod chunk;
//mod common;
mod value;
mod vm;

use crate::chunk::{ByteCode, Chunk};
use crate::value::Value;
use crate::vm::VM;

fn main() -> io::Result<()> {
    let mut vm = VM::new();

    let constant = vm.chunk.add_constant(Value::Float(1.2));
    vm.chunk.append(ByteCode::Constant(constant), 123);

    let constant = vm.chunk.add_constant(Value::Float(3.4));
    vm.chunk.append(ByteCode::Constant(constant), 123);

    vm.chunk.append(ByteCode::Add, 123);

    let constant = vm.chunk.add_constant(Value::Float(5.6));
    vm.chunk.append(ByteCode::Constant(constant), 123);

    vm.chunk.append(ByteCode::Div, 123);
    vm.chunk.append(ByteCode::Negate, 123);

    vm.chunk.append(ByteCode::Return, 123);
    //vm.chunk.disassemble("test chunk");
    vm.interpret();
    
    /*
    let mut input = String::new();
    loop {
        let n = io::stdin().read_line(&mut input)?;
        if n == 0 {
            break;
        } else {
            interpret(input);
        }
    }
    */
    Ok(())
}
