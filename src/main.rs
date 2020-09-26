use std::io;
use std::fs::File;
use std::io::prelude::*;

mod chunk;
//mod common;
mod compiler;
mod lexer;
mod value;
mod vm;

use crate::vm::{VM, InterpretResult, InterpretError};

fn main() -> InterpretResult {
    let vm = VM::new();
    if let Some(file_name) = std::env::args().nth(1) {
        run_file(&file_name, vm)
    } else {
        repl(vm)
    }
}

fn repl(mut vm: VM) -> InterpretResult {
    let mut line = String::new();
    loop {
        print!("> ");
        let bytes = io::stdin().read_line(&mut line).unwrap();
        if bytes == 0 {
            println!();
            break;
        } else {
            vm.interpret_str(&line);
        }
    }
    Ok(())
}

fn run_file(file_name: &str, mut vm: VM) -> InterpretResult {
    let mut file = File::open(file_name).map_err(|_| InterpretError::File)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).map_err(|_| InterpretError::File)?;
    vm.interpret_str(&contents)
}
