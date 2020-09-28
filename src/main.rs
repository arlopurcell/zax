use std::fs::File;
use std::io;
use std::io::prelude::*;

mod ast;
mod chunk;
mod code_gen;
mod common;
mod compiler;
mod heap;
mod lexer;
mod type_check;
mod vm;

use crate::common::{InterpretError, InterpretResult};
use crate::vm::VM;

fn main() -> InterpretResult {
    let vm = VM::new();
    if let Some(file_name) = std::env::args().nth(1) {
        run_file(&file_name, vm)
    } else {
        repl(vm)
    }
}

fn repl(mut vm: VM) -> InterpretResult {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut line = String::new();
        let bytes = io::stdin().read_line(&mut line).unwrap();
        if bytes == 0 {
            println!();
            break;
        } else {
            if let Err(e) = vm.interpret(&line) {
                eprintln!("{:?}", e);
            }
        }
    }
    Ok(())
}

fn run_file(file_name: &str, mut vm: VM) -> InterpretResult {
    let mut file = File::open(file_name).map_err(|_| InterpretError::File)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .map_err(|_| InterpretError::File)?;
    vm.interpret(&contents)
}
