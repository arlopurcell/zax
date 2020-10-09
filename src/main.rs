use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::process;

mod ast;
mod chunk;
mod code_gen;
mod common;
mod compiler;
mod gc;
mod heap;
mod lexer;
mod object;
mod parser;
mod type_check;
mod vm;

use crate::common::{InterpretError, InterpretResult};
use crate::vm::VM;

fn main() -> InterpretResult {
    let vm = VM::new();
    let result = if let Some(file_name) = std::env::args().nth(1) {
        run_file(&file_name, vm)
    } else {
        repl(vm)
    };

    match result {
        Err(InterpretError::Compile) => process::exit(1),
        Err(InterpretError::Runtime) => process::exit(2),
        Err(InterpretError::File) => process::exit(3),
        Ok(_) => (),
    };
    result
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
