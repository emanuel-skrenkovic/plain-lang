pub mod vm;
pub mod scan;
pub mod block;
pub mod compiler;

use std::fs;
use std::env;
use std::io::{stdin, stdout, Write};

use crate::vm::VM;
use crate::compiler::Compiler;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() <= 1 {
        let mut input = String::new();

        loop {
            print!("sage> ");
            stdout().flush().unwrap();
            stdin().read_line(&mut input).unwrap();

            let mut compiler = Compiler::new(input.clone());
            let program = compiler.compile();
            let program = program.unwrap();

            let mut vm = VM::new(program);
            vm.interpret();

            input.clear();
        }
    } else {
        let source = fs::read_to_string(&args[1]).unwrap();

        let mut compiler = Compiler::new(source);
        let program = compiler.compile();
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    };
}
