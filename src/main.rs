use std::env;
use std::fs;
use std::io::{stdin, stdout, Write};

use sage::compiler::Compiler;
use sage::scan::Scanner;
use sage::vm::VM;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() <= 1 {
        let mut input = String::new();

        loop {
            print!("sage> ");
            stdout().flush().unwrap();
            stdin().read_line(&mut input).unwrap();

            let mut scanner = Scanner::new(input.clone());
            let tokens = scanner.scan_tokens();

            let mut compiler = Compiler::new();
            let program = compiler.compile(tokens);
            let program = program.unwrap();

            let mut vm = VM::new(program);
            vm.interpret();
        }
    } else {
        let source = fs::read_to_string(&args[1]).unwrap();

        let mut scanner = Scanner::new(source);
        let mut compiler = Compiler::new();

        let tokens = scanner.scan_tokens();
        let program = compiler.compile(tokens);
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    };
}
