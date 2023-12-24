use std::env;
use std::fs;
use std::io::{stdin, stdout, stderr, Write};

use sage::compiler::Compiler;
use sage::scan::Scanner;
use sage::vm::VM;
use sage::llvm;

fn main() {
    let args: Vec<String> = env::args().collect();

    unsafe { llvm::llvm_test(); }

    if args.len() <= 1 {
        let mut input = String::new();

        loop {
            print!("sage> ");
            stdout().flush().unwrap();
            stdin().read_line(&mut input).unwrap();

            let mut scanner = Scanner::new(input.clone());
            let tokens = scanner.scan_tokens();

            let mut compiler = Compiler::new(input.clone());
            let program = compiler.compile(tokens);
            if !compiler.errors.is_empty() {
                for err in compiler.errors {
                    writeln!(stderr(), "{}", err).expect("Failed to write to stderr.");
                }
                stderr().flush().unwrap();
                std::process::exit(1);
            }
            let program = program.unwrap();

            let mut vm = VM::new(program);
            vm.interpret();
        }
    } else {
        let source = fs::read_to_string(&args[1]).unwrap();

        let mut scanner = Scanner::new(source.clone());
        let mut compiler = Compiler::new(source.clone());

        let tokens = scanner.scan_tokens();
        let program = compiler.compile(tokens);
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    };
}
