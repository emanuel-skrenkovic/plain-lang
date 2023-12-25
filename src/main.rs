use std::{env, fs};
use std::io::{stdin, stdout, stderr, Write};

use sage::compiler;
use sage::compiler_llvm;
use sage::scan;
use sage::vm;

fn main() {
    let args: Vec<String> = env::args().collect();

    let source = fs::read_to_string(&args[1]).unwrap();

    let mut scanner = scan::Scanner::new(source.clone());
    let tokens = scanner.scan_tokens();

    let mut compiler = compiler::Compiler::new(source.clone());
    let program = compiler.compile(tokens);
    if !compiler.errors.is_empty() {
        for err in compiler.errors {
            writeln!(stderr(), "{}", err).expect("Failed to write to stderr.");
        }
        stderr().flush().unwrap();
        std::process::exit(1);
    }
    let program = program.unwrap();

    let mut llvm = compiler_llvm::Backend::new();
    llvm.compile(program);

    std::process::exit(1);

    // TODO: Below is the regular compiler, this split is just for me to test the llvm implementation.
    // TODO: Remove above code.
    #[allow(unreachable_code)]
    {
        if args.len() <= 1 {
            let mut input = String::new();

            loop {
                print!("sage> ");
                stdout().flush().unwrap();
                stdin().read_line(&mut input).unwrap();

                let mut scanner = scan::Scanner::new(input.clone());
                let tokens = scanner.scan_tokens();

                let mut compiler = compiler::Compiler::new(input.clone());
                let program = compiler.compile(tokens);
                if !compiler.errors.is_empty() {
                    for err in compiler.errors {
                        writeln!(stderr(), "{}", err).expect("Failed to write to stderr.");
                    }
                    stderr().flush().unwrap();
                    std::process::exit(1);
                }
                let program = program.unwrap();

                let mut vm = vm::VM::new(program);
                vm.interpret();
            }
        } else {
            let source = fs::read_to_string(&args[1]).unwrap();

            let mut scanner = scan::Scanner::new(source.clone());
            let mut compiler = compiler::Compiler::new(source.clone());

            let tokens = scanner.scan_tokens();
            let program = compiler.compile(tokens);
            let program = program.unwrap();

            let mut vm = vm::VM::new(program);
            vm.interpret();
        };
    }
}
