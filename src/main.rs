use std::{env, fs};
use std::io::{stdin, stderr, stdout, Write};

use sage::compiler;
use sage::compiler_llvm;
use sage::scan;
use sage::vm;

fn main() {
    #[allow(unused)]
    let args: Vec<String> = env::args().collect();
    // let source = fs::read_to_string(&args[1]).unwrap();

    let source = include_str!("../test.sg").to_string();
    let mut scanner = scan::Scanner::new(source.clone());
    let compiler = compiler::Compiler::new(source.clone());

    let now = std::time::Instant::now();

    let now_scan = std::time::Instant::now();
    let tokens = scanner.scan_tokens();
    let after_scanning = now_scan.elapsed();

    let now_compile = std::time::Instant::now();
    let program = compiler.compile(tokens);
    let after_compiling = now_compile.elapsed();

    let program = match program {
        Ok(program) => program,
        Err(errors) => {
            for err in errors {
                writeln!(stderr(), "{}", err).expect("Failed to write to stderr.");
            }
            stderr().flush().unwrap();
            std::process::exit(1);
        }
    };

    let after_llvm = unsafe {
        let now_llvm = std::time::Instant::now();
        let mut ctx = compiler_llvm::Context::new(program);
        compiler_llvm::ProgramCompiler::compile(&mut ctx);
        now_llvm.elapsed()
    };

    let total = now.elapsed();
    println!
    (
        "Total time {:} seconds.
Tokenization took {:} seconds.
Compilation took: {:} seconds.
Outputting LLVM bytecode took: {:} seconds.",
        total.as_secs_f32(),
        after_scanning.as_secs_f32(),
        after_compiling.as_secs_f32(),
        after_llvm.as_secs_f32(),
    );

    std::process::exit(0);


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

                let compiler = compiler::Compiler::new(input.clone());
                let program = compiler.compile(tokens);

                // TODO: compiler consumes itself currently, have to actually return the erro
                // instead of reading the state of the compiler after compilation.
                // Not even sure if it should consume itself as running multiple compilation stuff
                // in parallel might be an option.

                // if !compiler.errors.is_empty() {
                //     for err in compiler.errors {
                //         writeln!(stderr(), "{}", err).expect("Failed to write to stderr.");
                //     }
                //     stderr().flush().unwrap();
                //     std::process::exit(1);
                // }
                let program = program.unwrap();

                let mut vm = vm::VM::new(program);
                vm.interpret();
            }
        } else {
            let source = fs::read_to_string(&args[1]).unwrap();

            let mut scanner = scan::Scanner::new(source.clone());
            let compiler    = compiler::Compiler::new(source.clone());

            let tokens = scanner.scan_tokens();
            let program = compiler.compile(tokens);
            let program = program.unwrap();

            let mut vm = vm::VM::new(program);
            vm.interpret();
        };
    }
}
