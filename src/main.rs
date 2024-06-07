use std::io::{stderr, Write};

use plang::compiler_llvm;
use plang::compiler;
use plang::scan;
use plang::types;
use plang::semantic_analysis;


fn main()
{
    // let args: Vec<String> = env::args().collect();
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

    let now_type_analysis = std::time::Instant::now();
    let (typed_program, type_info) = types::infer_types(&program);
    let after_type_analysis = now_type_analysis.elapsed();

    println!("{:#?}", typed_program);

    let now_semantic_analysis = std::time::Instant::now();
    let symbol_table = semantic_analysis::analyse(&typed_program, &type_info).unwrap();
    let after_semantic_analysis = now_semantic_analysis.elapsed();

    let after_llvm = unsafe {
        let now_llvm = std::time::Instant::now();
        let mut ctx = compiler_llvm::Context::new(typed_program, symbol_table);
        let module = compiler_llvm::compile(&mut ctx);
        let llvm_elapsed = now_llvm.elapsed();

        compiler_llvm::output_module_bitcode(module).expect("Failed to output LLVM bitcode.");

        llvm_elapsed
    };

    let now_compile_bytecode = std::time::Instant::now();
    let _ = std::process::Command::new("llc")
        .args(["-filetype=obj", "-O0", "-o", "bin/a.o", "bin/a.bc"])
        .spawn()
        .unwrap()
        .wait_with_output()
        .expect("Failed to compile LLVM bytecode");
    let after_compiling_bytecode = now_compile_bytecode.elapsed();

    let now_linking = std::time::Instant::now();
    let _ = std::process::Command::new("clang")
        .args(["-o", "bin/a", "bin/a.o"])
        .spawn()
        .unwrap()
        .wait_with_output()
        .expect("Failed to link output.");
    let after_linking = now_linking.elapsed();

    let total = now.elapsed();
    println!
    (
"
     Tokenization: {:?}.
     Parsing: {:?}.
     Type analysis: {:?}
     Semantic analysis: {:?}
     LLVM backend: {:?}.
     Compiling bytecode: {:?}
     Linking: {:?}

     Total time: {:?}.
",
        after_scanning,
        after_compiling,
        after_type_analysis,
        after_semantic_analysis,
        after_llvm,
        after_compiling_bytecode,
        after_linking,
        total,
    );

    std::process::exit(0);

//     #[allow(unreachable_code)]
//     {
//         // let args: Vec<String> = env::args().collect();
//         // let source = fs::read_to_string(&args[1]).unwrap();
//
//         let source = include_str!("../test.sg").to_string();
//         let mut scanner = scan::Scanner::new(source.clone());
//         let compiler = compiler::Compiler::new(source.clone());
//
//         let now = std::time::Instant::now();
//
//         let now_scan = std::time::Instant::now();
//         let tokens = scanner.scan_tokens();
//         let after_scanning = now_scan.elapsed();
//
//         let now_compile = std::time::Instant::now();
//         let program = compiler.compile(tokens);
//         let after_compiling = now_compile.elapsed();
//
//         let program = match program {
//             Ok(program) => program,
//             Err(errors) => {
//                 for err in errors {
//                     writeln!(stderr(), "{}", err).expect("Failed to write to stderr.");
//                 }
//                 stderr().flush().unwrap();
//                 std::process::exit(1);
//             }
//         };
//
//         let after_llvm = unsafe {
//             let now_llvm = std::time::Instant::now();
//             let mut ctx = compiler_llvm::Context::new(program);
//             let module = compiler_llvm::compile(&mut ctx);
//             let llvm_elapsed = now_llvm.elapsed();
//
//             compiler_llvm::output_module_bitcode(module).expect("Failed to output LLVM bitcode.");
//
//             llvm_elapsed
//         };
//
//         let now_compile_bytecode = std::time::Instant::now();
//         let _ = std::process::Command::new("llc")
//             .args(["-filetype=obj", "-O0", "-o", "bin/a.o", "bin/a.bc"])
//             .spawn()
//             .unwrap()
//             .wait_with_output()
//             .expect("Failed to compile LLVM bytecode");
//         let after_compiling_bytecode = now_compile_bytecode.elapsed();
//
//         let now_linking = std::time::Instant::now();
//         let _ = std::process::Command::new("clang")
//             .args(["-o", "bin/a", "bin/a.o"])
//             .spawn()
//             .unwrap()
//             .wait_with_output()
//             .expect("Failed to link output.");
//         let after_linking = now_linking.elapsed();
//
//         let total = now.elapsed();
//         println!
//         (
//             "
//      Tokenization: {:?}.
//      Parsing: {:?}.
//      LLVM backend: {:?}.
//      Compiling bytecode: {:?}
//      Linking: {:?}
//
//      Total time: {:?}.
// ",
//             after_scanning,
//             after_compiling,
//             after_llvm,
//             after_compiling_bytecode,
//             after_linking,
//             total,
//         );
//
//         std::process::exit(0);

        // TODO: Below is the regular compiler, this split is just for me to test the llvm implementation.
        // TODO: Remove above code.
        // {
        //     if args.len() <= 1 {
        //         let mut input = String::new();
        //
        //         loop {
        //             print!("plang> ");
        //             stdout().flush().unwrap();
        //             stdin().read_line(&mut input).unwrap();
        //
        //             let mut scanner = scan::Scanner::new(input.clone());
        //             let tokens = scanner.scan_tokens();
        //
        //             let compiler = compiler::Compiler::new(input.clone());
        //             let program = compiler.compile(tokens);
        //
        //             // TODO: compiler consumes itself currently, have to actually return the erro
        //             // instead of reading the state of the compiler after compilation.
        //             // Not even sure if it should consume itself as running multiple compilation stuff
        //             // in parallel might be an option.
        //
        //             // if !compiler.errors.is_empty() {
        //             //     for err in compiler.errors {
        //             //         writeln!(stderr(), "{}", err).expect("Failed to write to stderr.");
        //             //     }
        //             //     stderr().flush().unwrap();
        //             //     std::process::exit(1);
        //             // }
        //             let program = program.unwrap();
        //
        //             let mut vm = vm::VM::new(program);
        //             vm.interpret();
        //         }
        //     } else {
        //         let source = fs::read_to_string(&args[1]).unwrap();
        //
        //         let mut scanner = scan::Scanner::new(source.clone());
        //         let compiler = compiler::Compiler::new(source.clone());
        //
        //         let tokens = scanner.scan_tokens();
        //         let program = compiler.compile(tokens);
        //         let program = program.unwrap();
        //
        //         let mut vm = vm::VM::new(program);
        //         vm.interpret();
        //     };
        // }
    // }
}
