use std::io::{stderr, Write};

use plang::ast;
use plang::ast::Transformer;
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

    let now_transformation = std::time::Instant::now();
    let program = ast::GlobalsHoistingTransformer::transform(program);
    let after_transformation = now_transformation.elapsed();

    let now_type_analysis = std::time::Instant::now();
    let (program, type_info) = types::infer_types(&program);
    let after_type_analysis = now_type_analysis.elapsed();

    println!("{:#?}", program);

    let now_semantic_analysis = std::time::Instant::now();
    let symbol_table = semantic_analysis::analyse(&program).unwrap();
    let after_semantic_analysis = now_semantic_analysis.elapsed();

    let after_llvm = unsafe {
        let now_llvm = std::time::Instant::now();
        let mut ctx = compiler_llvm::Context::new(program, symbol_table, type_info);
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
     Transformation: {:?}
     Type analysis: {:?}
     Semantic analysis: {:?}
     LLVM backend: {:?}.
     Compiling bytecode: {:?}
     Linking: {:?}

     Total time: {:?}.
",
        after_scanning,
        after_compiling,
        after_transformation,
        after_type_analysis,
        after_semantic_analysis,
        after_llvm,
        after_compiling_bytecode,
        after_linking,
        total,
    );

    std::process::exit(0);
}
