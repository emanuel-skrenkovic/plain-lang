use std::io::{stderr, Write};

use plang::ast;
use plang::ast::Transformer;
use plang::compiler_llvm;
use plang::parse;
use plang::scan;
use plang::types;
use plang::error;
use plang::source;
use plang::semantic_analysis;


fn main()
{
    // let args: Vec<String> = env::args().collect();
    // let source = fs::read_to_string(&args[1]).unwrap();

    let source = include_str!("../test.sg").to_string();
    let mut scanner = scan::Scanner::new(source.clone());

    let s = source::Source { source: source.clone() }; 
    let mut reporter = error::Reporter::new(&source);

    let now = std::time::Instant::now();

    let now_scan       = std::time::Instant::now();
    let tokens         = scanner.scan_tokens();
    let after_scanning = now_scan.elapsed();

    let now_parse     = std::time::Instant::now();
    let program       = parse::Parser::new(reporter.clone(), tokens).parse();
    let after_parsing = now_parse.elapsed();

    if reporter.error {
        for err in reporter.errors {
            writeln!(stderr(), "{err}").expect("Failed to write to stderr.");
        }
        stderr().flush().unwrap();
        std::process::exit(1);
    }

    let program = match program {
        Ok(program) => program,
        Err(errors) => {
            for err in errors {
                writeln!(stderr(), "{err}").expect("Failed to write to stderr.");
            }
            stderr().flush().unwrap();
            std::process::exit(1);
        }
    };

    let (s, program, after_transformation) = {
        let now_transformation   = std::time::Instant::now();
        let program              = ast::GlobalsHoistingTransformer::transform(&s, program);
        let after_transformation = now_transformation.elapsed();
        (s, program, after_transformation)
    };

    let (program, type_info, after_type_analysis) = {
        let now_type_analysis    = std::time::Instant::now();
        let (program, type_info) = types::Typer::new(&s, &mut reporter).infer_types(program);
        let after_type_analysis  = now_type_analysis.elapsed();
        (program, type_info, after_type_analysis)
    };
    
    println!("{program:#?}");

    if reporter.error {
        for err in reporter.errors {
            writeln!(stderr(), "{err}").expect("Failed to write to stderr.");
        }
        stderr().flush().unwrap();
        std::process::exit(1);
    }

    let (program, symbol_table, after_semantic_analysis) = {
        let now_semantic_analysis   = std::time::Instant::now();
        let (program, symbol_table) = semantic_analysis::analyse(&s, program).unwrap();
        let after_semantic_analysis = now_semantic_analysis.elapsed();
        (program, symbol_table, after_semantic_analysis)
    };

    let after_llvm = unsafe {
        let now_llvm = std::time::Instant::now();
        let mut ctx = compiler_llvm::Context::new(program, symbol_table, type_info);
        let module = compiler_llvm::compile(&s, &mut ctx);
        let llvm_elapsed = now_llvm.elapsed();

        compiler_llvm::output_module_bitcode(module).expect("Failed to output LLVM bitcode.");

        llvm_elapsed
    };

    let now_compile_bytecode = std::time::Instant::now();
    let _ = std::process::Command::new("clang")
        .args(["-o", "bin/a", "bin/a.bc", "-O0"])
        .spawn()
        .unwrap()
        .wait_with_output()
        .expect("Failed to compile LLVM bytecode");
    let after_compiling_bytecode = now_compile_bytecode.elapsed();

    let total = now.elapsed();
    println!
    (
"
     Tokenization: {after_scanning:?}.
     Parsing: {after_parsing:?}.
     Transformation: {after_transformation:?}
     Type analysis: {after_type_analysis:?}
     Semantic analysis: {after_semantic_analysis:?}
     LLVM backend: {after_llvm:?}.
     Compiling bytecode: {after_compiling_bytecode:?}

     Total time: {total:?}.
");

    std::process::exit(0);
}
