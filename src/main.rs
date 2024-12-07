use std::io::{stderr, Write};

use plang::ast;
use plang::scan;
use plang::parse;
use plang::types;
use plang::source;
use plang::context;
use plang::compiler_llvm;
use plang::semantic_analysis;


fn main()
{
    // let args: Vec<String> = env::args().collect();
    // let source = fs::read_to_string(&args[1]).unwrap();

    let source = include_str!("../test.sg").to_string();
    let mut scanner = scan::Scanner::new(source.clone());

    let s = source::Source { source: source.clone() }; 

    let now = std::time::Instant::now();

    let now_scan       = std::time::Instant::now();
    let tokens         = scanner.scan_tokens();
    let after_scanning = now_scan.elapsed();

    let (context, mut program, global_nodes, after_parsing) = {
        let context = context::Context::new(s, tokens);

        let parser = parse::Parser::new(context);

        let now_parse          = std::time::Instant::now();
        let result = parser.parse();
        let after_parsing = now_parse.elapsed();
        
        let (program, global_nodes, context) = match result {
            Ok((program, global_nodes, context)) => (program, global_nodes, context),
            Err(errors) => {
                for err in errors {
                    writeln!(stderr(), "{err}").expect("Failed to write to stderr.");
                }
                stderr().flush().unwrap();
                std::process::exit(1);
            }
        };

        (context, program, global_nodes, after_parsing)
    };
    
    let (mut context, program, after_transformation) = {
        let now_transformation   = std::time::Instant::now();
        ast::GlobalsHoistingTransformer::transform(&context, &mut program);
        let after_transformation = now_transformation.elapsed();
        (context, program, after_transformation)
    };

    let (program, type_info, types, after_type_analysis) = {
        let now_type_analysis    = std::time::Instant::now();
        let (type_info, types) = types::Typer::new(&mut context).infer_types(&program, &global_nodes);
        let after_type_analysis  = now_type_analysis.elapsed();
        (program, type_info, types, after_type_analysis)
    };

    if context.error {
        for err in context.errors {
            writeln!(stderr(), "{err}").expect("Failed to write to stderr.");
        }
        stderr().flush().unwrap();
        std::process::exit(1);
    }

    let (program, symbol_table, after_semantic_analysis) = {
        let now_semantic_analysis   = std::time::Instant::now();
        let (program, symbol_table) = semantic_analysis::analyse(&context, program).unwrap();
        let after_semantic_analysis = now_semantic_analysis.elapsed();
        (program, symbol_table, after_semantic_analysis)
    };

    // println!("{program:#?}");

    let (after_llvm, after_module_output) = unsafe {
        let now_llvm     = std::time::Instant::now();

        let module = compiler_llvm::compile
        (
            &compiler_llvm::QueryData {
                source: context,
                nodes: program,
                global_nodes,
                symbol_table,
                type_info,
                types,
            }
        );

        let llvm_elapsed = now_llvm.elapsed();

        let now_module_output = std::time::Instant::now();
        compiler_llvm::output_module_bitcode(module).expect("Failed to output LLVM bitcode.");
        let after_module_output = now_module_output.elapsed();

        (llvm_elapsed, after_module_output)
    };

    let now_compile_bytecode = std::time::Instant::now();
    
    // println!("{types:#?}");

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
     LLVM module output: {after_module_output:?}.
     Compiling bytecode: {after_compiling_bytecode:?}

     Total time: {total:?}.
");
    std::process::exit(0);
}
