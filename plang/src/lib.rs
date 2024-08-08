#![allow(clippy::must_use_candidate)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]

pub mod ast;
pub mod scan;
pub mod scope;
pub mod types;
pub mod parse;
pub mod error;
pub mod source;
pub mod context;
pub mod compiler_llvm;
pub mod semantic_analysis;
mod parse_test;
