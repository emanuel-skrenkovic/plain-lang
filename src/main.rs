pub mod scan;
pub mod compiler;
pub mod block;
pub mod vm;

use std::fs;
use std::env;
use std::rc::Rc;
use std::cell::RefCell;

use crate::vm::VM;
use crate::block::Block;
use crate::compiler::Compiler;

fn main() {
    // let mut args: Vec<String> = env::args().collect();
    let args: Vec<String> = vec!["".to_owned(), "tests/add.sg".to_owned()];

    for arg in &args {
        println!("{}", arg);
    }

    let source = fs::read_to_string(&args[1]).unwrap();

    let block = Rc::new(RefCell::new(Block::new(256)));

    let mut compiler = Compiler::new(source, block.clone());
    compiler.compile();

    let mut vm = VM::new(block.clone());
    vm.interpret()
}
