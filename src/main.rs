pub mod vm;
pub mod scan;
pub mod block;
pub mod compiler;

use std::io::{self, BufRead};
use std::fs;
use std::env;
use std::rc::Rc;
use std::cell::RefCell;

use crate::vm::VM;
use crate::block::Block;
use crate::compiler::Compiler;

fn main() {
    let mut args: Vec<String> = env::args().collect();

    if args.len() <= 1 {
        loop {
            let stdin = io::stdin();
            for line in stdin.lock().lines() {

                let block = Rc::new(RefCell::new(Block::new(256)));

                let mut compiler = Compiler::new(line.unwrap(), block.clone());
                compiler.compile();

                let mut vm = VM::new(block.clone());
                vm.interpret()
            }
        }
    } else {
        let source = fs::read_to_string(&args[1]).unwrap();

        let block = Rc::new(RefCell::new(Block::new(256)));

        let mut compiler = Compiler::new(source, block.clone());
        compiler.compile();

        let mut vm = VM::new(block.clone());
        vm.interpret()
    };
}
