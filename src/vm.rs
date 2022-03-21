use std::rc::Rc;
use std::cell::RefCell;

use crate::block::{Block, Value};

pub struct VM {
    block: Rc<RefCell<Block>>,
    ip: u8,
    i: usize
}

impl VM {
    pub fn new(block: Rc<RefCell<Block>>) -> VM {
        let ip = (*block).borrow().code[0];
        VM { block, ip, i: 0 }
    }

    pub fn interpret(&mut self) {
        loop {
            self.ip = (*self.block).borrow().code[self.i];
            self.disassemble_instruction(self.ip);

            match self.ip {
                6 => { // Constant
                    self.i += 2;
                },
                _ => { self.i += 1; }
            }

            if self.i >= (*self.block).borrow().code.len() {
                break;
            }
        }
    }

    fn pop() -> Value {
        todo!()
    }

    fn disassemble_instruction(&self, instruction: u8) {
        match instruction {
            0 => println!("POP"),
            1 => println!("TRUE"),
            2 => println!("FALSE"),
            3 => println!("NOT"),
            4 => println!("ADD"),
            5 => println!("SUBTRACT"),
            6 => println!("CONSTANT "),
            _ => { }
        }
    }
}
