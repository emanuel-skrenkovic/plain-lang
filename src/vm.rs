use std::rc::Rc;
use std::cell::RefCell;
use std::mem::discriminant;
use std::collections::VecDeque;

use crate::block::{Block, Value};

pub struct VM {
    block: Rc<RefCell<Block>>,
    stack: VecDeque<Value>,
    ip: u8,
    i: usize
}

impl VM {
    pub fn new(block: Rc<RefCell<Block>>) -> VM {
        let ip = (*block).borrow().code[0];
        VM {
            block,
            stack: VecDeque::new(),
            ip,
            i: 0
        }
    }

    pub fn interpret(&mut self) {
        loop {
            // self.ip = (*self.block).borrow().code[self.i];
            self.ip = self.read_byte();
            self.disassemble_instruction(self.ip);

            match self.ip {
                0 => {
                    println!("    {:?}\n", self.pop());
                    self.i += 1;
                }
                4 => {
                    let a = self.pop();
                    let b = self.pop();

                    if discriminant(&a) != discriminant(&b) {
                        panic!("Cannot add two different types.");
                    }

                    let first = match a {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    let second = match b {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    self.push(Value::Number { val: first + second });
                }
                5 => {
                    let a = self.pop();
                    let b = self.pop();

                    if discriminant(&a) != discriminant(&b) {
                        panic!("Cannot add two different types.");
                    }

                    let second = match a {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    let first = match b {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    self.push(Value::Number { val: first - second });
                }
                6 => { // Constant
                    let index : usize= self.read_byte() as usize;
                    let value = (*self.block).borrow().values[index];
                    print!("    {:?}\n", value);

                    self.push(value);
                },
                _ => { self.i += 1; }
            }

            if self.i >= (*self.block).borrow().code.len() {
                break;
            }
        }
    }

    fn pop(&mut self) -> Value {
        self.stack.pop_front().unwrap()
    }

    fn push(&mut self, value: Value) {
        self.stack.push_front(value);
    }

    fn read_byte(&mut self) -> u8 {
        self.i += 1;
        self.ip = (*self.block).borrow().code[self.i - 1];

        self.ip
    }

    /*
    fn read_constant(&mut self) -> Value {
        let index = self.pop();

        if let Value::Number { val } = index {
            // let value = (*self.block).borrow().values[val as usize];

        }

        panic!("TODO: CANNOT READ CONSTANT")
    }
    */

    fn disassemble_instruction(&self, instruction: u8) {
        match instruction {
            0 => print!("POP"),
            1 => print!("TRUE"),
            2 => print!("FALSE"),
            3 => print!("NOT"),
            4 => print!("ADD\n"),
            5 => print!("SUBTRACT\n"),
            6 => print!("CONSTANT "),
            _ => { }
        }
    }
}
