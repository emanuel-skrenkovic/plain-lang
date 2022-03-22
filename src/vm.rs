use std::rc::Rc;
use std::cell::RefCell;
use std::mem::discriminant;
use std::collections::VecDeque;

use crate::block::{Block, Op, Value};

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

            match self.ip.try_into().unwrap() {
                Op::Pop => {
                    println!("    {:?}\n", self.pop());
                    self.i += 1;
                }
                Op::Add => {
                    let (a, b) = self.binary_op();

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
                Op::Subtract => {
                    let (a, b) = self.binary_op();

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
                Op::Multiply => {
                    let (a, b) = self.binary_op();

                    let first = match a {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    let second = match b {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    self.push(Value::Number { val: first * second });
                }
                Op::Divide => {
                    let (a, b) = self.binary_op();

                    let first = match a {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    let second = match b {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    self.push(Value::Number { val: first / second });
                }
                Op::Constant => { // Constant
                    let value = self.read_constant();
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

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte() as usize;
        (*self.block).borrow().values[index]
    }

    fn binary_op(&mut self) -> (Value, Value) {
        let a = self.pop();
        let b = self.pop();

        if discriminant(&a) != discriminant(&b) {
            panic!(
                "Binary operation with two different types is not supported."
            );
        }

        (a, b)
    }

    fn disassemble_instruction(&self, instruction: u8) {
        print!("OP ");
        match instruction {
            0 => print!("POP"),
            1 => println!("TRUE"),
            2 => println!("FALSE"),
            3 => println!("NOT"),
            4 => println!("ADD"),
            5 => println!("SUBTRACT"),
            6 => print!("CONSTANT"),
            _ => { }
        }
    }
}
