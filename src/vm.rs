use std::rc::Rc;
use std::cell::RefCell;
use std::mem::discriminant;
use std::collections::VecDeque;
use std::collections::HashMap;

use crate::block::{Block, Op, Value};

pub struct VM {
    block: Rc<RefCell<Block>>,
    stack: VecDeque<Value>,
    ip: u8,
    i: usize,

    globals: HashMap<u8, Value>
}

impl VM {
    pub fn new(block: Rc<RefCell<Block>>) -> VM {
        let ip = (*block).borrow().code[0];
        VM {
            block,
            stack: (0..1024).map(|_| Value::Unit).collect::<VecDeque<Value>>(),
            ip,
            i: 0,
            globals: HashMap::new()
        }
    }

    pub fn interpret(&mut self) {
        loop {
            self.ip = self.read_byte();
            self.disassemble_instruction(self.ip);

            match self.ip.try_into().unwrap() {
                Op::Pop => {
                    let value = self.pop();
                    println!("    {:?}\n", value);
                }
                Op::Add => {
                    let (a, b) = self.binary_op();

                    print!("A: "); println!("{:?}", a);
                    print!("B: "); println!("{:?}", b);

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

                    let second = match a {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    let first = match b {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    self.push(Value::Number { val: first / second });
                }
                Op::Equal => {
                    let (a, b) = self.binary_op();

                    let second = match a {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    let first = match b {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    self.push(Value::Bool { val: first == second });
                }
                Op::Less => {
                    let (a, b) = self.binary_op();

                    let second = match a {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    let first = match b {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    self.push(Value::Bool { val: first < second });
                }
                Op::Greater => {
                    let (a, b) = self.binary_op();

                    let second = match a {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    let first = match b {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    self.push(Value::Bool { val: first > second });
                }
                Op::GreaterEqual => {
                    let (a, b) = self.binary_op();

                    let second = match a {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    let first = match b {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    self.push(Value::Bool { val: first >= second });
                }
                Op::LessEqual => {
                    let (a, b) = self.binary_op();

                    let second = match a {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    let first = match b {
                        Value::Number { val } => val,
                        _ => { panic!("TODO: Not supported") }
                    };

                    self.push(Value::Bool { val: first <= second });
                }
                Op::Not => {
                    let value = self.pop();

                    let boolean = match value {
                        Value::Bool { val } => val,
                        _ => panic!("TODO: Not supported")
                    };

                    self.push(Value::Bool { val: !boolean });
                }
                Op::Constant => { // Constant
                    let value = self.read_constant();
                    self.push(value);
                },
                Op::GetVariable => {
                    let index = self.read_byte();
                    let value = *self.globals.get(&index).unwrap();
                    self.push(value);
                }
                Op::SetVariable => {
                    let index = self.read_byte();
                    let value = self.pop();

                    self.set_or_add_global(index, value);
                }
                _ => { self.i += 1; }
            }

            if self.i >= (*self.block).borrow().code.len() {
                break;
            }
        }
    }

    fn pop(&mut self) -> Value {
        self.stack.pop_front().expect("Could not pop empty stack.")
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

    fn peek(&self, index: usize) -> Value { // TODO: Option<Value> ?
        (*self.block).borrow().values[index]
    }

    fn set_or_add_global(&mut self, index: u8, value: Value) {
        if self.globals.contains_key(&index) {
            *self.globals.get_mut(&index).unwrap() = value;
        } else {
            self.globals.insert(index, value);
        }
    }

    // TODO: how to handle differring types
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

        if let Ok(i) = instruction.try_into() {
            match i {
                Op::Pop         => print!("POP"),
                Op::True        => println!("TRUE"),
                Op::False       => println!("FALSE"),
                Op::Not         => println!("NOT"),
                Op::Add         => println!("ADD"),
                Op::Subtract    => println!("SUBTRACT"),
                Op::Multiply    => println!("MULTIPLY"),
                Op::Divide      => println!("DIVIDE"),
                Op::Constant    => {
                    let index = (*self.block).borrow().code[self.i] as usize;
                    let value = (*self.block).borrow().values[index];

                    print!("CONSTANT: {:?}", value);
                },
                Op::Equal       => println!("EQUAL"),
                Op::Less        => println!("LESS"),
                Op::Greater     => println!("GREATER"),
                Op::GetVariable => println!("GET_VARIABLE"),
                Op::SetVariable => {
                    let index = (*self.block).borrow().code[self.i] as usize;
                    let value = self.peek(index);

                    println!("SET_VARIABLE: {:?}", value);
                },
                _ => { }
            }
        }
    }
}
