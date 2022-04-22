use std::rc::Rc;
use std::cell::RefCell;
use std::mem::discriminant;
use std::collections::VecDeque;

use crate::block::{Block, Op, Value};

const STACK_SIZE: usize = 1024;

fn init_stack() -> VecDeque<Value> {
    (0..STACK_SIZE).map(|_| Value::Unit).collect::<VecDeque<Value>>()
}

#[derive(Copy, Clone)]
struct Upvalue {
    closed: Value
}

struct CallFrame {
    ip: u8,
    i: usize
}

impl CallFrame {
    fn get_value(&self, stack: &VecDeque<Value>, index: usize) -> Value {
        stack[index + self.i]
    }

    fn set_value(&self, stack: &mut VecDeque<Value>, index: usize, value: Value) {
        stack[index + self.i] = value;
    }

    fn get_upvalue(&self, stack: &VecDeque<Value>, index: usize) -> Upvalue {
        Upvalue { closed: stack[index] }
    }

    fn set_upvalue(&mut self, stack: &mut VecDeque<Value>, index: usize, upvalue: Upvalue) {
        stack[index] = upvalue.closed;
    }
}

pub struct VM {
    block: Rc<RefCell<Block>>,

    stack: VecDeque<Value>,
    stack_top: usize,

    ip: u8,
    i: usize,
}

impl VM {
    pub fn new(block: Rc<RefCell<Block>>) -> VM {
        let ip = (*block).borrow().code[0];
        VM {
            block,

            stack: init_stack(),
            stack_top: 0,

            ip,
            i: 0
        }
    }

    pub fn interpret(&mut self) {
        let mut frames = VecDeque::new();
        frames.push_front(
            CallFrame {
                ip: 0,
                i: 0,
            }
        );

        loop {
            self.ip = self.read_byte();
            self.disassemble_instruction(self.ip);

            let frame_index = frames.len() - 1;
            let frame       = frames.get_mut(frame_index).unwrap();

            match self.ip.try_into().unwrap() {
                Op::Pop => {
                    let value = self.pop();
                    println!("POP {space:<width$} {:?} ", value, space=" ", width=22);
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
                    let index = self.read_byte() as usize;
                    let value = frame.get_value(&self.stack, index);// self.stack[index];

                    if discriminant(&value) == discriminant(&Value::Unit) {
                        panic!("Cannot access an undefined variable.");
                    }

                    self.push(value);
                }
                Op::SetVariable => {
                    let index = self.read_byte() as usize;
                    let value = self.peek(self.stack_top - 1);

                    // frame.frame[index] = value;
                    frame.set_value(&mut self.stack, index, value);
                }
                Op::GetUpvalue => {
                    let index = self.read_byte() as usize;
                    let upvalue = frame.get_upvalue(&self.stack, index);

                    if discriminant(&upvalue.closed) == discriminant(&Value::Unit) {
                        panic!("Cannot access an undefined variable.");
                    }

                    self.push(upvalue.closed);
                },
                Op::SetUpvalue => {
                    let index = self.read_byte() as usize;
                    let value = self.peek(self.stack_top - 1);

                    frame.set_upvalue(&mut self.stack, index, Upvalue{ closed: value });
                },
                Op::Frame => {
                    frames.push_back(CallFrame {
                        i: self.stack_top,
                        ip: self.ip
                    });
                }
                Op::Return => {
                    if frames.is_empty() {
                        break;
                    }

                    frames.pop_back();
                }
                Op::Jump => {
                    let jump = self.read_byte() as usize;
                    self.i += jump;

                }
                Op::CondJump => {
                    let jump = self.read_byte() as usize;
                    let value = self.pop();

                    let boolean = match value {
                        Value::Bool { val } => val,
                        _ => panic!("TODO: Not supported")
                    };

                    if !boolean {
                        self.i += jump;
                    }
                }
                Op::LoopJump => {
                    let jump = self.read_byte() as usize;
                    self.i -= jump;
                },
                _ => { self.i += 1; }
            }

            if self.i >= (*self.block).borrow().code.len() {
                break;
            }
        }
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        let value = self.stack[self.stack_top];

        self.stack[self.stack_top] = Value::Unit;

        value
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
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

    fn peek(&self, index: usize) -> Value {
        self.stack[index]
    }

    fn peek_op(&self, index: usize) -> u8 {
        (*self.block).borrow().code[index]
    }

    // TODO: how to handle differring types
    fn binary_op(&mut self) -> (Value, Value) {
        let a = self.pop();
        let b = self.pop();

        if discriminant(&a) != discriminant(&b) {
            panic!("Binary operation with two different types is not supported.");
        }

        (a, b)
    }

    fn disassemble_instruction(&self, instruction: u8) {
        print!("OP ");

        if let Ok(i) = instruction.try_into() {
            match i {
                Op::True        => self.print_simple_op("TRUE"),
                Op::False       => self.print_simple_op("FALSE"),
                Op::Not         => self.print_simple_op("NOT"),
                Op::Add         => self.print_simple_op("ADD"),
                Op::Subtract    => self.print_simple_op("SUBTRACT"),
                Op::Multiply    => self.print_simple_op("MULTIPLY"),
                Op::Divide      => self.print_simple_op("DIVIDE"),
                Op::Constant    => {
                    let index = self.peek_op(self.i) as usize;
                    let value = self.peek(index);

                    self.print_constant_op("CONSTANT", value);
                },
                Op::Equal       => self.print_simple_op("EQUAL"),
                Op::Less        => self.print_simple_op("LESS"),
                Op::Greater     => self.print_simple_op("GREATER"),
                Op::GetVariable => self.print_byte_op("GET_VARIABLE"),
                Op::SetVariable => {
                    let index = self.peek_op(self.i) as usize;
                    let value = self.peek(index);

                    self.print_constant_op("SET_VARIABLE", value);
                },
                Op::GetUpvalue => self.print_byte_op("GET_UPVALUE"),
                Op::SetUpvalue => {
                    let index = self.peek_op(self.i) as usize;
                    let value = self.peek(index);

                    self.print_constant_op("SET_UPVALUE", value);
                },
                Op::Frame       => self.print_simple_op("FRAME"),
                Op::Return      => self.print_simple_op("RETURN"),
                Op::Jump        => {
                    let jump = self.peek_op(self.i);
                    println!("{name:<width$} {slot:<slot_width$} {value}",
                             name="JUMP",
                             width=20,
                             slot=self.i,
                             slot_width=5,
                             value=jump);

                },
                Op::CondJump        => {
                    let jump = self.peek_op(self.i);
                    println!("{name:<width$} {slot:<slot_width$} {value}",
                             name="COND_JUMP",
                             width=20,
                             slot=self.i,
                             slot_width=5,
                             value=jump);

                },
                Op::LoopJump        => {
                    let jump = self.peek_op(self.i);
                    println!("{name:<width$} {slot:<slot_width$} {value}",
                             name="LOOP_JUMP",
                             width=20,
                             slot=self.i,
                             slot_width=5,
                             value=jump);

                },
                _ => { }
            }
        }
    }

    fn print_simple_op(&self, name: &str) {
        println!("{name:<width$} |", name=name, width=20);
    }

    fn print_byte_op(&self, name: &str) {
        println!("{name:<width$} {slot}", name=name, slot=self.i, width=20);
    }

    fn print_constant_op(&self, name: &str, value: Value) {
        println!("{name:<width$} {slot:<slot_width$} {value:?}", name=name,
                                                                 width=20,
                                                                 slot=self.i,
                                                                 slot_width=5,
                                                                 value=value);
    }
}
