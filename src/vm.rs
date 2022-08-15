use std::mem::discriminant;
use std::collections::VecDeque;

use crate::compiler::Program;
use crate::block::{Block, Op, Value};

const STACK_SIZE: usize = 1024;

fn init_stack() -> VecDeque<Value> {
    (0..STACK_SIZE).map(|_| Value::Unit).collect()
}

// Cache eviction on every scope change?
struct CallFrame {
    i: usize,
    block: Block
}

impl CallFrame {
    fn get_value(&self, index: usize) -> Value {
        self.block.values[index].clone()
    }

    fn set_value(&mut self, index: usize, value: Value) {
        self.block.values[index] = value;
    }

    fn get_upvalue(&self, _index: usize) -> Value {
        todo!()
    }

    fn set_upvalue(&mut self, _index: usize, _value: Value) {
        todo!()
    }

    fn read_byte(&mut self) -> u8 {
        let ip = self.block.code[self.i];
        self.i += 1;

        ip
    }

    fn read_constant(&mut self, index: usize) -> Value {
        // let index = self.read_byte() as usize;
        // self.program.block.values[index].clone()
        self.block.values[index].clone()
    }

    fn peek_op(&self, index: usize) -> u8 {
        self.block.code[index]
    }
}

pub struct VM {
    program: Program,

    stack: VecDeque<Value>,
    stack_top: usize,
}

impl VM {
    pub fn new(program: Program) -> VM {
        VM {
            program,

            stack: init_stack(),
            stack_top: 0
        }
    }

    pub fn interpret(&mut self) {
        let mut frames = VecDeque::new();
        frames.push_front(
            CallFrame {
                i: 0,
                block: self.program.block.clone() // TODO: remove the clone, give ownership instead
            }
        );

        loop {
            let mut frame_index = frames.len() - 1;
            let frame = &mut frames[frame_index];

            let ip = frame.read_byte();
            self.disassemble_instruction(frame, ip);

            match ip.try_into().unwrap() {

                Op::Pop => {
                    self.pop();
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

                    let result = Value::Number { val: first + second };
                    self.push(result);
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
                    let index = frame.read_byte();
                    let value = frame.read_constant(index as usize);
                    self.push(value);
                },

                Op::GetVariable => {
                    let index = frame.read_byte() as usize;
                    let value = frame.get_value(index);

                    if discriminant(&value) == discriminant(&Value::Unit) {
                        panic!("Cannot access an undefined variable.");
                    }

                    self.push(value);
                }

                Op::DeclareVariable => {
                    // TODO: section intentionally left blank
                },

                Op::SetVariable => {
                    let index = frame.read_byte() as usize;
                    let value = self.pop();

                    frame.set_value(index, value);
                }

                Op::GetUpvalue => {
                    let index = frame.read_byte() as usize;
                    let value = frame.get_upvalue(index);

                    if discriminant(&value) == discriminant(&Value::Unit) {
                        panic!("Cannot access an undefined variable.");
                    }

                    self.push(value);
                },

                Op::SetUpvalue => {
                    let index = frame.read_byte() as usize;
                    let value = self.peek(self.stack_top - 1).clone();

                    frame.set_upvalue(index, value);
                },

                Op::Frame => {
                    // TODO: need closures
                    let index = frame.read_byte() as usize;
                    let value = frame.read_constant(index);

                    let closure = match value {
                        Value::Closure { val } =>  val,
                        _ => panic!("Frame value of incorrect type. Expexted 'Closure'.")
                    };

                    let frame = CallFrame {
                        i: self.stack_top,
                        block: closure.code
                    };
                    frames.push_back(frame);
                }

                Op::Return => {
                    if frames.is_empty() {
                        break;
                    }

                    if frame_index > 0 {
                        frame_index -= 1;
                    }
                    frames.pop_back();
                }

                Op::Jump => {
                    let jump = frame.read_byte() as usize;
                    frame.i += jump;

                }

                Op::CondJump => {
                    let jump = frame.read_byte() as usize;
                    let value = self.pop();

                    let boolean = match value {
                        Value::Bool { val } => val,
                        _ => panic!("TODO: Not supported")
                    };

                    if !boolean {
                        frame.i += jump;
                    }
                }

                Op::LoopJump => {
                    let jump = frame.read_byte() as usize;
                    frame.i -= jump;
                },

                Op::Call => {
                    let index = frame.read_byte() as usize;
                    let value = frame.read_constant(index);// self.program.block.values[index].clone();

                    let function_code = match value {
                        Value::Function { name: _, block, arity: _ } =>  block,
                        _ => panic!("Frame value of incorrect type. Expexted 'Function'.")
                    };

                    let frame = CallFrame {
                        i: 0,
                        block: function_code
                    };
                    frames.push_back(frame);
                },
                _ => { frame.i += 1; }
            }

            if frames[frame_index].i >= frames[frame_index].block.code.len() {
                break;
            }
        }
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        let value = self.stack[self.stack_top].clone();

        self.stack[self.stack_top] = Value::Unit;

        value
    }

    fn push(&mut self, value: Value) {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn peek(&self, index: usize) -> &Value {
        &self.stack[index]
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

    fn disassemble_instruction(&self, frame: &CallFrame, instruction: u8) {
        print!("OP ");

        if let Ok(i) = instruction.try_into() {
            match i {
                Op::Pop => {
                    let value = self.peek(0);
                    self.print_constant_op(frame, "POP", value);
                },
                Op::True => self.print_simple_op("TRUE"),
                Op::False => self.print_simple_op("FALSE"),
                Op::Not => self.print_simple_op("NOT"),
                Op::Add => self.print_simple_op("ADD"),
                Op::Subtract => self.print_simple_op("SUBTRACT"),
                Op::Multiply => self.print_simple_op("MULTIPLY"),
                Op::Divide => self.print_simple_op("DIVIDE"),
                Op::Constant => {
                    let index = frame.peek_op(frame.i) as usize;
                    let value = frame.get_value(index);// self.peek(index);

                    self.print_constant_op(frame, "CONSTANT", &value);
                },
                Op::Equal => self.print_simple_op("EQUAL"),
                Op::Less => self.print_simple_op("LESS"),
                Op::Greater => self.print_simple_op("GREATER"),
                Op::DeclareVariable => self.print_simple_op("DECLARE_VARIABLE"),
                Op::GetVariable => {
                    let index = frame.peek_op(frame.i) as usize;
                    let value = frame.get_value(index);// self.peek(index);

                    self.print_constant_op(frame, "GET_VARIABLE", &value);
                },
                Op::SetVariable => {
                    let index = frame.peek_op(frame.i) as usize;
                    let value = self.peek(index);

                    self.print_constant_op(frame, "SET_VARIABLE", value);
                },
                Op::GetUpvalue => self.print_byte_op(frame, "GET_UPVALUE"),
                Op::SetUpvalue => {
                    let index = frame.peek_op(frame.i) as usize;
                    let value = self.peek(index);

                    self.print_constant_op(frame, "SET_UPVALUE", value);
                },
                Op::Frame => self.print_simple_op("FRAME"),
                Op::Return => self.print_simple_op("RETURN"),
                Op::Jump => {
                    let jump = frame.peek_op(frame.i);
                    println!("{name:<width$} {slot:<slot_width$} {value}",
                             name="JUMP",
                             width=20,
                             slot=frame.i,
                             slot_width=5,
                             value=jump);

                },
                Op::CondJump => {
                    let jump = frame.peek_op(frame.i);
                    println!("{name:<width$} {slot:<slot_width$} {value}",
                             name="COND_JUMP",
                             width=20,
                             slot=frame.i,
                             slot_width=5,
                             value=jump);

                },
                Op::LoopJump => {
                    let jump = frame.peek_op(frame.i);
                    println!("{name:<width$} {slot:<slot_width$} {value}",
                             name="LOOP_JUMP",
                             width=20,
                             slot=frame.i,
                             slot_width=5,
                             value=jump);

                },
                Op::Call => {
                    print!("CALL ");
                    let index = frame.peek_op(frame.i) as usize;
                    let value = self.peek(index);

                    let function_name = match value {
                        Value::Function { name, block: _, arity: _ } =>  name,
                        _ => panic!("Frame value of incorrect type. Expexted 'Function'.")
                    };

                    println!("FUNCTION '{}'", function_name);
                }
                _ => { }
            }
        }
    }

    fn print_simple_op(&self, name: &str) {
        println!("{name:<width$} |", name=name, width=20);
    }

    fn print_byte_op(&self, frame: &CallFrame, name: &str) {
        println!("{name:<width$} {slot}", name=name, slot=frame.i, width=20);
    }

    fn print_constant_op(&self, frame: &CallFrame, name: &str, value: &Value) {
        println!("{name:<width$} {slot:<slot_width$} {value:?}", name=name,
                                                                 width=20,
                                                                 slot=frame.i,
                                                                 slot_width=5,
                                                                 value=value);
    }
}
