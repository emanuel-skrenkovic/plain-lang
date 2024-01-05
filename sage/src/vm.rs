use std::mem::discriminant;
use std::collections::VecDeque;

use crate::types::Equatable;
use crate::compiler::Program;
use crate::block::{Block, Op, Value};

pub const STACK_SIZE: usize = 1024;

// Cache eviction on every scope change?
pub struct CallFrame
{
    pub position: usize,
    pub i: usize,
    pub block: Block
}

impl CallFrame
{
    #[must_use]
    pub fn new(position: usize, block: Block) -> CallFrame
    {
        CallFrame {
            position,
            i: 0,
            block
        }
    }

    pub fn get_value(&self, index: usize, stack: &VecDeque<Value>) -> Value
    {
        stack[index + self.position].clone()
    }

    pub fn set_value(&mut self, index: usize, value: Value, stack: &mut VecDeque<Value>)
    {
        stack[index + self.position] = value;
    }

    pub fn read_byte(&mut self) -> u8
    {
        let ip = self.block.code[self.i];
        self.i += 1;

        ip
    }

    pub fn read_constant(&mut self, index: usize) -> Value
    {
        self.block.constants[index].clone()
    }

    pub fn peek_op(&self, index: usize) -> u8
    {
        self.block.code[index]
    }
}

// TODO: use?
pub struct Stack
{
    values: VecDeque<Value>,
    stack_top: usize
}

impl <'vm> Stack
{
    pub fn new() -> Stack
    {
        Stack {
            values: VecDeque::with_capacity(1024),
            stack_top: 0
        }
    }

    pub fn push(&mut self, value: Value)
    {
        self.values.push_back(value);
        self.stack_top += 1;
    }

    pub fn pop(&mut self) -> Value
    {
        assert!(!self.values.is_empty());
        let value = self.values.pop_back().unwrap();
        self.stack_top -= 1;
        value
    }

    pub fn peek(&self, distance: usize) -> &Value
    {
        &self.values[self.stack_top - 1 - distance]
    }
}

pub struct VM
{
    program: Program,

    stack: VecDeque<Value>,
    stack_top: usize,
}

impl VM
{
    #[must_use]
    pub fn new(program: Program) -> VM
    {
        VM {
            program,

            stack: VecDeque::with_capacity(STACK_SIZE),
            stack_top: 0
        }
    }

    pub fn interpret(&mut self)
    {
        let mut frames = VecDeque::new();
        frames.push_front(
            CallFrame::new(self.stack_top, self.program.block.clone())
        );

        loop {
            let mut frame_index = frames.len() - 1;

            if frames[frame_index].i == frames[frame_index].block.code.len() {
                break;
            }

            let frame = &mut frames[frame_index];
            let ip = frame.read_byte();

            let Ok(operation) = ip.try_into() else {
                panic!("Could not parse operation '{}'.", ip);
            };

            match operation {
                Op::Pop => { self.pop(); }

                Op::Add => {
                    let (a, b) = self.binary_op();

                    let Value::Number { val: second } = a else {
                        panic!("TODO: Not supported")
                    };

                    let Value::Number { val: first } = b else {
                        panic!("TODO: Not supported");
                    };

                    let result = Value::Number { val: first + second };
                    self.push(result);
                }

                Op::Subtract => {
                    let (a, b) = self.binary_op();

                    let Value::Number { val: second } = a else {
                        panic!("TODO: Not supported")
                    };

                    let Value::Number { val: first } = b else {
                        panic!("TODO: Not supported");
                    };

                    self.push(Value::Number { val: first - second });
                }

                Op::Multiply => {
                    let (a, b) = self.binary_op();

                    let Value::Number { val: second } = a else {
                        panic!("TODO: Not supported")
                    };

                    let Value::Number { val: first } = b else {
                        panic!("TODO: Not supported");
                    };

                    self.push(Value::Number { val: first * second });
                }

                Op::Divide => {
                    let (a, b) = self.binary_op();

                    let Value::Number { val: second } = a else {
                        panic!("TODO: Not supported")
                    };

                    let Value::Number { val: first } = b else {
                        panic!("TODO: Not supported");
                    };

                    self.push(Value::Number { val: first / second });
                }

                Op::Equal => {
                    let (a, b) = self.binary_op();

                    let Ok(equality) = a.equals(&b) else {
                        panic!("Types cannot be equated.");
                    };

                    self.push(Value::Bool { val: equality });
                }

                Op::Less => {
                    let (a, b) = self.binary_op();

                    let Value::Number { val: second } = a else {
                        panic!("TODO: Not supported")
                    };

                    let Value::Number { val: first } = b else {
                        panic!("TODO: Not supported");
                    };

                    self.push(Value::Bool { val: first < second });
                }

                Op::Greater => {
                    let (a, b) = self.binary_op();

                    let Value::Number { val: second } = a else {
                        panic!("TODO: Not supported")
                    };

                    let Value::Number { val: first } = b else {
                        panic!("TODO: Not supported");
                    };

                    self.push(Value::Bool { val: first > second });
                }

                Op::GreaterEqual => {
                    let (a, b) = self.binary_op();

                    let Value::Number { val: second } = a else {
                        panic!("TODO: Not supported")
                    };

                    let Value::Number { val: first } = b else {
                        panic!("TODO: Not supported");
                    };

                    self.push(Value::Bool { val: first >= second });
                }

                Op::LessEqual => {
                    let (a, b) = self.binary_op();

                    let Value::Number { val: second } = a else {
                        panic!("TODO: Not supported")
                    };

                    let Value::Number { val: first } = b else {
                        panic!("TODO: Not supported");
                    };

                    self.push(Value::Bool { val: first <= second });
                }

                Op::Not => {
                    let Value::Bool { val } = self.pop() else {
                        panic!("TODO: Not supported")
                    };

                    self.push(Value::Bool { val: !val });
                }

                Op::Constant => {
                    let index = frame.read_byte();
                    let value = frame.read_constant(index as usize);
                    self.push(value);
                },

                Op::GetLocal => {
                    let index = frame.read_byte() as usize;
                    let value = frame.get_value(index, &self.stack);

                    if discriminant(&value) == discriminant(&Value::Unit) {
                        panic!("Cannot access an undefined variable.");
                    }

                    self.push(value);
                }

                Op::DeclareVariable => {
                    // Intentionally leaving it alone. The variable index is being emitted by
                    // the compiler, but it is not needed in this implementation.
                    let _ = frame.read_byte();
                    let _ = frame.read_byte();
                },

                Op::SetLocal => {
                    let value = self.peek(0).clone();

                    let index = frame.read_byte() as usize;
                    frame.set_value(index, value, &mut self.stack);
                }

                Op::GetUpvalue => {
                    let scope_distance = frame.read_byte() as usize;
                    let index = frame.read_byte() as usize;

                    let enclosing_scope = &frames[frame_index - scope_distance];
                    let value = enclosing_scope.get_value(index, &self.stack);

                    if discriminant(&value) == discriminant(&Value::Unit) {
                        panic!("Cannot access an undefined variable.");
                    }

                    self.push(value);
                },

                Op::SetUpvalue => {
                    let scope_distance = frame.read_byte() as usize;
                    let index = frame.read_byte() as usize;

                    let value = self.peek(0).clone();
                    let enclosing_scope = &mut frames[frame_index - scope_distance];

                    enclosing_scope.set_value(index, value, &mut self.stack);
                },

                Op::Return => {
                    let values_count = frame.read_byte();
                    let result = self.pop();

                    for _ in 0..values_count { self.pop(); }

                    if frames.is_empty() {
                        break;
                    }

                    if frame_index > 0 {
                        frame_index -= 1;
                    }
                    frames.pop_back();

                    self.push(result);
                }

                Op::Jump => {
                    let jump = frame.read_byte() as usize;
                    frame.i += jump;
                }

                Op::CondJump => {
                    let jump = frame.read_byte() as usize;

                    let Value::Bool { val } = self.pop() else {
                        panic!("TODO: Not supported");
                    };

                    if !val {
                        frame.i += jump;
                    }
                }

                Op::Loop => {
                    // Intentionally left blank.
                }

                Op::LoopCondJump => {
                    let jump = frame.read_byte() as usize;

                    let val = self.pop();
                    println!("VAL: {:?}", val);
                    let Value::Bool { val } = val else {
                        panic!("TODO: Not supported");
                    };

                    if !val {
                        frame.i += jump;
                    }
                }

                Op::LoopJump => {
                    let jump = frame.read_byte() as usize;
                    frame.i -= jump;
                },

                Op::Call => {
                    let scope_distance = frame.read_byte() as usize;
                    let index          = frame.read_byte() as usize;

                    let enclosing_scope = &frames[frame_index - scope_distance];
                    let function = enclosing_scope.get_value(
                        index,
                        &self.stack
                    );

                    let Value::Function { arity, closure, .. } = function else {
                        panic!("Frame value of incorrect type. Expected 'Function'.");
                    };

                    frames.push_back(CallFrame::new(self.stack_top - arity, closure.code));
                },

                _ => { frame.i += 1; }
            }

            self.disassemble_instruction(&frames[frame_index], frame_index, &frames, ip);
        }
    }

    pub fn pop(&mut self) -> Value
    {
        assert!(!self.stack.is_empty(), "Cannot pop empty stack.");
        let value = self.stack.pop_back().unwrap();
        self.stack_top -= 1;
        value
    }

    pub fn push(&mut self, value: Value)
    {
        self.stack.push_back(value);
        self.stack_top += 1;
    }

    pub fn peek(&self, distance: usize) -> &Value
    {
        &self.stack[self.stack_top - 1 - distance]
    }

    // TODO: how to handle differring types
    fn binary_op(&mut self) -> (Value, Value)
    {
        let a = self.pop();
        let b = self.pop();

        if discriminant(&a) != discriminant(&b) {
            panic!
            (
                "Binary operation with two different types is not supported. Type A: {:?} Type B: {:?}",
                discriminant(&a),
                discriminant(&b),
            );
        }

        (a, b)
    }

    fn disassemble_instruction(
        &self,
        frame: &CallFrame,
        frame_index: usize,
        frames: &VecDeque<CallFrame>,
        instruction: u8,
    )
    {
        print!("OP ");

        if let Ok(i) = instruction.try_into() {
            match i {
                Op::Pop => {
                    // let value = self.peek(0);
                    // self.print_constant_op(frame, "POP", value);
                    // TODO: print popped value. Stopped working after 'disassemble_instruction'
                    // call was moved to after execution.
                    self.print_simple_op("POP");
                },
                Op::True => self.print_simple_op("TRUE"),
                Op::False => self.print_simple_op("FALSE"),
                Op::Not => self.print_simple_op("NOT"),
                Op::Add => self.print_simple_op("ADD"),
                Op::Subtract => self.print_simple_op("SUBTRACT"),
                Op::Multiply => self.print_simple_op("MULTIPLY"),
                Op::Divide => self.print_simple_op("DIVIDE"),
                Op::Constant => {
                    // let index = frame.peek_op(frame.i - 1) as usize;
                    let value = self.peek(0);
                    self.print_constant_op(frame, "CONSTANT", &value);
                },
                Op::Equal => self.print_simple_op("EQUAL"),
                Op::Less => self.print_simple_op("LESS"),
                Op::Greater => self.print_simple_op("GREATER"),
                Op::DeclareVariable => self.print_simple_op("DECLARE_VARIABLE"),
                Op::GetLocal => {
                    let index = frame.peek_op(frame.i - 1) as usize;
                    let value = frame.get_value(index, &self.stack);

                    self.print_constant_op(frame, "GET_LOCAL", &value);
                },
                Op::SetLocal => {
                    let value = self.peek(0);

                    self.print_constant_op(frame, "SET_LOCAL", value);
                },
                Op::GetUpvalue => {
                    let value = self.peek(0);
                    self.print_constant_op(frame, "GET_UPVALUE", value);
                },
                Op::SetUpvalue => {
                    let value = self.peek(0);

                    self.print_constant_op(frame, "SET_UPVALUE", value);
                },
                Op::Frame => self.print_simple_op("FRAME"),
                Op::Return => self.print_simple_op("RETURN"),
                Op::Jump => {
                    self.print_simple_op("JUMP");
                    // let jump = frame.peek_op(ip);
                    // println!("{name:<width$} {slot:<slot_width$} {value}",
                    //          name="JUMP",
                    //          width=20,
                    //          slot=frame.i - 1,
                    //          slot_width=5,
                    //          value=jump);

                },
                Op::CondJump => {
                    self.print_simple_op("COND_JUMP");
                    // let jump = frame.peek_op(frame.i - 1);
                    // println!("{name:<width$} {slot:<slot_width$} {value}",
                    //          name="COND_JUMP",
                    //          width=20,
                    //          slot=frame.i - 1,
                    //          slot_width=5,
                    //          value=jump);

                },
                Op::Loop => {
                    self.print_simple_op("LOOP");
                    // let jump = frame.peek_op(frame.i - 1);
                    // println!("{name:<width$} {slot:<slot_width$} {value}",
                    //          name="COND_JUMP",
                    //          width=20,
                    //          slot=frame.i - 1,
                    //          slot_width=5,
                    //          value=jump);

                },
                Op::LoopCondJump => {
                    self.print_simple_op("LOOP_COND_JUMP");
                    // let jump = frame.peek_op(frame.i - 1);
                    // println!("{name:<width$} {slot:<slot_width$} {value}",
                    //          name="COND_JUMP",
                    //          width=20,
                    //          slot=frame.i - 1,
                    //          slot_width=5,
                    //          value=jump);

                },
                Op::LoopJump => {
                    self.print_simple_op("LOOP_JUMP");
                    // let jump = frame.peek_op(frame.i - 1);
                    // println!("{name:<width$} {slot:<slot_width$} {value}",
                    //          name="LOOP_JUMP",
                    //          width=20,
                    //          slot=frame.i - 1,
                    //          slot_width=5,
                    //          value=jump);

                },
                Op::Call => {
                    print!("CALL ");
                    let scope_distance = frame.peek_op(frame.i - 2) as usize;
                    let index = frame.peek_op(frame.i - 1) as usize;

                    let enclosing_scope = &frames[frame_index - scope_distance];
                    let value = enclosing_scope.get_value(index, &self.stack);

                    let function_name = match value {
                        Value::Function { name, .. } => name,
                        _ => panic!("Frame value of incorrect type. Expected 'Function'.")
                    };

                    println!("FUNCTION '{}'", function_name);
                }
                _ => { }
            }
        }
    }

    fn print_simple_op(&self, name: &str)
    {
        println!("{name:<width$} |", name=name, width=20);
    }

    // fn print_byte_op(&self, frame: &CallFrame, name: &str) {
    //     println!("{name:<width$} {slot}", name=name, slot=frame.i - 1, width=20);
    // }

    fn print_constant_op(&self, frame: &CallFrame, name: &str, value: &Value)
    {
        println!("{name:<width$} {slot:<slot_width$} {value:?}", name=name,
                                                                 width=20,
                                                                 slot=frame.i - 1,
                                                                 slot_width=5,
                                                                 value=value);
    }
}

#[cfg(test)]
mod vm_tests
{
    use super::*;

    #[test]
    fn stack_push_pushes_to_stack_front()
    {
        let mut stack = Stack::new();
        stack.push(Value::Unit);

        debug_assert!(!stack.values.is_empty());
        debug_assert_eq!(discriminant(&stack.values[0]), discriminant(&Value::Unit));
    }

    #[test]
    fn stack_peek_with_no_distance_peeks_stack_top()
    {
        let mut stack = Stack::new();
        stack.push(Value::Unit);

        let number_val = Value::Number { val: 5 };
        let assert_clone = number_val.clone();
        stack.push(number_val);

        debug_assert!(!stack.values.is_empty());

        let peeked = stack.peek(0);
        debug_assert_eq!(discriminant(peeked), discriminant(&assert_clone));
    }

    #[test]
    fn stack_pops_last_inserted_value()
    {
        let mut stack = Stack::new();
        stack.push(Value::Unit);

        let number_val = Value::Number { val: 5 };
        let assert_clone = number_val.clone();
        stack.push(number_val);

        debug_assert!(!stack.values.is_empty());

        let popped = stack.pop();
        debug_assert_eq!(discriminant(&popped), discriminant(&assert_clone));
    }
}
