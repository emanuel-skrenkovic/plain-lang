use crate::block;

pub trait Equatable
{
    fn equals(&self, other: &Self) -> Result<bool, ()>;
}

impl Equatable for block::Value
{
    fn equals(&self, other: &block::Value) -> Result<bool, ()> {
        match self {
            block::Value::Number { val } => {
                match other {
                    block::Value::Number { val: other_val } => Ok(val == other_val),
                    _ => { Err(()) }
                }
            },
            block::Value::Bool { val } => {
                match other {
                    block::Value::Bool { val: other_val } => Ok(val == other_val),
                    _ => { Err(()) }
                }
            },
            _ => { Err(()) }
        }
    }
}

// pub struct TypedProgram
// {
//     pub block: block::Block, // block Block bLoCk BlOcK blocK BLOCK
//     pub scopes: Vec<Scope>,
//     pub current_scope: usize,
//     pub scope_depth: usize,
// }
//
// pub struct CallFrame
// {
//     pub position: usize,
//     pub i: usize,
//     pub block: block::Block
// }
//
// impl CallFrame
// {
//     #[must_use]
//     pub fn new(position: usize, block: block::Block) -> Self
//     {
//         CallFrame {
//             position,
//             i: 0,
//             block
//         }
//     }
//
//     pub fn get_value
//     (
//         &self,
//         index: usize,
//         stack: &VecDeque<llvm::prelude::LLVMValueRef>
//     ) -> llvm::prelude::LLVMValueRef
//     {
//         stack[index + self.position].clone()
//     }
//
//     pub fn set_value
//     (
//         &mut self,
//         index: usize,
//         value: llvm::prelude::LLVMValueRef,
//         stack: &mut VecDeque<llvm::prelude::LLVMValueRef>
//     )
//     {
//         stack[index + self.position] = value;
//     }
//
//     pub fn read_byte(&mut self) -> u8
//     {
//         let ip = self.block.code[self.i];
//         self.i += 1;
//
//         ip
//     }
//
//     pub fn read_constant(&mut self, index: usize) -> block::Value
//     {
//         self.block.constants[index].clone()
//     }
//
//     pub fn peek_op(&self, index: usize) -> u8
//     {
//         self.block.code[index]
//     }
// }
//
// fn check_types(program: compiler::Program)
// {
//     let mut frames = VecDeque::new();
//     frames.push_front(
//         todo!()
//     );
//
//     loop {
//         let frame_index = frames.len() - 1;
//
//         if frames[frame_index].i == frames[frame_index].block.code.len() {
//             break;
//         }
//
//         let frame = &mut frames[frame_index];
//         let ip = frame.read_byte();
//
//         let Ok(operation) = ip.try_into() else {
//             panic!("Could not parse operation '{}'.", ip);
//         };
//
//         match operation {
//             _ => { frame.i += 1; }
//         }
//     }
// }
