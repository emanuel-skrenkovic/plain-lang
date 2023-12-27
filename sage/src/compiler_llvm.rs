// TODO: think about pulling this out and removing the llvm dependency from the
// compiler itself.

extern crate llvm_sys as llvm;

use std::collections::{VecDeque, HashMap};

use macros::binary_cstr;

use crate::vm;
use crate::block;
use crate::compiler;

// TODO: const strings for now. I'll need to get into memory management for the C strings and
// I don't want to do that right now. :( :) :( :)

// trait StringExtensions
// {
//     fn to_libc_cstr() -> *const libc::c_char;
// }
// impl StringExtensions for &str
// {
//     fn to_libc_cstr() -> *const libc::c_char
//     {
//         let c_buf: *const c_char = unsafe { hello() };
//         let c_str: &CStr = unsafe { CStr::from_ptr(c_buf) };
//         let str_slice: &str = c_str.to_str().unwrap();
//         let str_buf: String = str_slice.to_owned();  // if necessary
//
//         value..as_ptr() as *const _
//     }
// }

// END TODO

pub struct CompilationState
{
    // HashMap of variables per scope. Scope index is the key, variables are in the vec.
    // TODO: think of how to do indexing of variable refs.
    pub variables: HashMap<usize, Vec<llvm::prelude::LLVMValueRef>>
}

impl CompilationState
{
    pub fn new() -> Self {
        Self { variables: HashMap::new() }
    }
}

pub struct CallFrame
{
    pub position: usize,
    pub i: usize,
    pub block: block::Block
}

impl CallFrame
{
    #[must_use]
    pub fn new(position: usize, block: block::Block) -> CallFrame
    {
        CallFrame {
            position,
            i: 0,
            block
        }
    }

    pub fn get_value
    (
        &self,
        index: usize,
        stack: &VecDeque<llvm::prelude::LLVMValueRef>
    ) -> llvm::prelude::LLVMValueRef
    {
        stack[index + self.position].clone()
    }

    pub fn set_value
    (
        &mut self,
        index: usize,
        value: llvm::prelude::LLVMValueRef,
        stack: &mut VecDeque<llvm::prelude::LLVMValueRef>
    )
    {
        stack[index + self.position] = value;
    }

    pub fn read_byte(&mut self) -> u8
    {
        let ip = self.block.code[self.i];
        self.i += 1;

        ip
    }

    pub fn read_constant(&mut self, index: usize) -> block::Value
    {
        self.block.constants[index].clone()
    }

    pub fn peek_op(&self, index: usize) -> u8
    {
        self.block.code[index]
    }
}

pub struct Backend
{
    pub context: llvm::prelude::LLVMContextRef,
    pub modules: Vec<llvm::prelude::LLVMModuleRef>,
    pub builder: llvm::prelude::LLVMBuilderRef,

    pub stack: VecDeque<llvm::prelude::LLVMValueRef>,
    pub stack_top: usize,

    pub state: CompilationState,
}

// I think I'll have to implement the parser. Seems like the data we have in the
// interpreter is very much specialized towards it, and will probably not be enough
// to properly compile a program using llvm.
impl Backend
{
    pub fn new() -> Self
    {
        unsafe {
            let context = llvm::core::LLVMContextCreate();
            let builder = llvm::core::LLVMCreateBuilderInContext(context);
            Self {
                context,
                modules: vec![],
                builder,

                stack: VecDeque::with_capacity(vm::STACK_SIZE),
                stack_top: 0,

                state: CompilationState::new(),
            }
        }
    }

    // TODO: not yet determined what should be the input to this part of the compilation process.
    // I do get the feeling that the compiler::Program data structure is insufficient for
    // building any kind of program using llvm.
    // But, nothing to gain by not trying I guess.
    pub fn compile(&mut self, program: compiler::Program)
    {
        let mut frames = VecDeque::new();
        frames.push_front(
            CallFrame::new(self.stack_top, program.block.clone())
        );

        unsafe {
            let module = llvm::core::LLVMModuleCreateWithNameInContext(binary_cstr!("main"), self.context);
            self.modules.push(module);

            let main_function_type = llvm
                ::core
                ::LLVMFunctionType(llvm::core::LLVMInt32TypeInContext(self.context), std::ptr::null_mut(), 0, 0);

            let main_function = llvm::core::LLVMAddFunction(module, binary_cstr!("main"), main_function_type);

            let entry_block = llvm
                ::core
                ::LLVMAppendBasicBlockInContext(self.context, main_function, binary_cstr!("entry"));
            llvm::core::LLVMPositionBuilderAtEnd(self.builder, entry_block);

            loop {
                let frame_index = frames.len() - 1;
                // let mut frame_index = frames.len() - 1;

                if frames[frame_index].i == frames[frame_index].block.code.len() {
                    break;
                }

                let frame = &mut frames[frame_index];
                let ip = frame.read_byte();

                let Ok(operation) = ip.try_into() else {
                    panic!("Could not parse operation '{}'.", ip);
                };

                match operation {
                    block::Op::Pop => { self.pop(); }

                    block::Op::Constant => {
                        let index     = frame.read_byte();
                        let value     = frame.read_constant(index as usize);
                        let value_ref = self.build_constant(value);

                        self.push(value_ref);
                    }

                    block::Op::Add => {
                        let (rhs, lhs) = self.binary_op();

                        let result = llvm
                            ::core
                            ::LLVMBuildAdd(self.builder, lhs, rhs, binary_cstr!("_result"));

                        self.push(result);
                    },

                    block::Op::Equal => {
                        let (rhs, lhs) = self.binary_op();
                        let predicate  = llvm::LLVMRealPredicate::LLVMRealUEQ;

                        let result = llvm
                            ::core
                            ::LLVMBuildFCmp(self.builder, predicate,lhs, rhs, binary_cstr!("_eqcomp"));
                        self.push(result);
                    }

                    block::Op::DeclareVariable => {
                        let scope = program.current_scope;

                        // TODO: allocate upfront or something other than this.
                        if !self.state.variables.contains_key(&scope) {
                            self.state.variables.insert(scope, vec![0 as *mut llvm::LLVMValue; 1024]);
                        }

                        let index = frame.read_byte() as usize;

                        let variable = &program
                            .scopes[program.current_scope]
                            .variables[index];

                        let type_ref      = llvm::core::LLVMInt32TypeInContext(self.context);
                        let variable_name = variable.name.value.as_ptr();

                        let variable = llvm
                            ::core
                            ::LLVMBuildAlloca(self.builder, type_ref, variable_name as *const _);

                        self
                            .state
                            .variables
                            .get_mut(&program.current_scope)
                            .unwrap()[index] = variable;
                    }

                    block::Op::GetLocal => {
                        let index = frame.read_byte() as usize;
                        let value = frame.get_value(index, &self.stack);

                        self.push(value);
                    }

                    block::Op::SetLocal => {
                        let value = self.peek(0).clone();
                        let index = frame.read_byte() as usize;

                        let variable_ref = self.state.variables[&program.current_scope][index];
                        llvm::core::LLVMBuildStore(self.builder, value, variable_ref);
                    }

                    block::Op::Return => {
                        // let values_count = frame.read_byte();
                        // let result = self.pop();
                        //
                        // for _ in 0..values_count { self.pop(); }
                        //
                        // if frames.is_empty() {
                        //     break;
                        // }
                        //
                        // if frame_index > 0 {
                        //     frame_index -= 1;
                        // }
                        // frames.pop_back();
                        //
                        // self.push(result);
                    }

                    block::Op::Call => {
                        let _scope_distance = frame.read_byte() as usize;
                        let _index          = frame.read_byte() as usize;

                        // let enclosing_scope = &frames[frame_index - scope_distance];
                        // let function = enclosing_scope.get_value(
                        //     index,
                        //     &self.stack
                        // );

                        // let Value::Function { arity, closure, .. } = function else {
                        //     panic!("Frame value of incorrect type. Expected 'Function'.");
                        // };

                        // frames.push_back(CallFrame::new(self.stack_top - arity, closure.code));
                    },

                    _ => { frame.i += 1; }
                }

                self.disassemble_instruction(ip);
            }

            let a = self.state.variables.get(&0).unwrap()[0];

            let global_format_str = llvm::core::LLVMBuildGlobalStringPtr(
                self.builder,
                binary_cstr!("%s\n"),
                binary_cstr!("format_str"),
            );

            let printf_type = llvm
                ::core
                ::LLVMFunctionType
                (
                    llvm::core::LLVMInt32TypeInContext(self.context),
                    [llvm::core::LLVMPointerType(llvm::core::LLVMInt8TypeInContext(self.context), 0)].as_mut_ptr(),
                    1,
                    1,
                );

            let printf = llvm
                ::core
                ::LLVMAddFunction(module, binary_cstr!("printf"), printf_type);

            let mut param_values = [global_format_str, a];
            llvm::core::LLVMBuildCall2(
                self.builder,
                printf_type,
                printf,
                param_values.as_mut_ptr(),
                param_values.len() as u32,
                binary_cstr!("printf_call"),
            );

            let return_value = llvm
                ::core
                ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(self.context), 0, 0);
            llvm::core::LLVMBuildRet(self.builder, return_value);

            let failure_action = llvm
                ::analysis
                ::LLVMVerifierFailureAction::LLVMReturnStatusAction;

            let mut error: *mut i8 = std::ptr::null_mut();
            if llvm::analysis::LLVMVerifyModule(module, failure_action, &mut error) != 0 {
                eprintln!("Analysis error: {}", std::ffi::CStr::from_ptr(error).to_string_lossy());
                llvm::core::LLVMDisposeMessage(error);
                return;
            }

            let result = llvm
                ::bit_writer
                ::LLVMWriteBitcodeToFile(self.modules[0], binary_cstr!("bin/a.bc"));
            if result != 0 {
                eprintln!("Failed to output bitcode.");
                return
            }
        }
    }

    pub fn peek(&self, distance: usize) -> llvm::prelude::LLVMValueRef
    {
        self.stack[self.stack_top - 1 - distance]
    }

    pub fn pop(&mut self) -> llvm::prelude::LLVMValueRef
    {
        assert!(!self.stack.is_empty(), "Cannot pop empty stack.");
        let value = self.stack.pop_back().unwrap();
        self.stack_top -= 1;
        value
    }

    pub fn push(&mut self, value: llvm::prelude::LLVMValueRef)
    {
        self.stack.push_back(value);
        self.stack_top += 1;
    }

    // TODO: how to handle differring types
    fn binary_op(&mut self) -> (llvm::prelude::LLVMValueRef, llvm::prelude::LLVMValueRef)
    {
        let a = self.pop();
        let b = self.pop();

        // if discriminant(&a) != discriminant(&b) {
        //     panic!("Binary operation with two different types is not supported.");
        // }

        (a, b)
    }

    fn build_constant(&self, value: block::Value) -> llvm::prelude::LLVMValueRef
    {
        unsafe {
            match value {
                block::Value::Number { val } => {
                    llvm
                        ::core
                        ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(self.context), val as u64, 1)
                }

                block::Value::Bool { val } => {
                    let type_ref = llvm::core::LLVMInt8TypeInContext(self.context);
                    llvm::core::LLVMConstInt(type_ref, if val { 1 } else { 0 }, 0)
                }

                block::Value::Function { name, arity, .. } => {
                    let return_type = llvm::core::LLVMVoidTypeInContext(self.context);

                    // TODO: going to need parameter type information here.
                    let param_type_ref = llvm::core::LLVMVoidTypeInContext(self.context);

                    let type_ref = llvm
                        ::core
                        ::LLVMFunctionType(return_type, [param_type_ref].as_mut_ptr(), arity as u32, 0);

                    // TODO: current module, not just any.
                    llvm::core::LLVMAddFunction(self.modules[0], name.as_ptr() as *const _, type_ref)
                }

                // TODO: I now have strings in the compiler/parser! happyface
                block::Value::String { val } => {
                    llvm::core::LLVMConstString(val.as_ptr() as *const _, val.len() as u32, 1)
                }

                _ => panic!() // TODO: Result I guess. :/
            }
        }
    }

    fn disassemble_instruction(&self, instruction: u8)
    {
        print!("OP ");

        if let Ok(i) = instruction.try_into() {
            match i {
                block::Op::Pop => self.print_simple_op("POP"),
                block::Op::True => self.print_simple_op("TRUE"),
                block::Op::False => self.print_simple_op("FALSE"),
                block::Op::Not => self.print_simple_op("NOT"),
                block::Op::Add => self.print_simple_op("ADD"),
                block::Op::Subtract => self.print_simple_op("SUBTRACT"),
                block::Op::Multiply => self.print_simple_op("MULTIPLY"),
                block::Op::Divide => self.print_simple_op("DIVIDE"),
                block::Op::Constant => self.print_simple_op("CONSTANT"),
                block::Op::Equal => self.print_simple_op("EQUAL"),
                block::Op::Less => self.print_simple_op("LESS"),
                block::Op::Greater => self.print_simple_op("GREATER"),
                block::Op::DeclareVariable => self.print_simple_op("DECLARE_VARIABLE"),
                block::Op::GetLocal => self.print_simple_op("GET_LOCAL"),
                block::Op::SetLocal => self.print_simple_op("SET_LOCAL"),
                block::Op::GetUpvalue => self.print_simple_op("GET_UPVALUE"),
                block::Op::SetUpvalue => self.print_simple_op("SET_UPVALUE"),
                block::Op::Frame => self.print_simple_op("FRAME"),
                block::Op::Return => self.print_simple_op("RETURN"),
                block::Op::Jump => self.print_simple_op("JUMP"),
                block::Op::CondJump => self.print_simple_op("COND_JUMP"),
                block::Op::LoopJump => self.print_simple_op("LOOP_JUMP"),
                block::Op::Call => self.print_simple_op("CALL "),
                _ => { }
            }
        }
    }

    fn print_simple_op(&self, name: &str)
    {
        println!("{name:<width$} |", name=name, width=20);
    }
}

impl Drop for Backend
{
    fn drop(&mut self)
    {
        unsafe {
            llvm::core::LLVMDumpModule(self.modules[0]);

            llvm::core::LLVMDisposeBuilder(self.builder);
            llvm::core::LLVMDisposeModule(self.modules[0]);
            llvm::core::LLVMContextDispose(self.context);
        }
    }
}
