// TODO: think about pulling this out and removing the llvm dependency from the
// compiler itself.

extern crate llvm_sys as llvm;

use std::collections::VecDeque;

use macros::binary_cstr;

use crate::vm;
use crate::block;
use crate::block::Op;
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

#[derive(Debug, Clone)]
pub enum ValueInfo
{
    Unit,
    String,
    Bool,
    Number,
    Function { info: FunctionCall },
    CompiledFunction,
}

pub struct Context
{
    // HashMap of variables per scope. Scope index is the key, variables are in the vec.
    // TODO: think of how to do indexing of variable refs.
    pub variables: Vec<Vec<llvm::prelude::LLVMValueRef>>,
    // pub variables: HashMap<usize, Vec<llvm::prelude::LLVMValueRef>>,
    pub info: Vec<Vec<ValueInfo>>,
}

impl Context
{
    pub fn new() -> Self {
        Self { variables: vec![], info: vec![] }
    }
}

#[derive(Debug)]
pub struct FunctionContext
{
    pub position: usize,
    pub i: usize,
    pub block: block::Block
}

impl FunctionContext
{
    #[must_use]
    pub fn new(position: usize, block: block::Block) -> FunctionContext
    {
        FunctionContext {
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

    pub stack: VecDeque<llvm::prelude::LLVMValueRef>,
    pub stack_top: usize,

    pub compilation_context: Context,
}

// I think I'll have to implement the parser. Seems like the data we have in the
// interpreter is very much specialized towards it, and will probably not be enough
// to properly compile a program using llvm.
impl Backend
{
    pub fn new() -> Self
    {
        unsafe {
            Self {
                context: llvm::core::LLVMContextCreate(),
                modules: vec![],

                stack: VecDeque::with_capacity(vm::STACK_SIZE),
                stack_top: 0,

                compilation_context: Context::new(),
            }
        }
    }

    pub unsafe fn compile(&mut self, program: compiler::Program)
    {
        let module = llvm::core::LLVMModuleCreateWithNameInContext(binary_cstr!("main"), self.context);
        self.modules.push(module);

        let main_function_type = llvm
            ::core
            ::LLVMFunctionType(llvm::core::LLVMInt32TypeInContext(self.context), std::ptr::null_mut(), 0, 0);

        // TODO: I should expect a main function defined, not implicitly define it myself.
        // That way the special case of main gets reduced to only checking for its presence.
        let main_function = llvm::core::LLVMAddFunction(module, binary_cstr!("main"), main_function_type);

        let entry_block = llvm
            ::core
            ::LLVMAppendBasicBlockInContext(self.context, main_function, binary_cstr!("entry"));

        let builder = llvm::core::LLVMCreateBuilderInContext(self.context);
        llvm::core::LLVMPositionBuilderAtEnd(builder, entry_block);

        let mut frames = vec![FunctionContext::new(self.stack_top, program.block.clone())];

        // This goes through the instructions and does stuff.
        // It was factored out to support compiling functions other than main.
        // Note to self - it was a bit suspious how easy it was to factor this out.
        // Be on the lookout!
        self.walk_source(builder, module, &program, &mut frames);

        // START PRINTF CALL
        // self.add_printf(module, builder);
        // END PRINTF CALL

        let return_value = llvm
            ::core
            ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(self.context), 0, 0);
        llvm::core::LLVMBuildRet(builder, return_value);

        let failure_action = llvm
            ::analysis
            ::LLVMVerifierFailureAction::LLVMAbortProcessAction;

        let module_text = llvm::core::LLVMPrintModuleToString(module);
        println!("MODULE: \n{}", std::ffi::CStr::from_ptr(module_text).to_str().unwrap());

        let mut error: *mut i8 = std::ptr::null_mut();
        if llvm::analysis::LLVMVerifyModule(module, failure_action, &mut error) != 0 {
            eprintln!("Analysis error: {}", std::ffi::CStr::from_ptr(error).to_string_lossy());
            llvm::core::LLVMDisposeMessage(error);
            return;
        }

        let result = llvm
            ::bit_writer
            ::LLVMWriteBitcodeToFile(module, binary_cstr!("bin/a.bc"));

        if result != 0 {
            eprintln!("Failed to output bitcode.");
            return
        }
    }

    // TODO: REMOVE THIS! This is just for playing around.
    pub unsafe fn add_printf(&self, module: llvm::prelude::LLVMModuleRef, builder: llvm::prelude::LLVMBuilderRef)
    {
        let a = self.compilation_context.variables[0][2];

        let a_value = llvm::core::LLVMBuildLoad2(builder, llvm::core::LLVMInt32TypeInContext(self.context), a, binary_cstr!("a"));

        let global_format_str = llvm::core::LLVMBuildGlobalStringPtr(
            builder,
            binary_cstr!("%d\n"),
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

        let mut param_values = [global_format_str, a_value];
        llvm::core::LLVMBuildCall2(
            builder,
            printf_type,
            printf,
            param_values.as_mut_ptr(),
            param_values.len() as u32,
            binary_cstr!("printf_call"),
        );
    }

    pub unsafe fn walk_source
    (
        &mut self,
        builder: llvm::prelude::LLVMBuilderRef,
        module: llvm::prelude::LLVMModuleRef,
        program: &compiler::Program,
        frames: &mut Vec<FunctionContext>,
    )
    {
        // TODO: remove
        self.compilation_context.variables.push(vec![0 as *mut llvm::LLVMValue; 1024]);

        loop {
            if frames.len() == 0 { break }

            let frame_index = frames.len() - 1;
            if frames[frame_index].i == frames[frame_index].block.code.len() {
                break;
            }

            let frame = &mut frames[frame_index];

            let ip = frame.read_byte();
            self.disassemble_instruction(ip);

            let Ok(operation) = ip.try_into() else {
                panic!("Could not parse operation '{}'.", ip);
            };

            // If I ever want to move this to a dictionary, try to move it
            // into an array rather.
            match operation {
                block::Op::Pop => { self.pop(); }

                block::Op::Constant => {
                    let index     = frame.read_byte();
                    let value     = frame.read_constant(index as usize);

                    let value_ref = match value {
                        block::Value::Number { val } =>
                            llvm
                            ::core
                            ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(self.context), val as u64, 1),

                        block::Value::Bool { val } =>
                            llvm
                            ::core
                            ::LLVMConstInt(llvm::core::LLVMInt8TypeInContext(self.context), if val { 1 } else { 0 }, 0),

                        block::Value::String { val } =>
                            llvm
                            ::core
                            ::LLVMConstString(val.as_ptr() as *const _, val.len() as u32, 1),

                        block::Value::Function { name, arity, argument_type_names, return_type_name, closure } => {
                            let function_call = self
                                .build_function(module, name, arity, argument_type_names, return_type_name, closure.code);

                            let function_ref = function_call.function;

                            // TODO: fix this.
                            // if !self.compilation_context.info.len() < program.current_scope - 1 {
                            self.compilation_context.info.push(vec![]);
                            // }

                            self.compilation_context
                                .info[frame_index]
                                .push(ValueInfo::Function { info: function_call });

                            function_ref
                        }

                        _ => panic!() // TODO: Result I guess. :/
                    };

                    self.push(value_ref);
                }

                block::Op::Add => {
                    let (rhs, lhs) = self.binary_op();

                    let result = llvm
                    ::core
                    ::LLVMBuildAdd(builder, lhs, rhs, binary_cstr!("_add_result"));

                    self.push(result);
                },

                block::Op::Multiply => {
                    let (rhs, lhs) = self.binary_op();

                    let result = llvm
                    ::core
                    ::LLVMBuildMul(builder, lhs, rhs, binary_cstr!("_mul_result"));

                    self.push(result);
                }

                block::Op::Equal => {
                    let (rhs, lhs) = self.binary_op();
                    let predicate  = llvm::LLVMRealPredicate::LLVMRealUEQ;

                    let result = llvm
                    ::core
                    ::LLVMBuildFCmp(builder, predicate,lhs, rhs, binary_cstr!("_eqcomp"));
                    self.push(result);
                }

                block::Op::DeclareVariable => {
                    let index = frame.read_byte() as usize;
                    let scope = frame.read_byte() as usize;

                    // TODO: allocate upfront or something other than this.
                    if self.compilation_context.variables.len() <= frame_index {
                        self.compilation_context.variables.push(vec![0 as *mut llvm::LLVMValue; 1024]);
                    }

                    let variable = &program
                        .scopes[scope]
                        .variables[index];

                    let type_ref      = llvm::core::LLVMInt32TypeInContext(self.context);
                    let variable_name = variable.name.value.as_ptr();

                    let variable = llvm
                    ::core
                    ::LLVMBuildAlloca(builder, type_ref, variable_name as *const _);

                    self.compilation_context.variables[frame_index][index] = variable;
                }

                block::Op::GetLocal => {
                    let index = frame.read_byte() as usize;
                    let value = frame.get_value(index, &self.stack);

                    self.push(value);
                }

                block::Op::SetLocal => {
                    let value = self.peek(0).clone();
                    let index = frame.read_byte() as usize;


                    let variable_ref = self.compilation_context.variables[frame_index][index];
                    llvm::core::LLVMBuildStore(builder, value, variable_ref);

                    frame.set_value(index, value, &mut self.stack);
                }

                Op::GetUpvalue => {
                    let scope_distance = frame.read_byte() as usize;
                    let index          = frame.read_byte() as usize;

                    let enclosing_scope = &frames[frame_index - scope_distance];
                    let value = enclosing_scope.get_value(index, &self.stack);

                    self.push(value);
                },

                Op::SetUpvalue => {
                    let scope_distance = frame.read_byte() as usize;
                    let index          = frame.read_byte() as usize;

                    let value = self.peek(0).clone();
                    let enclosing_scope = &mut frames[frame_index - scope_distance];

                    let variable_ref = self.compilation_context.variables[frame_index - scope_distance][index];
                    llvm::core::LLVMBuildStore(builder, value, variable_ref);
                    enclosing_scope.set_value(index, value, &mut self.stack);
                },

                // TODO: Run LLVMBuildRet here?
                block::Op::Return => {
                    let values_count = frame.read_byte();
                    let result       = self.pop();

                    for _ in 0..values_count {
                        self.pop();
                    }

                    if frames.is_empty() { break }
                    frames.pop();

                    llvm::core::LLVMBuildRet(builder, result);
                    self.push(result);
                }

                block::Op::Call => {
                    let scope_distance = frame.read_byte() as usize;
                    let index          = frame.read_byte() as usize;

                    let scope_index   = frame_index - scope_distance;
                    let function_info = self.compilation_context.info[scope_index][index].clone();

                    let ValueInfo::Function { mut info  } = function_info else {
                        panic!("Expected function info")
                    };

                    // Only compile the function on Op::Call since only at that point
                    // will we have the stack in the correct state to get all the required
                    // values.
                    if !info.compiled {
                        let entry_block = llvm
                        ::core
                        ::LLVMAppendBasicBlockInContext(self.context, info.function, info.name.as_ptr() as * const _);

                        let builder = llvm::core::LLVMCreateBuilderInContext(self.context);
                        llvm::core::LLVMPositionBuilderAtEnd(builder, entry_block);

                        // #horribleways
                        let mut inner_frames = vec!
                        [
                            FunctionContext::new(self.stack_top - info.arity, info.code.clone())
                        ];

                        self.walk_source(builder, module, program, &mut inner_frames);
                        info.compiled = true;
                    }

                    let mut args: Vec<llvm::prelude::LLVMValueRef> = (0..info.arity)
                        // .map(|i| llvm::core::LLVMBuildLoad2(builder, info.param_types[i], self.peek(i), binary_cstr!("_test")))
                        .map(|i| self.peek(i))
                        .collect();

                    llvm::core::LLVMBuildCall2
                    (
                        builder,
                        info.function_type,
                        info.function,
                        args.as_mut_ptr(),
                        info.arity as u32,
                        info.name.as_ptr() as *const _,
                    );

                    self.compilation_context.info[scope_index][index] = ValueInfo::Function { info };
                },

                _ => { frame.i += 1; }
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

    // TODO: how to handle differring types. Probably just don't check at all.
    // Hoping to do all the type checking earlier.
    fn binary_op(&mut self) -> (llvm::prelude::LLVMValueRef, llvm::prelude::LLVMValueRef)
    {
        let a = self.pop();
        let b = self.pop();
        (a, b)
    }

    unsafe fn build_function
    (
        &mut self,
        module: llvm::prelude::LLVMModuleRef,
        name: String,
        arity: usize,
        argument_type_names: Vec<Option<String>>,
        return_type_name: String,
        code: block::Block,
    ) -> FunctionCall
    {
        let return_type = match return_type_name.as_str() {
            "unit"   => llvm::core::LLVMVoidTypeInContext(self.context),
            "number" => llvm::core::LLVMInt32TypeInContext(self.context),
            _        => panic!() // TODO
        };

        let mut param_types: Vec<llvm::prelude::LLVMTypeRef> = argument_type_names
            .iter()
            .filter_map(Option::as_ref)
            .map(|arg| match arg.as_str() {
                "unit"   => llvm::core::LLVMVoidTypeInContext(self.context),
                "number" => llvm::core::LLVMInt32TypeInContext(self.context),
                _        => panic!(), // TODO
            }).collect();

        let function_type_ref = llvm
            ::core
            ::LLVMFunctionType(return_type, param_types.as_mut_ptr(), arity as u32, 0);
        let function_ref = llvm
            ::core
            ::LLVMAddFunction(module, name.as_ptr() as *const _, function_type_ref);

        FunctionCall {
            name,
            function: function_ref,
            function_type: function_type_ref,
            arity: arity,
            param_types,
            return_type: return_type,
            code,

            // TODO: not sure about this one.
            compiled: false,
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

            llvm::core::LLVMDisposeModule(self.modules[0]);
            llvm::core::LLVMContextDispose(self.context);
        }
    }
}

// TODO: cache this function call somewhere per function so it can be
// pulled and used per call.
// TODO: remove the clone. I hate cloning everything.
#[derive(Debug, Clone)]
pub struct FunctionCall
{
    pub name: String,
    pub function: llvm::prelude::LLVMValueRef,
    pub function_type: llvm::prelude::LLVMTypeRef,
    pub arity: usize,
    pub param_types: Vec<llvm::prelude::LLVMTypeRef>,
    pub return_type: llvm::prelude::LLVMTypeRef,
    pub code: block::Block,

    pub compiled: bool,
}
