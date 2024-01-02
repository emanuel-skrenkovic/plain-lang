// TODO: think about pulling this out into a separate project
// and removing the llvm dependency from the compiler project.

extern crate llvm_sys as llvm;

use macros::binary_cstr;

use crate::block;
use crate::compiler;

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

pub struct CompilationState
{
    // HashMap of variables per scope. Scope index is the key, variables are in the vec.
    // TODO: think of how to do indexing of variable refs.
    pub variables: Vec<Vec<llvm::prelude::LLVMValueRef>>,
    pub info: Vec<Vec<ValueInfo>>,
}

impl CompilationState
{
    pub fn new() -> Self
    {
        Self { variables: vec![], info: vec![] }
    }
}

#[derive(Clone, Debug)]
pub struct StackFrame
{
    pub position: usize,
    pub i: usize,
    pub block: block::Block
}

impl StackFrame
{
    #[must_use]
    pub fn new(position: usize, block: block::Block) -> StackFrame
    {
        StackFrame {
            position,
            i: 0,
            block
        }
    }

    pub fn get_value
    (
        &self,
        index: usize,
        stack: &[llvm::prelude::LLVMValueRef]
    ) -> llvm::prelude::LLVMValueRef
    {
        stack[index + self.position].clone()
    }

    pub fn set_value
    (
        &mut self,
        index: usize,
        value: llvm::prelude::LLVMValueRef,
        stack: &mut Vec<llvm::prelude::LLVMValueRef>
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

    pub fn move_forward(&mut self)
    {
        self.i += 1;
    }
}

pub struct Stack
{
    pub buffer: Vec<llvm::prelude::LLVMValueRef>,
    pub top: usize,
}

impl Stack
{
    pub fn new() -> Self
    {
        Self { buffer: Vec::with_capacity(1024), top: 0 }
    }

    pub fn peek(&self, distance: usize) -> llvm::prelude::LLVMValueRef
    {
        self.buffer[self.top - 1 - distance]
    }

    pub fn pop(&mut self) -> llvm::prelude::LLVMValueRef
    {
        assert!(!self.buffer.is_empty(), "Cannot pop empty stack.");
        let value = self.buffer.pop().unwrap();
        self.top -= 1;
        value
    }

    pub fn push(&mut self, value: llvm::prelude::LLVMValueRef)
    {
        self.buffer.push(value);
        self.top += 1;
    }

    // TODO: how to handle differring types. Probably just don't check at all.
    // Hoping to do all the type checking earlier.
    fn binary_op(&mut self) -> (llvm::prelude::LLVMValueRef, llvm::prelude::LLVMValueRef)
    {
        let a = self.pop();
        let b = self.pop();
        (a, b)
    }
}

#[derive(Debug)]
pub struct Current<'a>
{
    pub builder: llvm::prelude::LLVMBuilderRef,
    pub basic_block: llvm::prelude::LLVMBasicBlockRef,

    pub module: llvm::prelude::LLVMModuleRef,
    pub function: llvm::prelude::LLVMValueRef,

    pub frame_index: usize,
    pub frames: &'a mut Vec<StackFrame>,

    pub branch: Option<&'a mut Branch>,
}

impl <'a> Current<'a>
{
    pub fn current_frame(&self) -> &StackFrame
    {
        &self.frames[self.frame_index]
    }

    pub fn current_frame_mut(&mut self) -> &mut StackFrame
    {
        &mut self.frames[self.frame_index]
    }
}

pub struct Context
{
    pub llvm_ctx: llvm::prelude::LLVMContextRef,
    pub modules: Vec<llvm::prelude::LLVMModuleRef>,
    pub program: compiler::Program,
    pub compilation_state: CompilationState,
}

impl Context
{
    #[must_use]
    pub unsafe fn new(program: compiler::Program) -> Self
    {
        Self {
            llvm_ctx: llvm::core::LLVMContextCreate(),
            modules: Vec::with_capacity(1),
            program,
            compilation_state: CompilationState::new()
        }
    }
}

impl Drop for Context
{
    fn drop(&mut self)
    {
        unsafe {
            for module in self.modules.drain(..) {
                llvm::core::LLVMDumpModule(module);
                llvm::core::LLVMDisposeModule(module);
            }

            llvm::core::LLVMContextDispose(self.llvm_ctx);
        }
    }
}

type EvalOp = unsafe fn(&mut Context, &mut Stack, &mut Current);

// The index of the operation corresponds with the u8 value of
// the operation enum.
// The order must be preserved! Otherwise, funny things happen.
const OPERATIONS: [Option<EvalOp>; 25] =
[
    Some(op_pop),
    None,                      // OP True
    None,                      // OP False
    Some(op_not),
    Some(op_add),
    Some(op_subtract),
    Some(op_multiply),
    Some(op_divide),
    Some(op_constant),
    Some(op_equal),
    Some(op_less),
    Some(op_greater),
    Some(op_greater_equal),
    Some(op_less_equal),
    Some(op_declare_variable),
    Some(op_set_local),
    Some(op_get_local),
    Some(op_set_upvalue),
    Some(op_get_upvalue),
    None,                      // OP Frame
    Some(op_return),
    Some(op_jump),
    Some(op_cond_jump),
    Some(op_loop_jump),
    Some(op_call),
];

fn get_op(op: block::Op) -> Option<EvalOp>
{
    OPERATIONS[op as usize]
}

pub struct ProgramCompiler { }

impl ProgramCompiler
{
    pub unsafe fn compile(ctx: &mut Context)
    {
        let module = llvm::core::LLVMModuleCreateWithNameInContext(binary_cstr!("main"), ctx.llvm_ctx);
        ctx.modules.push(module);

        let main_function_type = llvm
            ::core
            ::LLVMFunctionType(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), std::ptr::null_mut(), 0, 0);

        // TODO: I should expect a main function defined, not implicitly define it myself.
        // That way the special case of main gets reduced to only checking for its presence.
        let main_function = llvm::core::LLVMAddFunction(module, binary_cstr!("main"), main_function_type);

        let entry_block = llvm
            ::core
            ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, main_function, binary_cstr!("entry"));

        let builder = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
        llvm::core::LLVMPositionBuilderAtEnd(builder, entry_block);

        let mut stack  = Stack::new();
        let mut frames = vec![StackFrame::new(stack.top, ctx.program.block.clone())];

        // TODO: remove
        ctx.compilation_state.variables.push(vec![0 as *mut llvm::LLVMValue; 1024]);

        loop {
            if frames.len() == 0 { break }

            let frame_index = frames.len() - 1;
            if frames[frame_index].i == frames[frame_index].block.code.len() {
                break;
            }

            let mut current = Current {
                builder,
                basic_block: entry_block,
                module,
                function: main_function,
                frame_index,
                frames: &mut frames,
                branch: None
            };
            let ip = current.current_frame_mut().read_byte();
            disassemble_instruction(ip);

            let Ok(operation) = ip.try_into() else {
                panic!("Could not parse operation '{}'.", ip);
            };

            match get_op(operation) {
                Some(op) => op(ctx, &mut stack, &mut current),
                None     => current.current_frame_mut().move_forward(),
            }
        }

        Self::add_printf(ctx, module, builder);

        let return_value = llvm
            ::core
            ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), 0, 0);
        llvm::core::LLVMBuildRet(builder, return_value);

        verify_module(module);

        output_module_bitcode(module).unwrap();
    }

    // TODO: REMOVE THIS! This is just for playing around.
    pub unsafe fn add_printf(ctx: &mut Context, module: llvm::prelude::LLVMModuleRef, builder: llvm::prelude::LLVMBuilderRef)
    {
        let a = ctx.compilation_state.variables[0][0];

        let a_value = llvm::core::LLVMBuildLoad2(builder, llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), a, binary_cstr!("a"));

        let global_format_str = llvm::core::LLVMBuildGlobalStringPtr(
            builder,
            binary_cstr!("%d\n"),
            binary_cstr!("format_str"),
        );

        let printf_type = llvm
            ::core
            ::LLVMFunctionType
            (
                llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx),
                [llvm::core::LLVMPointerType(llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx), 0)].as_mut_ptr(),
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
}

pub struct FunctionCompiler { }

impl FunctionCompiler
{
    pub unsafe fn compile
    (
        ctx: &mut Context,
        stack: &mut Stack,
        current: &Current,
        builder: llvm::prelude::LLVMBuilderRef,
        basic_block: llvm::prelude::LLVMBasicBlockRef,
        module: llvm::prelude::LLVMModuleRef,
        frames: &mut Vec<StackFrame>,
    )
    {
        // TODO: remove
        ctx.compilation_state.variables.push(vec![0 as *mut llvm::LLVMValue; 1024]);

        loop {
            if frames.len() == 0 { break }

            let frame_index = frames.len() - 1;
            if frames[frame_index].i == frames[frame_index].block.code.len() {
                break;
            }

            let mut current = Current {
                builder: builder,
                basic_block,
                module,
                function: current.function,
                frame_index,
                frames,
                branch: None
            };
            let ip = current.current_frame_mut().read_byte();
            disassemble_instruction(ip);

            let Ok(operation) = ip.try_into() else {
                panic!("Could not parse operation '{}'.", ip);
            };
            match get_op(operation) {
                Some(op) => op(ctx, stack, &mut current),
                None     => current.current_frame_mut().move_forward(),
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Branch
{
    pub cond: llvm::prelude::LLVMValueRef,

    pub then_block: Option<llvm::prelude::LLVMBasicBlockRef>,
    pub else_block: Option<llvm::prelude::LLVMBasicBlockRef>,

    pub exit_counter: Option<usize>,

    pub parent_builder: llvm::prelude::LLVMBuilderRef,
    pub parent_block: llvm::prelude::LLVMBasicBlockRef,
}

impl Branch
{
    #[must_use]
    pub fn new
    (
        cond: llvm::prelude::LLVMValueRef,
        parent_builder: llvm::prelude::LLVMBuilderRef,
        parent_block: llvm::prelude::LLVMBasicBlockRef,
    ) -> Self
    {
        Self {
            cond,

            then_block: None,
            else_block: None,

            exit_counter: None,

            parent_builder,
            parent_block
        }
    }
}

pub fn write_branch(ctx: &Context, stack: &mut Stack, current: &Current, branch: Branch)
    -> Result<llvm::prelude::LLVMValueRef, String>
{
    let Some(then_branch) = branch.then_block else {
        return Err("Then block not built.".to_string())
    };

    let Some(else_branch) = branch.else_block else {
        return Err("Else block not built.".to_string())
    };

    unsafe {
        let end_block = llvm::core::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_branchend"));

        llvm::core::LLVMPositionBuilderAtEnd(current.builder, branch.parent_block);
        llvm::core::LLVMBuildCondBr(current.builder, branch.cond, then_branch, else_branch);

        // After the code for each block is built, write the BR instructions for each block.
        llvm::core::LLVMPositionBuilderAtEnd(current.builder, then_branch);
        llvm::core::LLVMBuildBr(current.builder, end_block);

        llvm::core::LLVMPositionBuilderAtEnd(current.builder, else_branch);
        llvm::core::LLVMBuildBr(current.builder, end_block);

        let (else_result, if_result) = stack.binary_op();
        let mut incoming_values = [if_result, else_result];
        let mut incoming_blocks = [then_branch, else_branch];

        // Move the builder back to the parent block.
        llvm::core::LLVMPositionBuilderAtEnd(current.builder, end_block);

        let phi_node = llvm
            ::core
            ::LLVMBuildPhi(current.builder, llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), binary_cstr!("_branchphi"));

        llvm::core::LLVMAddIncoming(phi_node, incoming_values.as_mut_ptr(), incoming_blocks.as_mut_ptr(), 2);

        // As the result of the if/else, move the incoming values into a phi node and return it
        // as the result of the branching expression.
        Ok(phi_node)
    }
}

pub struct BranchCompiler { }

impl BranchCompiler
{
    // Look ma, I'm doing inheritance!
    const BRANCH_OPERATIONS: [Option<EvalOp>; 25] =
    [
        Some(op_pop),
        None,                      // OP True
        None,                      // OP False
        Some(op_not),
        Some(op_add),
        Some(op_subtract),
        Some(op_multiply),
        Some(op_divide),
        Some(op_constant),
        Some(op_equal),
        Some(op_less),
        Some(op_greater),
        Some(op_greater_equal),
        Some(op_less_equal),
        Some(op_declare_variable),
        Some(op_set_local),
        Some(op_get_local),
        Some(op_set_upvalue),
        Some(op_get_upvalue),
        None,                      // OP Frame
        Some(op_return),
        Some(BranchCompiler::op_jump),
        Some(op_cond_jump),
        Some(op_loop_jump),
        Some(op_call),
    ];

    pub unsafe fn compile
    (
        ctx: &mut Context,
        stack: &mut Stack,
        current: &Current,
        mut branch: Branch,
        frames: &mut Vec<StackFrame>,
    ) -> Option<Branch>
    {
        let then_block = llvm
            ::core
            ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_condjumpbb"));
        llvm::core::LLVMPositionBuilderAtEnd(current.builder, then_block);
        branch.then_block = Some(then_block);

        loop {
            let frame_index = frames.len() - 1;
            if frames[frame_index].i == frames[frame_index].block.code.len() {
                break None
            }

            // This is the problem!
            // Branch gets replaced by the initial one on every instruction.
            let mut current = Current {
                builder: current.builder,
                basic_block: then_block,
                module: current.module,
                function: current.function,
                frame_index,
                frames,
                branch: Some(&mut branch)
            };
            let ip = current.current_frame_mut().read_byte();
            disassemble_instruction(ip);

            let Ok(operation) = TryInto::<block::Op>::try_into(ip) else {
                panic!("Could not parse operation '{}'.", ip);
            };

            match BranchCompiler::BRANCH_OPERATIONS[operation as usize] {
                Some(op) => op(ctx, stack, &mut current),
                None     => current.current_frame_mut().move_forward(),
            }

            if let Some(exit_counter) = current.branch.as_ref().unwrap().exit_counter {
                if exit_counter == 0 { current.frames.pop(); }
                else                 { current.branch.as_mut().unwrap().exit_counter = Some(exit_counter - 1); }
            }

            if current.frames.len() == 0 {
                return Some(branch)
            }
        }
    }

    // Replace Op::Jump and friends to be able to keep branching state while
    // keeping track of the state and compiling all the branch paths.
    pub unsafe fn op_jump(ctx: &mut Context, _stack: &mut Stack, current: &mut Current)
    {
        let frame = current.current_frame_mut();
        let jump  = frame.read_byte() as usize;


        let Some(branch) = current.branch.as_mut() else {
            panic!("Branch not initiated.")
        };

        branch.exit_counter = Some(jump - 1);

        let else_block = llvm
            ::core
            ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_jumpbb"));
        llvm::core::LLVMPositionBuilderAtEnd(current.builder, else_block);
        branch.else_block = Some(else_block);
    }
}

pub unsafe fn op_pop(_ctx: &mut Context, stack: &mut Stack, _current: &mut Current)
{
    stack.pop();
}

pub unsafe fn op_add(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();

    let result = llvm
        ::core
        ::LLVMBuildAdd(current.builder, lhs, rhs, binary_cstr!("_add_result"));

    stack.push(result);
}

pub unsafe fn op_subtract(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();

    let result = llvm
        ::core
        ::LLVMBuildSub(current.builder, lhs, rhs, binary_cstr!("_sub_result"));

    stack.push(result);
}

pub unsafe fn op_multiply(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();

    let result = llvm
        ::core
        ::LLVMBuildMul(current.builder, lhs, rhs, binary_cstr!("_mul_result"));

    stack.push(result);
}

pub unsafe fn op_divide(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();

    let result = llvm
        ::core
        ::LLVMBuildSDiv(current.builder, lhs, rhs, binary_cstr!("_div_result"));

    stack.push(result);
}

pub unsafe fn op_equal(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();
    let predicate  = llvm::LLVMRealPredicate::LLVMRealUEQ;

    let result = llvm
        ::core
        ::LLVMBuildFCmp(current.builder, predicate,lhs, rhs, binary_cstr!("_eqcomp"));

    stack.push(result);
}

pub unsafe fn op_less(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();
    let predicate  = llvm::LLVMRealPredicate::LLVMRealOLT;

    let result = llvm
        ::core
        ::LLVMBuildFCmp(current.builder, predicate,lhs, rhs, binary_cstr!("_ltcomp"));

    stack.push(result);
}

pub unsafe fn op_greater(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();
    let predicate  = llvm::LLVMRealPredicate::LLVMRealOGT;

    let result = llvm
        ::core
        ::LLVMBuildFCmp(current.builder, predicate,lhs, rhs, binary_cstr!("_gtcomp"));

    stack.push(result);
}

pub unsafe fn op_less_equal(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();
    let predicate  = llvm::LLVMRealPredicate::LLVMRealOLE;

    let result = llvm
        ::core
        ::LLVMBuildFCmp(current.builder, predicate,lhs, rhs, binary_cstr!("_lecomp"));

    stack.push(result);
}

pub unsafe fn op_greater_equal(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();
    let predicate  = llvm::LLVMRealPredicate::LLVMRealOGE;

    let result = llvm
        ::core
        ::LLVMBuildFCmp(current.builder, predicate,lhs, rhs, binary_cstr!("_gecomp"));

    stack.push(result);
}

pub unsafe fn op_not(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let value = stack.pop();
    let result = llvm::core::LLVMBuildNeg(current.builder, value, binary_cstr!("_neg"));
    stack.push(result);
}

pub unsafe fn op_constant(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame = current.current_frame_mut();
    let index = frame.read_byte();
    let value = frame.read_constant(index as usize);

    let value_ref = match value {
        block::Value::Number { val } =>
            llvm
            ::core
            ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), val as u64, 1),

        block::Value::Bool { val } =>
            llvm
            ::core
            ::LLVMConstInt(llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx), if val { 1 } else { 0 }, 0),

        block::Value::String { val } =>
            llvm
            ::core
            ::LLVMConstString(val.as_ptr() as *const _, val.len() as u32, 1),

        block::Value::Function { name, arity, argument_type_names, return_type_name, closure } => {
            let function_call = FunctionCall::build(ctx.llvm_ctx, current.module, name, arity, argument_type_names, return_type_name, closure.code);
            let function_ref  = function_call.function;

            // TODO: fix this.
            // if !self.compilation_context.info.len() < program.current_scope - 1 {
            ctx.compilation_state.info.push(vec![]);
            // }

            ctx.compilation_state
                .info[current.frame_index]
                .push(ValueInfo::Function { info: function_call });

            function_ref
        }

        _ => panic!() // TODO: Result I guess. :/
    };

    stack.push(value_ref);
}

pub unsafe fn op_get_local(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame = current.current_frame_mut();
    let index = frame.read_byte() as usize;
    let value = frame.get_value(index, &stack.buffer);

    stack.push(value);
}

pub unsafe fn op_declare_variable(ctx: &mut Context, _stack: &mut Stack, current: &mut Current)
{
    let frame = current.current_frame_mut();
    let index = frame.read_byte() as usize;
    let scope = frame.read_byte() as usize;

    // TODO: allocate upfront or something other than this.
    if ctx.compilation_state.variables.len() <= current.frame_index {
        ctx.compilation_state.variables.push(vec![0 as *mut llvm::LLVMValue; 1024]);
    }

    let variable = &ctx.program
        .scopes[scope]
        .variables[index];

    let type_ref      = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let variable_name = variable.name.value.as_ptr();

    let variable = llvm
        ::core
        ::LLVMBuildAlloca(current.builder, type_ref, variable_name as *const _);

    ctx.compilation_state.variables[current.frame_index][index] = variable;
}

pub unsafe fn op_set_local(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let value = stack.peek(0).clone();

    // These two are copied first because 'current' is borrowed to
    // get the frame.
    let frame_index = current.frame_index;
    let builder     = current.builder;

    let frame = current.current_frame_mut();
    let index = frame.read_byte() as usize;

    let variable_ref = ctx.compilation_state.variables[frame_index][index];
    llvm::core::LLVMBuildStore(builder, value, variable_ref);

    frame.set_value(index, value, &mut stack.buffer);
}

pub unsafe fn op_get_upvalue(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame          = current.current_frame_mut();
    let scope_distance = frame.read_byte() as usize;
    let index          = frame.read_byte() as usize;

    let enclosing_scope = &current.frames[current.frame_index - scope_distance];
    let value = enclosing_scope.get_value(index, &stack.buffer);

    stack.push(value);
}

pub unsafe fn op_set_upvalue(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame          = current.current_frame_mut();
    let scope_distance = frame.read_byte() as usize;
    let index          = frame.read_byte() as usize;

    let value = stack.peek(0).clone();

    let scope = current.frame_index - scope_distance;

    let enclosing_scope = &mut current.frames[scope];
    let variable_ref = ctx.compilation_state.variables[scope][index];
    llvm::core::LLVMBuildStore(current.builder, value, variable_ref);

    enclosing_scope.set_value(index, value, &mut stack.buffer);
}

pub unsafe fn op_return(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame  = current.current_frame_mut();
    let _      = frame.read_byte();
    let result = stack.pop();

    // TODO: think about this!
    // We don't need to pop the params in the LLVM implementation
    // because they will be values controlled by the outer scope.
    // This feels wrong in some way though.
    // for _ in 0..values_count {
    //     self.pop();
    // }

    if current.frames.is_empty() { return }
    current.frames.pop();

    llvm::core::LLVMBuildRet(current.builder, result);

    stack.push(result);
}

pub unsafe fn op_jump(ctx: &mut Context, _stack: &mut Stack, current: &mut Current)
{
    // TODO: I think  I need to do both - jump the stack while
    // evaluating my bytecode and create a jump in the LLVM IR.
    let frame = current.current_frame_mut();
    let jump = frame.read_byte() as usize;
    frame.i += jump;

    // TODO: This is not finished.
    let _block = llvm
        ::core
        ::LLVMCreateBasicBlockInContext(ctx.llvm_ctx, binary_cstr!("_jumpbb"));
}

/*

extern crate llvm_sys;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use std::ffi::{CString, CStr};

fn main() {
    unsafe {
        // Initialize LLVM
        LLVMInitializeCore(LLVMGetGlobalPassRegistry());

        // Create a context
        let context = LLVMContextCreate();

        // Create a module
        let module_name = CString::new("example_module").expect("Failed to create CString");
        let module = LLVMModuleCreateWithName(module_name.as_ptr());

        // Create a function in the module
        let function_type = LLVMFunctionType(LLVMInt32TypeInContext(context), std::ptr::null_mut(), 0, 0);
        let function_name = CString::new("main").expect("Failed to create CString");
        let function = LLVMAddFunction(module, function_name.as_ptr(), function_type);

        // Create basic blocks for if and else
        let entry_block = LLVMAppendBasicBlockInContext(context, function, c_str!("entry").as_ptr());
        let if_block = LLVMAppendBasicBlockInContext(context, function, c_str!("if_block").as_ptr());
        let else_block = LLVMAppendBasicBlockInContext(context, function, c_str!("else_block").as_ptr());
        let end_block = LLVMAppendBasicBlockInContext(context, function, c_str!("end").as_ptr());

        // Create a builder to build instructions
        let builder = LLVMCreateBuilderInContext(context);

        // Set the builder to start in the entry block
        LLVMPositionBuilderAtEnd(builder, entry_block);

        // Define a boolean variable
        let condition = LLVMConstInt(LLVMInt1TypeInContext(context), 1, 0);

        // Create a branch instruction based on the condition
        LLVMBuildCondBr(builder, condition, if_block, else_block);

        // Set the builder to start in the if block
        LLVMPositionBuilderAtEnd(builder, if_block);

        // Add instructions for the "if" branch
        let if_result = LLVMConstInt(LLVMInt32TypeInContext(context), 42, 0);
        LLVMBuildBr(builder, end_block);

        // Set the builder to start in the else block
        LLVMPositionBuilderAtEnd(builder, else_block);

        // Add instructions for the "else" branch
        let else_result = LLVMConstInt(LLVMInt32TypeInContext(context), 0, 0);
        LLVMBuildBr(builder, end_block);

        // Set the builder to start in the end block
        LLVMPositionBuilderAtEnd(builder, end_block);

        // Create a phi node to merge results from if and else blocks
        let phi_node = LLVMBuildPhi(builder, LLVMInt32TypeInContext(context), c_str!("result").as_ptr());

        // Add incoming values for the phi node
        let incoming_values = [if_result, else_result];
        let incoming_blocks = [if_block, else_block];
        LLVMAddIncoming(phi_node, incoming_values.as_ptr(), incoming_blocks.as_ptr(), 2);

        // Return the result of the phi node
        LLVMBuildRet(builder, phi_node);

        // Verify the module
        let mut error_str = std::ptr::null_mut();
        if LLVMVerifyModule(module, LLVMVerifierFailureAction, &mut error_str) != 0 {
            let error_message = CStr::from_ptr(error_str).to_string_lossy().into_owned();
            eprintln!("LLVM verification failed: {}", error_message);
            LLVMDisposeMessage(error_str);
        }

        // Clean up resources
        LLVMDisposeBuilder(builder);
        LLVMDisposeModule(module);
        LLVMContextDispose(context);
    }
}
 */

pub unsafe fn op_cond_jump(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame     = current.current_frame_mut();
    let _jump     = frame.read_byte() as usize;
    let condition = stack.pop();

    // For some reason, I store the booleans a i8, so, to work with
    // llvm conditions, I have to convert them to i1.
    let i1_condition = llvm::core::LLVMBuildTrunc
    (
        current.builder,
        condition,
        llvm::core::LLVMInt1TypeInContext(ctx.llvm_ctx),
        binary_cstr!("_bool_convert")
    );

    let branch = Branch::new(i1_condition, current.builder, current.basic_block);

    // TODO: I think that this **cannot** be a clone.
    // Seems like it should mutate the existing frame in order to avoid repeating code.
    let mut inner_frames = vec![current.frames[current.frame_index].clone()];

    let compiled_branch = BranchCompiler::compile(ctx, stack, &current, branch, &mut inner_frames);
    let Some(compiled_branch) = compiled_branch else {
        panic!("Failed to build branch code.")
    };

    // TODO: this feels off. Structure it better.
    let Ok(value) = write_branch(&ctx, stack, &current, compiled_branch) else {
        panic!("Failed to write branch code");
    };

    stack.push(value);
}

pub unsafe fn op_loop_jump(_ctx: &mut Context, _stack: &mut Stack, current: &mut Current)
{
    let frame = current.current_frame_mut();
    let jump  = frame.read_byte() as usize;
    frame.i -= jump;
}

pub unsafe fn op_call(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame          = current.current_frame_mut();
    let scope_distance = frame.read_byte() as usize;
    let index          = frame.read_byte() as usize;

    let scope         = current.frame_index - scope_distance;
    let function_info = ctx.compilation_state.info[scope][index].clone();

    let ValueInfo::Function { mut info  } = function_info else {
        panic!("Expected function info")
    };

    // Only compile the function on Op::Call since only at that point
    // will we have the stack in the correct state to get all the required
    // values.
    if !info.compiled {
        let entry_block = llvm
            ::core
            ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, info.function, info.name.as_ptr() as * const _);

        let function_builder = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
        llvm::core::LLVMPositionBuilderAtEnd(function_builder, entry_block);

        // #horribleways
        let mut inner_frames = vec!
        [
            StackFrame::new(stack.top - info.arity, info.code.clone())
        ];

        // TODO: find a better place for this.
        // I'm still deciding what should be whose responsibility, and the Current
        // struct seems like a terrible solution.
        current.function = info.function;

        FunctionCompiler::compile(ctx, stack, &current, function_builder, entry_block, current.module, &mut inner_frames);
        info.compiled = true;
    }

    let mut args: Vec<llvm::prelude::LLVMValueRef> = (0..info.arity)
        .map(|i| stack.peek(i))
        .collect();

    llvm::core::LLVMBuildCall2
    (
        current.builder,
        info.function_type,
        info.function,
        args.as_mut_ptr(),
        info.arity as u32,
        info.name.as_ptr() as *const _,
    );

    ctx.compilation_state.info[scope][index] = ValueInfo::Function { info };
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

impl FunctionCall
{
    unsafe fn build
    (
        context_ref: llvm::prelude::LLVMContextRef,
        module: llvm::prelude::LLVMModuleRef,
        name: String,
        arity: usize,
        argument_type_names: Vec<Option<String>>,
        return_type_name: String,
        code: block::Block,
    ) -> FunctionCall
    {
        let return_type = match return_type_name.as_str() {
            "unit"   => llvm::core::LLVMVoidTypeInContext(context_ref),
            "number" => llvm::core::LLVMInt32TypeInContext(context_ref),
            "bool"   => llvm::core::LLVMInt8TypeInContext(context_ref),
            _        => panic!("Unknown return type.") // TODO
        };

        let mut param_types: Vec<llvm::prelude::LLVMTypeRef> = argument_type_names
            .iter()
            .filter_map(Option::as_ref)
            .map(|arg| match arg.as_str() {
                "unit"   => llvm::core::LLVMVoidTypeInContext(context_ref),
                "number" => llvm::core::LLVMInt32TypeInContext(context_ref),
                "bool"   => llvm::core::LLVMInt8TypeInContext(context_ref),
                _        => panic!("Unknown param type."), // TODO
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
            compiled: false,
        }
    }
}

// TODO: remove - this is just for janky testing.
pub unsafe fn output_module_bitcode(module: llvm::prelude::LLVMModuleRef) -> Result<(), String>
{
    let result = llvm
        ::bit_writer
        ::LLVMWriteBitcodeToFile(module, binary_cstr!("bin/a.bc"));
    if result != 0 {
        return Err("Failed to output bitcode.".to_string())
    }
    Ok(())
}

pub unsafe fn verify_module(module: llvm::prelude::LLVMModuleRef)
{
    let failure_action = llvm
    ::analysis
    ::LLVMVerifierFailureAction::LLVMPrintMessageAction;

    let mut error: *mut i8 = std::ptr::null_mut();
    if llvm::analysis::LLVMVerifyModule(module, failure_action, &mut error) != 0 {
        print_module(module);
        eprintln!("Analysis error: {}", std::ffi::CStr::from_ptr(error).to_string_lossy());
        llvm::core::LLVMDisposeMessage(error);
    }
}

fn print_module(module: llvm::prelude::LLVMModuleRef)
{
    unsafe {
        let module_text = llvm::core::LLVMPrintModuleToString(module);
        println!("MODULE: \n{}", std::ffi::CStr::from_ptr(module_text).to_str().unwrap());
    }
}

fn disassemble_instruction(instruction: u8)
{
    print!("OP ");

    if let Ok(i) = instruction.try_into() {
        match i {
            block::Op::Pop             => print_simple_op("POP"),
            block::Op::True            => print_simple_op("TRUE"),
            block::Op::False           => print_simple_op("FALSE"),
            block::Op::Not             => print_simple_op("NOT"),
            block::Op::Add             => print_simple_op("ADD"),
            block::Op::Subtract        => print_simple_op("SUBTRACT"),
            block::Op::Multiply        => print_simple_op("MULTIPLY"),
            block::Op::Divide          => print_simple_op("DIVIDE"),
            block::Op::Constant        => print_simple_op("CONSTANT"),
            block::Op::Equal           => print_simple_op("EQUAL"),
            block::Op::Less            => print_simple_op("LESS"),
            block::Op::Greater         => print_simple_op("GREATER"),
            block::Op::DeclareVariable => print_simple_op("DECLARE_VARIABLE"),
            block::Op::GetLocal        => print_simple_op("GET_LOCAL"),
            block::Op::SetLocal        => print_simple_op("SET_LOCAL"),
            block::Op::GetUpvalue      => print_simple_op("GET_UPVALUE"),
            block::Op::SetUpvalue      => print_simple_op("SET_UPVALUE"),
            block::Op::Frame           => print_simple_op("FRAME"),
            block::Op::Return          => print_simple_op("RETURN"),
            block::Op::Jump            => print_simple_op("JUMP"),
            block::Op::CondJump        => print_simple_op("COND_JUMP"),
            block::Op::LoopJump        => print_simple_op("LOOP_JUMP"),
            block::Op::Call            => print_simple_op("CALL "),
            _ => { }
        }
    }
}

fn print_simple_op(name: &str)
{
    println!("{name:<width$} |", name=name, width=20);
}
