// TODO: think about pulling this out into a separate project
// and removing the llvm dependency from the compiler project.

extern crate llvm_sys as llvm;

use macros::binary_cstr;

use crate::block;
use crate::compiler;

#[derive(Clone, Debug)]
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
    pub variable_types: Vec<Vec<llvm::prelude::LLVMTypeRef>>,
    pub info: Vec<Vec<ValueInfo>>,
}

impl CompilationState
{
    const DEFAULT_CAP: usize = 1024;

    pub fn new() -> Self
    {
        Self {
            variables: Vec::with_capacity(Self::DEFAULT_CAP),
            variable_types: Vec::with_capacity(Self::DEFAULT_CAP),
            info: Vec::with_capacity(Self::DEFAULT_CAP),
        }
    }
}

#[derive(Debug)]
pub struct FramePosition<'a>
{
    pub frame_index: usize,
    pub frames: &'a mut Vec<StackFrame>,
}

// pub pub pub pub pub
// Information wants to be free, maaaan.
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

#[derive(Clone)]
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

    fn binary_op(&mut self) -> (llvm::prelude::LLVMValueRef, llvm::prelude::LLVMValueRef)
    {
        let a = self.pop();
        let b = self.pop();
        (a, b)
    }
}

#[derive(Debug)]
pub enum ContextSpecific<'a>
{
    FunctionSpecific { data: &'a FunctionCall },
    ControlFlowSpecific { data: &'a mut ControlFlow2 },
    BranchSpecific { data: &'a mut ControlFlow },
    LoopSpecific { data: &'a mut Loop },
}

// TODO: Think about having Current as an enum so it can
// have a different value depending on the compilation context.
// E.g. During branch compilation, it could be Current::Branch
// which would contain branch-specific information.
#[derive(Debug)]
pub struct Current<'a, 'frame>
{
    pub builder: llvm::prelude::LLVMBuilderRef,
    pub basic_block: llvm::prelude::LLVMBasicBlockRef,

    pub module: llvm::prelude::LLVMModuleRef,
    pub function: llvm::prelude::LLVMValueRef,

    pub frame_position: &'frame mut FramePosition<'a>,

    pub context_specific: ContextSpecific<'frame>,
}

impl <'a, 'b> Current<'a, 'b>
{
    pub fn current_frame(&self) -> &StackFrame
    {
        &self.frame_position.frames[self.frame_position.frame_index]
    }

    pub fn current_frame_mut(&mut self) -> &mut StackFrame
    {
        &mut self.frame_position.frames[self.frame_position.frame_index]
    }

    pub fn push_frame(&mut self, frame: StackFrame)
    {
        self.frame_position.frames.push(frame);
        self.frame_position.frame_index += 1;
    }

    pub fn pop_frame(&mut self)
    {
        self.frame_position.frames.pop();
        self.frame_position.frame_index -= 1;
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
const OPERATIONS: [Option<EvalOp>; 27] =
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
    Some(op_loop),
    Some(op_loop_cond_jump),
    Some(op_loop_jump),
    Some(op_call),
];

fn get_op(op: block::Op) -> Option<EvalOp>
{
    OPERATIONS[op as usize]
}

pub unsafe fn compile(ctx: &mut Context) -> *mut llvm::LLVMModule
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
    ctx.compilation_state.variable_types.push(vec![0 as *mut llvm::LLVMType; 1024]);

    let mut frame_position = FramePosition {
        frame_index: 0,
        frames: &mut frames,
    };

    // TODO: this is a temporary mess.
    let main_function_call = FunctionCall::build
    (
        ctx.llvm_ctx,
        module,
        "main".to_string(),
        0,
        vec![],
        "unit".to_string(),
        ctx.program.block.clone(),
    );

    let mut current = Current {
        builder,
        basic_block: entry_block,
        module,
        function: main_function,
        frame_position: &mut frame_position,
        context_specific: ContextSpecific::FunctionSpecific { data: &main_function_call },
    };

    loop {
        if current.frame_position.frames.len() == 0 { break }

        let ip = current.current_frame_mut().read_byte();
        disassemble_instruction(ip);

        let Ok(operation) = ip.try_into() else {
            panic!("Could not parse operation '{}'.", ip);
        };

        match get_op(operation) {
            Some(op) => op(ctx, &mut stack, &mut current),
            None     => current.current_frame_mut().move_forward()
        }

        if current.frame_position.frames.len() == 0 { break }
        if current.current_frame().i == current.current_frame().block.code.len() {
            break
        }
    }

    add_printf(ctx, module, builder);

    let return_value = llvm
        ::core
        ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), 0, 0);
    llvm::core::LLVMBuildRet(builder, return_value);

    verify_module(module);
    module
}

pub unsafe fn compile_function(ctx: &mut Context, stack: &mut Stack, mut current: Current)
{
    // TODO: remove
    ctx.compilation_state.variables.push(vec![0 as *mut llvm::LLVMValue; 1024]);
    ctx.compilation_state.variable_types.push(vec![0 as *mut llvm::LLVMType; 1024]);

    loop {
        if current.frame_position.frames.len() == 0 { break }

        let ip = current.current_frame_mut().read_byte();
        disassemble_instruction(ip);

        let Ok(operation) = ip.try_into() else {
            panic!("Could not parse operation '{}'.", ip);
        };

        if let block::Op::Return = operation {
            op_return(ctx, stack, &mut current);
            break;
        }
        match get_op(operation) {
            Some(op) => op(ctx, stack, &mut current),
            None     => current.current_frame_mut().move_forward(),
        }

        if current.current_frame().i == current.current_frame().block.code.len() {
            break
        }
    }
}

// TODO: the whole Branch thing is kinda whacky.
// Think about structuring it better.
#[derive(Clone, Debug)]
pub struct Branch
{
    pub cond: llvm::prelude::LLVMValueRef,

    pub then_block: Option<llvm::prelude::LLVMBasicBlockRef>,
    pub else_block: Option<llvm::prelude::LLVMBasicBlockRef>,

    pub end_counter: usize,

    pub parent_builder: llvm::prelude::LLVMBuilderRef,
    pub parent_block: llvm::prelude::LLVMBasicBlockRef,
}

impl Branch
{
    #[must_use]
    pub fn new
    (
        end_counter: usize,
        cond: llvm::prelude::LLVMValueRef,
        parent_builder: llvm::prelude::LLVMBuilderRef,
        parent_block: llvm::prelude::LLVMBasicBlockRef,
    ) -> Self
    {
        Self {
            cond,

            then_block: None,
            else_block: None,

            end_counter,

            parent_builder,
            parent_block
        }
    }
}

pub unsafe fn write_branch2
(
    ctx: &Context, stack: &mut Stack, current: &Current, control_flow: ControlFlow2
) -> Result<llvm::prelude::LLVMValueRef, String>
{
    todo!()
}

pub unsafe fn write_branch(ctx: &Context, stack: &mut Stack, current: &Current, control_flow: ControlFlow)
    -> Result<llvm::prelude::LLVMValueRef, String>
{
    let results_count = control_flow.inner_blocks.len();

    let mut results      = Vec::with_capacity(results_count);
    let mut result_types = Vec::with_capacity(results_count);

    for _ in 0..control_flow.inner_blocks.len() {
        let result      = stack.pop();
        let result_type = llvm::core::LLVMTypeOf(result);

        results.push(result);
        result_types.push(result_type);
    }

    // Need to reverse to make it the correct order since we
    // popped the values from a stack.
    results.reverse();
    result_types.reverse();

    let mut is_result_primitive = false;
    let mut primitive_type      = std::ptr::null_mut();

    let mut result_types_primitive = Vec::with_capacity(results_count);

    for t in result_types {
        let result_type_primitive = is_type_primitive(ctx.llvm_ctx, t);
        result_types_primitive.push(result_type_primitive);

        if !is_result_primitive && result_type_primitive {
            is_result_primitive = true;
            primitive_type      = t;
        }
    }

    let mut incoming_values = Vec::with_capacity(results_count);
    let mut incoming_blocks = Vec::with_capacity(results_count);

    // In case of returning a primitive from a branching expression, there is a possibility of one (or two) of the
    // blocks being a reference to a variable. In that case, a dereference is needed so the return types
    // of branches match (e.g. (i32, i32) instead of (ptr, i32)). Deref is an instruction in itself, and
    // the phi instruction needs to be at the top of the block - this means that we have to set up an
    // intermediate block to dereference the values and then move to the end block after that with consolidated branches.

    for i in 0..results_count {
        if is_result_primitive && !result_types_primitive[i] {
            // Create a block for the deref.
            let result_block = llvm
                ::core
                ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_branchresult"));
            llvm::core::LLVMPositionBuilderAtEnd(current.builder, control_flow.inner_blocks[i].block);
            llvm::core::LLVMBuildBr(current.builder, result_block);

            llvm::core::LLVMPositionBuilderAtEnd(current.builder, result_block);

            // Then write the deref and push the block as the returning block.
            let deref = deref_if_ptr(ctx.llvm_ctx, current.builder, results[i], primitive_type);
            incoming_values.push(deref);
            incoming_blocks.push(result_block);
        } else {
            incoming_values.push(results[i]);
            incoming_blocks.push(control_flow.inner_blocks[i].block);
        }
    }

    let end_block = llvm
        ::core
        ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_branchend"));
    llvm::core::LLVMPositionBuilderAtEnd(current.builder, end_block);

    for i in 1..control_flow.inner_blocks.len() {
        let previous_block = &control_flow.inner_blocks[i- 1];
        let current_block  = &control_flow.inner_blocks[i];

        if let Some(cond) = current_block.cond {
            llvm::core::LLVMPositionBuilderAtEnd(current.builder, previous_block.parent_block);
            llvm::core::LLVMBuildCondBr(current.builder, cond, previous_block.block, current_block.block);
        } else {
            if let Some(previous_cond) = previous_block.cond {
                llvm::core::LLVMPositionBuilderAtEnd(current.builder, previous_block.parent_block);
                llvm::core::LLVMBuildCondBr(current.builder, previous_cond, previous_block.block, current_block.block);
            }

            llvm::core::LLVMPositionBuilderAtEnd(current.builder, previous_block.block);
            llvm::core::LLVMBuildBr(current.builder, end_block);
        }
    }

    let Some(last) = control_flow.inner_blocks.last() else {
        return Err("Failed to get last control flow block.".to_string())
    };
    llvm::core::LLVMPositionBuilderAtEnd(current.builder, last.block);
    llvm::core::LLVMBuildBr(current.builder, end_block);

    let count = incoming_values.len() as u32;

    let incoming_values = incoming_values.as_mut_ptr();
    let incoming_blocks = incoming_blocks.as_mut_ptr();

    llvm::core::LLVMPositionBuilderAtEnd(current.builder, end_block);
    let phi_node = llvm::core::LLVMBuildPhi(current.builder, primitive_type, binary_cstr!("_branchphi"));
    llvm::core::LLVMAddIncoming(phi_node, incoming_values, incoming_blocks, count);

    // As the result of the if/else, move the incoming values into a phi node and return it
    // as the result of the branching expression.
    Ok(phi_node)
}

pub unsafe fn compile_branch(ctx: &mut Context, stack: &mut Stack, mut current: Current)
{
    let mut diff = 0;

    loop {
        let before = current.current_frame().i;

        let ip = current.current_frame_mut().read_byte();
        disassemble_instruction(ip);

        let Ok(operation) = TryInto::<block::Op>::try_into(ip) else {
            panic!("Could not parse operation '{}'.", ip);
        };

        match get_op(operation) {
            Some(op) => op(ctx, stack, &mut current),
            None     => current.current_frame_mut().move_forward(),
        }

        if current.frame_position.frames.len() == 0 {
            break
        }

        if current.current_frame().i == current.current_frame().block.code.len() {
            break
        }

        let after = current.current_frame().i;
        diff += after - before;

        let ContextSpecific::ControlFlowSpecific { ref data } = current.context_specific else {
            panic!();
        };

        if diff >= data.end_counter {
            break
        }
    }
}

#[derive(Clone, Debug)]
pub struct ControlFlowBlock
{
    pub cond: Option<llvm::prelude::LLVMValueRef>,

    pub parent_block: llvm::prelude::LLVMBasicBlockRef,
    pub block: llvm::prelude::LLVMBasicBlockRef,
}

// Structure control flow as a directed cyclic graph.
#[derive(Clone, Debug)]
pub struct ControlFlowNode
{
    pub id: usize,
    pub parent: llvm::prelude::LLVMBasicBlockRef,

    pub cond: Option<llvm::prelude::LLVMValueRef>,
    pub value: Option<llvm::prelude::LLVMValueRef>,

    pub true_branch: Option<usize>,
    pub false_branch: Option<usize>,
}

impl ControlFlowNode
{
    fn is_branch(&self) -> bool
    {
        self.true_branch.is_some() && self.false_branch.is_some()
    }
}

#[derive(Clone, Debug)]
pub struct ControlFlow2
{
    node: ControlFlowNode,
    pub inner_nodes: Vec<ControlFlowNode>,
    pub end_counter: usize,
}

#[derive(Clone, Debug)]
pub struct ControlFlow
{
    pub parent_builder: llvm::prelude::LLVMBuilderRef,
    pub parent_block: llvm::prelude::LLVMBasicBlockRef,

    // Holds the initial condition of the control flow structure.
    // pub cond_basic_block: llvm::prelude::LLVMBasicBlockRef,
    // List of inner blocks, those could be e.g. else-if blocks
    // or the increment block in the for loop.
    // Op::Jump - push a block into this.
    // Op::LoopJump - pop a block from this.

    // I think I have to push the parent block here as well.
    pub inner_blocks: Vec<ControlFlowBlock>,

    // Holds the final block of the control flow structure.
    // Not sure if this is needed, or it can be created on-demand
    // after the inner_basic_blocks is empty.
    // pub end_basic_block: Option<llvm::prelude::LLVMBasicBlockRef>,

    pub end_counter: usize,
}

// TODO: have a similar structure to Branch where I first compose the pieces
// while looping through instructions, and then build the thing "around" parts.
#[derive(Debug)]
pub struct Loop
{
    // pub init_bb: llvm::prelude::LLVMBasicBlockRef,
    // pub cond_bb: llvm::prelude::LLVMBasicBlockRef,
    // pub loop_body_bb: llvm::prelude::LLVMBasicBlockRef,

    pub cond_bb: llvm::prelude::LLVMBasicBlockRef,
    pub next_bb: llvm::prelude::LLVMBasicBlockRef,
    pub end_bb: llvm::prelude::LLVMBasicBlockRef,
}

pub unsafe fn compile_loop(ctx: &mut Context, stack: &mut Stack, mut current: Current)
{
    // TODO: remove
    ctx.compilation_state.variables.push(vec![0 as *mut llvm::LLVMValue; 1024]);
    ctx.compilation_state.variable_types.push(vec![0 as *mut llvm::LLVMType; 1024]);

    loop {
        let ip = current.current_frame_mut().read_byte();
        disassemble_instruction(ip);

        let Ok(operation) = TryInto::<block::Op>::try_into(ip) else {
            panic!("Could not parse operation '{}'.", ip);
        };

        if let block::Op::LoopJump = operation {
            op_loop_jump(ctx, stack, &mut current);
            break
        }

        match get_op(operation) {
            Some(op) => op(ctx, stack, &mut current),
            None     => current.current_frame_mut().move_forward(),
        }

        if current.frame_position.frames.len() == 0 {
            break
        }

        if current.current_frame().i == current.current_frame().block.code.len() {
            break
        }
    }
}

pub unsafe fn op_pop(_ctx: &mut Context, stack: &mut Stack, _current: &mut Current)
{
    stack.pop();
}

pub unsafe fn op_add(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();

    // TODO: fix operand type being hardcoded.
    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let lhs = deref_if_ptr(ctx.llvm_ctx, current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(ctx.llvm_ctx, current.builder, rhs, expected_operand_type);

    let result = llvm
        ::core
        ::LLVMBuildAdd(current.builder, lhs, rhs, binary_cstr!("_add_result"));

    stack.push(result);
}

pub unsafe fn op_subtract(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();

    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let lhs = deref_if_ptr(ctx.llvm_ctx, current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(ctx.llvm_ctx, current.builder, rhs, expected_operand_type);

    let result = llvm
        ::core
        ::LLVMBuildSub(current.builder, lhs, rhs, binary_cstr!("_sub_result"));

    stack.push(result);
}

pub unsafe fn op_multiply(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();

    // TODO: fix operand type being hardcoded.
    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let lhs = deref_if_ptr(ctx.llvm_ctx, current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(ctx.llvm_ctx, current.builder, rhs, expected_operand_type);

    let result = llvm
        ::core
        ::LLVMBuildMul(current.builder, lhs, rhs, binary_cstr!("_mul_result"));

    stack.push(result);
}

pub unsafe fn op_divide(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();

    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let lhs = deref_if_ptr(ctx.llvm_ctx, current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(ctx.llvm_ctx, current.builder, rhs, expected_operand_type);

    let result = llvm
        ::core
        ::LLVMBuildSDiv(current.builder, lhs, rhs, binary_cstr!("_div_result"));

    stack.push(result);
}

pub unsafe fn op_equal(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();
    // let predicate  = llvm::LLVMRealPredicate::LLVMRealUEQ;

    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let lhs = deref_if_ptr(ctx.llvm_ctx, current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(ctx.llvm_ctx, current.builder, rhs, expected_operand_type);

    let predicate = llvm::LLVMIntPredicate::LLVMIntEQ;

    let result = llvm
        ::core
        ::LLVMBuildICmp(current.builder, predicate, lhs, rhs, binary_cstr!("_eqcomp"));

    stack.push(result);
}

pub unsafe fn op_less(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();

    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let lhs = deref_if_ptr(ctx.llvm_ctx, current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(ctx.llvm_ctx, current.builder, rhs, expected_operand_type);

    let predicate  = llvm::LLVMIntPredicate::LLVMIntSLT;

    let result = llvm
        ::core
        ::LLVMBuildICmp(current.builder, predicate, lhs, rhs, binary_cstr!("_ltcomp"));

    stack.push(result);
}

pub unsafe fn op_greater(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();

    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let lhs = deref_if_ptr(ctx.llvm_ctx, current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(ctx.llvm_ctx, current.builder, rhs, expected_operand_type);

    let predicate  = llvm::LLVMIntPredicate::LLVMIntSGT;

    let result = llvm
        ::core
        ::LLVMBuildICmp(current.builder, predicate,lhs, rhs, binary_cstr!("_gtcomp"));

    stack.push(result);
}

pub unsafe fn op_less_equal(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();
    let predicate  = llvm::LLVMIntPredicate::LLVMIntSLE;

    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let lhs = deref_if_ptr(ctx.llvm_ctx, current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(ctx.llvm_ctx, current.builder, rhs, expected_operand_type);

    let result = llvm
        ::core
        ::LLVMBuildICmp(current.builder, predicate,lhs, rhs, binary_cstr!("_lecomp"));

    stack.push(result);
}

pub unsafe fn op_greater_equal(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let (rhs, lhs) = stack.binary_op();
    let predicate  = llvm::LLVMIntPredicate::LLVMIntSGE;

    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let lhs = deref_if_ptr(ctx.llvm_ctx, current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(ctx.llvm_ctx, current.builder, rhs, expected_operand_type);

    let result = llvm
        ::core
        ::LLVMBuildICmp(current.builder, predicate,lhs, rhs, binary_cstr!("_gecomp"));

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

            // TODO: fix this. It shouldn't be needed.
            ctx.compilation_state.info.push(vec![]);

            ctx.compilation_state
                .info[current.frame_position.frame_index]
                .push(ValueInfo::Function { info: function_call });

            function_ref
        }

        _ => panic!() // TODO: Result I guess. :/
    };

    stack.push(value_ref);
}

pub unsafe fn op_get_local(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame = current.current_frame_mut();
    let index = frame.read_byte() as usize;

    // println!("{} {}", current.frame_position.frame_index, index);

    // let value = frame.get_value(index, &stack.buffer);
    // TODO: think about the semantics of this. For now, I think it should simply
    // pass the value as-is.

    let variable_ref = ctx.compilation_state.variables[current.frame_position.frame_index][index];
    // {
    //     let c_str = llvm::core::LLVMPrintValueToString(variable_ref);
    //     eprintln!("value: {}", std::ffi::CStr::from_ptr(c_str).to_string_lossy());
    // }

    stack.push(variable_ref);
}

pub unsafe fn op_declare_variable(ctx: &mut Context, _stack: &mut Stack, current: &mut Current)
{
    let frame = current.current_frame_mut();
    let index = frame.read_byte() as usize;
    let scope = frame.read_byte() as usize;

    // TODO: allocate upfront or something other than this.
    if ctx.compilation_state.variables.len() <= current.frame_position.frame_index {
        ctx.compilation_state.variables.push(vec![0 as *mut llvm::LLVMValue; 1024]);
    }

    if ctx.compilation_state.variable_types.len() <= current.frame_position.frame_index {
        ctx.compilation_state.variable_types.push(vec![0 as *mut llvm::LLVMType; 1024]);
    }

    let variable = &ctx.program
        .scopes[scope]
        .variables[index];

    let type_ref      = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let variable_name = variable.name.value.as_ptr();

    let variable = llvm
        ::core
        ::LLVMBuildAlloca(current.builder, type_ref, variable_name as *const _);

    ctx.compilation_state
        .variables[current.frame_position.frame_index][index] = variable;
    ctx.compilation_state
       .variable_types[current.frame_position.frame_index][index] = llvm::core::LLVMTypeOf(variable);
}

pub unsafe fn op_set_local(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let value = stack.peek(0);

    // These two are copied first because 'current' is borrowed to
    // get the frame.
    let frame_index = current.frame_position.frame_index;
    let builder     = current.builder;

    let frame = current.current_frame_mut();
    let index = frame.read_byte() as usize;

    let variable_type = ctx.compilation_state.variable_types[frame_index][index];
    let value         = deref_if_ptr(ctx.llvm_ctx, builder, value, variable_type);

    let variable_ref = ctx.compilation_state.variables[frame_index][index];
    llvm::core::LLVMBuildStore(builder, value, variable_ref);

    frame.set_value(index, value, &mut stack.buffer);
}

pub unsafe fn op_get_upvalue(_ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame          = current.current_frame_mut();
    let scope_distance = frame.read_byte() as usize;
    let index          = frame.read_byte() as usize;

    let enclosing_scope = &current.frame_position.frames[current.frame_position.frame_index - scope_distance];
    let value = enclosing_scope.get_value(index, &stack.buffer);

    stack.push(value);
}

pub unsafe fn op_set_upvalue(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame          = current.current_frame_mut();
    let scope_distance = frame.read_byte() as usize;
    let index          = frame.read_byte() as usize;

    let value = stack.peek(0).clone();

    let scope = current.frame_position.frame_index - scope_distance;

    let enclosing_scope = &mut current.frame_position.frames[scope];
    let variable_ref = ctx.compilation_state.variables[scope][index];
    llvm::core::LLVMBuildStore(current.builder, value, variable_ref);

    enclosing_scope.set_value(index, value, &mut stack.buffer);
}

pub unsafe fn op_return(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame  = current.current_frame_mut();
    let _      = frame.read_byte();
    let result = stack.pop();

    if current.frame_position.frames.is_empty() { return }
    current.pop_frame();

    // TODO: this won't work with branching.
    let ContextSpecific::FunctionSpecific { data } = current.context_specific else {
        panic!();
    };

    let result = if is_type_primitive(ctx.llvm_ctx, data.return_type) {
        deref_if_ptr(ctx.llvm_ctx, current.builder, result, data.return_type)
    } else {
        result
    };

    llvm::core::LLVMBuildRet(current.builder, result);
    stack.push(result);
}

pub unsafe fn op_jump(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame = current.current_frame_mut();
    let jump  = frame.read_byte() as usize;

    let ContextSpecific::ControlFlowSpecific { data: ref mut branch } = current.context_specific else {
        // return
        panic!("Branch not initiated.")
    };

    let branch_block = llvm
        ::core
        ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_jumpbb"));
    llvm::core::LLVMPositionBuilderAtEnd(current.builder, branch_block);

    let node = ControlFlowNode {
        id: branch.inner_nodes.len(),
        parent: current.basic_block,
        cond: None,
        value: None,
        true_branch: None,
        false_branch: None,
    };
    branch.inner_nodes.push(node);

    current.basic_block = branch_block;
    branch.end_counter = jump;

    let node = ControlFlowNode {
        id: branch.inner_nodes.len(),
        parent: current.basic_block,
        cond: None,
        value: None,
        true_branch: None,
        false_branch: None
    };
    branch.inner_nodes.push(node);
    compile_branch(ctx, stack, current);
}

// TODO: I have no way of knowing if I'm in the loop until I reach the end of the loop block.
// This is not ideal.
// Loop will start with this same OP, same as if.
pub unsafe fn op_cond_jump(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame     = current.current_frame_mut();
    let jump      = frame.read_byte() as usize;
    let condition = stack.pop();

    // For some reason, I store booleans as i8, so, to work with
    // llvm conditions, I have to convert them to i1.
    let i1_condition = llvm::core::LLVMBuildTrunc
    (
        current.builder,
        condition,
        llvm::core::LLVMInt1TypeInContext(ctx.llvm_ctx),
        binary_cstr!("_bool_convert")
    );

    let then_block = llvm
        ::core
        ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_condjumpbb"));
    llvm::core::LLVMPositionBuilderAtEnd(current.builder, then_block);

    let mut control_flow = ControlFlow2 {
        node: ControlFlowNode {
            id: 0,
            parent: current.basic_block,
            cond: Some(i1_condition),
            value: None,
            true_branch: None,
            false_branch: None
        },
        inner_nodes: vec![],
        end_counter: jump,
    };

    let branch_current = Current {
        builder: current.builder,
        basic_block: then_block,
        module: current.module,
        function: current.function,
        frame_position: current.frame_position,
        context_specific: ContextSpecific::ControlFlowSpecific {
            data: &mut control_flow
        },
    };
    compile_branch(ctx, stack, branch_current);
    let Ok(phi) = write_branch2(&ctx, stack, &current, control_flow) else {
        panic!("Failed to write branch code");
    };

    stack.push(phi);
}

// I think I have to add this operation in order to be able to know if I'm doing a loop
// or a normal branch.
pub unsafe fn op_loop(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let cond_bb = llvm
        ::core
        ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_loop_cond_bb"));

    let end_bb = llvm
        ::core
        ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_after_loop_bb"));

    let mut loop_data = Loop {
        cond_bb,
        next_bb: 0 as llvm::prelude::LLVMBasicBlockRef,
        end_bb,
    };

    llvm::core::LLVMBuildBr(current.builder, loop_data.cond_bb);
    llvm::core::LLVMPositionBuilderAtEnd(current.builder, loop_data.cond_bb);

    let mut control_flow = ControlFlow {
        parent_builder: current.builder,
        parent_block: current.basic_block,
        inner_blocks: vec![],
        end_counter: 0,
    };

    let loop_current = Current {
        builder: current.builder,
        basic_block: loop_data.next_bb,
        module: current.module,
        function: current.function,
        frame_position: current.frame_position,
        context_specific: todo!() // ContextSpecific::ControlFlowSpecific { data: &mut control_flow }
    };
    compile_loop(ctx, stack, loop_current);
    llvm::core::LLVMPositionBuilderAtEnd(current.builder, end_bb);
    // This kind of sucks, too dependent on current state of compilation.
    // I'd rather have a more functional approach without so much mutations involved.
    current.basic_block = end_bb;
}

pub unsafe fn op_loop_cond_jump(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame = current.current_frame_mut();
    let _     = frame.read_byte() as usize;
    let cmp   = stack.pop();

    let ContextSpecific::LoopSpecific { ref mut data } = current.context_specific else {
        panic!("Loop not initiated.");
    };

    // For some reason, I store booleans as i8, so, to work with
    // llvm conditions, I have to convert them to i1.
    let i1_cmp = llvm::core::LLVMBuildTrunc
    (
        current.builder,
        cmp,
        llvm::core::LLVMInt1TypeInContext(ctx.llvm_ctx),
        binary_cstr!("_bool_convert")
    );

    let next_bb = llvm::core::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_loopnextbb"));
    llvm::core::LLVMBuildCondBr(current.builder, i1_cmp, next_bb, data.end_bb);
    llvm::core::LLVMPositionBuilderAtEnd(current.builder, next_bb);

    data.next_bb = next_bb;
    // current.basic_block = next_bb;

    // current.basic_block = next_bb;

    // I need a way to create another block between this and loop body.
    // With for loops, we have the loop increment part, and the current issue is that
    // the loop increment part is treated as the loop body - an extra block is missing.

    // What if I only have the next block available an then on loop_cond_jump I set it
    // and in loop jump I pass the compilation onto the next block?
}

pub unsafe fn op_loop_jump(ctx: &mut Context, _stack: &mut Stack, current: &mut Current)
{
    let frame = current.current_frame_mut();
    let _     = frame.read_byte() as usize;

    let ContextSpecific::LoopSpecific { ref mut data } = current.context_specific else {
        return
        // panic!("Loop not initiated.");
    };

    // current.frames.pop();

    let next_bb = llvm::core::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function, binary_cstr!("_loopnextbb"));
    llvm::core::LLVMBuildBr(current.builder, next_bb);
    llvm::core::LLVMPositionBuilderAtEnd(current.builder, next_bb);

    current.basic_block = next_bb;
    data.next_bb = next_bb;
}

pub unsafe fn op_call(ctx: &mut Context, stack: &mut Stack, current: &mut Current)
{
    let frame          = current.current_frame_mut();
    let scope_distance = frame.read_byte() as usize;
    let index          = frame.read_byte() as usize;

    let scope         = current.frame_position.frame_index - scope_distance;
    let function_info = ctx.compilation_state.info[scope][index].clone();

    let ValueInfo::Function { mut info  } = function_info else {
        panic!("Expected function info")
    };

    // Only compile the function on Op::Call since only at that point
    // will we have the stack in the correct state.
    if !info.compiled {
        let entry_block = llvm
            ::core
            ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, info.function, info.name.as_ptr() as * const _);

        let function_builder = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
        llvm::core::LLVMPositionBuilderAtEnd(function_builder, entry_block);

        current.push_frame(StackFrame::new(stack.top - info.arity, info.code.clone()));

        ctx.compilation_state.variables.push(vec![0 as *mut llvm::LLVMValue; 1024]);
        ctx.compilation_state.variable_types.push(vec![0 as *mut llvm::LLVMType; 1024]);

        // **IMPORTANT** it is necessary to clone the stack, as we don't want to actually proceed
        // with the program, instead, we have to run the compilation of the function in its own stack.
        let mut stack_clone = stack.clone();

        // Instead of using the actual arguments (which *are* values), we need to
        // replace the arguments with the parameters.
        for _ in 0..info.arity { stack_clone.pop(); }
        for i in 0..info.arity {
            // TODO: think about having explicit pass as pointer vs pass as value.
            // Need to have specific copy/move semantics defined for that.
            let param = llvm::core::LLVMGetParam(info.function, i as u32);
            ctx.compilation_state.variables[current.frame_position.frame_index][i] = param;
            ctx.compilation_state.variable_types[current.frame_position.frame_index][i] = llvm::core::LLVMTypeOf(param);

            stack_clone.push(param);
        }

        // TODO: I feel like I maybe, probably, potentially should have "constructors" for scenarios:
        // * new function
        // * same function
        // The current way is easy to mess up with all the bookkeeping we need to do when compiling
        // structured pieces of code.
        let function_current = Current {
            builder: function_builder,
            basic_block: entry_block,
            module: current.module,
            function: info.function,
            frame_position: current.frame_position,
            context_specific: ContextSpecific::FunctionSpecific { data: &info },
        };

        compile_function(ctx, &mut stack_clone, function_current);
        info.compiled = true;
    }

    // TODO: I'm not sure of the semantics of arguments.
    // I'm thinking copy by default and take the reference explicitly.
    let mut args: Vec<llvm::prelude::LLVMValueRef> = (0..info.arity)
        .map(|i| deref_if_ptr(ctx.llvm_ctx, current.builder, stack.peek(i), info.param_types[i]))
        .rev()
        .collect();

    let call_result = llvm::core::LLVMBuildCall2
    (
        current.builder,
        info.function_type,
        info.function,
        args.as_mut_ptr(),
        info.arity as u32,
        info.name.as_ptr() as *const _,
    );
    stack.push(call_result);
    ctx.compilation_state.info[scope][index] = ValueInfo::Function { info };
}

#[derive(Clone, Debug)]
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
            arity,
            param_types,
            return_type,
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

unsafe fn is_type_primitive(ctx: llvm::prelude::LLVMContextRef, value_type: llvm::prelude::LLVMTypeRef) -> bool
{
    // TODO: more stuff
    value_type == llvm::core::LLVMInt32TypeInContext(ctx)
}

unsafe fn deref_if_ptr
(
    ctx: llvm::prelude::LLVMContextRef,
    builder: llvm::prelude::LLVMBuilderRef,
    value: llvm::prelude::LLVMValueRef,
    expected_type: llvm::prelude::LLVMTypeRef,
) -> llvm::prelude::LLVMValueRef
{
    let value_type   = llvm::core::LLVMTypeOf(value);
    let pointer_type = llvm::core::LLVMPointerTypeInContext(ctx, 0);

    if pointer_type == value_type {
        return llvm
            ::core
            ::LLVMBuildLoad2(builder, expected_type, value, binary_cstr!("_deref"));
    }

    value
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

// TODO: REMOVE THIS! This is just for playing around.
pub unsafe fn add_printf(ctx: &mut Context, module: llvm::prelude::LLVMModuleRef, builder: llvm::prelude::LLVMBuilderRef)
{
    let a = ctx.compilation_state.variables[0][1];

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

#[allow(unused)]
unsafe fn print_stack(stack: &Stack)
{
    for value in &stack.buffer {
        let c_str = llvm::core::LLVMPrintValueToString(*value);
        eprintln!("value: {}", std::ffi::CStr::from_ptr(c_str).to_string_lossy());
    }
}

fn print_module(module: llvm::prelude::LLVMModuleRef)
{
    unsafe {
        let module_text = llvm::core::LLVMPrintModuleToString(module);
        println!("MODULE: \n{}", std::ffi::CStr::from_ptr(module_text).to_str().unwrap());
        llvm::core::LLVMDisposeMessage(module_text);
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
            block::Op::Loop            => print_simple_op("LOOP"),
            block::Op::LoopCondJump    => print_simple_op("LOOP_COND_JUMP"),
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
