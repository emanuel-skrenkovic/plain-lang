extern crate llvm_sys as llvm;

use macros::binary_cstr;

use crate::{compiler};

pub enum ValueInfo
{
    Unit,
    String,
    Bool,
    Number,
    Function { info: FunctionCall },
    CompiledFunction,
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
    pub code: Vec<compiler::Stmt>,

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
        code: Vec<compiler::Stmt>,
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

pub struct CompilationState
{
    // HashMap of variables per scope. Scope index is the key, variables are in the vec.
    // TODO: think of how to do indexing of variable refs.
    pub variables: Vec<Vec<llvm::prelude::LLVMValueRef>>,
    pub variable_types: Vec<Vec<llvm::prelude::LLVMTypeRef>>,
    pub info: Vec<Vec<ValueInfo>>,
}

impl Default for CompilationState
{
    fn default() -> Self
    {
        Self::new()
    }
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

pub struct Context
{
    pub llvm_ctx: llvm::prelude::LLVMContextRef,
    pub modules: Vec<llvm::prelude::LLVMModuleRef>,
    // pub program: compiler::Program,
    pub program: Vec<compiler::Stmt>,
    pub compilation_state: CompilationState,
}

impl Context
{
    #[must_use]
    pub unsafe fn new(program: Vec<compiler::Stmt>) -> Self
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

    // TODO: remove
    ctx.compilation_state.variables.push(vec![std::ptr::null_mut(); 1024]);
    ctx.compilation_state.variable_types.push(vec![std::ptr::null_mut(); 1024]);

    // TODO: this is a temporary mess.
    let main_function_call = FunctionCall::build
    (
        ctx.llvm_ctx,
        module,
        "main".to_string(),
        0,
        vec![],
        "unit".to_string(),
        ctx.program.clone(),
    );

    // for stmt in &ctx.program {
    //     match_statement(ctx, stmt);
    // }

    add_printf(ctx, module, builder);

    let return_value = llvm
    ::core
    ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), 0, 0);
    llvm::core::LLVMBuildRet(builder, return_value);

    verify_module(module);
    module
}

pub unsafe fn match_statement(ctx: &mut Context, stmt: &compiler::Stmt)
{
    match stmt {
        compiler::Stmt::Function { params, body } => (),

        compiler::Stmt::Declaration { name, initializer } => (),

        compiler::Stmt::Block { statements } => (),

        compiler::Stmt::Var { name, initializer } => (),

        compiler::Stmt::Const { name, initializer } => (),

        compiler::Stmt::For { } => (),

        compiler::Stmt::While { condition, body } => (),

        compiler::Stmt::Return { } => (),

        compiler::Stmt::Expr { expr } => match_expression(ctx, &expr),
    }
}

pub unsafe fn match_expression(ctx: &mut Context, expr: &compiler::Expr)
{
    match expr {
        compiler::Expr::Bad { token } => (),

        compiler::Expr::Block { statements, value } => (),

        compiler::Expr::If { condition, then_branch, else_branch } => (),

        compiler::Expr::Binary { left, right, operator } => (),

        compiler::Expr::Grouping => (),

        compiler::Expr::Literal { value } => (),

        compiler::Expr::Variable { name } => (),

        compiler::Expr::Unary => (),

        compiler::Expr::Assignment { name, value } => (),

        compiler::Expr::Logical => (),

        compiler::Expr::Call { arguments } => (),

        compiler::Expr::Function { params, body } => (),
    }
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
        llvm::core::LLVMDisposeMessage(module_text);
    }
}

// // TODO: remove - this is just for janky testing.
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

// TODO: REMOVE THIS! This is just for playing around.
pub unsafe fn add_printf(ctx: &mut Context, module: llvm::prelude::LLVMModuleRef, builder: llvm::prelude::LLVMBuilderRef)
{
    // let a = ctx.compilation_state.variables[0][1];

    // let a_value = llvm::core::LLVMBuildLoad2(builder, llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), a, binary_cstr!("a"));

    let global_format_str = llvm::core::LLVMBuildGlobalStringPtr(
        builder,
        binary_cstr!("%d\n"),
        binary_cstr!("format_str"),
    );

    let printf_type = llvm::core::LLVMFunctionType
    (
        llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx),
        [llvm::core::LLVMPointerType(llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx), 0)].as_mut_ptr(),
        1,
        1,
    );

    let printf = llvm
        ::core
        ::LLVMAddFunction(module, binary_cstr!("printf"), printf_type);

    let mut param_values = [global_format_str];
    llvm::core::LLVMBuildCall2(
        builder,
        printf_type,
        printf,
        param_values.as_mut_ptr(),
        param_values.len() as u32,
        binary_cstr!("printf_call"),
    );
}