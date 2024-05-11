extern crate llvm_sys as llvm;

use std::collections::HashMap;
use macros::binary_cstr;

use crate::{block, compiler, scan};

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
    // pub variables: Vec<Vec<llvm::prelude::LLVMValueRef>>,
    // pub variable_types: Vec<Vec<llvm::prelude::LLVMTypeRef>>,

    pub variables: Vec<HashMap<String, llvm::prelude::LLVMValueRef>>,
    pub variable_types: Vec<HashMap<String, llvm::prelude::LLVMTypeRef>>,

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

#[derive(Debug)]
pub struct Current
{
    pub builder: llvm::prelude::LLVMBuilderRef,
    pub basic_block: llvm::prelude::LLVMBasicBlockRef,

    pub module: llvm::prelude::LLVMModuleRef,
    pub function: FunctionCall,
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

    let mut current = Current {
        builder,
        basic_block: entry_block,
        module,
        function: main_function_call,
    };

    for stmt in &ctx.program.clone() {
        match_statement(ctx, &mut current, stmt);
    }

    add_printf(ctx, module, builder);

    let return_value = llvm
    ::core
    ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), 0, 0);
    llvm::core::LLVMBuildRet(builder, return_value);

    verify_module(module);
    module
}

pub unsafe fn match_statement(ctx: &mut Context, current: &mut Current, stmt: &compiler::Stmt)
{
    match stmt {
        compiler::Stmt::Function { params: _, body: _ } => (),

        compiler::Stmt::Declaration { name: _, initializer: _ } => (),

        compiler::Stmt::Block { statements: _ } => (),

        compiler::Stmt::Var { name, initializer } => {
            let type_ref      = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
            let variable_name = name.value.as_ptr();

            let variable = llvm
                ::core
                ::LLVMBuildAlloca(current.builder, type_ref, variable_name as *const _);

            let value = match_expression(ctx, current, initializer);
            llvm::core::LLVMBuildStore(current.builder, value, variable);

            ctx.compilation_state.variables[0].insert(name.value.clone(), value);
        },

        compiler::Stmt::Const { name, initializer } => {
            let type_ref      = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
            let variable_name = name.value.as_ptr();

            let variable = llvm
                ::core
                ::LLVMBuildAlloca(current.builder, type_ref, variable_name as *const _);

            let value = match_expression(ctx, current, initializer);
            llvm::core::LLVMBuildStore(current.builder, value, variable);

            println!("{:?}", value);

            ctx.compilation_state.variables[0].insert(name.value.clone(), value);
        },

        compiler::Stmt::For { } => (),

        compiler::Stmt::While { condition: _, body: _ } => (),

        compiler::Stmt::Return { } => (),

        compiler::Stmt::Expr { expr } => { match_expression(ctx, current, expr.as_ref()); },
    }
}

pub unsafe fn match_expression(ctx: &mut Context, current: &mut Current, expr: &compiler::Expr) -> llvm::prelude::LLVMValueRef
{
    match expr {
        compiler::Expr::Bad { token: _ } => todo!(),

        compiler::Expr::Block { statements: _, value: _ } => todo!(),

        compiler::Expr::If { condition: _, then_branch: _, else_branch: _ } => todo!(),

        compiler::Expr::Binary { left, right, operator }
            => binary_expr(ctx, current, left.as_ref(), right.as_ref(), operator),

        compiler::Expr::Grouping => todo!(),

        compiler::Expr::Literal { value } => match value {
            block::Value::Number { val } =>
                llvm
                    ::core
                    ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), *val as u64, 1),

            block::Value::Bool { val } =>
                llvm
                    ::core
                    ::LLVMConstInt(llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx), if *val { 1 } else { 0 }, 0),

            block::Value::String { val } =>
                llvm
                    ::core
                    ::LLVMConstString(val.as_ptr() as *const _, val.len() as u32, 1),

            _ => panic!()
        },

        compiler::Expr::Variable { name } => {
            // ctx.compilation_state.variables[current.frame_position.frame_index][name.value]

            let variables = &ctx.compilation_state.variables[0];
            *variables.get(&name.value).unwrap()
        },

        compiler::Expr::Unary => todo!(),

        compiler::Expr::Assignment { name: _, value: _ } => todo!(),

        compiler::Expr::Logical => todo!(),

        compiler::Expr::Call { arguments } => {
            // TODO: I'm not sure of the semantics of arguments.
            // I'm thinking copy by default and take the reference explicitly.
            let mut args: Vec<llvm::prelude::LLVMValueRef> = arguments
                .iter()
                .enumerate()
                .map(|(i, a)| {
                    let arg = match_expression(ctx, current, a);
                    deref_if_ptr(ctx.llvm_ctx, current.builder, arg, current.function.param_types[i])
                })
                .collect();

            llvm::core::LLVMBuildCall2
            (
                current.builder,
                current.function.function_type,
                current.function.function,
                args.as_mut_ptr(),
                current.function.arity as u32,
                current.function.name.as_ptr() as *const _,
            )
        },

        compiler::Expr::Function { params, body } => {
            ctx.compilation_state.variables.push(HashMap::new());
            ctx.compilation_state.variable_types.push(HashMap::new());

            let function_call = FunctionCall::build
            (
                ctx.llvm_ctx,
                current.module,
                "_temp_name".to_string(),
                params.len(),
                vec![Some("number".to_string()), Some("number".to_string())],
                "number".to_string(),
                body.into_iter().map(|s| *s.clone()).collect(),
            );
            let function_ref = function_call.function;

            let entry_block = llvm::core::LLVMAppendBasicBlockInContext
            (
                ctx.llvm_ctx,
                function_ref,
                "_entry".as_ptr() as * const _
            );

            let function_builder = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
            llvm::core::LLVMPositionBuilderAtEnd(function_builder, entry_block);

            for (i, param) in params.iter().enumerate() {
                let param_ref  = llvm::core::LLVMGetParam(function_ref, i as u32);
                let param_name = &param.value;

                let state = &mut ctx.compilation_state;
                state.variables[0].insert(param_name.clone(), param_ref);
                state.variable_types[0].insert(param_name.clone(), llvm::core::LLVMTypeOf(param_ref));
            }

            let mut function_current = Current {
                builder: function_builder,
                basic_block: entry_block,
                module: current.module,
                function: function_call.clone(),
            };

            current.function = function_call;

            for stmt in &body[..body.len()-1] {
                match_statement(ctx, &mut function_current, stmt);
            }

            let result = match body.last().unwrap().as_ref() {
                compiler::Stmt::Expr { expr } => {
                    match_expression(ctx, &mut function_current, expr)
                }
                _ => panic!() // TODO
            };

            let return_type = current.function.return_type;
            let result = if is_type_primitive(ctx.llvm_ctx, return_type) {
                deref_if_ptr(ctx.llvm_ctx, function_current.builder, result, return_type)
            } else {
                result
            };

            llvm::core::LLVMBuildRet(function_current.builder, result);

            // TODO: fix this. It shouldn't be needed.
            // ctx.compilation_state.info.push(vec![]);
            //
            // ctx.compilation_state
            //     .info[current.frame_position.frame_index]
            //     .push(ValueInfo::Function { info: function_call });

            current.function.compiled = true;

            function_ref
        },
    }
}

pub unsafe fn binary_expr
(
    ctx: &mut Context,
    current: &mut Current,
    left: &compiler::Expr,
    right: &compiler::Expr,
    operator: &scan::Token,
) -> llvm::prelude::LLVMValueRef
{
    let lhs = match_expression(ctx, current, left);
    let rhs = match_expression(ctx, current, right);

    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
    let lhs = deref_if_ptr(ctx.llvm_ctx, current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(ctx.llvm_ctx, current.builder, rhs, expected_operand_type);

    match operator.kind {
        scan::TokenKind::Plus => llvm
            ::core
            ::LLVMBuildAdd(current.builder, lhs, rhs, binary_cstr!("_add_result")),
        scan::TokenKind::Minus => llvm
            ::core
            ::LLVMBuildSub(current.builder, lhs, rhs, binary_cstr!("_sub_result")),
        scan::TokenKind::Star => llvm
            ::core
            ::LLVMBuildMul(current.builder, lhs, rhs, binary_cstr!("_mul_result")),
        scan::TokenKind::Slash => llvm
            ::core
            ::LLVMBuildSDiv(current.builder, lhs, rhs, binary_cstr!("_sub_result")),
        _ => panic!()
    }

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
    let a = ctx.compilation_state.variables[0].get("result").unwrap();
    // let a_value = llvm::core::LLVMBuildLoad2(builder, llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), a.clone(), binary_cstr!("a"));

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

    let mut param_values = [global_format_str, *a];
    llvm::core::LLVMBuildCall2(
        builder,
        printf_type,
        printf,
        param_values.as_mut_ptr(),
        param_values.len() as u32,
        binary_cstr!("printf_call"),
    );
}