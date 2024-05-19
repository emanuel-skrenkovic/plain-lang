extern crate llvm_sys as llvm;

use std::collections::HashMap;
use macros::binary_cstr;

use crate::{block, compiler, scan, semantic_analysis};

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
                _        => panic!()
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
        }
    }
}

pub struct Scope
{
    pub index: usize,
    pub path: Vec<usize>,

    pub variables: HashMap<String, llvm::prelude::LLVMValueRef>,
    pub variable_types: HashMap<String, llvm::prelude::LLVMTypeRef>,
}

#[derive(Debug)]
pub struct Current
{
    pub builder: llvm::prelude::LLVMBuilderRef,
    pub basic_block: llvm::prelude::LLVMBasicBlockRef,

    pub module: llvm::prelude::LLVMModuleRef,
    pub function: FunctionCall,

    pub scope_depth: usize,
}

pub struct Context
{
    pub llvm_ctx: llvm::prelude::LLVMContextRef,
    pub modules: Vec<llvm::prelude::LLVMModuleRef>,

    pub program: Vec<compiler::Stmt>,

    // #horribleways Change it later.
    pub functions: HashMap<String, FunctionCall>,

    pub scopes: Vec<Scope>,
    pub current_scope_index: usize,

    pub declarations: HashMap<String, (usize, FunctionCall)>,

    pub symbol_table: semantic_analysis::SymbolTable,
}

impl Context
{
    #[must_use]
    pub unsafe fn new(program: Vec<compiler::Stmt>, symbol_table: semantic_analysis::SymbolTable) -> Self
    {
        Self {
            llvm_ctx: llvm::core::LLVMContextCreate(),
            modules: Vec::with_capacity(1),
            program,
            functions: HashMap::new(),
            scopes: Vec::with_capacity(1024),
            current_scope_index: 0,
            declarations: HashMap::new(),
            symbol_table,
        }
    }

    pub fn begin_scope(&mut self)
    {
        let parent_scope = if self.scopes.len() == 0 { None }
                           else                      { Some(&self.scopes[self.current_scope_index]) };

        // New scope path will contain the parent as well, so extending with the
        // index of the parent.
        let new_scope_path = if let Some(parent_scope) = parent_scope {
            let mut new_scope_path = Vec::with_capacity(parent_scope.path.len() + 1);
            new_scope_path.extend_from_slice(&parent_scope.path);
            new_scope_path.push(parent_scope.index);
            new_scope_path
        } else {
            vec![]
        };

        let new_scope = Scope {
            index: 0,
            path: new_scope_path,
            variables: HashMap::new(),
            variable_types: HashMap::new(),
        };

        // Push the new scope and get its index. Use it as the ID of the Scope struct.
        self.scopes.push(new_scope);

        let new_scope_index = self.scopes.len() - 1;
        self.scopes[new_scope_index].index = new_scope_index;

        self.current_scope_index = new_scope_index;
    }

    pub fn end_scope(&mut self)
    {
        let scope                = &self.scopes[self.current_scope_index];
        let parent_scope         = scope.path.last().unwrap();
        self.current_scope_index = *parent_scope;
    }

    pub fn current_scope(&self) -> &Scope
    {
        &self.scopes[self.current_scope_index]
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope
    {
        &mut self.scopes[self.current_scope_index]
    }

    pub fn get_variable(&self, name: &str) -> Option<llvm::prelude::LLVMValueRef>
    {
        let scope = self.current_scope();

        if let Some(var) = scope.variables.get(name) {
            return Some(*var)
        };

        for i in scope.path.iter().rev() {
            let scope = &self.scopes[*i];

            if let Some(var) = scope.variables.get(name) {
                return Some(*var)
            };
        }

        None
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

    let builder = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);

    // TODO: this is a temporary mess.
    let main_function_call = FunctionCall::build
    (
        ctx.llvm_ctx,
        module,
        "main".to_string(),
        0,
        vec![],
        "number".to_string(),
        ctx.program.clone(),
    );

    let entry_block = llvm
        ::core
        ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, main_function_call.function, binary_cstr!("entry"));
    llvm::core::LLVMPositionBuilderAtEnd(builder, entry_block);

    let mut current = Current {
        builder,
        basic_block: entry_block,
        module,
        function: main_function_call,
        scope_depth: 0,
    };

    for scope in &ctx.symbol_table.scopes {
        for (name, declaration) in &scope.declarations {
            match &declaration {
                &semantic_analysis::Declaration::Function { params, body } => {
                    let function_call = FunctionCall::build
                    (
                        ctx.llvm_ctx,
                        current.module,
                        name.to_string(),
                        params.len(),
                        vec![Some("number".to_string()); params.len()],
                        "number".to_string(),
                        body.iter().map(|s| *s.clone()).collect(),
                    );

                    ctx.declarations.insert(name.clone(), (scope.index, function_call));
                }
            }
        }
    }

    // printf

    // let printf_type = llvm::core::LLVMFunctionType
    // (
    //     llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx),
    //     [llvm::core::LLVMPointerType(llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx), 0)].as_mut_ptr(),
    //     1,
    //     1,
    // );

    // let printf = llvm
    //     ::core
    //     ::LLVMAddFunction(module, binary_cstr!("printf"), printf_type);

    // let printf = FunctionCall::build
    // (
    //     ctx.llvm_ctx,
    //     current.module,
    //     "printf\0".to_string(),
    //     2,
    //     vec![Some("string".to_string()), Some("number".to_string())],
    //     "number".to_string(),
    //     vec![],
    // );

    let printf = printf_function(ctx, module);
    ctx.declarations.insert("printf".to_owned(), (0, printf));

    //

    ctx.begin_scope();

    for stmt in &ctx.program.clone() {
        match_statement(ctx, &mut current, stmt);
    }

    // add_printf(ctx, module, builder);

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
        compiler::Stmt::Function { name, params, body } => {
            ctx.begin_scope();

            let (_, function_call) = ctx.declarations.get(&name.value).unwrap();
            let function_call = function_call.clone();
            let function_ref = function_call.function;

            let entry_block = llvm::core::LLVMAppendBasicBlockInContext
            (
                ctx.llvm_ctx,
                function_ref,
                "_entry\0".as_ptr() as * const _
            );

            let function_builder = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
            llvm::core::LLVMPositionBuilderAtEnd(function_builder, entry_block);

            let mut function_current = Current {
                builder: function_builder,
                basic_block: entry_block,
                module: current.module,
                function: function_call.clone(),
                scope_depth: current.scope_depth + 1,
            };

            for (i, param) in params.iter().enumerate() {
                let param_ref  = llvm::core::LLVMGetParam(function_ref, i as u32);
                llvm::core::LLVMSetValueName2(param_ref, param.value.as_ptr() as * const _, param.value.len());
                let param_name = &param.value;

                ctx.current_scope_mut().variables.insert(param_name.clone(), param_ref);
                ctx.current_scope_mut().variable_types.insert(param_name.clone(), llvm::core::LLVMTypeOf(param_ref));
            }

            // current.function = function_call;

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

            ctx.end_scope();

            ctx.functions.insert(name.value.to_string(), function_call.clone());
        },

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

            ctx.current_scope_mut().variables.insert(name.value.clone(), variable);
        },

        compiler::Stmt::Const { name, initializer } => {
            let type_ref      = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
            let variable_name = name.value.as_ptr();

            let variable = llvm
                ::core
                ::LLVMBuildAlloca(current.builder, type_ref, variable_name as *const _);

            let value = match_expression(ctx, current, initializer);
            llvm::core::LLVMBuildStore(current.builder, value, variable);

            ctx.current_scope_mut().variables.insert(name.value.clone(), variable);
            ctx.current_scope_mut().variable_types.insert(name.value.clone(), type_ref);
        },

        compiler::Stmt::For { } => (),

        compiler::Stmt::While { condition, body } => {
            let start_branch = llvm
                ::core
                ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function.function, binary_cstr!("_while_start"));

            let body_branch = llvm
                ::core
                ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function.function, binary_cstr!("_while_body"));

            let end_branch = llvm
                ::core
                ::LLVMAppendBasicBlockInContext(ctx.llvm_ctx, current.function.function, binary_cstr!("_while_end"));

            llvm::core::LLVMBuildBr(current.builder, start_branch);

            // condition

            llvm::core::LLVMPositionBuilderAtEnd(current.builder, start_branch);

            let condition_expr = match_expression(ctx, current, condition);
            llvm::core::LLVMBuildCondBr(current.builder, condition_expr, body_branch, end_branch);

            // body

            llvm::core::LLVMPositionBuilderAtEnd(current.builder, body_branch);

            for stmt in body {
                match_statement(ctx, current, stmt);
            }

            llvm::core::LLVMBuildBr(current.builder, start_branch);

            // end

            llvm::core::LLVMPositionBuilderAtEnd(current.builder, end_branch);
        },

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

        compiler::Expr::Variable { name } => ctx.get_variable(&name.value).unwrap(),

        compiler::Expr::Unary => todo!(),

        compiler::Expr::Assignment { name, value } => {
            let value_expr = match_expression(ctx, current, value);
            let variable_ref = ctx.get_variable(&name.value).unwrap();
            llvm::core::LLVMBuildStore(current.builder, value_expr, variable_ref)
        },

        compiler::Expr::Logical => todo!(),

        compiler::Expr::Call { name, arguments } => {
            let (scope, function) = ctx
                .declarations
                .get(&name.value)
                .unwrap().clone();

            let current_scope = ctx.current_scope();
            if current_scope.index != scope && !current_scope.path.contains(&scope) {
                panic!("Function '{}' not in scope.", name.value);
            }

            // TODO: I'm not sure of the semantics of arguments.
            // I'm thinking copy by default and take the reference explicitly.
            let mut args: Vec<llvm::prelude::LLVMValueRef> = arguments
                .iter()
                .enumerate()
                .map(|(i, a)| {
                    let arg = match_expression(ctx, current, a);

                    if is_type_primitive(ctx.llvm_ctx, function.param_types[i]) {
                        deref_if_ptr(ctx.llvm_ctx, current.builder, arg, function.param_types[i])
                    } else {
                        arg
                    }

                    // deref_if_ptr(ctx.llvm_ctx, current.builder, arg, function.param_types[i])
                })
                .collect();

            let scope = ctx.current_scope();

            // TODO: the order of the variables gets shuffled
            // depending on the variables since we're dealing with a hashmap.
            for var in scope.variables.values() {
                let arg = deref_if_ptr(ctx.llvm_ctx, current.builder, *var, llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx));
                args.push(arg);
            }

            for i in &scope.path {
                let closed_scope = &ctx.scopes[*i];

                for var in closed_scope.variables.values() {
                    let arg = deref_if_ptr(ctx.llvm_ctx, current.builder, *var, llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx));
                    args.push(arg);
                }
            }

            let result = llvm::core::LLVMBuildCall2
            (
                current.builder,
                function.function_type,
                function.function,
                args.as_mut_ptr(),
                function.arity as u32,
                function.name.as_ptr() as *const _,
            );

            deref_if_ptr(ctx.llvm_ctx, current.builder, result, function.function_type)
        },

        compiler::Expr::Function { params, body }
            => closure(ctx, current, "_closure", params.to_vec(), body.to_vec()).function,
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
        scan::TokenKind::LeftAngle => llvm
            ::core
            ::LLVMBuildICmp(current.builder, llvm::LLVMIntPredicate::LLVMIntSLT, lhs, rhs, binary_cstr!("_ltcomp")),
        _ => panic!()
    }
}

unsafe fn closure
(
    ctx: &mut Context,
    current: &mut Current,
    name: &str,
    params: Vec<scan::Token>,
    body: Vec<Box<compiler::Stmt>>,
) -> FunctionCall
{
    ctx.begin_scope();

    let mut closed_params: Vec<String> = params.iter().map(|p| p.value.clone()).collect();

    let scope = ctx.current_scope();

    for var in scope.variables.keys() {
        closed_params.push(var.clone());
    }

    for i in &scope.path {
        let closed_scope = &ctx.scopes[*i];

        for value in closed_scope.variables.keys() {
            closed_params.push(value.to_string());
        }
    }

    let function_call = FunctionCall::build
    (
        ctx.llvm_ctx,
        current.module,
        name.to_string(),
        closed_params.len(),
        vec![Some("number".to_string()); closed_params.len()],
        "number".to_string(),
        body.iter().map(|s| *s.clone()).collect(),
    );
    let function_ref = function_call.function;

    let entry_block = llvm::core::LLVMAppendBasicBlockInContext
    (
        ctx.llvm_ctx,
        function_ref,
        "_entry\0".as_ptr() as * const _
    );

    let function_builder = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
    llvm::core::LLVMPositionBuilderAtEnd(function_builder, entry_block);

    let mut function_current = Current {
        builder: function_builder,
        basic_block: entry_block,
        module: current.module,
        function: function_call.clone(),
        scope_depth: current.scope_depth + 1,
    };

    for (i, param) in closed_params.iter().enumerate() {
        let param_ref  = llvm::core::LLVMGetParam(function_ref, i as u32);
        llvm::core::LLVMSetValueName2(param_ref, param.as_ptr() as * const _, param.len());

        ctx.current_scope_mut().variables.insert(param.clone(), param_ref);
        ctx.current_scope_mut().variable_types.insert(param.clone(), llvm::core::LLVMTypeOf(param_ref));
    }

    current.function = function_call.clone();

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

    ctx.end_scope();

    function_call
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
    let a = ctx.get_variable("result").unwrap();
    let a_value = llvm::core::LLVMBuildLoad2(builder, llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), a, binary_cstr!("a"));

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

pub unsafe fn printf_function
(
    ctx: &mut Context, module: llvm::prelude::LLVMModuleRef,
) -> FunctionCall
{
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

    FunctionCall {
        name: "printf".to_string(),
        function: printf,
        function_type: printf_type,
        arity: 2,
        param_types: vec![llvm::core::LLVMPointerType(llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx), 0), llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx)],
        return_type: llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx),
        code: vec![],
    }
}
