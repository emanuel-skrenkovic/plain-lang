extern crate llvm_sys as llvm;

use std::collections::HashMap;
use macros::binary_cstr;

use crate::{ast, scope, scan, semantic_analysis, types};

#[derive(Clone, Debug)]
pub struct FunctionDefinition
{
    pub name: String,

    pub function: llvm::prelude::LLVMValueRef,
    pub function_type: llvm::prelude::LLVMTypeRef,

    pub arity: usize,
    pub param_types: Vec<llvm::prelude::LLVMTypeRef>,

    pub return_type: llvm::prelude::LLVMTypeRef,

    pub code: Vec<ast::Stmt>,

    pub closure: bool,
}

impl FunctionDefinition
{
    unsafe fn build
    (
        context_ref: llvm::prelude::LLVMContextRef,
        module: llvm::prelude::LLVMModuleRef,
        name: String,
        arity: usize,
        argument_type_kinds: &[Box<types::TypeKind>],
        return_type_kind: &types::TypeKind,
        code: Vec<ast::Stmt>,
        closure: bool,
    ) -> Self
    {
        let return_type = to_llvm_type(context_ref, return_type_kind);

        // let mut param_types: Vec<llvm::prelude::LLVMTypeRef> = argument_type_names
        //     .iter()
        //     .filter_map(Option::as_ref)
        //     .map(|arg| match arg.as_str() {
        //         "unit" => llvm::core::LLVMVoidTypeInContext(context_ref),
        //         "i32"  => llvm::core::LLVMInt32TypeInContext(context_ref),
        //         "bool" => llvm::core::LLVMInt8TypeInContext(context_ref),
        //         _      => panic!()
        //     }).collect();

        let mut param_types: Vec<llvm::prelude::LLVMTypeRef> = argument_type_kinds
            .iter()
            .map(|arg| {
                to_llvm_type(context_ref, arg)
            })
            .collect();

        let function_type = llvm
            ::core
            ::LLVMFunctionType(return_type, param_types.as_mut_ptr(), arity as u32, 0);
        let function = llvm
            ::core
            ::LLVMAddFunction(module, name.as_ptr() as *const _, function_type);

        FunctionDefinition {
            name,
            function,
            function_type,
            arity,
            param_types,
            return_type,
            code,
            closure,
        }
    }
}

pub unsafe fn to_llvm_type(context_ref: llvm::prelude::LLVMContextRef, type_kind: &types::TypeKind) -> llvm::prelude::LLVMTypeRef
{
    match type_kind {
        types::TypeKind::Unknown => todo!(),
        types::TypeKind::Unit => llvm::core::LLVMVoidTypeInContext(context_ref),
        types::TypeKind::Bool => llvm::core::LLVMInt8TypeInContext(context_ref),
        types::TypeKind::I32 => llvm::core::LLVMInt32TypeInContext(context_ref),
        types::TypeKind::String => todo!(),
        types::TypeKind::Function { parameter_kinds: _, return_kind: _ } => todo!(),
        types::TypeKind::Closure { captured_kinds: _, parameter_kinds: _, return_kind: _ } => todo!(),
        types::TypeKind::Reference { underlying: _ } => todo!(),
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
    pub ctx: llvm::prelude::LLVMContextRef,
    pub module: llvm::prelude::LLVMModuleRef,

    pub builder: llvm::prelude::LLVMBuilderRef,
    pub basic_block: llvm::prelude::LLVMBasicBlockRef,

    pub function: FunctionDefinition,
    pub name: Option<String>,
}

impl Current
{
    pub unsafe fn new
    (
        llvm_ctx: llvm::prelude::LLVMContextRef,
        module: llvm::prelude::LLVMModuleRef,
        function: FunctionDefinition,
    ) -> Self
    {

        let entry_block = llvm::core::LLVMAppendBasicBlockInContext
        (
            llvm_ctx,
            function.function,
            "_entry\0".as_ptr() as * const _
        );

        let builder = llvm::core::LLVMCreateBuilderInContext(llvm_ctx);
        llvm::core::LLVMPositionBuilderAtEnd(builder, entry_block);

        Current {
            ctx: llvm_ctx,
            builder,
            basic_block: entry_block,
            module,
            function,
            name: None,
        }
    }

    pub unsafe fn set_position(&mut self, basic_block: llvm::prelude::LLVMBasicBlockRef)
    {
        self.basic_block = basic_block;
        llvm::core::LLVMPositionBuilderAtEnd(self.builder, basic_block);
    }

    pub unsafe fn append_block
    (
        &self,
        name: &str
    ) -> llvm::prelude::LLVMBasicBlockRef
    {
        use std::ffi::CString;
        let block_name = CString::new(name).unwrap();
        llvm::core::LLVMAppendBasicBlockInContext
        (
            self.ctx,
            self.function.function,
            block_name.as_ptr(),
        )
    }

    pub unsafe fn build_break(&self, basic_block: llvm::prelude::LLVMBasicBlockRef)
    {
        llvm::core::LLVMBuildBr(self.builder, basic_block);
    }

    pub unsafe fn build_condition
    (
        &self,
        condition: llvm::prelude::LLVMValueRef,
        then_block: llvm::prelude::LLVMBasicBlockRef,
        else_block: llvm::prelude::LLVMBasicBlockRef,
    )
    {
        llvm
            ::core
            ::LLVMBuildCondBr(self.builder, condition, then_block, else_block);
    }
}

pub struct Context
{
    pub llvm_ctx: llvm::prelude::LLVMContextRef,
    pub modules: Vec<llvm::prelude::LLVMModuleRef>,

    pub program: Vec<ast::Node>,

    pub module_scopes: scope::Module<(llvm::prelude::LLVMValueRef, llvm::prelude::LLVMTypeRef)>,
    pub declarations: HashMap<String, (usize, FunctionDefinition)>,

    pub symbol_table: semantic_analysis::SymbolTable,
}

impl Context
{
    #[must_use]
    pub unsafe fn new(program: Vec<ast::Node>, symbol_table: semantic_analysis::SymbolTable) -> Self
    {
        Self {
            llvm_ctx: llvm::core::LLVMContextCreate(),
            modules: Vec::with_capacity(1),
            program,
            module_scopes: scope::Module::new(),
            declarations: HashMap::new(),
            symbol_table,
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

// TODO: there is a problem with the different order of compilation.
// Here, we compile the main function first, in other places, we evaluate in
// order of declaration.
pub unsafe fn compile(ctx: &mut Context) -> *mut llvm::LLVMModule
{
    println!("{:#?}", ctx.program);

    let module = llvm::core::LLVMModuleCreateWithNameInContext(binary_cstr!("main"), ctx.llvm_ctx);
    ctx.modules.push(module);

    // start global scope
    ctx.module_scopes.begin_scope();

    // Adding globals.
    let printf = printf_function(ctx, module);
    ctx.declarations.insert("printf".to_owned(), (0, printf));

    forward_declare(ctx);

    // start main

    let (_, main_function_call) = ctx
        .declarations
        .get("main")
        .expect("Expect main function.")
        .clone();

    let mut current = Current::new(ctx.llvm_ctx, module, main_function_call.clone());

    // Compile main first.
    {
        ctx.module_scopes.begin_scope();

        for stmt in &main_function_call.code[..main_function_call.code.len()-1] {
            match_statement(ctx, &mut current, stmt);
        }

        let result = match main_function_call.code.last() {
            Some(ast::Stmt::Expr { expr }) => match_expression(ctx, &mut current, &expr.value),
            _                                   => panic!() // TODO
        };

        let return_type = main_function_call.return_type;
        let result      = deref_if_primitive(current.builder, result, return_type);
        llvm::core::LLVMBuildRet(current.builder, result);

        ctx.module_scopes.end_scope();
    }

    // Compile the rest of the program
    ctx.module_scopes.begin_scope();

    for stmt in &ctx.program.clone() {
        if let ast::Node::Stmt(stmt) = stmt {
            match_statement(ctx, &mut current, stmt);
        }
    }

    ctx.module_scopes.end_scope();

    // Global scope.
    ctx.module_scopes.end_scope();

    verify_module(module);
    module
}

pub unsafe fn match_statement
(
    ctx: &mut Context,
    current: &mut Current,
    stmt: &ast::Stmt
)
{
    match stmt {
        ast::Stmt::Function { name, params, param_types: _, body } => {
            if name.value == "main" { return }

            ctx.module_scopes.begin_scope();

            let (_, function_call) = ctx.declarations.get(&name.value).unwrap();

            // TODO: I don't like this clone here.
            let function_call = function_call.clone();
            let function_ref  = function_call.function;
            let return_type   = function_call.return_type;

            let mut function_current = Current::new(ctx.llvm_ctx, current.module, function_call);

            for (i, param) in params.iter().enumerate() {
                let param_ref  = llvm::core::LLVMGetParam(function_ref, i as u32);
                llvm::core::LLVMSetValueName2(param_ref, param.value.as_ptr() as * const _, param.value.len());

                let param_name = &param.value;
                ctx.module_scopes.add_to_current(&param_name, (param_ref, llvm::core::LLVMTypeOf(param_ref)));
            }

            for stmt in &body[..body.len()-1] {
                match_statement(ctx, &mut function_current, stmt);
            }

            let result = match body.last().unwrap(/* TODO: remove unwrap */).as_ref() {
                ast::Stmt::Expr { expr } => match_expression(ctx, &mut function_current, &expr.value),
                _                             => llvm::core::LLVMConstNull(return_type) // TODO
            };

            let result = deref_if_primitive(function_current.builder, result, return_type);

            llvm::core::LLVMBuildRet(function_current.builder, result);

            ctx.module_scopes.end_scope();
        },

        ast::Stmt::Declaration { name: _, initializer: _ } => (),

        ast::Stmt::Block { statements: _ } => (),

        ast::Stmt::Var { name, initializer } => {
            let type_ref      = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
            let variable_name = name.value.as_ptr();

            let variable = llvm
                ::core
                ::LLVMBuildAlloca(current.builder, type_ref, variable_name as *const _);

            current.name = Some(name.value.clone());

            let value = match_expression(ctx, current, &initializer.value);
            llvm::core::LLVMBuildStore(current.builder, value, variable);

            ctx
                .module_scopes
                .add_to_current(&name.value, (variable, llvm::core::LLVMTypeOf(variable)));
        },

        ast::Stmt::Const { name, initializer } => {
            let type_ref      = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
            let variable_name = name.value.as_ptr();

            let variable = llvm
                ::core
                ::LLVMBuildAlloca(current.builder, type_ref, variable_name as *const _);

            current.name = Some(name.value.clone());

            let value = match_expression(ctx, current, &initializer.value);
            llvm::core::LLVMBuildStore(current.builder, value, variable);

            ctx.module_scopes.add_to_current(&name.value, (variable, type_ref));
        },

        ast::Stmt::For { initializer, condition, advancement, body } => {
            let start_branch       = current.append_block("_for_start");
            let condition_branch   = current.append_block("_for_condition");
            let body_branch        = current.append_block("_for_body");
            let advancement_branch = current.append_block("_for_advancement");
            let end_branch         = current.append_block("_for_end");

            current.build_break(start_branch);

            // initializer
            {
                current.set_position(start_branch);
                match_statement(ctx, current, initializer);
                current.build_break(body_branch);
            }

            // condition
            {
                current.set_position(condition_branch);
                let condition_expr = match_expression(ctx, current, &condition.value);
                current.build_condition(condition_expr, body_branch, end_branch);
            }

            // body
            {
                current.set_position(body_branch);
                for stmt in body {
                    match_statement(ctx, current, stmt);
                }
                current.build_break(advancement_branch);
            }

            // advancement
            {
                current.set_position(advancement_branch);
                match_statement(ctx, current, advancement);
                current.build_break(condition_branch);
            }

            current.set_position(end_branch);
        },

        ast::Stmt::While { condition, body } => {
            let start_branch = current.append_block("_while_start");
            let body_branch  = current.append_block("_while_body");
            let end_branch   = current.append_block("_while_end");

            current.build_break(start_branch);

            // condition
            {
                current.set_position(start_branch);
                let condition_expr = match_expression(ctx, current, &condition.value);
                current.build_condition(condition_expr, body_branch, end_branch);
            }

            // body
            {
                current.set_position(body_branch);
                for stmt in body {
                    match_statement(ctx, current, stmt);
                }
                current.build_break(start_branch);
            }

            current.set_position(end_branch);

        },

        ast::Stmt::Unary { } => (),

        ast::Stmt::Return { } => (),

        ast::Stmt::Expr { expr } => { match_expression(ctx, current, &expr.value); },
    }
}

pub unsafe fn match_expression(ctx: &mut Context, current: &mut Current, expr: &ast::Expr) -> llvm::prelude::LLVMValueRef
{
    match expr {
        ast::Expr::Bad { token: _ } => todo!(),

        ast::Expr::Block { statements, value } => {
            ctx.module_scopes.begin_scope();

            for stmt in statements {
                match_statement(ctx, current, stmt);
            }

            let expr = match_expression(ctx, current, value);

            ctx.module_scopes.end_scope();

            let return_type = llvm::core::LLVMTypeOf(expr);
            deref_if_ptr(current.builder, expr, return_type)
        },

        ast::Expr::If { condition, then_branch, then_value, else_branch, else_value } => {
            let branch_entry_block = current.append_block("_entry_branch");
            current.build_break(branch_entry_block);
            current.set_position(branch_entry_block);

            let condition_expr = match_expression(ctx, current, condition);

            let mut incoming_values = Vec::with_capacity(2);
            let mut incoming_blocks = Vec::with_capacity(2);

            let then_block = current.append_block("_then_branch");
            current.set_position(then_block);

            let (then_result, then_exit_block) = {
                for stmt in then_branch {
                    match_statement(ctx, current, stmt);
                }

                let then_result = match_expression(ctx, current, then_value);

                (then_result, current.basic_block)
            };

            incoming_values.push(then_result);
            incoming_blocks.push(then_exit_block);

            let else_block = current.append_block("_else_branch");
            current.set_position(else_block);

            let (else_result, else_exit_block) = {
                for stmt in else_branch {
                    match_statement(ctx, current, stmt);
                }

                let else_result = match_expression(ctx, current, else_value);

                (else_result, current.basic_block)
            };

            incoming_values.push(else_result);
            incoming_blocks.push(else_exit_block);

            let end_block = current.append_block("_end_branch");

            for block in &incoming_blocks {
                current.set_position(*block);
                current.build_break(end_block);
            }

            current.set_position(branch_entry_block);

            current.build_condition(condition_expr, then_block, else_block);

            let count           = incoming_values.len() as u32;
            let incoming_values = incoming_values.as_mut_ptr();
            let incoming_blocks = incoming_blocks.as_mut_ptr();

            current.set_position(end_block);

            let branch_value_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
            let phi_node          = llvm::core::LLVMBuildPhi(current.builder, branch_value_type, binary_cstr!("_branchphi"));

            llvm::core::LLVMAddIncoming(phi_node, incoming_values, incoming_blocks, count);

            phi_node
        },

        ast::Expr::Binary { left, right, operator }
            => binary_expr(ctx, current, left.as_ref(), right.as_ref(), operator),

        // TODO: maybe simply extend the AST with type information,
        // instead of keeping the type info in a separate place.
        // That would avoid the issue of finding type info for a freestanding
        // literal expression.
        ast::Expr::Literal { value } => {
            let name        = &current.name.take().unwrap();
            let declaration = ctx.symbol_table.module.get_in_scope(ctx.module_scopes.current_scope_index, name).unwrap(/*TODO: remove unwrap*/);

            match declaration.type_kind {
                types::TypeKind::Unit =>
                    llvm
                        ::core
                        ::LLVMConstNull(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx)),

                types::TypeKind::Bool =>
                    llvm
                        ::core
                        ::LLVMConstInt(llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx), if value.value == "true" { 1 } else { 0 }, 0),

                types::TypeKind::I32 => {
                    let val = value.value.parse::<u64>().unwrap(/*TODO: remove unwrap*/);
                    llvm
                        ::core
                        ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), val, 1)
                }

                types::TypeKind::String => {
                    let value = &value.value;
                    let val   = value[1..value.len()-1].to_string(); // Strip away '"' from start and end.
                    llvm
                        ::core
                        ::LLVMConstString(val.as_ptr() as *const _, val.len() as u32, 0)
                }


                _ => panic!(),
            }
        }

        ast::Expr::Variable { name } => {
            let (variable, _) = ctx.module_scopes.get(&name.value).unwrap();
            *variable
        }

        ast::Expr::Assignment { name, value } => {
            let value_expr        = match_expression(ctx, current, value);
            let (variable_ref, _) = ctx.module_scopes.get(&name.value).unwrap();

            llvm::core::LLVMBuildStore(current.builder, value_expr, *variable_ref)
        },

        ast::Expr::Logical => todo!(),

        ast::Expr::Call { name, arguments } => {
            let (scope, function) = ctx
                .declarations
                .get(&name.value)
                .unwrap_or_else(|| panic!("Expect variable '{}'.", &name.value))
                .clone();

            let current_scope = ctx.module_scopes.current_scope();
            if current_scope.index != scope && !current_scope.path.contains(&scope) {
                panic!("Function '{}' not in scope.", name.value);
            }

            // TODO: only do the below if it's a closure.
            let mut closed_variables = if function.closure {
                variables_in_scope(ctx)
                    .iter()
                    .filter(|(key, _)| key != &name.value)
                    .map(|(_, var)| {
                        prime_argument(current.builder, *var, llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx))
                    })
                    .collect()
            } else {
                vec![]
            };

            // TODO: I'm not sure of the semantics of arguments.
            // I'm thinking copy by default and take the reference explicitly.
            let mut initial_args: Vec<llvm::prelude::LLVMValueRef> = arguments
                .iter()
                .enumerate()
                .map(|(i, a)| {
                    let arg = match_expression(ctx, current, a);
                    prime_argument(current.builder, arg, function.param_types[i])
                })
                .collect();

            let total_args_count = initial_args.len() + closed_variables.len();

            let mut args: Vec<llvm::prelude::LLVMValueRef> = Vec::with_capacity(total_args_count);
            args.append(&mut initial_args);
            args.append(&mut closed_variables);

            let result = llvm::core::LLVMBuildCall2
            (
                current.builder,
                function.function_type,
                function.function,
                args.as_mut_ptr(),
                function.arity as u32,
                format!("{}_call", function.name).as_ptr() as *const _,
            );

            deref_if_primitive(current.builder, result, function.function_type)
        },

        ast::Expr::Function { params, param_types: _, body }
            => {
                let name = current.name.take().unwrap(/*TODO: remove unwrap*/);
                closure(ctx, current, &name, params.to_vec(), body.to_vec())
            }
    }
}

pub unsafe fn binary_expr
(
    ctx: &mut Context,
    current: &mut Current,
    left: &ast::Expr,
    right: &ast::Expr,
    operator: &scan::Token,
) -> llvm::prelude::LLVMValueRef
{
    let lhs = match_expression(ctx, current, left);
    let rhs = match_expression(ctx, current, right);

    let expected_operand_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);

    let lhs = deref_if_ptr(current.builder, lhs, expected_operand_type);
    let rhs = deref_if_ptr(current.builder, rhs, expected_operand_type);

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

        scan::TokenKind::RightAngle => llvm
            ::core
            ::LLVMBuildICmp(current.builder, llvm::LLVMIntPredicate::LLVMIntSGT, lhs, rhs, binary_cstr!("_gtcomp")),

        scan::TokenKind::EqualEqual => llvm
            ::core
            ::LLVMBuildICmp(current.builder, llvm::LLVMIntPredicate::LLVMIntEQ, lhs, rhs, binary_cstr!("_eqcomp")),

        scan::TokenKind::BangEqual => llvm
            ::core
            ::LLVMBuildICmp(current.builder, llvm::LLVMIntPredicate::LLVMIntNE, lhs, rhs, binary_cstr!("_neqcomp")),

        scan::TokenKind::GreaterEqual => llvm
            ::core
            ::LLVMBuildICmp(current.builder, llvm::LLVMIntPredicate::LLVMIntSGE, lhs, rhs, binary_cstr!("_gecomp")),

        scan::TokenKind::LessEqual => llvm
            ::core
            ::LLVMBuildICmp(current.builder, llvm::LLVMIntPredicate::LLVMIntSLE, lhs, rhs, binary_cstr!("_lecomp")),

        _ => panic!()
    }
}

unsafe fn closure
(
    ctx: &mut Context,
    current: &mut Current,
    name: &str,
    params: Vec<scan::Token>,
    body: Vec<Box<ast::Stmt>>,
) -> llvm::prelude::LLVMValueRef
{
    ctx.module_scopes.begin_scope();

    let mut closed_variables: Vec<String> = variables_in_scope(ctx)
        .into_iter()
        .filter(|(n, _)| n != &name)
        .map(|(name, _)| name.to_string())
        .collect();

    let mut params: Vec<String> = params
        .iter()
        .map(|p| p.value.clone())
        .collect();

    let total_values_count = params.len() + closed_variables.len();
    let mut closed_params  = Vec::with_capacity(total_values_count);

    closed_params.append(&mut params);
    closed_params.append(&mut closed_variables);

    let (_, function_call) = ctx.declarations.get(name).unwrap();

    let function_ref = function_call.function;
    let return_type  = function_call.return_type;

    let mut function_current = Current::new(ctx.llvm_ctx, current.module, function_call.clone());

    for (i, param) in closed_params.iter().enumerate() {
        let param_ref = llvm::core::LLVMGetParam(function_ref, i as u32);
        llvm::core::LLVMSetValueName2(param_ref, param.as_ptr() as * const _, param.len());

        ctx.module_scopes.add_to_current(&param, (param_ref, llvm::core::LLVMTypeOf(param_ref)));
    }

    if body.len() > 0 {
        for stmt in &body[..body.len()-1] {
            match_statement(ctx, &mut function_current, stmt);
        }
    }

    let result = match body.last().map(|s| s.as_ref()) {
        Some(ast::Stmt::Expr { expr }) => match_expression(ctx, &mut function_current, &expr.value),
        _ => llvm::core::LLVMConstNull(return_type),
    };


    let result = deref_if_primitive(function_current.builder, result, return_type);
    llvm::core::LLVMBuildRet(function_current.builder, result);

    ctx.module_scopes.end_scope();

    function_ref
}

unsafe fn forward_declare(ctx: &mut Context)
{
    for scope in &ctx.symbol_table.module.scopes {
        for i in 0..scope.values.len() {
            let name        = &scope.names[i];
            let declaration = &scope.values[i];

            match &declaration.kind {
                semantic_analysis::DeclarationKind::Function {
                    function: semantic_analysis::Function { params, body }
                } => {
                    let types::TypeKind::Function { parameter_kinds, return_kind } = &declaration.type_kind else {
                        panic!("Expected function type kind.");
                    };

                    let function_call = FunctionDefinition::build
                    (
                        ctx.llvm_ctx,
                        ctx.modules[0],
                        name.to_string(),
                        params.len(),

                        // TODO: actual types should be available after semantic analysis.
                        // TODO: closures here act differently.
                        // vec![Some("number".to_string()); params.len()],
                        parameter_kinds,

                        return_kind,
                        body.iter().map(|s| *s.clone()).collect(),
                        false,
                    );

                    ctx.declarations.insert(name.clone(), (scope.index, function_call));
                }

                semantic_analysis::DeclarationKind::Closure {
                    captures,
                    function: semantic_analysis::Function { params, body }
                } => {
                    let types::TypeKind::Function { parameter_kinds, return_kind } = &declaration.type_kind else {
                        panic!("Expected function type kind.");
                    };

                    let function_call = FunctionDefinition::build
                    (
                        ctx.llvm_ctx,
                        ctx.modules[0],
                        name.to_string(),
                        captures.len() + params.len(),

                        // TODO: actual types should be available after semantic analysis.
                        // TODO: closures here act differently.
                        // vec![Some("number".to_string()); captures.len() + params.len()],
                        parameter_kinds,

                        return_kind,
                        body.iter().map(|s| *s.clone()).collect(),
                        true,
                    );

                    ctx.declarations.insert(name.clone(), (scope.index, function_call));
                }

                _ => (),
            }
        }
    }
}

unsafe fn is_pointer(var: llvm::prelude::LLVMValueRef) -> bool
{
    let var_type      = llvm::core::LLVMTypeOf(var);
    let var_type_kind = llvm::core::LLVMGetTypeKind(var_type);

    var_type_kind == llvm::LLVMTypeKind::LLVMPointerTypeKind
}

const PRIMITIVE_TYPES: [llvm::LLVMTypeKind; 1] = [
    llvm::LLVMTypeKind::LLVMIntegerTypeKind
];

// TODO: shit name
unsafe fn prime_argument
(
    builder: llvm::prelude::LLVMBuilderRef,
    value: llvm::prelude::LLVMValueRef,
    expected_type: llvm::prelude::LLVMTypeRef,
) -> llvm::prelude::LLVMValueRef
{
    let type_kind = llvm::core::LLVMGetTypeKind(expected_type);

    if PRIMITIVE_TYPES.contains(&type_kind) && is_pointer(value) {
        // Deref pointers of primitive types.
        return llvm
            ::core
            ::LLVMBuildLoad2(builder, expected_type, value, binary_cstr!("_deref"));
    } else if !PRIMITIVE_TYPES.contains(&type_kind) && !is_pointer(value) {
        // Take address of raw, non-primitive types.
        let ptr = llvm
            ::core
            ::LLVMBuildAlloca(builder, llvm::core::LLVMTypeOf(value), binary_cstr!("_alloc"));
        llvm::core::LLVMBuildStore(builder, value, ptr);
        return ptr
    }

    value
}

// TODO
unsafe fn deref_if_primitive
(
    builder: llvm::prelude::LLVMBuilderRef,
    value: llvm::prelude::LLVMValueRef,
    expected_type: llvm::prelude::LLVMTypeRef,
) -> llvm::prelude::LLVMValueRef
{
    // TODO: cache this?
    let type_kind = llvm::core::LLVMGetTypeKind(expected_type);

    if is_pointer(value) && PRIMITIVE_TYPES.contains(&type_kind) {
        return llvm
            ::core
            ::LLVMBuildLoad2(builder, expected_type, value, binary_cstr!("_deref"));
    }

    value
}

unsafe fn deref_if_ptr
(
    builder: llvm::prelude::LLVMBuilderRef,
    value: llvm::prelude::LLVMValueRef,
    expected_type: llvm::prelude::LLVMTypeRef,
) -> llvm::prelude::LLVMValueRef
{
    if is_pointer(value) {
        return llvm
            ::core
            ::LLVMBuildLoad2(builder, expected_type, value, binary_cstr!("_deref"));
    }

    value
}

pub unsafe fn variables_in_scope(ctx: &Context) -> Vec<(&str, llvm::prelude::LLVMValueRef)>
{
    let scope = ctx.module_scopes.current_scope();

    let mut vars: Vec<(&str, llvm::prelude::LLVMValueRef)> = Vec::with_capacity(1024);

    for i in 0..scope.values.len() {
        let key        = &scope.names[i];
        let (value, _) = &scope.values[i];

        vars.push((key, *value));
    }

    for i in &scope.path {
        let closed_scope = &ctx.module_scopes.scopes[*i];

        for i in 0..closed_scope.values.len() {
            let key        = &closed_scope.names[i];
            let (value, _) = &closed_scope.values[i];

            vars.push((key, *value));
        }
    }

    vars
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

// TODO: remove this hack!
pub unsafe fn printf_function
(
    ctx: &mut Context, module: llvm::prelude::LLVMModuleRef,
) -> FunctionDefinition
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

    FunctionDefinition {
        name: "printf".to_string(),
        function: printf,
        function_type: printf_type,
        arity: 2,
        param_types: vec![llvm::core::LLVMPointerType(llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx), 0), llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx)],
        return_type: llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx),
        code: vec![],
        closure: false,
    }
}
