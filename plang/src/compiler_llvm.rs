extern crate llvm_sys as llvm;

use std::ffi;
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

pub unsafe fn to_llvm_type(context_ref: llvm::prelude::LLVMContextRef, type_kind: &types::TypeKind) -> llvm::prelude::LLVMTypeRef
{
    match type_kind {
        types::TypeKind::Unknown          => todo!(),
        types::TypeKind::Unit             => llvm::core::LLVMVoidTypeInContext(context_ref),
        types::TypeKind::Bool             => llvm::core::LLVMInt8TypeInContext(context_ref),
        types::TypeKind::I32              => llvm::core::LLVMInt32TypeInContext(context_ref),
        types::TypeKind::String { len }   => {
            let char_type = llvm::core::LLVMInt8TypeInContext(context_ref);
            llvm::core::LLVMArrayType2(char_type, *len as u64 + 1)
        }
        types::TypeKind::Function { parameter_kinds, return_kind }  => {
            let return_type = to_llvm_type(context_ref, return_kind);

            let mut param_types: Vec<llvm::prelude::LLVMTypeRef> = parameter_kinds
                .iter()
                .map(|arg| to_llvm_type(context_ref, arg))
                .collect();

            let arity = param_types.len();

            let function_type = llvm
                ::core
                ::LLVMFunctionType(return_type, param_types.as_mut_ptr(), arity as u32, 0);

            llvm::core::LLVMPointerType(function_type, 0)
        }
        types::TypeKind::Closure { .. }   => todo!(),
        types::TypeKind::Struct { .. }    => {
            // TODO: I think I'll need to simply grab the declaration/type pointer
            // here.

            /*
            let mut element_types: Vec<llvm::prelude::LLVMTypeRef> = field_types
                .iter()
                .map(|t| to_llvm_type(context_ref, t))
                .collect();

            llvm::core::LLVMStructTypeInContext(context_ref, element_types.as_mut_ptr(), element_types.len() as u32, 0)
            */
            std::ptr::null_mut()
        }
        types::TypeKind::Reference { .. } => todo!(),
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
pub struct Builder
{
    pub ctx: llvm::prelude::LLVMContextRef,
    pub module: llvm::prelude::LLVMModuleRef,

    pub builder: llvm::prelude::LLVMBuilderRef,

    pub basic_block: Option<llvm::prelude::LLVMBasicBlockRef>,
}

impl Builder
{
    pub unsafe fn new
    (
        llvm_ctx: llvm::prelude::LLVMContextRef,
        module: llvm::prelude::LLVMModuleRef,
        builder: llvm::prelude::LLVMBuilderRef,
    ) -> Self
    {
        Self {
            ctx: llvm_ctx,
            builder,
            module,
            basic_block: None,
        }
    }

    pub unsafe fn set_position(&mut self, basic_block: llvm::prelude::LLVMBasicBlockRef)
    {
        self.basic_block = Some(basic_block);
        llvm::core::LLVMPositionBuilderAtEnd(self.builder, basic_block);
    }

    pub unsafe fn append_block
    (
        &self,
        function_ref: llvm::prelude::LLVMValueRef,
        name: &str
    ) -> llvm::prelude::LLVMBasicBlockRef
    {
        let block_name = ffi::CString::new(name).unwrap();
        llvm::core::LLVMAppendBasicBlockInContext
        (
            self.ctx,
            function_ref,
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

    pub unsafe fn build_function
    (
        &self,
        name: String,
        argument_type_kinds: &[&types::TypeKind],
        return_type_kind: &types::TypeKind,
        code: Vec<ast::Stmt>,
        closure: bool,
    ) -> FunctionDefinition
    {
        let return_type = to_llvm_type(self.ctx, return_type_kind);

        let arity = argument_type_kinds.len();

        let mut param_types: Vec<llvm::prelude::LLVMTypeRef> = argument_type_kinds
            .iter()
            .map(|arg| to_llvm_type(self.ctx, arg))
            .collect();

        let function_type = llvm
            ::core
            ::LLVMFunctionType(return_type, param_types.as_mut_ptr(), arity as u32, 0);
        let function = llvm
            ::core
            ::LLVMAddFunction(self.module, name.as_ptr() as *const _, function_type);

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

pub struct Context
{
    pub llvm_ctx: llvm::prelude::LLVMContextRef,
    pub modules: Vec<llvm::prelude::LLVMModuleRef>,

    pub program: Vec<ast::Node>,

    pub module_scopes: scope::Module<(llvm::prelude::LLVMValueRef, llvm::prelude::LLVMTypeRef)>,
    pub declarations: HashMap<String, (usize, FunctionDefinition)>,

    // TODO: this goes first!!!!!
    pub declarations2: HashMap<String, llvm::prelude::LLVMTypeRef>,

    pub symbol_table: semantic_analysis::SymbolTable,
    pub type_info: scope::Module<types::TypeKind>,

    pub function: Option<FunctionDefinition>,
    pub name: Option<String>,
}

impl Context
{
    #[must_use]
    pub unsafe fn new
    (
        program: Vec<ast::Node>,
        symbol_table: semantic_analysis::SymbolTable,
        type_info: scope::Module<types::TypeKind>,
    ) -> Self
    {
        Self {
            llvm_ctx: llvm::core::LLVMContextCreate(),
            modules: Vec::with_capacity(1),
            program,
            module_scopes: scope::Module::new(),
            declarations: HashMap::new(),
            declarations2: HashMap::new(),
            symbol_table,
            type_info,
            function: None,
            name: None,
        }
    }

    fn function_ref(&self) -> llvm::prelude::LLVMValueRef
    {
        let Some(ref function_call) = self.function else {
            panic!("Expect function");
        };

        function_call.function
    }

    fn current_scope(&self) -> usize
    {
        self.module_scopes.current_scope_index
    }

    fn is_global(&self) -> bool
    {
        self.current_scope() == 0
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
    let module = llvm::core::LLVMModuleCreateWithNameInContext(binary_cstr!("main"), ctx.llvm_ctx);
    ctx.modules.push(module);

    // Adding globals.
    let printf = printf_function(ctx, module);
    ctx.declarations.insert("printf".to_owned(), (0, printf));

    let builder     = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
    let mut builder = Builder::new(ctx.llvm_ctx, module, builder);

    forward_declare(ctx, &mut builder);

    // Compile the rest of the program
    ctx.module_scopes.begin_scope();

    for stmt in &ctx.program.clone() {
        let ast::Node::Stmt(stmt) = stmt else {
            continue
        };

        match_statement(ctx, &mut builder, stmt);
    }

    ctx.module_scopes.end_scope();

    verify_module(module);
    module
}

pub unsafe fn match_statement
(
    ctx: &mut Context,
    builder: &mut Builder,
    stmt: &ast::Stmt
)
{
    match stmt {
        ast::Stmt::Struct { name, members, .. } => {
            // TODO: this needs to happen in 'forward_declare' and stored in declarations.;
            let struct_type = llvm::core::LLVMStructCreateNamed(ctx.llvm_ctx, name.value.as_ptr() as * const _);

            let struct_type_kind = ctx.type_info.get_from_scope(ctx.current_scope(), &name.value).unwrap();
            let types::TypeKind::Struct { field_types, .. } = struct_type_kind else {
                panic!("Expect 'struct' type.");
            };

            let mut element_types: Vec<llvm::prelude::LLVMTypeRef> = field_types
                .iter()
                .map(|t| to_llvm_type(ctx.llvm_ctx, &t))
                .collect();

            llvm::core::LLVMStructSetBody(struct_type, element_types.as_mut_ptr(), members.len() as u32, 0);

            // TODO: I need a place to store the thingy somewhere.
            ctx.declarations2.insert(name.value.clone(), struct_type);
        }

        ast::Stmt::Function { name, params, body, .. } => {
            ctx.module_scopes.begin_scope();

            let (_, function_call) = ctx.declarations.get(&name.value).unwrap();

            // TODO: I don't like this clone here.
            let function_call = function_call.clone();
            let function_ref  = function_call.function;
            let return_type   = function_call.return_type;

            let entry_block = llvm::core::LLVMAppendBasicBlockInContext
            (
                ctx.llvm_ctx,
                function_call.function,
                "_entry\0".as_ptr() as * const _
            );

            let new_builder = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
            llvm::core::LLVMPositionBuilderAtEnd(new_builder, entry_block);

            ctx.function = Some(function_call);
            let mut function_builder = Builder::new(ctx.llvm_ctx, builder.module, new_builder);

            for (i, param) in params.iter().enumerate() {
                let param_ref  = llvm::core::LLVMGetParam(function_ref, i as u32);
                llvm::core::LLVMSetValueName2(param_ref, param.value.as_ptr() as * const _, param.value.len());

                let param_name = &param.value;
                ctx.module_scopes.add_to_current(param_name, (param_ref, llvm::core::LLVMTypeOf(param_ref)));
            }

            for stmt in &body[..body.len()-1] {
                match_statement(ctx, &mut function_builder, stmt);
            }

            let result = match body.last().unwrap(/* TODO: remove unwrap */).as_ref() {
                ast::Stmt::Expr { expr } => match_expression(ctx, &mut function_builder, expr),
                _                        => llvm::core::LLVMConstNull(return_type) // TODO
            };

            let result = deref_if_primitive(function_builder.builder, result, return_type);
            llvm::core::LLVMBuildRet(function_builder.builder, result);

            ctx.module_scopes.end_scope();
        },

        ast::Stmt::Var { name, initializer, .. } => {
            let variable_name = name.value.as_ptr();
            let type_ref      = to_llvm_type(ctx.llvm_ctx, &initializer.type_kind);
            let type_ref = if type_ref.is_null() { 
                let types::TypeKind::Struct{ name: ref struct_type_name, .. } = initializer.type_kind else {
                    panic!();
                };
                // #horribleways
                *ctx.declarations2.get(struct_type_name).unwrap() 
            } else { 
                type_ref 
            };

            let variable = llvm
                ::core
                ::LLVMBuildAlloca(builder.builder, type_ref, variable_name as *const _);

            ctx.name = Some(name.value.clone());

            let value = match_expression(ctx, builder, initializer);
            llvm::core::LLVMBuildStore(builder.builder, value, variable);

            ctx.module_scopes
               .add_to_current(&name.value, (variable, llvm::core::LLVMTypeOf(variable)));
        },

        ast::Stmt::Const { name, initializer, .. } => {
            let type_ref      = to_llvm_type(ctx.llvm_ctx, &initializer.type_kind);
            let variable_name = ffi::CString::new(name.value.clone()).unwrap();

            ctx.name = Some(name.value.clone());

            // Global scoped variables work differently.
            let variable = if ctx.is_global() {
                let global = llvm::core::LLVMAddGlobal(builder.module, type_ref, variable_name.as_ptr());
                let value  = match_expression(ctx, builder, initializer);

                llvm::core::LLVMSetInitializer(global, value);

                global
            } else {
                let variable = llvm
                    ::core
                    ::LLVMBuildAlloca(builder.builder, type_ref, variable_name.as_ptr());

                let value = match_expression(ctx, builder, initializer);
                llvm::core::LLVMBuildStore(builder.builder, value, variable);

                variable
            };

            ctx.module_scopes.add_to_current(&name.value, (variable, type_ref));
        },

        ast::Stmt::For { initializer, condition, advancement, body } => {
            let start_branch       = builder.append_block(ctx.function_ref(), "_for_start");
            let condition_branch   = builder.append_block(ctx.function_ref(), "_for_condition");
            let body_branch        = builder.append_block(ctx.function_ref(), "_for_body");
            let advancement_branch = builder.append_block(ctx.function_ref(), "_for_advancement");
            let end_branch         = builder.append_block(ctx.function_ref(), "_for_end");

            builder.build_break(start_branch);

            // initializer
            {
                builder.set_position(start_branch);
                match_statement(ctx, builder, initializer);
                builder.build_break(body_branch);
            }

            // condition
            {
                builder.set_position(condition_branch);
                let condition_expr = match_expression(ctx, builder, condition);
                builder.build_condition(condition_expr, body_branch, end_branch);
            }

            // body
            {
                builder.set_position(body_branch);
                for stmt in body {
                    match_statement(ctx, builder, stmt);
                }
                builder.build_break(advancement_branch);
            }

            // advancement
            {
                builder.set_position(advancement_branch);
                match_statement(ctx, builder, advancement);
                builder.build_break(condition_branch);
            }

            builder.set_position(end_branch);
        },

        ast::Stmt::While { condition, body } => {
            let start_branch = builder.append_block(ctx.function_ref(), "_while_start");
            let body_branch  = builder.append_block(ctx.function_ref(), "_while_body");
            let end_branch   = builder.append_block(ctx.function_ref(), "_while_end");

            builder.build_break(start_branch);

            // condition
            {
                builder.set_position(start_branch);
                let condition_expr = match_expression(ctx, builder, condition);
                builder.build_condition(condition_expr, body_branch, end_branch);
            }

            // body
            {
                builder.set_position(body_branch);
                for stmt in body {
                    match_statement(ctx, builder, stmt);
                }
                builder.build_break(start_branch);
            }

            builder.set_position(end_branch);

        },

        ast::Stmt::Expr { expr } => { match_expression(ctx, builder, expr); },

        _ => ()
    }
}

pub unsafe fn match_expression(ctx: &mut Context, builder: &mut Builder, expr: &ast::ExprInfo) -> llvm::prelude::LLVMValueRef
{
    match &expr.value {
        ast::Expr::Bad { .. } => todo!(),

        ast::Expr::Block { statements, value } => {
            ctx.module_scopes.begin_scope();

            for stmt in statements {
                match_statement(ctx, builder, stmt);
            }

            let expr = match_expression(ctx, builder, value);

            ctx.module_scopes.end_scope();

            let return_type = llvm::core::LLVMTypeOf(expr);
            deref_if_ptr(builder.builder, expr, return_type)
        },

        ast::Expr::If { condition, then_branch, then_value, else_branch, else_value } => {
            let branch_entry_block = builder.append_block(ctx.function_ref(), "_entry_branch");
            builder.build_break(branch_entry_block);
            builder.set_position(branch_entry_block);

            let condition_expr = match_expression(ctx, builder, condition);

            let mut incoming_values = Vec::with_capacity(2);
            let mut incoming_blocks = Vec::with_capacity(2);

            let then_block = builder.append_block(ctx.function_ref(), "_then_branch");
            builder.set_position(then_block);

            let (then_result, then_exit_block) = {
                for stmt in then_branch {
                    match_statement(ctx, builder, stmt);
                }

                let then_result = match_expression(ctx, builder, then_value);

                (then_result, builder.basic_block.unwrap())
            };

            incoming_values.push(then_result);
            incoming_blocks.push(then_exit_block);

            let else_block = builder.append_block(ctx.function_ref(), "_else_branch");
            builder.set_position(else_block);

            let (else_result, else_exit_block) = {
                for stmt in else_branch {
                    match_statement(ctx, builder, stmt);
                }

                let else_result = match_expression(ctx, builder, else_value);

                (else_result, builder.basic_block.unwrap())
            };

            incoming_values.push(else_result);
            incoming_blocks.push(else_exit_block);

            let end_block = builder.append_block(ctx.function_ref(), "_end_branch");

            for block in &incoming_blocks {
                builder.set_position(*block);
                builder.build_break(end_block);
            }

            builder.set_position(branch_entry_block);

            builder.build_condition(condition_expr, then_block, else_block);

            let count           = incoming_values.len() as u32;
            let incoming_values = incoming_values.as_mut_ptr();
            let incoming_blocks = incoming_blocks.as_mut_ptr();

            builder.set_position(end_block);

            let branch_value_type = to_llvm_type(ctx.llvm_ctx, &expr.type_kind);
            let phi_node          = llvm::core::LLVMBuildPhi(builder.builder, branch_value_type, binary_cstr!("_branchphi"));

            llvm::core::LLVMAddIncoming(phi_node, incoming_values, incoming_blocks, count);

            phi_node
        },

        ast::Expr::Binary { left, right, operator } => binary_expr(ctx, builder, left, right, operator),

        ast::Expr::Literal { value } => {
            match expr.type_kind {
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

                types::TypeKind::String { .. } => {
                    let value   = &value.value;
                    let trimmed = value[1..value.len()-1].to_string(); // Strip away '"' from start and end.
                    let val     = ffi::CString::new(trimmed).unwrap();
                    llvm
                        ::core
                        ::LLVMConstStringInContext(ctx.llvm_ctx, val.as_ptr(), value.len() as u32, 0)
                }

                _ => panic!("Unrecognized literal type {:#?}", expr),
            }
        }

        ast::Expr::Variable { name } => {
            let (variable, _) = ctx.module_scopes.get(&name.value).unwrap();
            *variable
        }

        ast::Expr::Assignment { name, value } => {
            let value_expr        = match_expression(ctx, builder, value);
            let (variable_ref, _) = ctx.module_scopes.get(&name.value).unwrap();

            let value = deref_if_primitive(builder.builder, value_expr, to_llvm_type(ctx.llvm_ctx, &value.type_kind));
            llvm::core::LLVMBuildStore(builder.builder, value, *variable_ref)
        },

        ast::Expr::MemberAccess { instance_name, member_name } => {
            let instance_type = ctx.type_info.get_in_scope(ctx.current_scope(), &instance_name.value);
            let Some(types::TypeKind::Struct { field_names, .. }) = instance_type else {
                panic!("Expect 'struct' instance type.");
            };

            let member_index = field_names
                .iter()
                .position(|f| f == &member_name.value)
                .unwrap();
            // First index in GEP instruction is of the struct itself.
            let member_index = member_index + 1; 
            
            let mut indices: Vec<llvm::prelude::LLVMValueRef> = (0..field_names.len() + 1)
                .map(|i| {
                    let t = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
                    llvm::core::LLVMConstInt(t, i as u64, 0)
                })
                .collect();

            let (struct_ptr, struct_type) = *ctx
                .module_scopes
                .get_from_scope(ctx.current_scope(), &instance_name.value)
                .unwrap(); 

            let struct_ptr = deref_if_ptr(builder.builder, struct_ptr, struct_type);

            println!("MEMBER INDEX {:?}", member_index);

            let elem_type = to_llvm_type(ctx.llvm_ctx, &expr.type_kind);
            let member_ref = llvm::core::LLVMBuildInBoundsGEP2
            (
                builder.builder, 
                elem_type, 
                struct_ptr,// struct_ptr.as_mut().unwrap(), 
                indices.as_mut_ptr(), 
                member_index as u32, 
                member_name.value.clone().as_ptr() as * const _,
            );

            deref_if_primitive(builder.builder, member_ref, elem_type)
        }

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
                    .enumerate()
                    .filter(|(_, (key, _))| key != &name.value)
                    .map(|(i, (name, var))| {
                        let type_kind = ctx
                            .type_info
                            .get_from_scope(ctx.current_scope(), name)
                            .expect("Expect argument type kind.");
                        let source_type      = to_llvm_type(ctx.llvm_ctx, type_kind);
                        let destination_type = function.param_types[i];

                        prime_argument(builder.builder, *var, source_type, destination_type)
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
                    let arg              = match_expression(ctx, builder, a);
                    let source_type      = to_llvm_type(ctx.llvm_ctx, &a.type_kind);
                    let destination_type = function.param_types[i];

                    prime_argument(builder.builder, arg, source_type, destination_type)
                })
                .collect();

            let total_args_count = initial_args.len() + closed_variables.len();

            let mut args: Vec<llvm::prelude::LLVMValueRef> = Vec::with_capacity(total_args_count);
            args.append(&mut initial_args);
            args.append(&mut closed_variables);

            let result = llvm::core::LLVMBuildCall2
            (
                builder.builder,
                function.function_type,
                function.function,
                args.as_mut_ptr(),
                function.arity as u32,
                format!("{}_call", function.name).as_ptr() as *const _,
            );

            deref_if_primitive(builder.builder, result, function.function_type)
        },

        ast::Expr::Function { params, body, .. }
            => {
                let name = ctx.name.take().unwrap(/*TODO: remove unwrap*/);
                closure(ctx, builder, &name, params.to_vec(), body.to_vec())
            }

        ast::Expr::Struct { name, members, values } => {
            let type_kind = ctx
                .type_info
                .get_from_scope(ctx.current_scope(), &name.value)
                .unwrap(/*TODO: remove unwrap*/);

            let types::TypeKind::Struct { name: struct_type_name, .. } = type_kind else {
                panic!();
            };
            // let llvm_type = to_llvm_type(ctx.llvm_ctx, type_kind);
            let llvm_type = *ctx.declarations2.get(struct_type_name).unwrap();

            // TODO: actual alloc size.
            let struct_pointer = llvm::core::LLVMBuildAlloca
            (
                builder.builder, 
                llvm_type.as_mut().unwrap(), 
                "test".as_ptr() as * const _
            );

            let mut indices: Vec<llvm::prelude::LLVMValueRef> = (0..members.len() + 1)
                .map(|i| {
                    let t = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
                    llvm::core::LLVMConstInt(t, i as u64, 0)
                }).collect();

            // Struct fields initialisation.
            for i in 0..members.len() {
                let member_name        = &members[i];
                let member_initializer = values[i].as_ref();
                
                let value = match_expression(ctx, builder, member_initializer);

                let elem_type = to_llvm_type(ctx.llvm_ctx, &member_initializer.type_kind);
                let member_ref = llvm::core::LLVMBuildInBoundsGEP2
                (
                    builder.builder, 
                    elem_type, 
                    struct_pointer,
                    indices.as_mut_ptr(), 
                    i as u32 + 1,
                    member_name.value.clone().as_ptr() as * const _,
                );

                let value = deref_if_primitive(builder.builder, value, elem_type);
                llvm::core::LLVMBuildStore(builder.builder, value, member_ref);
            }
            
            struct_pointer
        }
    }
}

pub unsafe fn binary_expr
(
    ctx: &mut Context,
    builder: &mut Builder,
    left: &ast::ExprInfo,
    right: &ast::ExprInfo,
    operator: &scan::Token,
) -> llvm::prelude::LLVMValueRef
{
    let lhs = match_expression(ctx, builder, left);
    let rhs = match_expression(ctx, builder, right);

    let expected_operand_type = to_llvm_type(ctx.llvm_ctx, &left.type_kind);

    if ctx.is_global() {
        match operator.kind {
            scan::TokenKind::Plus  => llvm::core::LLVMConstAdd(lhs, rhs),

            scan::TokenKind::Minus => llvm::core::LLVMConstSub(lhs, rhs),

            scan::TokenKind::Star  => llvm::core::LLVMConstMul(lhs, rhs),

            // TODO: I don't know how to do division :(
            // scan::TokenKind::Slash => llvm
            //     ::core
            //     ::LLVMConstSDiv(lhs, rhs),

            scan::TokenKind::LeftAngle => llvm
                ::core
                ::LLVMConstICmp(llvm::LLVMIntPredicate::LLVMIntSLT, lhs, rhs),

            scan::TokenKind::RightAngle => llvm
                ::core
                ::LLVMConstICmp(llvm::LLVMIntPredicate::LLVMIntSGT, lhs, rhs),

            scan::TokenKind::EqualEqual => llvm
                ::core
                ::LLVMConstICmp(llvm::LLVMIntPredicate::LLVMIntEQ, lhs, rhs),

            scan::TokenKind::BangEqual => llvm
                ::core
                ::LLVMConstICmp(llvm::LLVMIntPredicate::LLVMIntNE, lhs, rhs),

            scan::TokenKind::GreaterEqual => llvm
                ::core
                ::LLVMConstICmp(llvm::LLVMIntPredicate::LLVMIntSGE, lhs, rhs),

            scan::TokenKind::LessEqual => llvm
                ::core
                ::LLVMConstICmp(llvm::LLVMIntPredicate::LLVMIntSLE, lhs, rhs),

            _ => panic!()
        }
    } else {
        let lhs = deref_if_ptr(builder.builder, lhs, expected_operand_type);
        let rhs = deref_if_ptr(builder.builder, rhs, expected_operand_type);

        match operator.kind {
            scan::TokenKind::Plus => llvm
                ::core
                ::LLVMBuildAdd(builder.builder, lhs, rhs, binary_cstr!("_add_result")),

            scan::TokenKind::Minus => llvm
                ::core
                ::LLVMBuildSub(builder.builder, lhs, rhs, binary_cstr!("_sub_result")),

            scan::TokenKind::Star => llvm
                ::core
                ::LLVMBuildMul(builder.builder, lhs, rhs, binary_cstr!("_mul_result")),

            scan::TokenKind::Slash => llvm
                ::core
                ::LLVMBuildSDiv(builder.builder, lhs, rhs, binary_cstr!("_sub_result")),

            scan::TokenKind::LeftAngle => llvm
                ::core
                ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntSLT, lhs, rhs, binary_cstr!("_ltcomp")),

            scan::TokenKind::RightAngle => llvm
                ::core
                ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntSGT, lhs, rhs, binary_cstr!("_gtcomp")),

            scan::TokenKind::EqualEqual => llvm
                ::core
                ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntEQ, lhs, rhs, binary_cstr!("_eqcomp")),

            scan::TokenKind::BangEqual => llvm
                ::core
                ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntNE, lhs, rhs, binary_cstr!("_neqcomp")),

            scan::TokenKind::GreaterEqual => llvm
                ::core
                ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntSGE, lhs, rhs, binary_cstr!("_gecomp")),

            scan::TokenKind::LessEqual => llvm
                ::core
                ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntSLE, lhs, rhs, binary_cstr!("_lecomp")),

            _ => panic!()
        }
    }
}

unsafe fn closure
(
    ctx: &mut Context,
    builder: &mut Builder,
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

    let entry_block = llvm::core::LLVMAppendBasicBlockInContext
    (
        ctx.llvm_ctx,
        function_call.function,
        "_entry\0".as_ptr() as * const _
    );

    let new_builder = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
    llvm::core::LLVMPositionBuilderAtEnd(new_builder, entry_block);

    ctx.function = Some(function_call.clone());
    let mut function_builder = Builder::new(ctx.llvm_ctx, builder.module, new_builder);

    for (i, param) in closed_params.iter().enumerate() {
        let param_ref = llvm::core::LLVMGetParam(function_ref, i as u32);
        llvm::core::LLVMSetValueName2(param_ref, param.as_ptr() as * const _, param.len());

        ctx.module_scopes.add_to_current(param, (param_ref, llvm::core::LLVMTypeOf(param_ref)));
    }

    if !body.is_empty() {
        for stmt in &body[..body.len()-1] {
            match_statement(ctx, &mut function_builder, stmt);
        }
    }

    let result = match body.last().map(|s| s.as_ref()) {
        Some(ast::Stmt::Expr { expr }) => match_expression(ctx, &mut function_builder, expr),
        _                              => llvm::core::LLVMConstNull(return_type),
    };


    let result = deref_if_primitive(function_builder.builder, result, return_type);
    llvm::core::LLVMBuildRet(function_builder.builder, result);

    ctx.module_scopes.end_scope();

    function_ref
}

unsafe fn forward_declare(ctx: &mut Context, builder: &mut Builder)
{
    for scope in &ctx.symbol_table.module.scopes {
        for i in 0..scope.values.len() {
            let name        = &scope.names[i];
            let declaration = &scope.values[i];

            match &declaration.kind {
                semantic_analysis::DeclarationKind::Function {
                    function: semantic_analysis::Function { body, .. }
                } => {
                    let Some(kind) = &ctx.type_info.get_in_scope(scope.index, name) else {
                        panic!("Expected type kind.");
                    };

                    let types::TypeKind::Function { parameter_kinds, return_kind } = kind else {
                        panic!("Expected function type kind.");
                    };

                    let parameter_types: Vec<&types::TypeKind> = parameter_kinds
                        .iter()
                        .map(|p| p.as_ref())
                        .collect();

                    let function_call = builder.build_function
                    (
                        name.to_string(),

                        &parameter_types,

                        return_kind,
                        body.iter().map(|s| *s.clone()).collect(),
                        false,
                    );

                    ctx.declarations.insert(name.clone(), (scope.index, function_call));
                }

                semantic_analysis::DeclarationKind::Closure {
                    captures,
                    function: semantic_analysis::Function { body, .. }
                } => {
                    let Some(kind) = &ctx.type_info.get_in_scope(scope.index, name) else {
                        panic!("Expected type kind.");
                    };

                    let mut capture_kinds: Vec<&types::TypeKind> = captures.iter().map(|c| {
                        let value_type = ctx
                            .type_info
                            .get_from_scope(scope.index, c)
                            .unwrap_or_else(|| panic!("Expect closed variable '{}'", c));

                        value_type
                    }).collect();

                    let types::TypeKind::Function { parameter_kinds, return_kind } = kind else {
                        panic!("Expected function type kind.");
                    };

                    let mut parameter_types: Vec<&types::TypeKind> = parameter_kinds
                        .iter()
                        .map(|p| p.as_ref())
                        .collect();

                    let arity = capture_kinds.len() + parameter_types.len();
                    let mut arg_types = Vec::with_capacity(arity);

                    arg_types.append(&mut capture_kinds);
                    arg_types.append(&mut parameter_types);

                    let function_call = builder.build_function
                    (
                        name.to_string(),

                        &arg_types,

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

const PRIMITIVE_TYPES: [llvm::LLVMTypeKind; 2] = [
    llvm::LLVMTypeKind::LLVMIntegerTypeKind,
    llvm::LLVMTypeKind::LLVMVoidTypeKind,
];

// TODO: shit name
unsafe fn prime_argument
(
    builder: llvm::prelude::LLVMBuilderRef,
    value: llvm::prelude::LLVMValueRef,
    source_type: llvm::prelude::LLVMTypeRef,
    destination_type: llvm::prelude::LLVMTypeRef,
) -> llvm::prelude::LLVMValueRef
{
    let source_type_kind      = llvm::core::LLVMGetTypeKind(source_type);
    let destination_type_kind = llvm::core::LLVMGetTypeKind(destination_type);

    let is_pointer = is_pointer(value);

    // Handle primitives
    if PRIMITIVE_TYPES.contains(&source_type_kind) {
        let is_source_pointer        = PRIMITIVE_TYPES.contains(&source_type_kind) && is_pointer;
        let is_destination_primitive = PRIMITIVE_TYPES.contains(&destination_type_kind);

        return match (is_source_pointer, is_destination_primitive) {
            // Deref pointers into a primitive type.
            (true, true) => llvm
                ::core
                ::LLVMBuildLoad2(builder, source_type, value, binary_cstr!("_deref")),

            (true, false) => {
                // Take address if passing primitive into a pointer.
                let ptr = llvm
                    ::core
                    ::LLVMBuildAlloca(builder, source_type, binary_cstr!("_alloc"));
                llvm::core::LLVMBuildStore(builder, value, ptr);
                ptr
            }

            (false, false) | (false, true) => value
        }
    }

    if is_pointer {
        return value
    }

    // Take pointer of if passing pointer type as value.
    let ptr = llvm
        ::core
        ::LLVMBuildAlloca(builder, source_type, binary_cstr!("_alloc"));
    llvm::core::LLVMBuildStore(builder, value, ptr);
    ptr
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
        param_types: vec![
            llvm::core::LLVMPointerType(llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx), 0),
            llvm::core::LLVMVoidTypeInContext(ctx.llvm_ctx),
        ],
        return_type: llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx),
        code: vec![],
        closure: false,
    }
}
