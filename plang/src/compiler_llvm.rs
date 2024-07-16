extern crate llvm_sys as llvm;
use std::collections::BTreeSet;

use macros::binary_cstr;

use crate::{ast, scope, scan, semantic_analysis, types};


#[derive(Clone, Debug)]
pub enum Definition
{
    Function { 
        name: String,

        function: llvm::prelude::LLVMValueRef,
        function_type: llvm::prelude::LLVMTypeRef,

        arity: usize,

        param_types: Vec<llvm::prelude::LLVMTypeRef>,
        return_type: llvm::prelude::LLVMTypeRef,

        code: Vec<ast::Stmt>,

        closure: bool,
        variadic: bool,
    },

    Struct { 
        name: String,
        member_names: Vec<String>,
        member_types: Vec<llvm::prelude::LLVMTypeRef>,
        type_ref: llvm::prelude::LLVMTypeRef,
    },
}

pub unsafe fn to_llvm_type(ctx: &Context, type_kind: &types::TypeKind) -> llvm::prelude::LLVMTypeRef
{
    match type_kind {
        types::TypeKind::Unknown          => todo!(),
        types::TypeKind::Unit             => llvm::core::LLVMVoidTypeInContext(ctx.llvm_ctx),
        types::TypeKind::Bool             => llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx),
        types::TypeKind::I32              => llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx),
        types::TypeKind::String { len }   => {
            let char_type = llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx);
            llvm::core::LLVMArrayType2(char_type, *len as u64 + 1)
        }
        types::TypeKind::Function { parameter_kinds, return_kind, variadic }  => {
            let return_type = to_llvm_type(ctx, return_kind);

            let mut param_types: Vec<llvm::prelude::LLVMTypeRef> = parameter_kinds
                .iter()
                .map(|arg| to_llvm_type(ctx, arg))
                .collect();

            let varargs = if *variadic { 1 } else { 0 };
            let function_type = llvm
                ::core
                ::LLVMFunctionType(return_type, param_types.as_mut_ptr(), param_types.len() as u32, varargs);

            llvm::core::LLVMPointerType(function_type, 0)
        }
        types::TypeKind::Closure { .. } => todo!(),
        types::TypeKind::Struct { name, .. }  => {
            let Some(Definition::Struct { type_ref, .. }) = ctx.get_definition(name) else {
                panic!();
            };

            *type_ref
        }
        types::TypeKind::Reference { .. } => llvm::core::LLVMPointerTypeInContext(ctx.llvm_ctx, 0)
    }
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
        let block_name = CStr::from_str(name);
        llvm::core::LLVMAppendBasicBlockInContext
        (
            self.ctx,
            function_ref,
            block_name.value,
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

    pub unsafe fn build_struct_definition(&self, ctx: &Context, name: &str) -> Definition
    {
        let struct_type_kind       = ctx.type_info.get_from_scope(ctx.current_scope(), name);
        let Some(struct_type_kind) = struct_type_kind else {
            panic!("Failed to find struct type.");
        };

        let struct_definition_name = CStr::from_str(name);
        let struct_type            = llvm::core::LLVMStructCreateNamed(ctx.llvm_ctx, struct_definition_name.value);

        let types::TypeKind::Struct { member_names, member_types, .. } = struct_type_kind else {
            panic!("Expect 'struct' type.");
        };

        let mut member_types: Vec<llvm::prelude::LLVMTypeRef> = member_types
            .iter()
            .map(|t| to_llvm_type(ctx, t))
            .collect();

        llvm::core::LLVMStructSetBody(struct_type, member_types.as_mut_ptr(), member_names.len() as u32, 0);

        Definition::Struct {
            name: name.to_string(),
            member_names: member_names.clone(),
            member_types: member_types.clone(),
            type_ref: struct_type,
        }
    }

    pub unsafe fn build_function
    (
        &self,
        name: &str,
        mut param_types: Vec<llvm::prelude::LLVMTypeRef>,
        return_type: llvm::prelude::LLVMTypeRef,
        code: Vec<ast::Stmt>,
        closure: bool,
        variadic: bool,
    ) -> Definition
    {
        let arity = param_types.len();

        let vararg = if variadic { 1 } else { 0 };
        let function_type = llvm
            ::core
            ::LLVMFunctionType(return_type, param_types.as_mut_ptr(), arity as u32, vararg);

        let function_name = CStr::from_str(name);
        let function = llvm
            ::core
            ::LLVMAddFunction(self.module, function_name.value, function_type);

        let name = name.to_string();

        Definition::Function {
            name,
            function,
            function_type,
            arity,
            param_types,
            return_type,
            code,
            closure,
            variadic,
        }
    }

    pub unsafe fn struct_member_access
    (
        &self, 
        ctx: &Context,
        struct_pointer: llvm::prelude::LLVMValueRef,
        member_index: usize,
        member_type: llvm::prelude::LLVMTypeRef,
    ) -> llvm::prelude::LLVMValueRef
    {
        let index_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);
        let indices = 
        [
            llvm::core::LLVMConstInt(index_type, member_index as u64, 0)
        ].as_mut_ptr();

        let member_ref_name = CStr::from_str("_member_access");
        llvm::core::LLVMBuildInBoundsGEP2
        (
            self.builder, 
            member_type, 
            struct_pointer,
            indices,
            1,
            member_ref_name.value
        )
    }

    pub unsafe fn assign_to_address
    (
        &self,
        value: llvm::prelude::LLVMValueRef,
        type_ref: llvm::prelude::LLVMTypeRef,
        name: &str,
    ) -> llvm::prelude::LLVMValueRef
    {
        let name = CStr::from_str(name);
        let container = llvm
            ::core
            ::LLVMBuildAlloca(self.builder, type_ref, name.value);
        llvm::core::LLVMBuildStore(self.builder, value, container);
        container
    }

    // TODO: shit name
    unsafe fn prime_argument
    (
        &self,
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
                    ::LLVMBuildLoad2(self.builder, source_type, value, binary_cstr!("_deref")),

                (true, false) => {
                    // Take address if passing primitive into a pointer.
                    self.assign_to_address(value, source_type, "_alloc")
                }

                (false, false) | (false, true) => value
            }
        }

        if is_pointer {
            return value
        }

        // Take pointer of if passing pointer type as value.
        self.assign_to_address(value, source_type, "_alloc")
    }
}

pub struct Context
{
    pub llvm_ctx: llvm::prelude::LLVMContextRef,
    pub modules: Vec<llvm::prelude::LLVMModuleRef>,

    pub program: Vec<ast::Node>,

    pub module_scopes: scope::Module<(llvm::prelude::LLVMValueRef, llvm::prelude::LLVMTypeRef)>,

    // TODO: think about SoA, at least for type refs.
    pub definition_names: Vec<String>,
    pub definitions: Vec<Definition>,

    pub symbol_table: semantic_analysis::SymbolTable,
    pub type_info: scope::Module<types::TypeKind>,

    pub function: Option<llvm::prelude::LLVMValueRef>,
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

            definition_names: Vec::with_capacity(1024),
            definitions: Vec::with_capacity(1024),

            symbol_table,
            type_info,
            function: None,
            name: None,
        }
    }

    fn function_ref(&self) -> llvm::prelude::LLVMValueRef
    {
        let Some(ref function_ref) = self.function else {
            panic!("Expect function");
        };

        *function_ref
    }

    fn current_scope(&self) -> usize
    {
        self.module_scopes.current_scope_index
    }

    fn is_global(&self) -> bool
    {
        self.current_scope() == 0
    }

    fn get_definition(&self, name: &str) -> Option<&Definition>
    {
        let index = self.definition_names.iter().position(|n| n == name)?;
        Some(&self.definitions[index])
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

    let builder     = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
    let mut builder = Builder::new(ctx.llvm_ctx, module, builder);

    // Adding globals.
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

unsafe fn forward_declare(ctx: &mut Context, builder: &mut Builder)
{
    for scope in &ctx.symbol_table.module.scopes {
        for i in 0..scope.values.len() {
            let name = &scope.names[i];
            if ctx.definition_names.contains(name) { continue }

            let declaration = &scope.values[i];

            match &declaration.kind {
                semantic_analysis::DeclarationKind::NativeFunction { name } => {
                    let Some(kind) = &ctx.type_info.get_from_scope(scope.index, name) else {
                        panic!("Expected type kind.");
                    };

                    let types::TypeKind::Function { parameter_kinds, return_kind, variadic } = kind else {
                        panic!("Expected function type kind.");
                    };

                    let param_types: Vec<llvm::prelude::LLVMTypeRef> = parameter_kinds
                        .iter()
                        .map(|p| to_llvm_type(ctx, p))
                        .collect();

                    let return_type = to_llvm_type(ctx, return_kind);

                    let function_call = builder.build_function
                    (
                        name,
                        param_types,
                        return_type,
                        vec![],
                        false,
                        *variadic,
                    );

                    ctx.definition_names.push(name.to_string());
                    ctx.definitions.push(function_call);
                }

                semantic_analysis::DeclarationKind::Function {
                    function: semantic_analysis::Function { body, .. }
                } => {
                    let Some(kind) = &ctx.type_info.get_from_scope(scope.index, name) else {
                        panic!("Expected type kind.");
                    };

                    let types::TypeKind::Function { parameter_kinds, return_kind, variadic } = kind else {
                        panic!("Expected function type kind.");
                    };

                    let param_types: Vec<llvm::prelude::LLVMTypeRef> = parameter_kinds
                        .iter()
                        .map(|p| to_llvm_type(ctx, p))
                        .collect();

                    let return_type = to_llvm_type(ctx, return_kind);

                    let function_call = builder.build_function
                    (
                        name,
                        param_types,
                        return_type,
                        body.iter().map(|s| *s.clone()).collect(),
                        false,
                        *variadic,
                    );

                    ctx.definition_names.push(name.to_owned());
                    ctx.definitions.push(function_call);
                }

                semantic_analysis::DeclarationKind::Closure {
                    captures,
                    function: semantic_analysis::Function { body, .. }
                } => {
                    let Some(kind) = &ctx.type_info.get_from_scope(scope.index, name) else {
                        panic!("Expected type kind.");
                    };

                    let mut capture_kinds = vec![
                        llvm::core::LLVMPointerTypeInContext(ctx.llvm_ctx, 0); 
                        captures.len()
                    ];

                    let types::TypeKind::Function { parameter_kinds, return_kind, variadic } = kind else {
                        panic!("Expected function type kind, found {:?}.", kind);
                    };

                    let mut parameter_types: Vec<llvm::prelude::LLVMTypeRef> = parameter_kinds
                        .iter()
                        .map(|p| to_llvm_type(ctx, p.as_ref()))
                        .collect();

                    let mut param_types = Vec::with_capacity(capture_kinds.len() + parameter_types.len());

                    param_types.append(&mut capture_kinds);
                    param_types.append(&mut parameter_types);

                    let return_type = to_llvm_type(ctx, return_kind);

                    let function_call = builder.build_function
                    (
                        name,
                        param_types,
                        return_type,
                        body.iter().map(|s| *s.clone()).collect(),
                        true,
                        *variadic,
                    );

                    ctx.definition_names.push(name.to_owned());
                    ctx.definitions.push(function_call);
                }

                semantic_analysis::DeclarationKind::Struct { name, .. } => {
                    let definition = builder.build_struct_definition(ctx, name);

                    ctx.definition_names.push(name.to_owned());
                    ctx.definitions.push(definition);
                }

                _ => (),
            }
        }
    }
}

pub unsafe fn match_statement(ctx: &mut Context, builder: &mut Builder, stmt: &ast::Stmt)
{
    match stmt {
        ast::Stmt::Function { name, params, body, .. } => {
            ctx.module_scopes.begin_scope();

            let function_call = ctx.get_definition(&name.value).unwrap().clone();
            let Definition::Function { function: function_ref, return_type, param_types, .. } = function_call else {
                panic!();
            };

            ctx.function = Some(function_ref);

            let new_builder          = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
            let mut function_builder = Builder::new(ctx.llvm_ctx, builder.module, new_builder);

            let entry_block = function_builder.append_block(function_ref, "_entry");
            function_builder.set_position(entry_block);

            for (i, param) in params.iter().enumerate() {
                let param_ref  = llvm::core::LLVMGetParam(function_ref, i as u32);
                let param_name = CStr::from_str(&param.value);
                llvm::core::LLVMSetValueName2(param_ref, param_name.value, param_name.len);

                let value = (param_ref, param_types[i]);
                ctx.module_scopes.add_to_current(&param.value, value);
            }

            for stmt in &body[..body.len()-1] {
                match_statement(ctx, &mut function_builder, stmt);
            }

            // TODO: looks horrible.
            let result = match body.last().unwrap(/* TODO: remove unwrap */).as_ref() {
                ast::Stmt::Expr { expr } => match_expression(ctx, &mut function_builder, expr),
                _                        => llvm::core::LLVMConstNull(return_type) // TODO
            };

            let result = deref_if_primitive(function_builder.builder, result, return_type);
            llvm::core::LLVMBuildRet(function_builder.builder, result);

            ctx.module_scopes.end_scope();
        },

        ast::Stmt::Var { name, initializer, .. } => {
            ctx.name = Some(name.value.clone());

            let value    = match_expression(ctx, builder, initializer);
            let type_ref = to_llvm_type(ctx, &initializer.type_kind);
            let variable = builder.assign_to_address(value, type_ref, &name.value);

            ctx.module_scopes.add_to_current(&name.value, (variable, type_ref));
        },

        ast::Stmt::Const { name, initializer, .. } => {
            ctx.name = Some(name.value.clone());

            let type_ref = to_llvm_type(ctx, &initializer.type_kind);

            // Global scoped variables work differently.
            let variable = if ctx.is_global() {
                let value  = match_expression(ctx, builder, initializer);

                let variable_name = CStr::new(name.value.clone());
                let global        = llvm::core::LLVMAddGlobal(builder.module, type_ref, variable_name.value);
                llvm::core::LLVMSetInitializer(global, value);

                global
            } else {
                let value    = match_expression(ctx, builder, initializer);
                let variable = builder.assign_to_address(value, type_ref, &name.value);

                variable
            };

            ctx.module_scopes.add_to_current(&name.value, (variable, type_ref));
        },

        ast::Stmt::For { initializer, condition, advancement, body, .. } => {
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

        ast::Stmt::While { condition, body, .. } => {
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

        ast::Expr::Block { statements, value, .. } => {
            ctx.module_scopes.begin_scope();

            for stmt in statements {
                match_statement(ctx, builder, stmt);
            }

            let value = match_expression(ctx, builder, value);

            ctx.module_scopes.end_scope();

            let return_type = to_llvm_type(ctx, &expr.type_kind);
            deref_if_ptr(builder.builder, value, return_type)
        },

        ast::Expr::If { condition, then_branch, then_value, else_branch, else_value, .. } => {
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

            let branch_value_type = to_llvm_type(ctx, &expr.type_kind);
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
                    let val     = CStr::new(trimmed);
                    llvm
                        ::core
                        ::LLVMConstStringInContext(ctx.llvm_ctx, val.value, value.len() as u32, 0)
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

            let value = deref_if_primitive(builder.builder, value_expr, to_llvm_type(ctx, &value.type_kind));
            llvm::core::LLVMBuildStore(builder.builder, value, *variable_ref)
        },

        ast::Expr::MemberAssignment { instance_name, member_name, value } => {
            let instance_type = ctx.type_info.get_from_scope(ctx.current_scope(), &instance_name.value);
            let Some(types::TypeKind::Struct { member_names, .. }) = instance_type else {
                panic!("Expect 'struct' instance type.");
            };

            let member_index = member_names
                .iter()
                .position(|f| f == &member_name.value)
                .unwrap();

            let value_expr = match_expression(ctx, builder, value);
            let value_type = to_llvm_type(ctx, &value.type_kind);

            let (struct_value, struct_type_ref) = *ctx
                .module_scopes
                .get_from_scope(ctx.current_scope(), &instance_name.value)
                .unwrap();

            let struct_pointer = if is_pointer(struct_value) {
                struct_value
            } else {
                builder.assign_to_address(struct_value, struct_type_ref, "_alloca")
            };

            let member_ref = builder.struct_member_access(ctx, struct_pointer, member_index, value_type);
            let value      = deref_if_primitive(builder.builder, value_expr, value_type);

            llvm::core::LLVMBuildStore(builder.builder, value, member_ref)
        },

        ast::Expr::MemberAccess { instance_name, member_name } => {
            let instance_type = ctx.type_info.get_from_scope(ctx.current_scope(), &instance_name.value);
            let Some(types::TypeKind::Struct { member_names, .. }) = instance_type else {
                panic!("Expect 'struct' instance type.");
            };

            let member_index = member_names
                .iter()
                .position(|f| f == &member_name.value)
                .unwrap();

            let (struct_value, struct_type_ref) = *ctx
                .module_scopes
                .get_from_scope(ctx.current_scope(), &instance_name.value)
                .unwrap();

            let struct_pointer = if is_pointer(struct_value) {
                struct_value
            } else {
                builder.assign_to_address(struct_value, struct_type_ref, "_alloca")
            };

            let elem_type  = to_llvm_type(ctx, &expr.type_kind);
            let member_ref = builder.struct_member_access(ctx, struct_pointer, member_index, elem_type);

            deref_if_primitive(builder.builder, member_ref, elem_type)
        }

        ast::Expr::Logical => todo!(),

        ast::Expr::Call { name, arguments } => {
            let function = ctx
                .get_definition(&name.value)
                .unwrap_or_else(|| panic!("Expect variable '{}'.", &name.value))
                .clone();

            let Definition::Function { function, function_type, param_types, closure, arity, return_type, variadic, .. } = function else {
                panic!("{} is not an instance of a callable.", &name.value)
            };

            let mut closed_variables = if closure {
                captured_variables(ctx)
                    .iter()
                    .filter(|(key, _)| key != &name.value)
                    .map(|(_, var)| *var)
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
                    let arg         = match_expression(ctx, builder, a);
                    let source_type = to_llvm_type(ctx, &a.type_kind);

                    // gosh darn varargs.
                    let destination_type = if i >= param_types.len() { to_llvm_type(ctx, &a.type_kind) } 
                                           else                      { param_types[i] };

                    builder.prime_argument(arg, source_type, destination_type)
                })
                .collect();

            let total_args_count = initial_args.len() + closed_variables.len();

            let mut args: Vec<llvm::prelude::LLVMValueRef> = Vec::with_capacity(total_args_count);
            args.append(&mut initial_args);
            args.append(&mut closed_variables);

            let is_void = is_void(return_type);

            // LLVM requires the name of the call result to be an empty string
            // when the function return type is 'void'.
            let call_result_name = if is_void { "" } 
                                   else       { "_call" };

            let call_name = CStr::from_str(call_result_name);
            let result = llvm::core::LLVMBuildCall2
            (
                builder.builder,
                function_type,
                function,
                args.as_mut_ptr(),
                if variadic { args.len() } else { arity } as u32,
                call_name.value,
            );

            if is_void { result } 
            else       { deref_if_primitive(builder.builder, result, function_type) }
        },

        ast::Expr::Function { params, body, .. } => {
            let name = ctx.name.take().unwrap(/*TODO: remove unwrap*/);
            closure(ctx, builder, &name, params.to_vec(), body.to_vec())
        }

        ast::Expr::Struct { name, values, .. } => {
            let type_kind = ctx
                .type_info
                .get_from_scope(ctx.current_scope(), &name.value)
                .unwrap(/*TODO: remove unwrap*/);

            let types::TypeKind::Struct { name: struct_type_name, .. } = type_kind else {
                panic!();
            };

            let struct_definition = ctx.get_definition(struct_type_name).unwrap().clone();
            let Definition::Struct { type_ref, .. } = struct_definition else {
                panic!();
            };

            // TODO: better name
            let struct_alloc_name = CStr::from_str("_struct_alloc");
            let struct_pointer    = llvm
                ::core
                ::LLVMBuildAlloca(builder.builder, type_ref, struct_alloc_name.value);

            // Struct members initialisation.
            for (i, member_initializer) in values.iter().enumerate() {
                let initializer_type = to_llvm_type(ctx, &member_initializer.type_kind);
                let member_ref       = builder.struct_member_access(ctx, struct_pointer, i, initializer_type);

                let value = match_expression(ctx, builder, member_initializer);
                let value = deref_if_primitive(builder.builder, value, initializer_type);

                llvm::core::LLVMBuildStore(builder.builder, value, member_ref);
            }

            llvm
                ::core
                ::LLVMBuildLoad2(builder.builder, type_ref, struct_pointer, binary_cstr!("_deref"))
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

    let expected_operand_type = to_llvm_type(ctx, &left.type_kind);

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

    let mut closed_variables: Vec<String> = captured_variables(ctx)
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

    let function_call = ctx.get_definition(name).unwrap().clone();
    let Definition::Function { function, param_types, return_type, .. } = function_call else {
        panic!()
    };

    ctx.function = Some(function);
     
    let new_builder          = llvm::core::LLVMCreateBuilderInContext(ctx.llvm_ctx);
    let mut function_builder = Builder::new(ctx.llvm_ctx, builder.module, new_builder);

    let entry_block = function_builder.append_block(function, "_entry");
    function_builder.set_position(entry_block);

    for (i, param) in closed_params.iter().enumerate() {
        let param_ref     = llvm::core::LLVMGetParam(function, i as u32);
        let param_name = CStr::new(param.clone());
        llvm::core::LLVMSetValueName2(param_ref, param_name.value, param_name.len);

        let value = (param_ref, param_types[i]);
        ctx.module_scopes.add_to_current(param, value);
    }

    // #horribleways
    if llvm::core::LLVMGetTypeKind(return_type) == llvm::LLVMTypeKind::LLVMVoidTypeKind {
        for stmt in &body[..body.len()] {
            match_statement(ctx, &mut function_builder, stmt);
        }
        llvm::core::LLVMBuildRetVoid(function_builder.builder);
    } else {
        for stmt in &body[..body.len() - 1] {
            match_statement(ctx, &mut function_builder, stmt);
        }

        let result = match body.last().map(|s| s.as_ref()) {
            Some(ast::Stmt::Expr { expr }) => match_expression(ctx, &mut function_builder, expr),
            _                              => llvm::core::LLVMConstNull(return_type),
        };
        llvm::core::LLVMBuildRet(function_builder.builder, result);
    };
    
    ctx.module_scopes.end_scope();

    function
}

unsafe fn is_pointer(var: llvm::prelude::LLVMValueRef) -> bool
{
    let var_type      = llvm::core::LLVMTypeOf(var);
    let var_type_kind = llvm::core::LLVMGetTypeKind(var_type);

    var_type_kind == llvm::LLVMTypeKind::LLVMPointerTypeKind
}

const PRIMITIVE_TYPES: [llvm::LLVMTypeKind; 3] = [
    llvm::LLVMTypeKind::LLVMIntegerTypeKind,
    llvm::LLVMTypeKind::LLVMVoidTypeKind,
    llvm::LLVMTypeKind::LLVMStructTypeKind,
];

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

pub unsafe fn is_void(type_ref: llvm::prelude::LLVMTypeRef) -> bool
{
    llvm::core::LLVMGetTypeKind(type_ref) == llvm::LLVMTypeKind::LLVMVoidTypeKind
}

pub unsafe fn captured_variables(ctx: &Context) -> Vec<(&str, llvm::prelude::LLVMValueRef)>
{
    let scope = ctx.module_scopes.current_scope();

    let global_scope = &ctx.module_scopes.scopes[0];
    let globals      = global_scope.names.iter().map(|n| n.as_str()).collect::<Vec<&str>>();
    let to_remove    = BTreeSet::<&str>::from_iter(globals);

    let capacity = scope.names.len() - global_scope.names.len();

    let mut vars: Vec<(&str, llvm::prelude::LLVMValueRef)> = Vec::with_capacity(capacity);

    for i in 0..scope.names.len() {
        let name = &scope.names[i];
        if to_remove.contains(name.as_str()) { continue }

        let (value, _) = scope.values[i];

        vars.push((name.as_str(), value));
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

struct CStr
{
    value: *mut i8,
    len: usize,
}

impl CStr
{
    pub fn new(value: String) -> Self
    {
        use std::ffi;

        let len   = value.len();
        let value = ffi::CString::new(value).unwrap();
        let value = value.into_raw();

        Self { value, len }
    }

    pub fn from_str(value: &str) -> Self
    {
        use std::ffi;

        let len   = value.len();
        let value = ffi::CString::new(value).unwrap();
        let value = value.into_raw();

        Self { value, len }
    }
}

impl Drop for CStr
{
    fn drop(&mut self)
    {
        unsafe {
            use std::ffi;
            let _ = ffi::CString::from_raw(self.value);
        }
    }
}
