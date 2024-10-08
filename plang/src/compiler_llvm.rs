extern crate llvm_sys as llvm;
use std::collections::BTreeSet;

use macros::binary_cstr;

use crate::{ast, context, scope, scan, semantic_analysis, types};


const PRIMITIVE_TYPES: [llvm::LLVMTypeKind; 3] = [
    llvm::LLVMTypeKind::LLVMIntegerTypeKind,
    llvm::LLVMTypeKind::LLVMVoidTypeKind,
    llvm::LLVMTypeKind::LLVMStructTypeKind,
];

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

/// # Safety
/// TODO
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
        types::TypeKind::Function { value: types::Function { parameter_kinds, return_kind, variadic } }  => {
            // TODO: can we pull this from type_kinds?
            let return_type = to_llvm_type(ctx, return_kind);

            let mut param_types: Vec<llvm::prelude::LLVMTypeRef> = parameter_kinds
                .iter()
                .map(|arg| to_llvm_type(ctx, arg))
                .collect();

            let varargs = i32::from(*variadic);
            
            let arity = param_types.len().try_into().unwrap();
            let function_type = llvm
                ::core
                ::LLVMFunctionType(return_type, param_types.as_mut_ptr(), arity, varargs);

            llvm::core::LLVMPointerType(function_type, 0)
        }
        types::TypeKind::Closure { .. } => todo!(),
        types::TypeKind::Array { .. } => todo!(),
        types::TypeKind::Slice { element_kind, capacity, .. } => {
            let element_type = to_llvm_type(ctx, element_kind);
            let length       = u64::try_from(*capacity).unwrap();

            llvm::core::LLVMArrayType2(element_type, length)
        },
        types::TypeKind::Struct { value: types::Struct { name, .. }, .. }  => {
            let definition = ctx.get_definition(name).expect("Expect definition.");

            let Definition::Struct { type_ref, .. } = definition else {
                panic!("Expect struct definition.");
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

    pub parent_function: Option<llvm::prelude::LLVMValueRef>,
}

impl Builder
{
    /// # Safety
    /// TODO
    pub unsafe fn new
    (
        llvm_ctx: llvm::prelude::LLVMContextRef,
        module: llvm::prelude::LLVMModuleRef,
        parent_function: Option<llvm::prelude::LLVMValueRef>,
    ) -> Self
    {
        let builder = llvm::core::LLVMCreateBuilderInContext(llvm_ctx);
        Self {
            ctx: llvm_ctx,
            builder,
            module,
            basic_block: None,
            parent_function,
        }
    }

    /// # Safety
    /// TODO
    pub unsafe fn set_position(&mut self, basic_block: llvm::prelude::LLVMBasicBlockRef)
    {
        self.basic_block = Some(basic_block);
        llvm::core::LLVMPositionBuilderAtEnd(self.builder, basic_block);
    }

    /// # Safety
    /// TODO
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

    /// # Safety
    /// TODO
    pub unsafe fn build_break(&self, basic_block: llvm::prelude::LLVMBasicBlockRef)
    {
        llvm::core::LLVMBuildBr(self.builder, basic_block);
    }

    /// # Safety
    /// TODO
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

    /// # Safety
    /// TODO
    pub unsafe fn build_struct_definition(&self, ctx: &Context, name: &str) -> Definition
    {
        let struct_type_kind = ctx
            .type_info
            .get_from_scope(ctx.current_scope(), name)
            .expect("Failed to find struct type.");

        let struct_definition_name = CStr::from_str(name);
        let struct_type            = llvm::core::LLVMStructCreateNamed(ctx.llvm_ctx, struct_definition_name.value);

        let struct_value = struct_type_kind
            .as_struct()
            .expect("Expect 'struct' type.");

        let mut member_types: Vec<llvm::prelude::LLVMTypeRef> = struct_value.member_types
            .iter()
            .map(|t| to_llvm_type(ctx, t))
            .collect();

        llvm::core::LLVMStructSetBody(
            struct_type, 
            member_types.as_mut_ptr(), 
            struct_value.member_names.len().try_into().unwrap(), 
            0,
        );

        Definition::Struct {
            name: name.to_owned(),
            member_names: struct_value.member_names.clone(),
            member_types: member_types.clone(),
            type_ref: struct_type,
        }
    }

    /// # Safety
    /// TODO
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

        let vararg = i32::from(variadic);
        let function_type = llvm
            ::core
            ::LLVMFunctionType(return_type, param_types.as_mut_ptr(), arity.try_into().unwrap(), vararg);

        let function_name = CStr::from_str(name);
        let function = llvm
            ::core
            ::LLVMAddFunction(self.module, function_name.value, function_type);

        let name = name.to_owned();

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

    /// # Safety
    /// TODO
    pub unsafe fn struct_member_access
    (
        &self, 
        struct_pointer: llvm::prelude::LLVMValueRef,
        struct_type: llvm::prelude::LLVMTypeRef,
        member_index: usize,
        name: &str,
    ) -> llvm::prelude::LLVMValueRef
    {
        let member_ref_name = CStr::from_str(&format!("_member_access_{name}"));
        llvm::core::LLVMBuildStructGEP2
        (
            self.builder, 
            struct_type, 
            struct_pointer,
            u32::try_from(member_index).unwrap(),
            member_ref_name.value
        )
    }

    /// # Safety
    /// TODO
    pub unsafe fn array_index
    (
        &self,
        array_pointer: llvm::prelude::LLVMValueRef,
        element_type: llvm::prelude::LLVMTypeRef,
        index: llvm::prelude::LLVMValueRef,
    ) -> llvm::prelude::LLVMValueRef
    {
        assert!(is_pointer(array_pointer));
        assert!(!is_pointer(index));

        llvm::core::LLVMBuildInBoundsGEP2
        (
            self.builder, 
            element_type, 
            array_pointer, 
            [index].as_mut_ptr(), 
            1, 
            CStr::from_str("_index_result").value,
        )
    }

    /// # Safety
    /// TODO
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

                (false, false | true) => value
            }
        }

        if is_pointer {
            return value
        }

        // Take pointer of if passing pointer type as value.
        self.assign_to_address(value, source_type, "_alloc")
    }

    unsafe fn deref_if_primitive
    (
        &self,
        value: llvm::prelude::LLVMValueRef,
        expected_type: llvm::prelude::LLVMTypeRef,
    ) -> llvm::prelude::LLVMValueRef
    {
        // TODO: cache this?
        let type_kind = llvm::core::LLVMGetTypeKind(expected_type);

        if is_pointer(value) && PRIMITIVE_TYPES.contains(&type_kind) {
            return llvm
                ::core
                ::LLVMBuildLoad2(self.builder, expected_type, value, binary_cstr!("_deref"));
        }

        value    
    }

    unsafe fn deref_if_ptr
    (
        &self,
        value: llvm::prelude::LLVMValueRef,
        expected_type: llvm::prelude::LLVMTypeRef,
    ) -> llvm::prelude::LLVMValueRef
    {
        if is_pointer(value) {
            return llvm
                ::core
                ::LLVMBuildLoad2(self.builder, expected_type, value, binary_cstr!("_deref"));
        }

        value
    }

    unsafe fn deref
    (
        &self,
        value: llvm::prelude::LLVMValueRef,
        expected_type: llvm::prelude::LLVMTypeRef,
    ) -> llvm::prelude::LLVMValueRef
    {
        llvm
            ::core
            ::LLVMBuildLoad2(self.builder, expected_type, value, binary_cstr!("_deref"))
    }

    unsafe fn llvm_condition
    (
        &self, 
        condition: llvm::prelude::LLVMValueRef,
    ) -> llvm::prelude::LLVMValueRef
    {
        let trunc_op_name  = CStr::from_str("_trunc_result");
        let condition_type = llvm::core::LLVMInt1TypeInContext(self.ctx);
        llvm
            ::core
            ::LLVMBuildTrunc(self.builder, condition, condition_type, trunc_op_name.value)
    }
}

pub struct Context
{
    pub llvm_ctx: llvm::prelude::LLVMContextRef,
    pub modules: Vec<llvm::prelude::LLVMModuleRef>,

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
    /// # Safety
    /// TODO
    #[must_use]
    pub unsafe fn new
    (
        symbol_table: semantic_analysis::SymbolTable,
        type_info: scope::Module<types::TypeKind>,
    ) -> Self
    {
        Self {
            llvm_ctx: llvm::core::LLVMContextCreate(),
            modules: Vec::with_capacity(1),
            module_scopes: scope::Module::new(),

            definition_names: Vec::with_capacity(128),
            definitions: Vec::with_capacity(128),

            symbol_table,
            type_info,
            function: None,
            name: None,
        }
    }

    fn function_ref(&self) -> llvm::prelude::LLVMValueRef
    {
        self.function.expect("Expect function.")
    }

    unsafe fn start_function(&mut self, function: llvm::prelude::LLVMValueRef) -> Builder
    {
        let parent_function = std::mem::replace(&mut self.function, Some(function));
        Builder::new(self.llvm_ctx, self.modules[0], parent_function)
    }

    #[allow(clippy::needless_pass_by_value)]
    fn end_function(&mut self, function_builder: &mut Builder)
    {
        self.function = function_builder.parent_function;
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
                // llvm::core::LLVMDumpModule(module);
                llvm::core::LLVMDisposeModule(module);
            }

            llvm::core::LLVMContextDispose(self.llvm_ctx);
        }
    }
}

/// # Safety
/// TODO
pub unsafe fn compile(comp_ctx: &context::Context, ctx: &mut Context, program: &[ast::Node]) -> *mut llvm::LLVMModule
{
    let module = llvm::core::LLVMModuleCreateWithNameInContext(binary_cstr!("main"), ctx.llvm_ctx);

    ctx.modules.push(module);

    let mut builder = Builder::new(ctx.llvm_ctx, module, None);

    // Compile the rest of the program
    ctx.module_scopes.begin_scope();

    declare_native_functions(ctx, &mut builder);

    for stmt in program {
        let ast::Node::Stmt(stmt) = stmt else {
            continue
        };

        match_statement(comp_ctx, ctx, &mut builder, stmt);
    }

    ctx.module_scopes.end_scope();

    verify_module(module);
    module
}

unsafe fn declare_native_functions(ctx: &mut Context, builder: &mut Builder)
{
    for scope in &ctx.symbol_table.module.scopes {
        for i in 0..scope.values.len() {
            let name = &scope.names[i];
            if ctx.definition_names.contains(name) { continue }

            let declaration = &scope.values[i];

            let semantic_analysis::DeclarationKind::NativeFunction { name } = &declaration.kind else {
                continue
            };

            let kind = &ctx
                .type_info
                .get_from_scope(scope.index, name)
                .expect("Expected type kind.");

            let function = kind.as_function().expect("Expect function type kind.");

            let param_types: Vec<llvm::prelude::LLVMTypeRef> = function.parameter_kinds
                .iter()
                .map(|p| to_llvm_type(ctx, p))
                .collect();

            let return_type = to_llvm_type(ctx, &function.return_kind);

            let function_call = builder
                .build_function(name, param_types, return_type, vec![], false, function.variadic);

            let name = *name;
            ctx.definition_names.push(name.to_owned());
            ctx.definitions.push(function_call.clone());

            let Definition::Function { function, function_type, .. } = function_call else {
                panic!("Expect function definition.")
            };

            ctx.module_scopes.add_to_current(name, (function, function_type));
        }
    }
}

/// # Safety
/// TODO
pub unsafe fn match_statement(comp_ctx: &context::Context, ctx: &mut Context, builder: &mut Builder, stmt: &ast::Stmt)
{
    match stmt {
        ast::Stmt::Struct { name, .. } => {
            let name = comp_ctx.token_value(*name);

            let definition = builder.build_struct_definition(ctx, name);

            let Definition::Struct { type_ref, .. } = definition else { 
                panic!("Expect struct definition.") 
            };

            ctx.definition_names.push(name.to_owned());
            ctx.definitions.push(definition);

            ctx.module_scopes.add_to_current(name, (std::ptr::null_mut(), type_ref));
        }

        ast::Stmt::Function { name, params, body, .. } => {
            let function_name = comp_ctx.token_value(*name);

            let function_call = if let Some(function_call) = ctx.get_definition(function_name) { 
                function_call.clone() 
            } else {
                let kind = &ctx
                    .type_info
                    .get_from_scope(ctx.current_scope(), function_name)
                    .expect("Expected type kind.");

                let function = kind.as_function().unwrap_or_else(|_| panic!("Expected function type kind found {kind:?}."));

                let param_types: Vec<llvm::prelude::LLVMTypeRef> = function.parameter_kinds
                    .iter()
                    .map(|p| to_llvm_type(ctx, p))
                    .collect();

                let return_type = to_llvm_type(ctx, &function.return_kind);
                let body        = body.iter().map(|s| *s.clone()).collect();

                let function_call = builder
                    .build_function(function_name, param_types, return_type, body, false, function.variadic);

                ctx.definition_names.push(function_name.to_owned());
                ctx.definitions.push(function_call.clone());

                function_call
            };

            let Definition::Function { function: function_ref, return_type, param_types, function_type, .. } = function_call else {
                panic!("Expect function definition");
            };

            ctx.module_scopes.add_to_current(function_name, (function_ref, function_type));

            let mut builder = ctx.start_function(function_ref);

            let entry_block = builder.append_block(function_ref, "_entry");
            builder.set_position(entry_block);

            ctx.module_scopes.begin_scope();

            for (i, param) in params.iter().enumerate() {
                let param = comp_ctx.token_value(*param);

                let param_ref  = llvm::core::LLVMGetParam(function_ref, i.try_into().unwrap());
                let param_name = CStr::from_str(param);
                llvm::core::LLVMSetValueName2(param_ref, param_name.value, param_name.len);

                let value = (param_ref, param_types[i]);
                ctx.module_scopes.add_to_current(param, value);
            }

            for stmt in &body[..body.len()-1] {
                match_statement(comp_ctx, ctx, &mut builder, stmt);
            }

            if let Some(ast::Stmt::Expr { expr }) = body.last().map(std::convert::AsRef::as_ref) {
                let result = match_expression(comp_ctx, ctx, &mut builder, expr);

                if let Ok(result) = result {
                    let result = builder.deref_if_primitive(result, return_type);
                    llvm::core::LLVMBuildRet(builder.builder, result);
                };
            }

            ctx.module_scopes.end_scope();
            ctx.end_function(&mut builder);
        },

        ast::Stmt::Var { name, initializer, .. } => {
            let name = comp_ctx.token_value(*name);
            ctx.name = Some(name.to_owned());

            let index = ctx
                .module_scopes
                .add_to_current(name, (std::ptr::null_mut(), std::ptr::null_mut()));

            let type_ref = to_llvm_type(ctx, &initializer.type_kind);    
            let value    = match_expression(comp_ctx, ctx, builder, initializer).unwrap(/* TODO: remove */);
            let variable = if is_pointer(value) { value } else { builder.assign_to_address(value, type_ref, name) };

            ctx.module_scopes.update_in_current(index, (variable, type_ref));
        },

        ast::Stmt::Const { name, initializer, .. } => {
            let name = comp_ctx.token_value(*name);
            ctx.name = Some(name.to_owned());

            let index = ctx
                .module_scopes
                .add_to_current(name, (std::ptr::null_mut(), std::ptr::null_mut()));

            let kind = &initializer.type_kind;

            let type_ref = to_llvm_type(ctx, kind);

            // Global scoped variables work differently.
            let variable = if ctx.is_global() {
                let value  = match_expression(comp_ctx, ctx, builder, initializer).unwrap(/* TODO: remove */);

                let variable_name = CStr::new(name.to_owned());
                let global        = llvm::core::LLVMAddGlobal(builder.module, type_ref, variable_name.value);
                llvm::core::LLVMSetInitializer(global, value);

                global
            } else {
                let value = match_expression(comp_ctx, ctx, builder, initializer).unwrap(/* TODO: remove */);

                if is_declaration(kind) { value } 
                else                    { builder.assign_to_address(value, type_ref, name) }
            };

            ctx.module_scopes.update_in_current(index, (variable, type_ref));
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
                match_statement(comp_ctx, ctx, builder, initializer);
                builder.build_break(body_branch);
            }

            // condition
            {
                builder.set_position(condition_branch);
                let condition_expr = match_expression(comp_ctx, ctx, builder, condition).unwrap(/* TODO: remove */);
                builder.build_condition(condition_expr, body_branch, end_branch);
            }

            // body
            {
                builder.set_position(body_branch);
                for stmt in body {
                    match_statement(comp_ctx, ctx, builder, stmt);
                }
                builder.build_break(advancement_branch);
            }

            // advancement
            {
                builder.set_position(advancement_branch);
                match_statement(comp_ctx, ctx, builder, advancement);
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
                let condition_expr = match_expression(comp_ctx, ctx, builder, condition).unwrap(/* TODO: remove */);
                builder.build_condition(condition_expr, body_branch, end_branch);
            }

            // body
            {
                builder.set_position(body_branch);
                for stmt in body {
                    match_statement(comp_ctx, ctx, builder, stmt);
                }
                builder.build_break(start_branch);
            }

            builder.set_position(end_branch);

        },

        ast::Stmt::Expr { expr } => { let _ = match_expression(comp_ctx, ctx, builder, expr); },
    }
}

#[derive(Debug)]
pub struct ReturnSentinel 
{
    pub value: Option<llvm::prelude::LLVMValueRef>
}

/// # Safety
/// TODO
pub unsafe fn match_expression
(
    comp_ctx: &context::Context,
    ctx: &mut Context, 
    builder: &mut Builder, 
    expr: &ast::ExprInfo,
) -> Result<llvm::prelude::LLVMValueRef, ReturnSentinel>
{
    let result: llvm::prelude::LLVMValueRef = match &expr.value {
        ast::Expr::Bad { .. } => todo!(),

        ast::Expr::Block { statements, value, .. } => {
            ctx.module_scopes.begin_scope();

            for stmt in statements {
                match_statement(comp_ctx, ctx, builder, stmt);
            }

            let value = value
                .as_ref()
                .map(|value| match_expression(comp_ctx, ctx, builder, value));

            ctx.module_scopes.end_scope();

            let return_type = to_llvm_type(ctx, &expr.type_kind); 

            // Check for error only after end_scope is called.
            if let Some(value) = value {
                // Evaluate Result only after the scope is ended so 
                // the scope ends properly.
                // TODO: it is bad that this type of gymnastics is required
                // FIX IT.
                let value = value?;
                builder.deref_if_ptr(value, return_type)
            } else {
                // TODO: get rid of null.
                std::ptr::null_mut()
            }
        },

        ast::Expr::If { conditions, branches, .. } => {
            let start_block = builder.basic_block.unwrap();

            // Appends the cascading blocks for if, else-if conditions.
            let condition_blocks = {
                let mut condition_blocks = Vec::with_capacity(conditions.len());
                condition_blocks.push(start_block);

                for _ in 0..conditions.len() {
                    let else_block = builder.append_block(ctx.function_ref(), "_if_condition");
                    condition_blocks.push(else_block);
                }

                condition_blocks
            };

            let num_branches      = branches.len();
            let mut branch_blocks = Vec::with_capacity(num_branches);
            let mut branch_values = Vec::with_capacity(num_branches);

            // Appends all the code blocks.
            for _ in 0..num_branches {
                let branch_block = builder.append_block(ctx.function_ref(), "_branch_branch");
                branch_blocks.push(branch_block);
            }
            
            let end_block = builder.append_block(ctx.function_ref(), "_end_branch");

            // Back to the beginning  to start writing the conditions.
            builder.set_position(start_block);

            // Since we use cascading if-else if, if we have an else at the end,
            // we need to jump to it instead of after the whole if-else construct.
            let has_else = conditions.len() < branches.len();

            for (i, condition_block) in condition_blocks.iter().enumerate() {
                builder.set_position(*condition_block);

                if let Some(condition) = conditions.get(i) {
                    let condition_type = to_llvm_type(ctx, &condition.type_kind);
                    let condition = match_expression(comp_ctx, ctx, builder, condition)?;
                    // TODO: think about semantics
                    let condition = builder.deref_if_ptr(condition, condition_type);

                    let trunc_name     = CStr::from_str("_trunc_result");
                    let condition_type = llvm::core::LLVMInt1TypeInContext(ctx.llvm_ctx);
                    let i1_condition   = llvm
                        ::core
                        ::LLVMBuildTrunc(builder.builder, condition, condition_type, trunc_name.value);

                    let then_block = branch_blocks[i];
                    let else_block = condition_blocks[i + 1] ;

                    builder.build_condition(i1_condition, then_block, else_block);
                } else {
                    let else_block = if has_else { *branch_blocks.last().unwrap() } 
                                     else        { end_block };

                    builder.build_break(else_block);
                };
            }

            // Writes the code for each of the code blocks.
            for (i, branch) in branches.iter().enumerate() {
                let block = branch_blocks[i];
                builder.set_position(block);
                
                let block_result = match_expression(comp_ctx, ctx, builder, branch);
                let Ok(block_result) = block_result else {
                    let empty = llvm
                        ::core
                        ::LLVMConstNull(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx));
                    branch_values.push(empty);
                    continue
                };

                let branch_type  = to_llvm_type(ctx, &branch.type_kind);
                let block_result = if branch.type_kind == types::TypeKind::Unit {
                   std::ptr::null_mut()
                } else {
                    builder.deref_if_primitive(block_result, branch_type)
                };

                branch_values.push(block_result);
                builder.build_break(end_block);
            }

            builder.set_position(end_block);

            let branch_value_type = to_llvm_type(ctx, &expr.type_kind);
            if is_void(branch_value_type) {
                return Ok(std::ptr::null_mut())
            }

            let phi_name = CStr::from_str("_branchphi");
            let phi_node = llvm
                ::core
                ::LLVMBuildPhi(builder.builder, branch_value_type, phi_name.value);

            let incoming_values_count = u32::try_from(branch_values.len()).unwrap();
            let incoming_values       = branch_values.as_mut_ptr();
            let incoming_blocks       = branch_blocks.as_mut_ptr();

            llvm
                ::core
                ::LLVMAddIncoming(phi_node, incoming_values, incoming_blocks, incoming_values_count);

            phi_node
        },

        ast::Expr::Binary { left, right, operator } => binary_expr(comp_ctx, ctx, builder, left, right, operator),

        ast::Expr::Unary { operator, expr } => {
            let value = match_expression(comp_ctx, ctx, builder, expr)?;
            let value = builder.deref_if_primitive(value, to_llvm_type(ctx, &expr.type_kind));

            let operator_kind = comp_ctx.token_kind(*operator);

            match operator_kind {
                scan::TokenKind::Minus => {
                    let name = CStr::from_str("_neg_result");
                    llvm::core::LLVMBuildNeg(builder.builder, value, name.value)
                }

                scan::TokenKind::Bang => {
                    let zero = llvm
                        ::core
                        ::LLVMConstInt(llvm::core::LLVMInt1TypeInContext(ctx.llvm_ctx), 0.try_into().unwrap(), 0);

                    let value = builder.llvm_condition(value);

                    let name = CStr::from_str("_not_result");
                    llvm
                        ::core
                        ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntEQ, value, zero, name.value)
                }

                _ => todo!()
            }
        }

        ast::Expr::Literal { value } => {
            match expr.type_kind {
                types::TypeKind::Unit =>
                    llvm
                        ::core
                        ::LLVMConstNull(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx)),

                types::TypeKind::Bool =>
                    llvm
                        ::core
                        ::LLVMConstInt(llvm::core::LLVMInt8TypeInContext(ctx.llvm_ctx), u64::from(comp_ctx.token_value(*value) == "true"), 0),

                types::TypeKind::I32 => {
                    let val = comp_ctx.token_value(*value).parse::<u64>().unwrap(/*TODO: remove unwrap*/);
                    llvm
                        ::core
                        ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), val, 1)
                }

                types::TypeKind::String { .. } => {
                    let value   = comp_ctx.token_value(*value);
                    let trimmed = value[1..value.len()-1].to_owned(); // Strip away '"' from start and end.
                    let val     = CStr::new(trimmed);
                    llvm
                        ::core
                        ::LLVMConstStringInContext(ctx.llvm_ctx, val.value, value.len().try_into().unwrap(), 0)
                }

                _ => panic!("Unrecognized literal type {expr:#?}"),
            }
        }

        ast::Expr::Variable { name } => {
            let (variable, _) = ctx.module_scopes.get(comp_ctx.token_value(*name)).unwrap();
            *variable
        }

        ast::Expr::Assignment { left, right } => {
            let value_expr = match_expression(comp_ctx, ctx, builder, right)?;

            let value = match &left.value {
                ast::Expr::Variable { name } =>  {
                    let (variable_ref, _) = ctx.module_scopes.get(comp_ctx.token_value(*name)).unwrap();

                    let value = builder.deref_if_primitive(value_expr, to_llvm_type(ctx, &right.type_kind));
                    llvm::core::LLVMBuildStore(builder.builder, value, *variable_ref)
                },

                ast::Expr::MemberAccess { left: instance, right: member_name } => {
                    let struct_val = match_expression(comp_ctx, ctx, builder, instance)?;

                    let struct_value = instance.type_kind
                        .as_struct()
                        .expect("Expect 'struct' instance type.");

                    // In case the struct is passed in as pointer, get the struct type
                    // from out definitions, as pointers are opaque.
                    let struct_type_kind_index = ctx
                        .definition_names
                        .iter()
                        .position(|n| n == &struct_value.name)
                        .unwrap();

                    let Definition::Struct { type_ref: struct_type, .. } = ctx.definitions[struct_type_kind_index] else {
                        panic!("Expect struct defintion.")
                    };

                    let struct_pointer  = if is_pointer(struct_val) { 
                        struct_val 
                    } else { 
                        builder.assign_to_address(struct_val, struct_type, "_alloca") 
                    };

                    let member       = comp_ctx.token_value(*member_name);
                    let member_index = struct_value
                        .member_names
                        .iter()
                        .position(|f| f == member)
                        .unwrap();

                    let value_type = to_llvm_type(ctx, &right.type_kind);

                    let member_ref = builder.struct_member_access(struct_pointer, struct_type, member_index, member);
                    let value      = builder.deref_if_primitive(value_expr, value_type);

                    llvm::core::LLVMBuildStore(builder.builder, value, member_ref)
                }

                ast::Expr::Index { container, value } => {
                    let slice_val = match_expression(comp_ctx, ctx, builder, container)?;
                    let index     = match_expression(comp_ctx, ctx, builder, value)?;

                    let value_type = to_llvm_type(ctx, &right.type_kind);

                    let member_pointer = builder.array_index(slice_val, value_type, index);

                    let value = builder.deref_if_primitive(value_expr, value_type);
                    llvm::core::LLVMBuildStore(builder.builder, value, member_pointer)
               }

                _ => panic!("Unknown left-hand expression in assignment.")
            };

            value
        },

        ast::Expr::MemberAccess { left, right } => {
            let struct_val = match_expression(comp_ctx, ctx, builder, left)?;

            let struct_value = left.type_kind
                .as_struct()
                .expect("Expect 'struct' instance type.");

            // In case the struct is passed in as pointer, get the struct type
            // from out definitions, as pointers are opaque.
            let struct_type_kind_index = ctx
                .definition_names
                .iter()
                .position(|n| n == &struct_value.name)
                .unwrap();

            let Definition::Struct { type_ref: struct_type, .. } = ctx.definitions[struct_type_kind_index] else {
                panic!("Expect struct defintion.")
            };

            let struct_pointer  = if is_pointer(struct_val) { 
                struct_val 
            } else { 
                builder.assign_to_address(struct_val, struct_type, "_alloca") 
            };

            let member       = comp_ctx.token_value(*right);
            let member_index = struct_value
                .member_names
                .iter()
                .position(|f| f == member)
                .unwrap();

            builder.struct_member_access(struct_pointer, struct_type, member_index, member)
        }

        ast::Expr::Index { container, value } => {
            let container_value = match_expression(comp_ctx, ctx, builder, container)?;

            let val_type = to_llvm_type(ctx, &value.type_kind);
            let val      = match_expression(comp_ctx, ctx, builder, value)?;
            let val      = builder.deref_if_primitive(val, val_type);

            let result = builder.array_index(container_value, val_type, val);
            builder.deref_if_primitive(result, val_type)
        }

        ast::Expr::Return { value, .. } => {
            let result = match_expression(comp_ctx, ctx, builder, value)?;

            // TODO: this should be the type of the return value of the function.
            let expected_type = to_llvm_type(ctx, &value.type_kind);
            let result        = builder.deref_if_primitive(result, expected_type);

            // TODO: this should also be based on the return type of the function.
            let result = match value.type_kind {
                types::TypeKind::Unit => llvm::core::LLVMBuildRetVoid(builder.builder),
                _                     => llvm::core::LLVMBuildRet(builder.builder, result)
            };

            return Err
            (
                ReturnSentinel { value: Some(result) }
            )
        },

        ast::Expr::Call { name, arguments } => {
            let name = comp_ctx.token_value(*name);

            let function = ctx
                .get_definition(name)
                .expect("Expect variable '{name}'.")
                .clone();

            let Definition::Function { 
                function, 
                function_type, 
                param_types, 
                closure, 
                arity, 
                return_type, 
                variadic, 
                .. 
            } = function else {
                panic!("{name} is not an instance of a callable.")
            };

            let mut closed_variables = if closure {
                captured_variables(ctx, name)
                    .iter()
                    .map(|(_, var)| {
                        let var_type  = llvm::core::LLVMTypeOf(*var);
                        let type_kind = llvm::core::LLVMGetTypeKind(var_type);

                        // Take the address of all the captured variables.
                        // The capture should refer to the actual variable, not copy
                        // all the values.
                        if PRIMITIVE_TYPES.contains(&type_kind) {
                            builder.assign_to_address(*var, var_type, "_address_of")
                        } else {
                            *var
                        }
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
                    let arg         = match_expression(comp_ctx, ctx, builder, a)?;
                    let source_type = to_llvm_type(ctx, &a.type_kind);

                    // gosh darn varargs.
                    let destination_type = if i >= param_types.len() { source_type } 
                                           else                      { param_types[i] };

                    Ok::<llvm::prelude::LLVMValueRef, ReturnSentinel>
                    (
                        builder.prime_argument(arg, source_type, destination_type)
                    )
                })
                .map_while(Result::ok)
                .collect();

            let total_args_count = initial_args.len() + closed_variables.len();

            let mut args: Vec<llvm::prelude::LLVMValueRef> = Vec::with_capacity(total_args_count);
            args.append(&mut initial_args);
            args.append(&mut closed_variables);

            // LLVM requires the name of the call result to be an empty string
            // when the function return type is 'void'.
            let is_void = is_void(return_type);
            let call_result_name = if is_void { "" } else { "_call" };

            let call_name = CStr::from_str(call_result_name);
            let call_name = call_name.value;

            let arity      = if variadic { args.len() } else { arity };
            let arity: u32 = arity.try_into().unwrap();

            let result = llvm
                ::core
                ::LLVMBuildCall2(builder.builder, function_type, function, args.as_mut_ptr(), arity, call_name);

            if is_void { result } 
            else       { builder.deref_if_primitive(result, function_type) }
        },

        ast::Expr::ReceiverCall { receiver, name, arguments } => {
            let name = comp_ctx.token_value(*name);

            let receiver_val = match_expression(comp_ctx, ctx, builder, receiver).unwrap(/* TODO: remove unwrap */);

            let function = ctx
                .get_definition(name)
                .expect("Expect variable '{name}'.")
                .clone();

            let Definition::Function { 
                function, 
                function_type, 
                param_types, 
                closure, 
                arity, 
                return_type, 
                variadic, 
                .. 
            } = function else {
                panic!("{name} is not an instance of a callable.")
            };

            let mut closed_variables = if closure {
                captured_variables(ctx, name)
                    .iter()
                    .map(|(_, var)| {
                        let var_type  = llvm::core::LLVMTypeOf(*var);
                        let type_kind = llvm::core::LLVMGetTypeKind(var_type);

                        // Take the address of all the captured variables.
                        // The capture should refer to the actual variable, not copy
                        // all the values.
                        if PRIMITIVE_TYPES.contains(&type_kind) {
                            builder.assign_to_address(*var, var_type, "_address_of")
                        } else {
                            *var
                        }
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
                    let arg         = match_expression(comp_ctx, ctx, builder, a)?;
                    let source_type = to_llvm_type(ctx, &a.type_kind);

                    // gosh darn varargs.
                    let destination_type = if i >= param_types.len() { source_type } 
                                           else                      { param_types[i] };

                    Ok::<llvm::prelude::LLVMValueRef, ReturnSentinel>
                    (
                        builder.prime_argument(arg, source_type, destination_type)
                    )
                })
                .map_while(Result::ok)
                .collect();

            // Defined args + receiver.
            let total_args_count = initial_args.len() + closed_variables.len() + 1;

            let mut args: Vec<llvm::prelude::LLVMValueRef> = Vec::with_capacity(total_args_count);

            let receiver_type = to_llvm_type(ctx, &receiver.type_kind);
            let receiver = builder.deref_if_primitive(receiver_val, receiver_type);
            args.push(receiver);

            args.append(&mut initial_args);
            args.append(&mut closed_variables);

            // LLVM requires the name of the call result to be an empty string
            // when the function return type is 'void'.
            let is_void = is_void(return_type);
            let call_result_name = if is_void { "" } else { "_call" };

            let call_name = CStr::from_str(call_result_name);
            let call_name = call_name.value;

            let arity      = if variadic { args.len() } else { arity };
            let arity: u32 = arity.try_into().unwrap();

            let result = llvm
                ::core
                ::LLVMBuildCall2(builder.builder, function_type, function, args.as_mut_ptr(), arity, call_name);

            if is_void { result } 
            else       { builder.deref_if_primitive(result, function_type) }
        },

        ast::Expr::Function { params, body, .. } => {
            let name = ctx.name.take().unwrap();
            let body = body.iter().map(|s| *s.clone()).collect::<Vec<ast::Stmt>>();
            closure(comp_ctx, ctx, builder, &name, params, &body)
        }

        ast::Expr::Struct { name, values, .. } => {
            let type_kind = ctx
                .type_info
                .get_from_scope(ctx.current_scope(), comp_ctx.token_value(*name))
                .unwrap(/*TODO: remove unwrap*/);

            let struct_value = type_kind
                .as_struct()
                .expect("Expected struct type kind.");

            let struct_type_name = &struct_value.name;
            let definition = builder.build_struct_definition(ctx, struct_type_name);

            ctx.definition_names.push(struct_type_name.to_owned());
            ctx.definitions.push(definition);

            let struct_definition = ctx
                .get_definition(struct_type_name)
                .expect("Expect struct definition.")
                .clone();

            let Definition::Struct { type_ref, member_names, member_types, .. } = struct_definition else {
                panic!("Expect struct definition.");
            };

            let mut aggregate: llvm::prelude::LLVMValueRef = llvm::core::LLVMGetUndef(type_ref);

            // Struct members initialisation.
            for (i, member_initializer) in values.iter().enumerate() {
                let member      = &member_names[i];
                let member_type = member_types[i];

                let value = match_expression(comp_ctx, ctx, builder, member_initializer)?;
                let value = builder.deref_if_primitive(value, member_type);

                let name  = CStr::from_str(member);
                let index = u32::try_from(i).unwrap();

                aggregate = llvm
                    ::core
                    ::LLVMBuildInsertValue(builder.builder, aggregate, value, index, name.value);
            }

            aggregate
        }

        ast::Expr::Slice { initial_values, .. } => {
            let slice_type = &expr.type_kind;

            let types::TypeKind::Slice { element_kind, capacity, .. } = slice_type else {
                panic!("Expect Slice type kind.")
            };

            let capacity = u64::try_from(*capacity).unwrap();

            let initial_values: Vec<llvm::prelude::LLVMValueRef> = initial_values
                .iter()
                .map(|v| match_expression(comp_ctx, ctx, builder, v))
                .map_while(Result::ok)
                .collect();

            let element_type = to_llvm_type(ctx, element_kind);

            let capacity_value = llvm
                ::core
                ::LLVMConstInt(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx), capacity, 1);

            let arr_ptr = llvm::core::LLVMBuildArrayMalloc
            (
                builder.builder, 
                element_type, 
                capacity_value, 
                CStr::new(ctx.name.take().unwrap()).value
            );

            let i32_type = llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx);

            for (i, value) in initial_values.iter().enumerate() {
                let index    = llvm::core::LLVMConstInt(i32_type, u64::try_from(i).unwrap(), 1);
                let location = builder.array_index(arr_ptr, element_type, index);
                let value    = builder.deref_if_ptr(*value, element_type);

                llvm::core::LLVMBuildStore(builder.builder, value, location);
            }

            // This is supposed to be a struct containing size, capacity and the pointer
            // to the element.
            // Instead of just being a pointer.
            arr_ptr
        },
   };

    Ok(result)
}

/// # Safety
/// TODO
pub unsafe fn binary_expr
(
    comp_ctx: &context::Context,
    ctx: &mut Context,
    builder: &mut Builder,
    left: &ast::ExprInfo,
    right: &ast::ExprInfo,
    operator: &scan::TokenId,
) -> llvm::prelude::LLVMValueRef
{
    let lhs = match_expression(comp_ctx, ctx, builder, left).unwrap(/* TODO: remove */);
    let rhs = match_expression(comp_ctx, ctx, builder, right).unwrap(/* TODO: remove */);

    let expected_operand_type = to_llvm_type(ctx, left.type_kind.as_primitive());

    let operator_kind = comp_ctx.token_kind(*operator);

    if ctx.is_global() {
        match operator_kind {
            scan::TokenKind::Plus  => llvm::core::LLVMConstAdd(lhs, rhs),

            scan::TokenKind::Minus => llvm::core::LLVMConstSub(lhs, rhs),

            scan::TokenKind::Star  => llvm::core::LLVMConstMul(lhs, rhs),

            scan::TokenKind::Slash => llvm
                ::core
                ::LLVMBuildSDiv(builder.builder, lhs, rhs, binary_cstr!("_div_result")),

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

            scan::TokenKind::Ampersand | scan::TokenKind::AmpersandAmpersand => llvm
                ::core
                ::LLVMBuildAdd(builder.builder, lhs, rhs, binary_cstr!("_and_result")),

            scan::TokenKind::Pipe | scan::TokenKind::PipePipe => llvm
                ::core
                ::LLVMBuildOr(builder.builder, lhs, rhs, binary_cstr!("_or_result")),

            scan::TokenKind::RightAngleRightAngle => llvm
                ::core
                ::LLVMBuildAShr(builder.builder, lhs, rhs, binary_cstr!("_shr_result")),

            scan::TokenKind::LeftAngleLeftAngle => llvm
                ::core
                ::LLVMConstShl(lhs, rhs),

            scan::TokenKind::Caret => llvm
                ::core
                ::LLVMConstXor(lhs, rhs),

            _ => panic!("Unsupported binary operation in global scope.")
        }
    } else {
        match operator_kind {
            scan::TokenKind::Plus => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildAdd(builder.builder, lhs, rhs, binary_cstr!("_add_result"))
            }

            scan::TokenKind::Minus => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildSub(builder.builder, lhs, rhs, binary_cstr!("_sub_result"))
            }

            scan::TokenKind::Star => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildMul(builder.builder, lhs, rhs, binary_cstr!("_mul_result"))
            }

            scan::TokenKind::Slash => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildSDiv(builder.builder, lhs, rhs, binary_cstr!("_sub_result"))
            }

            scan::TokenKind::LeftAngle => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntSLT, lhs, rhs, binary_cstr!("_ltcomp"))
            }

            scan::TokenKind::RightAngle => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntSGT, lhs, rhs, binary_cstr!("_gtcomp"))
            }

            scan::TokenKind::EqualEqual => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntEQ, lhs, rhs, binary_cstr!("_eqcomp"))
            }

            scan::TokenKind::BangEqual => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntNE, lhs, rhs, binary_cstr!("_neqcomp"))
            }

            scan::TokenKind::GreaterEqual => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntSGE, lhs, rhs, binary_cstr!("_gecomp"))
                }

            scan::TokenKind::LessEqual => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildICmp(builder.builder, llvm::LLVMIntPredicate::LLVMIntSLE, lhs, rhs, binary_cstr!("_lecomp"))
            } 

            scan::TokenKind::Ampersand | scan::TokenKind::AmpersandAmpersand => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildAnd(builder.builder, lhs, rhs, binary_cstr!("_and_result"))
            }

            scan::TokenKind::Pipe | scan::TokenKind::PipePipe => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildOr(builder.builder, lhs, rhs, binary_cstr!("_or_result"))
            }

            scan::TokenKind::RightAngleRightAngle => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildAShr(builder.builder, lhs, rhs, binary_cstr!("_lsh_result"))
            }

            scan::TokenKind::LeftAngleLeftAngle => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildShl(builder.builder, lhs, rhs, binary_cstr!("_ashr_result"))
            }

            scan::TokenKind::Caret => {
                let lhs = builder.deref_if_ptr(lhs, expected_operand_type);
                let rhs = builder.deref_if_ptr(rhs, expected_operand_type);

                llvm
                    ::core
                    ::LLVMBuildXor(builder.builder, lhs, rhs, binary_cstr!("_xor_result"))
            }

            scan::TokenKind::PlusEqual => {
                assert!(is_pointer(lhs), "left operand must be an lvalue");

                let lhs_value = builder.deref(lhs, expected_operand_type);
                let rhs_value = builder.deref_if_ptr(rhs, expected_operand_type);

                let add_name   = CStr::from_str("_add_result");
                let add_result = llvm::core::LLVMBuildAdd(builder.builder, lhs_value, rhs_value, add_name.value);

                llvm::core::LLVMBuildStore(builder.builder, add_result, lhs);
                llvm
                    ::core
                    ::LLVMConstNull(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx))
            }

            scan::TokenKind::MinusEqual => {
                assert!(is_pointer(lhs), "left operand must be an lvalue");

                let lhs_value = builder.deref(lhs, expected_operand_type);
                let rhs_value = builder.deref_if_ptr(rhs, expected_operand_type);

                let sub_name   = CStr::from_str("_sub_result");
                let sub_result = llvm::core::LLVMBuildSub(builder.builder, lhs_value, rhs_value, sub_name.value);

                llvm::core::LLVMBuildStore(builder.builder, sub_result, lhs);
                llvm
                    ::core
                    ::LLVMConstNull(llvm::core::LLVMInt32TypeInContext(ctx.llvm_ctx))            
            }

            _ => panic!("Unknown binary operand '{}'.", comp_ctx.token_value(*operator))
        }
    }
}

unsafe fn closure
(
    comp_ctx: &context::Context,
    ctx: &mut Context,
    builder: &mut Builder,
    name: &str,
    params: &[scan::TokenId],
    body: &[ast::Stmt],
) -> llvm::prelude::LLVMValueRef
{
    let mut closed_variables: Vec<String> = captured_variables(ctx, name)
        .into_iter()
        .map(|(name, _)| name.to_owned())
        .collect();

    let mut params: Vec<String> = params
        .iter()
        .map(|p| comp_ctx.token_value(*p).to_owned())
        .collect();

    let total_values_count = params.len() + closed_variables.len();
    let mut closed_params  = Vec::with_capacity(total_values_count);

    // Maybe keep original TypeKind next to type somewhere. LLVM representation 
    // could be different from our semantics.

    closed_params.append(&mut params);
    closed_params.append(&mut closed_variables);

    let kind = &ctx
        .type_info
        .get_from_scope(ctx.current_scope(), name)
        .expect("Expected type kind.");

    let function = kind.as_function().expect("Expected function type kind, found {kind:?}.");

    let mut parameter_types: Vec<llvm::prelude::LLVMTypeRef> = function.parameter_kinds
        .iter()
        .map(|p| to_llvm_type(ctx, p.as_ref()))
        .collect();

    let mut param_types = Vec::with_capacity(parameter_types.len());
    param_types.append(&mut parameter_types);

    let return_type          = to_llvm_type(ctx, &function.return_kind);
    let body: Vec<ast::Stmt> = body.to_vec();

    let function_call = builder
        .build_function(name, param_types, return_type, body.clone(), true, function.variadic);

    ctx.definition_names.push(name.to_owned());
    ctx.definitions.push(function_call.clone());

    let Definition::Function { function, return_type, param_types, .. } = function_call else {
        panic!("Expect function definition.")
    };
     
    let mut builder = ctx.start_function(function);

    let entry_block = builder.append_block(function, "_entry");
    builder.set_position(entry_block);

    ctx.module_scopes.begin_scope();

    for (i, param) in closed_params.iter().enumerate() {
        let param_ref  = llvm::core::LLVMGetParam(function, i.try_into().unwrap());
        let param_name = CStr::new(param.clone());
        llvm::core::LLVMSetValueName2(param_ref, param_name.value, param_name.len);

        let value = (param_ref, param_types[i]);
        ctx.module_scopes.add_to_current(param, value);
    }

    // #horribleways
    if is_void(return_type) {
        // TODO: this is incorrect I think.
        for stmt in &body[..body.len()] {
            match_statement(comp_ctx, ctx, &mut builder, stmt);
        }

        llvm::core::LLVMBuildRetVoid(builder.builder);
    } else {
        for stmt in &body[..body.len() - 1] {
            match_statement(comp_ctx, ctx, &mut builder, stmt);
        }

        if let Some(ast::Stmt::Expr { expr }) = body.last() {
            let result = match_expression(comp_ctx, ctx, &mut builder, expr);

            if let Ok(result) = result {
                llvm::core::LLVMBuildRet(builder.builder, result);
            };
        }
    };
    
    ctx.module_scopes.end_scope();
    ctx.end_function(&mut builder);

    function
}

fn is_declaration(kind: &types::TypeKind) -> bool
{
    matches!(kind, types::TypeKind::Struct { .. } | types::TypeKind::Function { .. })
}

unsafe fn is_pointer(var: llvm::prelude::LLVMValueRef) -> bool
{
    let var_type      = llvm::core::LLVMTypeOf(var);
    let var_type_kind = llvm::core::LLVMGetTypeKind(var_type);

    var_type_kind == llvm::LLVMTypeKind::LLVMPointerTypeKind
}

/// # Safety
/// TODO
pub unsafe fn is_void(type_ref: llvm::prelude::LLVMTypeRef) -> bool
{
    llvm::core::LLVMGetTypeKind(type_ref) == llvm::LLVMTypeKind::LLVMVoidTypeKind
}

/// # Safety
/// TODO
pub unsafe fn captured_variables<'a>
(
    ctx: &'a Context, 
    self_name: &str,
) -> Vec<(&'a str, llvm::prelude::LLVMValueRef)>
{
    let scope = ctx.module_scopes.current_scope();

    let global_scope = &ctx.module_scopes.scopes[0];
    let globals      = global_scope.names.iter().map(std::string::String::as_str).collect::<Vec<&str>>();
    let to_remove    = BTreeSet::<&str>::from_iter(globals);

    let capacity = scope.names.len() - global_scope.names.len();

    let mut vars: Vec<(&str, llvm::prelude::LLVMValueRef)> = Vec::with_capacity(capacity);

    // Walk front to back in the scope until the closure.
    for i in 0..scope.names.len() {
        let name = &scope.names[i];

        // Once the scope reaches self, exit early. Everything in the 
        // loop after that is going to be out of scope for our closure.
        if name == self_name { 
            break 
        }

        if to_remove.contains(name.as_str()) { continue }

        let (value, _) = scope.values[i];

        vars.push((name.as_str(), value));
    }

    vars
}

/// # Safety
/// TODO
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

/// # Safety
/// TODO: remove - this is just for janky testing.
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
