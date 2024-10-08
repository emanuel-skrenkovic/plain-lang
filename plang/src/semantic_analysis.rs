use crate::{ast, scan, scope, context};

#[derive(Clone, Debug)]
pub struct Function
{
    pub params: Vec<scan::TokenId>,
    pub body: Vec<Box::<ast::Stmt>>,
}

#[derive(Clone, Debug)]
pub struct Declaration
{
    pub kind: DeclarationKind,
}

#[derive(Clone, Debug)]
pub enum DeclarationKind
{
    NativeFunction 
    {
        name: &'static str
    },

    Function 
    {
        function: Function // Function
        // function Function FuNcTiOn
    },

    Closure 
    {
        captures: Vec<String>,
        function: Function,
    },

    Struct
    {
        name: String,
        member_names: Vec<String>,
        member_types: Vec<String>,
    },

    Const { initializer: Box<ast::ExprInfo> },

    Var,
}

static NATIVE_FUNCTIONS: [Declaration; 1] = 
[
    Declaration { kind: DeclarationKind::NativeFunction { name: "printf" }}
];

#[derive(Debug)]
pub struct SymbolTable
{
    pub module: scope::Module<Declaration>,
}

// TODO: build symbol table to forward-declare
// all declarations.
// Walking the AST later is problematic if we don't have this.
pub fn analyse(ctx: &context::Context, program: Vec<ast::Node>) -> Result<(Vec<ast::Node>, SymbolTable), String>
{
    let mut symbol_table = SymbolTable {
        module: scope::Module::new(),
    };

    symbol_table.module.begin_scope();

    handle_native_functions(&mut symbol_table);
    forward_declarations(ctx, &program, &mut symbol_table);

    symbol_table.module.end_scope();

    Ok((program, symbol_table))
}

pub fn handle_native_functions(symbol_table: &mut SymbolTable)
{
    for native in &NATIVE_FUNCTIONS {
        let DeclarationKind::NativeFunction { name } = native.kind else {
            continue
        };

        symbol_table.module.add_to_current(name, native.clone());    
    }
}

pub fn forward_declarations(ctx: &context::Context, program: &[ast::Node], symbol_table: &mut SymbolTable)
{
    for stmt in program {
        let ast::Node::Stmt(stmt) = stmt else {
            continue
        };

        match_statement(ctx, symbol_table, stmt);
    }
}

pub fn match_statement(ctx: &context::Context, symbol_table: &mut SymbolTable, stmt: &ast::Stmt)
{
    match stmt {
        ast::Stmt::Function { name, params, body, .. } => {
            let kind = DeclarationKind::Function { 
                function: Function { 
                    params: params.clone(), 
                    body: body.clone(),
                },
            };
            let declaration = Declaration { kind };
            symbol_table.module.add_to_current(ctx.token_value(*name), declaration);

            symbol_table.module.begin_scope();

            for stmt in &body[..body.len()-1] {
                match_statement(ctx, symbol_table, stmt);
            }

            symbol_table.module.end_scope();
        },

        ast::Stmt::Struct { name, members, member_types } => {
            let member_names = members.iter().map(|m| ctx.token_value(*m).to_owned()).collect();
            let member_types = member_types.iter().map(|t| ctx.token_value(*t).to_owned()).collect();

            let declaration = Declaration {
                kind: DeclarationKind::Struct {
                    name: ctx.token_value(*name).to_owned(),
                    member_names, 
                    member_types, 
                }
            };

            symbol_table.module.add_to_current(ctx.token_value(*name), declaration);
        }

        ast::Stmt::Var { name, initializer, .. } => {
            if let ast::Expr::Function { params, body, .. } = &initializer.value {
                let kind = DeclarationKind::Function {
                    function: Function {
                        params: params.clone(),
                        body: body.clone(),
                    },
                };
                let declaration = Declaration { kind };
                symbol_table.module.add_to_current(ctx.token_value(*name), declaration);

                symbol_table.module.begin_scope();

                if !body.is_empty() {
                    for stmt in &body[..body.len()-1] {
                        match_statement(ctx, symbol_table, stmt);
                    }
                }

                symbol_table.module.end_scope();
            } else {
                let declaration = Declaration {
                    kind: DeclarationKind::Var,
                };

                symbol_table
                    .module
                    .add_to_current(ctx.token_value(*name), declaration);
            }
        }

        ast::Stmt::Const { name, initializer, .. } => {
            if let ast::Expr::Function { params, body, .. } = &initializer.value {
                let function = Function {
                    params: params.clone(),
                    body: body.clone(),
                };

                let captures = symbol_table
                    .module
                    .captures();

                let declaration = Declaration {
                    kind: DeclarationKind::Closure {
                        captures,
                        function,
                    },
                };

                symbol_table.module.add_to_current(ctx.token_value(*name), declaration);

                symbol_table.module.begin_scope();

                if !body.is_empty() {
                    for stmt in &body[..body.len()-1] {
                        match_statement(ctx, symbol_table, stmt);
                    }
                }

                symbol_table.module.end_scope();
            } else {
                let declaration = Declaration {
                    kind: DeclarationKind::Const { initializer: initializer.clone() },
                };

                symbol_table.module.add_to_current(ctx.token_value(*name), declaration);
            }
        }

        _ => ()
    }
}
