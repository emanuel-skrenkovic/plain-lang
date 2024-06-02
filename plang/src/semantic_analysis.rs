use crate::{ast, scan, scope, types};

#[derive(Clone, Debug)]
pub struct Function
{
    pub params: Vec<scan::Token>,
    pub body: Vec<Box::<ast::Stmt>>,
}

#[derive(Clone, Debug)]
pub struct Declaration
{
    pub kind: DeclarationKind,
    pub type_kind: types::TypeKind,
}

#[derive(Clone, Debug)]
pub enum DeclarationKind
{
    Function {
        function: Function
    },

    Closure {
        captures: Vec<String>,
        function: Function,
    },

    Const,

    Var,
}

#[derive(Debug)]
pub struct SymbolTable
{
    pub module: scope::Module<Declaration>,
}

// TODO: build symbol table to forward-declare
// all declarations.
// Walking the AST later is problematic if we don't have this.
pub fn analyse
(
    program: &[ast::Stmt],
    type_info: &scope::Module<types::TypeKind>,
) -> Result<SymbolTable, String>
{
    let mut symbol_table = SymbolTable {
        module: scope::Module::new(),
    };

    symbol_table.module.begin_scope();

    declare_main(program, &mut symbol_table, type_info)?;
    forward_declarations(program, &mut symbol_table, type_info);

    symbol_table.module.end_scope();

    println!("{:#?}", symbol_table);

    Ok(symbol_table)
}

pub fn declare_main
(
    program: &[ast::Stmt],
    symbol_table: &mut SymbolTable,
    type_info: &scope::Module<types::TypeKind>,
) -> Result<(), String>
{
    for statement in program {
        if let ast::Stmt::Function { name, params, param_types, body } = statement {
            if name.value != "main" {
                continue
            }

            let declaration = Declaration {
                kind: DeclarationKind::Function {
                    function: Function {
                        params: params.clone(),
                        body: body.clone(),
                    },
                },
                type_kind: type_info.get_in_scope(symbol_table.module.current_scope_index, &name.value).unwrap().clone(),
            };

            symbol_table.module.add_to_current(&name.value, declaration);

            symbol_table.module.begin_scope();

            for stmt in &body[..body.len()-1] {
                match_statement(symbol_table, type_info, stmt);
            }

            if let Some(ast::Stmt::Expr { expr }) = body.last().map(|s| s.as_ref()) {
                match_expression(expr);
            };

            symbol_table.module.end_scope();
            return Ok(())
        }
    }

    Err("Expect 'main' function".to_string())
}

pub fn forward_declarations(
    program: &[ast::Stmt],
    symbol_table: &mut SymbolTable,
    type_info: &scope::Module<types::TypeKind>,
)
{
    for stmt in &program.to_owned() {
        match_statement(symbol_table, type_info, stmt);
    }
}

pub fn match_statement
(
    symbol_table: &mut SymbolTable,
    type_info: &scope::Module<types::TypeKind>,
    stmt: &ast::Stmt,
)
{
    match stmt {
        ast::Stmt::Function { name, params, param_types, body } => {
            if name.value == "main" { return }

            let declaration = Declaration {
                kind: DeclarationKind::Function {
                    function: Function {
                        params: params.clone(),
                        body: body.clone(),
                    }
                },
                type_kind: type_info.get_in_scope(symbol_table.module.current_scope_index, &name.value).unwrap().clone(),
            };

            symbol_table.module.add_to_current(&name.value.clone(), declaration);

            symbol_table.module.begin_scope();

            for stmt in &body[..body.len()-1] {
                match_statement(symbol_table, type_info, stmt);
            }

            if let Some(ast::Stmt::Expr { expr }) = body.last().map(|s| s.as_ref()) {
                match_expression(expr);
            };

            symbol_table.module.end_scope();
        },

        ast::Stmt::Declaration { name: _, initializer: _ } => (),

        ast::Stmt::Block { statements: _ } => (),

        ast::Stmt::Var { name, initializer } => {
            match initializer.as_ref() {
                ast::Expr::Function { params, param_types, body } => {
                    let kind = DeclarationKind::Function {
                        function: Function {
                            params: params.clone(),
                            body: body.clone(),
                        },
                    };
                    let declaration = Declaration {
                        kind,
                        type_kind: type_info.get_in_scope(symbol_table.module.current_scope_index, &name.value).unwrap().clone(),
                    };

                    symbol_table.module.add_to_current(&name.value, declaration);

                    symbol_table.module.begin_scope();

                    if body.len() > 0 {
                        for stmt in &body[..body.len()-1] {
                            match_statement(symbol_table, type_info, stmt);
                        }
                    }

                    if let Some(ast::Stmt::Expr { expr }) = body.last().map(|s| s.as_ref()) {
                        match_expression(expr);
                    };

                    symbol_table.module.end_scope();
                }
                _ => {
                    let declaration = Declaration {
                        kind: DeclarationKind::Var,
                        type_kind: type_info.get_in_scope(symbol_table.module.current_scope_index, &name.value).unwrap().clone(),
                    };

                    symbol_table
                        .module
                        .add_to_current(&name.value.clone(), declaration);
                }
            }
        }

        ast::Stmt::Const { name, initializer } => {
            match initializer.as_ref() {
                ast::Expr::Function { params, param_types, body } => {
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
                        type_kind: type_info.get_in_scope(symbol_table.module.current_scope_index, &name.value).unwrap().clone(),
                    };

                    symbol_table.module.add_to_current(&name.value, declaration);

                    symbol_table.module.begin_scope();

                    if body.len() > 0 {
                        for stmt in &body[..body.len()-1] {
                            match_statement(symbol_table, type_info, stmt);
                        }
                    }

                    if let Some(ast::Stmt::Expr { expr }) = body.last().map(|s| s.as_ref()) {
                        match_expression(expr);
                    };

                    symbol_table.module.end_scope();
                }
                _ => {
                    let declaration = Declaration {
                        kind: DeclarationKind::Const,
                        type_kind: type_info.get_in_scope(symbol_table.module.current_scope_index, &name.value).unwrap().clone(),
                    };
                    symbol_table
                        .module
                        .add_to_current(&name.value, declaration);
                }
            }
        }
,

        ast::Stmt::For { initializer: _, condition: _, advancement: _, body: _ } => (),

        ast::Stmt::While { condition: _, body: _ } => (),

        ast::Stmt::Unary { } => (),

        ast::Stmt::Return { } => (),

        ast::Stmt::Expr { expr: _ } => (),
    }
}

pub fn match_expression(expr: &ast::Expr)
{
    match expr {
        ast::Expr::Bad { token: _ } => (),

        ast::Expr::Block { statements: _, value: _ } => (),

        ast::Expr::If { condition: _, then_branch: _, then_value: _, else_branch: _, else_value: _ } => (),

        ast::Expr::Binary { left: _, right: _, operator: _ } => (),

        ast::Expr::Literal { value: _ } => (),

        ast::Expr::Variable { name: _ } => (),

        ast::Expr::Assignment { name: _, value: _ } => (),

        ast::Expr::Logical => (),

        ast::Expr::Call { name: _, arguments: _ } => (),

        ast::Expr::Function { params: _, param_types: _, body: _ } => (),
    }
}
