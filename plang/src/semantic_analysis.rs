use crate::{compiler, scan, scope};

#[derive(Clone, Debug)]
pub struct Function
{
    pub params: Vec<scan::Token>,
    pub body: Vec<Box::<compiler::Stmt>>,
}

#[derive(Clone, Debug)]
pub enum Declaration
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
pub fn analyse(program: &[compiler::Stmt]) -> Result<SymbolTable, String>
{
    let mut symbol_table = SymbolTable {
        module: scope::Module::new(),
    };

    symbol_table.module.begin_scope();

    declare_main(program, &mut symbol_table)?;
    forward_declarations(program, &mut symbol_table);

    symbol_table.module.end_scope();

    println!("{:#?}", symbol_table);

    Ok(symbol_table)
}

pub fn declare_main(program: &[compiler::Stmt], symbol_table: &mut SymbolTable) -> Result<(), String>
{
    for statement in program {
        if let compiler::Stmt::Function { name, params, body } = statement {
            if name.value != "main" {
                continue
            }

            symbol_table.module.current_scope_mut().values.insert
            (
                name.value.clone(),
                Declaration::Function {
                    function: Function {
                        params: params.clone(),
                        body: body.clone(),
                    },
                },
            );

            symbol_table.module.begin_scope();

            for stmt in &body[..body.len()-1] {
                match_statement(symbol_table, stmt);
            }

            if let Some(compiler::Stmt::Expr { expr }) = body.last().map(|s| s.as_ref()) {
                match_expression(expr);
            };

            symbol_table.module.end_scope();
            return Ok(())
        }
    }

    Err("Expect 'main' function".to_string())
}

pub fn forward_declarations(program: &[compiler::Stmt], symbol_table: &mut SymbolTable)
{
    for stmt in &program.to_owned() {
        match_statement(symbol_table, stmt);
    }
}

pub fn match_statement(symbol_table: &mut SymbolTable, stmt: &compiler::Stmt)
{
    match stmt {
        compiler::Stmt::Function { name, params, body } => {
            if name.value == "main" { return }

            symbol_table.module.current_scope_mut().values.insert
            (
                name.value.clone(),
                Declaration::Function {
                    function: Function {
                        params: params.clone(),
                        body: body.clone(),
                    }
                },
            );

            symbol_table.module.begin_scope();

            for stmt in &body[..body.len()-1] {
                match_statement(symbol_table, stmt);
            }

            if let Some(compiler::Stmt::Expr { expr }) = body.last().map(|s| s.as_ref()) {
                match_expression(expr);
            };

            symbol_table.module.end_scope();
        },

        compiler::Stmt::Declaration { name: _, initializer: _ } => (),

        compiler::Stmt::Block { statements: _ } => (),

        compiler::Stmt::Var { name, initializer } => {
            match initializer.as_ref() {
                compiler::Expr::Function { params, body } => {
                    symbol_table.module.current_scope_mut().values.insert
                    (
                        name.value.clone(),
                        Declaration::Function {
                            function: Function {
                                params: params.clone(),
                                body: body.clone(),
                            },
                        },
                    );

                    symbol_table.module.begin_scope();

                    for stmt in &body[..body.len()-1] {
                        match_statement(symbol_table, stmt);
                    }

                    if let Some(compiler::Stmt::Expr { expr }) = body.last().map(|s| s.as_ref()) {
                        match_expression(expr);
                    };

                    symbol_table.module.end_scope();
                }
                _ => {
                    symbol_table
                        .module
                        .current_scope_mut()
                        .values
                        .insert
                        (name.value.clone(), Declaration::Var);
                }
            }
        }

        compiler::Stmt::Const { name, initializer } => {
            match initializer.as_ref() {
                compiler::Expr::Function { params, body } => {
                    let function = Function {
                        params: params.clone(),
                        body: body.clone(),
                    };

                    let captures = symbol_table.module.captures();

                    symbol_table.module.current_scope_mut().values.insert
                    (
                        name.value.clone(),
                        Declaration::Closure {
                            captures,
                            function,
                        },
                    );

                    symbol_table.module.begin_scope();

                    for stmt in &body[..body.len()-1] {
                        match_statement(symbol_table, stmt);
                    }

                    if let Some(compiler::Stmt::Expr { expr }) = body.last().map(|s| s.as_ref()) {
                        match_expression(expr);
                    };

                    symbol_table.module.end_scope();
                }
                _ => {
                    symbol_table
                        .module
                        .current_scope_mut()
                        .values
                        .insert
                        (name.value.clone(), Declaration::Const);
                }
            }
        }
,

        compiler::Stmt::For { initializer: _, condition: _, advancement: _, body: _ } => (),

        compiler::Stmt::While { condition: _, body: _ } => (),

        compiler::Stmt::Unary { } => (),

        compiler::Stmt::Return { } => (),

        compiler::Stmt::Expr { expr: _ } => (),
    }
}

pub fn match_expression(expr: &compiler::Expr)
{
    match expr {
        compiler::Expr::Bad { token: _ } => (),

        compiler::Expr::Block { statements: _, value: _ } => (),

        compiler::Expr::If { condition: _, then_branch: _, then_value: _, else_branch: _, else_value: _ } => (),

        compiler::Expr::Binary { left: _, right: _, operator: _ } => (),

        compiler::Expr::Literal { value: _ } => (),

        compiler::Expr::Variable { name: _ } => (),

        compiler::Expr::Assignment { name: _, value: _ } => (),

        compiler::Expr::Logical => (),

        compiler::Expr::Call { name: _, arguments: _ } => (),

        compiler::Expr::Function { params: _, body: _ } => (),
    }
}
