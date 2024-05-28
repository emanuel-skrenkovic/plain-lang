use std::collections::HashMap;

use crate::{compiler, scan, scope};


#[derive(Debug)]
pub struct Scope
{
    pub index: usize,
    pub path: Vec<usize>,
    pub declarations: HashMap<String, Declaration>,
}

#[derive(Clone, Debug)]
pub enum Declaration
{
    Function {
        params: Vec<scan::Token>,
        body: Vec<Box::<compiler::Stmt>>
    }
}

#[derive(Debug)]
pub struct SymbolTable
{
    pub module: scope::Module<Declaration>,
}

// TODO: build symbol table to forward-declare
// all declarations.
// Walking the AST later is problematic if we don't have this.
pub fn analyse(program: &[compiler::Stmt]) -> SymbolTable
{
    let symbol_table = forward_declarations(program);
    ensure_main(&symbol_table);

    println!("{:#?}", symbol_table);

    symbol_table
}

pub fn ensure_main(symbol_table: &SymbolTable)
{
    let entry_scope = &symbol_table.module.scopes[0]; // TODO: is this correct?

    let Some(main) = entry_scope.values.get("main") else {
        panic!("Expect 'main' function");
    };

    let Declaration::Function { params, body: _ } = main; /*else {
        panic!("Main is not a function!");
    };*/

    if !params.is_empty() {
        panic!("Invalid main function signature: expect no parameters.");
    }
}

pub fn forward_declarations(program: &[compiler::Stmt]) -> SymbolTable
{
    let mut symbol_table = SymbolTable {
        module: scope::Module::new(),
    };

    symbol_table.module.begin_scope();

    for stmt in &program.to_owned() {
        match_statement(&mut symbol_table, stmt);
    }

    symbol_table.module.end_scope();

    symbol_table
}

pub fn match_statement(symbol_table: &mut SymbolTable, stmt: &compiler::Stmt)
{
    match stmt {
        compiler::Stmt::Function { name, params, body } => {
            symbol_table.module.current_scope_mut().values.insert
            (
                name.value.clone(),
                Declaration::Function {
                    params: params.clone(),
                    body: body.clone(),
                },
            );

            symbol_table.module.begin_scope();

            for stmt in &body[..body.len()-1] {
                match_statement(symbol_table, stmt);
            }

            match body.last().map(|s| s.as_ref()) {
                Some(compiler::Stmt::Expr { expr }) => match_expression(expr),
                _                                   => ()
            }

            symbol_table.module.end_scope();
        },

        compiler::Stmt::Declaration { name: _, initializer: _ } => (),

        compiler::Stmt::Block { statements: _ } => (),

        compiler::Stmt::Var { name: _, initializer: _ } => (),

        compiler::Stmt::Const { name: _, initializer: _ } => (),

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
