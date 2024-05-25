use std::collections::HashMap;

use crate::{compiler, scan};


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
    pub scopes: Vec<Scope>,
    pub current_scope_index: usize,
}

// TODO: implement a generic scope?
impl SymbolTable
{
    pub fn current_scope(&self) -> &Scope
    {
        &self.scopes[self.current_scope_index]
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope
    {
        &mut self.scopes[self.current_scope_index]
    }

    pub fn begin_scope(&mut self)
    {
        let parent_scope = if self.scopes.is_empty() { None }
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
            declarations: HashMap::new(),
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

    pub fn get_declaration(&self, name: &str) -> Option<Declaration>
    {
        let scope = self.current_scope();

        if let Some(decl) = scope.declarations.get(name) {
            return Some(decl.clone())
        };

        for i in scope.path.iter().rev() {
            let scope = &self.scopes[*i];

            if let Some(decl) = scope.declarations.get(name) {
                return Some(decl.clone())
            };
        }

        None
    }
}

// TODO: build symbol table to forward-declare
// all declarations.
// Walking the AST later is problematic if we don't have this.
pub fn analyse(program: &[compiler::Stmt]) -> SymbolTable
{
    let symbol_table = forward_declarations(program);

    // println!("{:#?}", symbol_table);

    symbol_table
}

pub fn forward_declarations(program: &[compiler::Stmt]) -> SymbolTable
{
    let mut symbol_table = SymbolTable {
        scopes: vec![],
        current_scope_index: 0,
    };

    symbol_table.begin_scope();

    for stmt in &program.to_owned() {
        match_statement(&mut symbol_table, stmt);
    }

    symbol_table
}

pub fn match_statement(symbol_table: &mut SymbolTable, stmt: &compiler::Stmt)
{
    match stmt {
        compiler::Stmt::Function { name, params, body } => {
            symbol_table.current_scope_mut().declarations.insert
            (
                name.value.clone(),
                Declaration::Function {
                    params: params.clone(),
                    body: body.clone(),
                },
            );

            symbol_table.begin_scope();

            for stmt in &body[..body.len()-1] {
                match_statement(symbol_table, stmt);
            }

            // TODO: remove as much as possible of the unwraps and clones.
            if let Some(last) = body.last() {
                match last.as_ref() {
                    compiler
                        ::Stmt
                        ::Expr { expr } => match_expression(expr),
                    _                   => ()
                };
            }

            symbol_table.end_scope();
        },

        compiler::Stmt::Declaration { name: _, initializer: _ } => (),

        compiler::Stmt::Block { statements: _ } => (),

        compiler::Stmt::Var { name: _, initializer: _ } => (),

        compiler::Stmt::Const { name: _, initializer: _ } => (),

        compiler::Stmt::For {  } => (),

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
