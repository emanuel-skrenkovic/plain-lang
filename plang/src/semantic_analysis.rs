use crate::{ast, scan, scope};

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

    Const { initializer: Box<ast::ExprInfo> },

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
pub fn analyse(program: &[ast::Node]) -> Result<SymbolTable, String>
{
    let mut symbol_table = SymbolTable {
        module: scope::Module::new(),
    };

    symbol_table.module.begin_scope();
    forward_declarations(program, &mut symbol_table);
    symbol_table.module.end_scope();

    Ok(symbol_table)
}

pub fn forward_declarations(program: &[ast::Node], symbol_table: &mut SymbolTable)
{
    for stmt in &program.to_owned() {
        let ast::Node::Stmt(stmt) = stmt else {
            continue
        };
        match_statement(symbol_table, stmt);
    }
}

pub fn match_statement(symbol_table: &mut SymbolTable, stmt: &ast::Stmt)
{
    match stmt {
        ast::Stmt::Function { name, params, body, .. } => {
            let declaration = Declaration {
                kind: DeclarationKind::Function {
                    function: Function {
                        params: params.clone(),
                        body: body.clone(),
                    }
                },
            };

            symbol_table.module.add_to_current(&name.value.clone(), declaration);

            symbol_table.module.begin_scope();

            for stmt in &body[..body.len()-1] {
                match_statement(symbol_table, stmt);
            }

            symbol_table.module.end_scope();
        },

        ast::Stmt::Var { name, initializer, .. } => {
            match &initializer.value {
                ast::Expr::Function { params, body, .. } => {
                    let kind = DeclarationKind::Function {
                        function: Function {
                            params: params.clone(),
                            body: body.clone(),
                        },
                    };
                    let declaration = Declaration {
                        kind,
                    };

                    symbol_table.module.add_to_current(&name.value, declaration);

                    symbol_table.module.begin_scope();

                    if !body.is_empty() {
                        for stmt in &body[..body.len()-1] {
                            match_statement(symbol_table, stmt);
                        }
                    }

                    symbol_table.module.end_scope();
                }
                _ => {
                    let declaration = Declaration {
                        kind: DeclarationKind::Var,
                    };

                    symbol_table
                        .module
                        .add_to_current(&name.value.clone(), declaration);
                }
            }
        }

        ast::Stmt::Const { name, initializer, .. } => {
            match &initializer.value {
                ast::Expr::Function { params, body, .. } => {
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

                    symbol_table.module.add_to_current(&name.value, declaration);

                    symbol_table.module.begin_scope();

                    if !body.is_empty() {
                        for stmt in &body[..body.len()-1] {
                            match_statement(symbol_table, stmt);
                        }
                    }

                    symbol_table.module.end_scope();
                }

                _ => {
                    let declaration = Declaration {
                        kind: DeclarationKind::Const { initializer: initializer.clone() },
                    };
                    symbol_table
                        .module
                        .add_to_current(&name.value, declaration);
                }
            }
        }

        _ => ()
    }
}
