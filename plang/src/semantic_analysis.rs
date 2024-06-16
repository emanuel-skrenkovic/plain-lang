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
    program: &[ast::Node],
    type_info: &scope::Module<types::TypeKind>,
) -> Result<SymbolTable, String>
{
    let mut symbol_table = SymbolTable {
        module: scope::Module::new(),
    };

    symbol_table.module.begin_scope();
    forward_declarations(program, &mut symbol_table, type_info);
    symbol_table.module.end_scope();

    Ok(symbol_table)
}

pub fn forward_declarations(
    program: &[ast::Node],
    symbol_table: &mut SymbolTable,
    type_info: &scope::Module<types::TypeKind>,
)
{
    for stmt in &program.to_owned() {
        let ast::Node::Stmt(stmt) = stmt else {
            continue
        };
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
        ast::Stmt::Function { name, params, return_type: _, param_types: _, body } => {
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

            symbol_table.module.end_scope();
        },

        ast::Stmt::Declaration { name: _, initializer: _ } => (),

        ast::Stmt::Block { statements: _ } => (),

        ast::Stmt::Var { name, initializer } => {
            match &initializer.value {
                ast::Expr::Function { params, return_type: _, param_types: _, body } => {
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

                    if !body.is_empty() {
                        for stmt in &body[..body.len()-1] {
                            match_statement(symbol_table, type_info, stmt);
                        }
                    }

                    symbol_table.module.end_scope();
                }
                _ => {
                    let type_kind = type_info
                        .get_in_scope(symbol_table.module.current_scope_index, &name.value)
                        .unwrap()
                        .clone();

                    let declaration = Declaration {
                        kind: DeclarationKind::Var,
                        type_kind,
                    };

                    symbol_table
                        .module
                        .add_to_current(&name.value.clone(), declaration);
                }
            }
        }

        ast::Stmt::Const { name, initializer } => {
            match &initializer.value {
                ast::Expr::Function { params, return_type: _, param_types: _, body } => {
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

                    if !body.is_empty() {
                        for stmt in &body[..body.len()-1] {
                            match_statement(symbol_table, type_info, stmt);
                        }
                    }

                    symbol_table.module.end_scope();
                }
                _ => {
                    let declaration = Declaration {
                        kind: DeclarationKind::Const,
                        type_kind: initializer.type_kind.clone(),
                    };
                    symbol_table
                        .module
                        .add_to_current(&name.value, declaration);
                }
            }
        }

        ast::Stmt::For { initializer: _, condition: _, advancement: _, body: _ } => (),

        ast::Stmt::While { condition: _, body: _ } => (),

        ast::Stmt::Unary { } => (),

        ast::Stmt::Return { } => (),

        ast::Stmt::Expr { expr: _ } => (),
    }
}
