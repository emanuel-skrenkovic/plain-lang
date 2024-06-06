use crate::{ast, scan, scope};


#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind
{
    Unknown,

    Unit,

    Bool,

    I32,

    String,

    Function {
        parameter_kinds: Vec<Box<TypeKind>>,
        return_kind: Box<TypeKind>,
    },

    Closure {
        captured_kinds: Vec<Box<TypeKind>>,
        parameter_kinds: Vec<Box<TypeKind>>,
        return_kind: Box<TypeKind>,
    },

    Reference {
        underlying: Box<TypeKind>,
    },
}

pub fn infer_types(program: &[ast::Node]) -> (Vec<ast::Node>, scope::Module<TypeKind>)
{
    let mut typed_program                      = Vec::with_capacity(program.len());
    let mut type_info: scope::Module<TypeKind> = scope::Module::new();

    type_info.begin_scope();

    for stmt in &mut program.to_owned() {
        if let ast::Node::Stmt(stmt) = stmt {
            let stmt = match_statement(&mut type_info, stmt);
            typed_program.push(ast::Node::Stmt(stmt));
        }
    }

    type_info.end_scope();

    println!("{:#?}", type_info);

    (typed_program, type_info)
}

pub fn match_statement(type_info: &mut scope::Module<TypeKind>, stmt: &mut ast::Stmt) -> ast::Stmt
{
    match stmt {
        ast::Stmt::Function { name, params: _, param_types, body } => {
            type_info.begin_scope();

            for mut statement in body.iter_mut() {
                match_statement(type_info, &mut statement);
            }

            let return_kind = if body.is_empty() {
                TypeKind::Unit
            } else if let Some(ast::Stmt::Expr { expr }) = body.last_mut().map(|s| s.as_mut()) {
                match_expression(type_info, &mut expr.value)
            } else {
                TypeKind::Unit
            };

            type_info.end_scope();

            let parameter_kinds: Vec<Box<TypeKind>> = param_types
                .iter()
                .map(token_type)
                .map(Box::new)
                .collect();

            let kind = TypeKind::Function {
                parameter_kinds,
                return_kind: Box::new(return_kind),
            };

            type_info.add_to_current(&name.value, kind);
        },

        ast::Stmt::Declaration { name: _, initializer } => {
            initializer.type_kind = match_expression(type_info, &mut initializer.value);
        },

        ast::Stmt::Block { statements: _ } => (),

        ast::Stmt::Var { name, initializer } => {
            let kind = match_expression(type_info, &mut initializer.value);
            initializer.type_kind = kind.clone();
            type_info.add_to_current(&name.value, kind);
        }

        ast::Stmt::Const { name, initializer } => {
            let kind = match_expression(type_info, &mut initializer.value);
            initializer.type_kind = kind.clone();
            type_info.add_to_current(&name.value, kind);
        }

        ast::Stmt::For { initializer: _, condition: _, advancement: _, body: _ } => (),

        ast::Stmt::While { condition: _, body: _ } => (),

        ast::Stmt::Unary { } => (),

        ast::Stmt::Return { } => (),

        ast::Stmt::Expr { expr } => {
            expr.type_kind = match_expression(type_info, &mut expr.value);
        },
    }

    stmt.to_owned()
}

pub fn match_expression(type_info: &mut scope::Module<TypeKind>, expr: &mut ast::Expr) -> TypeKind
{
    match expr {
        ast::Expr::Bad { token: _ } => TypeKind::Unknown,

        ast::Expr::Block { statements: _, value } => {
            let kind = match_expression(type_info, &mut value.value);
            value.type_kind = kind.clone();
            kind
        }

        ast::Expr::If { condition: _, then_branch: _, then_value, else_branch: _, else_value } => {
            let then_branch_type = match_expression(type_info, &mut then_value.value);
            let else_branch_type = match_expression(type_info, &mut else_value.value);

            if then_branch_type != else_branch_type {
                panic!("Incompatible types between branches");
            }

            let kind = then_branch_type.clone();

            then_value.type_kind = then_branch_type;
            else_value.type_kind = else_branch_type;

            kind
        },

        ast::Expr::Binary { left, right, operator: _ } => {
            let left_type  = match_expression(type_info, &mut left.value);
            let right_type = match_expression(type_info, &mut right.value);

            if left_type != right_type {
                panic!("Binary operation between incompatible types.");
            }

            let kind = left_type.clone();

            left.type_kind  = left_type;
            right.type_kind = right_type;

            kind
        },

        ast::Expr::Literal { value } => token_type(&value),
        ast::Expr::Variable { name } => {
            // TODO: fetch defined variable and return its type.
            type_info.get(&name.value).unwrap().clone()
        },

        ast::Expr::Assignment { name: _, value: _ } => TypeKind::Unit,

        ast::Expr::Logical => todo!(),

        ast::Expr::Call { name: _, arguments } => {
            // TODO: get function by name
            // and then use its return type kind as the type kind here.
            // todo!()
            // type_info.get(&name.value).unwrap().clone()

            for arg in arguments.iter_mut() {
                let kind = match_expression(type_info, &mut arg.value);
                arg.type_kind = kind;
            }

            // type_info.get(&name.value).unwrap().clone()
            TypeKind::Unit
        },

        ast::Expr::Function { params: _, param_types, body } => {
            // TODO: handle captured variables as well.

            let parameter_kinds: Vec<Box<TypeKind>> = param_types
                .iter()
                .map(token_type)
                .map(Box::new)
                .collect();

            let return_kind = if body.is_empty() {
                TypeKind::Unit
            } else if let Some(ast::Stmt::Expr { expr }) = body.last_mut().map(|s| s.as_mut()) {
                match_expression(type_info, &mut expr.value)
            } else {
                TypeKind::Unit
            };

            TypeKind::Function { parameter_kinds, return_kind: Box::new(return_kind) }
        },
    }
}

fn token_type(token: &scan::Token) -> TypeKind
{
    match token.kind {
        scan::TokenKind::True | scan::TokenKind::False  => TypeKind::Bool,
        _ => {
            if token.value.starts_with('\"') {
                TypeKind::String
            } else if let Some(first) = token.value.chars().nth(0) {
                if !char::is_numeric(first) {
                    return TypeKind::Unknown
                }

                let Ok(_) = token.value.parse::<i32>() else {
                    return TypeKind::Unknown
                };

                TypeKind::I32
            } else {
                TypeKind::Unknown
            }
        }
    }
}
