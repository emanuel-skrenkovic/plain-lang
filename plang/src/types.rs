use crate::{compiler, scan, scope};

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

pub fn infer_types(program: &[compiler::Stmt]) -> scope::Module<TypeKind>
{
    let mut type_info: scope::Module<TypeKind> = scope::Module::new();

    type_info.begin_scope();

    for stmt in &program.to_owned() {
        match_statement(&mut type_info, stmt);
    }

    type_info.end_scope();

    println!("{:#?}", type_info);

    type_info
}

pub fn match_statement(type_info: &mut scope::Module<TypeKind>, stmt: &compiler::Stmt)
{
    match stmt {
        compiler::Stmt::Function { name, params: _, param_types, body } => {
            type_info.begin_scope();

            for statement in body {
                match_statement(type_info, statement);
            }

            let return_kind = if body.is_empty() {
                TypeKind::Unit
            } else if let Some(compiler::Stmt::Expr { expr }) = body.last().map(|s| s.as_ref()) {
                match_expression(type_info, expr)
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

        compiler::Stmt::Declaration { name: _, initializer: _ } => (),

        compiler::Stmt::Block { statements: _ } => (),

        compiler::Stmt::Var { name, initializer } => {
            let kind = match_expression(type_info, initializer);
            type_info.add_to_current(&name.value, kind);
        }

        compiler::Stmt::Const { name, initializer } => {
            let kind = match_expression(type_info, initializer);
            type_info.add_to_current(&name.value, kind);
        }

        compiler::Stmt::For { initializer: _, condition: _, advancement: _, body: _ } => (),

        compiler::Stmt::While { condition: _, body: _ } => (),

        compiler::Stmt::Unary { } => (),

        compiler::Stmt::Return { } => (),

        compiler::Stmt::Expr { expr: _ } => (),
    }
}

pub fn match_expression(type_info: &mut scope::Module<TypeKind>, expr: &compiler::Expr) -> TypeKind
{
    match expr {
        compiler::Expr::Bad { token: _ } => TypeKind::Unknown,

        compiler::Expr::Block { statements: _, value } => match_expression(type_info, value),

        compiler::Expr::If { condition: _, then_branch: _, then_value, else_branch: _, else_value } => {
            let then_branch_type = match_expression(type_info, then_value);
            let else_branch_type = match_expression(type_info, else_value);

            if then_branch_type != else_branch_type {
                panic!("Incompatible types between branches");
            }

            then_branch_type
        },

        compiler::Expr::Binary { left, right, operator: _ } => {
            let left_type  = match_expression(type_info, left);
            let right_type = match_expression(type_info, right);

            if left_type != right_type {
                panic!("Binary operation between incompatible types.");
            }

            left_type
        },

        compiler::Expr::Literal { value } => token_type(value),
        compiler::Expr::Variable { name } => {
            // TODO: fetch defined variable and return its type.
            type_info.get(&name.value).unwrap().clone()
        },

        compiler::Expr::Assignment { name: _, value: _ } => TypeKind::Unit,

        compiler::Expr::Logical => todo!(),

        compiler::Expr::Call { name: _, arguments: _ } => {
            // TODO: get function by name
            // and then use its return type kind as the type kind here.
            todo!()
        },

        compiler::Expr::Function { params: _, param_types, body } => {
            // TODO: handle captured variables as well.

            let parameter_kinds: Vec<Box<TypeKind>> = param_types
                .iter()
                .map(token_type)
                .map(Box::new)
                .collect();

            let return_kind = if body.is_empty() {
                TypeKind::Unit
            } else if let Some(compiler::Stmt::Expr { expr }) = body.last().map(|s| s.as_ref()) {
                match_expression(type_info, expr)
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
