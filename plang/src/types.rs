use crate::{ast, scan, scope};


#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind
{
    Unknown,

    Unit,

    Bool,

    I32,

    String { len: usize },

    Function {
        parameter_kinds: Vec<Box<TypeKind>>,
        return_kind: Box<TypeKind>,
    },

    Closure {
        captured_kinds: Vec<Box<TypeKind>>,
        parameter_kinds: Vec<Box<TypeKind>>,
        return_kind: Box<TypeKind>,
    },

    Struct {
        name: String,
        field_names: Vec<String>,
        field_types: Vec<Box<TypeKind>>,
    },

    Reference {
        underlying: Box<TypeKind>,
    },
}

pub fn infer_types(program: &[ast::Node]) -> (Vec<ast::Node>, scope::Module<TypeKind>)
{
    let mut typed_program = Vec::with_capacity(program.len());
    let mut type_info     = scope::Module::new();

    type_info.begin_scope();

    let mut program = program.to_owned();

    infer_global_types(&mut program, &mut type_info);

    for stmt in &mut program {
        let ast::Node::Stmt(stmt) = stmt else {
            continue
        };

        let stmt = match_statement(&mut type_info, stmt);
        typed_program.push(ast::Node::Stmt(stmt));
    }

    type_info.end_scope();

    (typed_program, type_info)
}

pub fn infer_global_types(program: &mut [ast::Node], type_info: &mut scope::Module<TypeKind>)
{
    for node in program.iter_mut() {
        let ast::Node::Stmt(stmt) = node else {
            continue
        };

        match stmt {
            ast::Stmt::Function { name, return_type, param_types, .. } => {
                let parameter_kinds: Vec<Box<TypeKind>> = param_types
                    .iter()
                    .map(|t| type_from_identifier(t, type_info))
                    .map(Box::new)
                    .collect();

                let kind = TypeKind::Function {
                    parameter_kinds,
                    return_kind: Box::new(type_from_identifier(return_type, type_info)),
                };

                type_info.add_to_current(&name.value, kind);
            }

            ast::Stmt::Const { name, initializer, type_name } => {
                let kind = match_expression(type_info, &mut initializer.value);

                if let Some(type_name) = type_name {
                    let defined_type = type_from_identifier(type_name, type_info);

                    if defined_type != kind {
                        panic!("Defined type '{:?}' does not match expression type '{:?}'", defined_type, kind);
                    }
                }

                type_info.add_to_current(&name.value, kind);
            }

            ast::Stmt::Struct { name, members, member_types } => {
                let field_types: Vec<Box<TypeKind>> = member_types
                    .iter()
                    .map(|t| type_from_identifier(t, type_info))
                    .map(Box::new)
                    .collect();

                let field_names = members.iter().map(|m| m.value.clone()).collect();

                let kind = TypeKind::Struct {
                    name: name.value.clone(),
                    field_names,
                    field_types,
                };

                type_info.add_to_current(&name.value, kind);
            }

            _ => ()
        }
    }
}

pub fn match_statement(type_info: &mut scope::Module<TypeKind>, stmt: &mut ast::Stmt) -> ast::Stmt
{
    match stmt {
        ast::Stmt::Function { name, params, return_type, param_types, body } => {
            type_info.begin_scope();

            for i in 0..params.len() {
                let param      = &params[i];
                let param_type = &param_types[i];
                let kind       = type_from_identifier(param_type, type_info);

                println!("PARAM {} KIND {:?}", param.value, kind);
                type_info.add_to_current(&param.value, kind);
            }

            for statement in body.iter_mut() {
                match_statement(type_info, statement);
            }

            let return_kind = if body.is_empty() {
                TypeKind::Unit
            } else if let Some(ast::Stmt::Expr { expr }) = body.last_mut().map(|s| s.as_mut()) {
                match_expression(type_info, &mut expr.value)
            } else {
                TypeKind::Unit
            };

            if return_kind != type_from_identifier(return_type, type_info) {
                panic!("Returned value does not match function definition.\nFunction: {} Value type: {:?} Return type: {:?}", name.value, return_kind, return_type);
            }

            type_info.end_scope();

            let parameter_kinds: Vec<Box<TypeKind>> = param_types
                .iter()
                .map(|t| type_from_identifier(t, type_info))
                .map(Box::new)
                .collect();

            let kind = TypeKind::Function {
                parameter_kinds,
                return_kind: Box::new(return_kind),
            };

            type_info.add_to_current(&name.value, kind);
        },

        ast::Stmt::Struct { name, members, member_types } => {
            let field_types: Vec<Box<TypeKind>> = member_types
                .iter()
                .map(|t| type_from_identifier(t, type_info))
                .map(Box::new)
                .collect();

            let field_names = members.iter().map(|m| m.value.clone()).collect();

            let kind = TypeKind::Struct {
                name: name.value.clone(),
                field_names,
                field_types,
            };

            type_info.add_to_current(&name.value, kind);
        }

        ast::Stmt::Declaration { initializer, .. } => {
            initializer.type_kind = match_expression(type_info, &mut initializer.value);
        },

        ast::Stmt::Var { name, initializer, type_name } => {
            let kind = match_expression(type_info, &mut initializer.value);

            if let Some(type_name) = type_name {
                let defined_type = type_from_identifier(type_name, type_info);

                if defined_type != kind {
                    panic!("Defined type '{:?}' does not match expression type '{:?}'", defined_type, kind);
                }
            }

            initializer.type_kind = kind.clone();
            type_info.add_to_current(&name.value, kind);
        }

        ast::Stmt::Const { name, initializer, type_name } => {
            let kind = match_expression(type_info, &mut initializer.value);

            if let Some(type_name) = type_name {
                let defined_type = type_from_identifier(type_name, type_info);

                if defined_type != kind {
                    panic!("Defined type '{:?}' does not match expression type '{:?}'", defined_type, kind);
                }
            }

            initializer.type_kind = kind.clone();
            type_info.add_to_current(&name.value, kind);
        }

        ast::Stmt::For { initializer, condition, advancement, body } => {
            match_statement(type_info, initializer);

            let condition_type = match_expression(type_info, &mut condition.value);
            if condition_type != TypeKind::Bool {
                panic!("'while' condition type must be a boolean. Found type: {:?}", condition_type);
            }
            condition.type_kind = condition_type;

            match_statement(type_info, advancement);

            for stmt in body {
                match_statement(type_info, stmt);
            }
        }

        ast::Stmt::While { condition, body } => {
            let condition_type = match_expression(type_info, &mut condition.value);
            if condition_type != TypeKind::Bool {
                panic!("'while' condition type must be a boolean. Found type: {:?}", condition_type);
            }

            condition.type_kind = condition_type;

            for stmt in body {
                match_statement(type_info, stmt);
            }
        }

        ast::Stmt::Expr { expr } => {
            expr.type_kind = match_expression(type_info, &mut expr.value);
        },

        _ => ()
    }

    stmt.to_owned()
}

pub fn match_expression(type_info: &mut scope::Module<TypeKind>, expr: &mut ast::Expr) -> TypeKind
{
    match expr {
        ast::Expr::Bad { .. } => TypeKind::Unknown,

        ast::Expr::Block { value, .. } => {
            let kind = match_expression(type_info, &mut value.value);
            value.type_kind = kind.clone();
            kind
        }

        ast::Expr::If { condition, then_value, else_value, .. } => {
            let condition_type   = match_expression(type_info, &mut condition.value);
            let then_branch_type = match_expression(type_info, &mut then_value.value);
            let else_branch_type = match_expression(type_info, &mut else_value.value);

            if then_branch_type != else_branch_type {
                panic!("Incompatible types between branches");
            }

            if condition_type != TypeKind::Bool {
                panic!("'if' condition type must be a boolean. Found type: {:?}", condition_type);
            }

            let kind = then_branch_type.clone();

            condition.type_kind  = condition_type;
            then_value.type_kind = then_branch_type;
            else_value.type_kind = else_branch_type;

            kind
        },

        ast::Expr::Binary { left, right, operator } => {
            let left_type  = match_expression(type_info, &mut left.value);
            let right_type = match_expression(type_info, &mut right.value);

            if left_type != right_type {
                panic!("Binary operation between incompatible types. Left: {:?} Right: {:?}", left_type, right_type);
            }

            let kind = match operator.kind {
                scan::TokenKind::Plus
                | scan::TokenKind::Minus
                | scan::TokenKind::Star
                | scan::TokenKind::Slash
                    => left_type.clone(),

                scan::TokenKind::EqualEqual
                | scan::TokenKind::BangEqual
                | scan::TokenKind::GreaterEqual
                | scan::TokenKind::LessEqual
                | scan::TokenKind::LeftAngle
                | scan::TokenKind::RightAngle
                    => TypeKind::Bool,

                _ => panic!("Unrecognized binary expression token. {:?}", operator.kind)
            };

            left.type_kind  = left_type;
            right.type_kind = right_type;

            kind
        },

        ast::Expr::Literal { value } => token_type(value),

        ast::Expr::Variable { name } => type_info.get(&name.value).unwrap().clone(),

        ast::Expr::Assignment { value, .. } => {
            let kind = match_expression(type_info, &mut value.value);
            value.type_kind = kind.clone();

            TypeKind::Unit
        },

        ast::Expr::MemberAccess { instance_name, member_name } => {
            let instance_type = type_info.get(&instance_name.value).unwrap();

            println!("{}, {:?}", instance_name.value, instance_type);

            let TypeKind::Struct { field_names, field_types, .. } = instance_type else {
                panic!("TODO");
            };

            let index = field_names.iter().position(|n| n == &member_name.value);
            let Some(index) = index else {
                panic!();
            };

            *field_types[index].clone()
        }
        ast::Expr::Logical => todo!(),

        ast::Expr::Call { name, arguments } => {
            for arg in arguments.iter_mut() {
                arg.type_kind = match_expression(type_info, &mut arg.value);
            }

            if name.value == "printf" {
                TypeKind::Unknown
            } else {
                let Some(TypeKind::Function { return_kind, .. }) = type_info.get(&name.value) else {
                    panic!("PANIC PANIC");
                };

                *return_kind.clone()
            }
        },

        ast::Expr::Function { param_types, body, .. } => {
            // TODO: handle captured variables as well.

            let parameter_kinds: Vec<Box<TypeKind>> = param_types
                .iter()
                .map(|t| type_from_identifier(t, type_info))
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

        ast::Expr::Struct { name, values, .. } => {
            for value in values.iter_mut() {
                let kind = match_expression(type_info, &mut value.value);
                value.type_kind = kind;
            }
            type_info.get(&name.value).unwrap().clone()
        }
    }
}

fn type_from_identifier(token: &scan::Token, type_info: &scope::Module<TypeKind>) -> TypeKind
{
    match token.value.as_str() {
        "i32" => TypeKind::I32,
        "string" => TypeKind::String { len: token.value.len() },
        "bool" => TypeKind::Bool,
        _ => type_info.get(&token.value).unwrap().clone()
    }
}

fn token_type(token: &scan::Token) -> TypeKind
{
    match token.kind {
        scan::TokenKind::True | scan::TokenKind::False  => TypeKind::Bool,
        _ => {
            if token.value.starts_with('\"') {
                TypeKind::String { len: token.value.len() }
            } else if let Some(first) = token.value.chars().next() {
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
