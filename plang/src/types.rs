use crate::{ast, error, scan, scope};


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

pub fn infer_types
(
    reporter: &mut error::ErrorReporter, 
    program: &[ast::Node],
) -> (Vec<ast::Node>, scope::Module<TypeKind>)
{
    let mut typed_program = Vec::with_capacity(program.len());
    let mut type_info     = scope::Module::new();

    type_info.begin_scope();

    let mut program = program.to_owned();

    let _ = infer_global_types(reporter, &mut program, &mut type_info);

    for stmt in &mut program {
        let ast::Node::Stmt(stmt) = stmt else {
            continue
        };

        let Ok(stmt) = match_statement(reporter, &mut type_info, stmt) else {
            continue
        };

        typed_program.push(ast::Node::Stmt(stmt));
    }

    type_info.end_scope();

    (typed_program, type_info)
}

pub fn infer_global_types(
    reporter: &mut error::ErrorReporter,
    program: &mut [ast::Node], 
    type_info: &mut scope::Module<TypeKind>,
) -> Result<(), error::CompilerError>
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


                let return_kind = match return_type {
                    Some(return_type) => type_from_identifier(return_type, type_info),
                    None              => TypeKind::Unit,
                };
                let kind = TypeKind::Function {
                    parameter_kinds,
                    return_kind: Box::new(return_kind),
                };

                type_info.add_to_current(&name.value, kind);
            }

            ast::Stmt::Const { name, initializer, type_name } => {
                let kind = match_expression(reporter, type_info, &mut initializer.value)?;

                if let Some(type_name) = type_name {
                    let defined_type = type_from_identifier(type_name, type_info);

                    if defined_type != kind {
                        let message = format!("Defined type '{:?}' does not match expression type '{:?}'", defined_type, kind);
                        reporter.error_at(&message, error::CompilerErrorKind::TypeError, name);
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

    Ok(())
}

pub fn match_statement
(
    reporter: &mut error::ErrorReporter, 
    type_info: &mut scope::Module<TypeKind>, 
    stmt: &mut ast::Stmt,
) -> Result<ast::Stmt, error::CompilerError>
{
    match stmt {
        ast::Stmt::Function { name, params, return_type, param_types, body } => {
            type_info.begin_scope();

            for i in 0..params.len() {
                let param      = &params[i];
                let param_type = &param_types[i];
                let kind       = type_from_identifier(param_type, type_info);

                type_info.add_to_current(&param.value, kind);
            }

            for statement in body.iter_mut() {
                let _ = match_statement(reporter, type_info, statement);
            }

            let return_kind = if body.is_empty() {
                TypeKind::Unit
            } else if let Some(ast::Stmt::Expr { expr }) = body.last_mut().map(|s| s.as_mut()) {
                match_expression(reporter, type_info, &mut expr.value)?
                    
            } else {
                TypeKind::Unit
            };

            let specified_return_kind = match return_type {
                Some(return_type) => type_from_identifier(return_type, type_info),
                None              => TypeKind::Unit,
            };

            if return_kind != specified_return_kind {
                let message = format!
                (
                    "Returned value does not match function definition.\nFunction: {} Value type: {:?} Return type: {:?}", 
                    name.value, 
                    return_kind, 
                    return_type,
                );
                reporter.error_at
                (
                    &message, 
                    error::CompilerErrorKind::TypeError, 
                    return_type.as_ref().unwrap_or(name),
                );
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
            let kind = match_expression(reporter, type_info, &mut initializer.value)?;
            initializer.type_kind = kind;
        },

        ast::Stmt::Var { name, initializer, type_name } => {
            let kind = match_expression(reporter, type_info, &mut initializer.value)?;

            if let Some(type_name) = type_name {
                let defined_type = type_from_identifier(type_name, type_info);

                if defined_type != kind {
                    let message = format!("Defined type '{:?}' does not match expression type '{:?}'", defined_type, kind);
                    return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, type_name));
                }
            }

            initializer.type_kind = kind.clone();
            type_info.add_to_current(&name.value, kind);
        }

        ast::Stmt::Const { name, initializer, type_name } => {
            let kind = match_expression(reporter, type_info, &mut initializer.value)?;

            if let Some(type_name) = type_name {
                let defined_type = type_from_identifier(type_name, type_info);

                if defined_type != kind {
                    let message = format!("Defined type '{:?}' does not match expression type '{:?}'", defined_type, kind);
                    reporter.error_at(&message, error::CompilerErrorKind::TypeError, type_name);
                }
            }

            initializer.type_kind = kind.clone();
            type_info.add_to_current(&name.value, kind);
        }

        ast::Stmt::For { token, initializer, condition, advancement, body } => {
            match_statement(reporter, type_info, initializer)?;

            let condition_type = match_expression(reporter, type_info, &mut condition.value)?;
            if condition_type != TypeKind::Bool {
                let message = format!("'for' condition type must be a boolean. Found type: {:?}", condition_type);
                reporter.error_at(&message, error::CompilerErrorKind::TypeError, token);
            }
            condition.type_kind = condition_type;

            let _ = match_statement(reporter, type_info, advancement);

            for stmt in body {
                let _ = match_statement(reporter, type_info, stmt);
            }
        }

        ast::Stmt::While { token, condition, body } => {
            let condition_type = match_expression(reporter, type_info, &mut condition.value)?;
            if condition_type != TypeKind::Bool {
                let message = format!("'while' condition type must be a boolean. Found type: {:?}", condition_type);
                reporter.error_at(&message, error::CompilerErrorKind::TypeError, token);
            }

            condition.type_kind = condition_type;

            for stmt in body {
                let _ = match_statement(reporter, type_info, stmt);
            }
        }

        ast::Stmt::Expr { expr } => expr.type_kind = match_expression(reporter, type_info, &mut expr.value)?,

        _ => ()
    }

    Ok(stmt.to_owned())
}

pub fn match_expression
(
    reporter: &mut error::ErrorReporter, 
    type_info: &mut scope::Module<TypeKind>, 
    expr: &mut ast::Expr,
) -> Result<TypeKind, error::CompilerError>
{
    let kind = match expr {
        ast::Expr::Bad { .. } => TypeKind::Unknown,

        ast::Expr::Block { value, .. } => {
            let kind = match_expression(reporter, type_info, &mut value.value)?;
            value.type_kind = kind.clone();
            kind
        }

        ast::Expr::If { token, condition, then_value, else_value, .. } => {
            let condition_type   = match_expression(reporter, type_info, &mut condition.value)?;
            let then_branch_type = match_expression(reporter, type_info, &mut then_value.value)?;
            let else_branch_type = match_expression(reporter, type_info, &mut else_value.value)?;

            if then_branch_type != else_branch_type {
                reporter.error_at("Incompatible types between branches", error::CompilerErrorKind::TypeError, token);
            }

            if condition_type != TypeKind::Bool {
                let message = format!("'if' condition type must be a boolean. Found type: {:?}", condition_type);
                reporter.error_at(&message, error::CompilerErrorKind::TypeError, token);
            }

            let kind = then_branch_type.clone();

            condition.type_kind  = condition_type;
            then_value.type_kind = then_branch_type;
            else_value.type_kind = else_branch_type;

            kind
        },

        ast::Expr::Binary { left, right, operator } => {
            let left_type  = match_expression(reporter, type_info, &mut left.value)?;
            let right_type = match_expression(reporter, type_info, &mut right.value)?;

            if left_type != right_type {
                let message = format!
                (
                    "Binary operation between incompatible types. Left: {:?} Right: {:?}", 
                    left_type, 
                    right_type,
                );
                reporter.error_at(&message, error::CompilerErrorKind::TypeError, operator);
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

        // TODO: fn token_type can return error. Refactor.
        ast::Expr::Literal { value } => token_type(value),

        ast::Expr::Variable { name } 
            => {
            let Some(kind) = type_info.get(&name.value) else {
                let message = format!("Failed to get type info for {}", &name.value);
                return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, name))
            };

            kind.clone()
        }

        ast::Expr::Assignment { value, .. } => {
            let kind = match_expression(reporter, type_info, &mut value.value)?;
            value.type_kind = kind.clone();

            TypeKind::Unit
        },

        ast::Expr::MemberAssignment { instance_name, member_name, value } => {
            let kind = match_expression(reporter, type_info, &mut value.value)?;
            value.type_kind = kind.clone();

            let Some(instance_type) = type_info.get(&instance_name.value) else {
                let message = format!("Failed to find type name of {}.", &instance_name.value);
                return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, member_name));
            };

            let TypeKind::Struct { name, field_names, field_types, .. } = instance_type else {
                let message = format!("Instance type is not a struct. Found: {:?}", instance_type);
                return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, instance_name));
            };

            let Some(index) = field_names.iter().position(|n| n == &member_name.value) else {
                let message = format!("{} is not a member of {}", &member_name.value, name);
                return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, member_name));
            };

            let member_type = &field_types[index];
            if member_type.as_ref() != &kind {
                let message = format!(
                    "Value of assignment does not match member type. Found {:?}. Expected {:?}",
                    kind,
                    member_type,
                );

                return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, member_name))
            }

            TypeKind::Unit
        }

        ast::Expr::MemberAccess { instance_name, member_name } => {
            let Some(instance_type) = type_info.get(&instance_name.value) else {
                let message = format!("Failed to get type info for {}", &instance_name.value);
                return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, instance_name))
            };

            let TypeKind::Struct { field_names, field_types, .. } = instance_type else {
                let message = format!("Instance type is not a struct. Found: {:?}", instance_type);
                return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, instance_name));
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
                let kind     = match_expression(reporter, type_info, &mut arg.value);
                let Ok(kind) = kind else { continue };

                arg.type_kind = kind; 
            }

            let kind = if name.value == "printf" {
                TypeKind::Unknown
            } else {
                let Some(TypeKind::Function { return_kind, .. }) = type_info.get(&name.value) else {
                    panic!("PANIC PANIC");
                };

                *return_kind.clone()
            };

            kind
        },

        ast::Expr::Function { param_types, body, .. } => {
            // TODO: handle captured variables as well.
            type_info.begin_scope();

            let parameter_kinds: Vec<Box<TypeKind>> = param_types
                .iter()
                .map(|t| type_from_identifier(t, type_info))
                .map(Box::new)
                .collect();

            for statement in body.iter_mut() {
                let _ = match_statement(reporter, type_info, statement);
            }

            let return_kind = if body.is_empty() {
                TypeKind::Unit
            } else if let Some(ast::Stmt::Expr { expr }) = body.last_mut().map(|s| s.as_mut()) {
                match_expression(reporter, type_info, &mut expr.value)?
            } else {
                TypeKind::Unit
            };

            type_info.end_scope();

            TypeKind::Function { parameter_kinds, return_kind: Box::new(return_kind) }
        },

        ast::Expr::Struct { name, values, members } => {
            let Some(instance_type) = type_info.get(&name.value) else {
                let message = format!("Failed to find type {}.", &name.value);
                return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, name));
            };

            let instance_type = instance_type.clone();

            let TypeKind::Struct { field_names, field_types, .. } = instance_type else {
                let message = format!("Instance type is not a struct. Found: {:?}", name);
                return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, name));
            };

            for (i, value) in values.iter_mut().enumerate() {
                let kind     = match_expression(reporter, type_info, &mut value.value);
                let Ok(kind) = kind else { continue };

                let member_name = &members[i];

                let Some(index) = field_names.iter().position(|n| n == &member_name.value) else {
                    let message = format!("{} is not a member of {}", &member_name.value, name);
                    return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, member_name));
                };

                let member_type = &field_types[index];
                if member_type.as_ref() != &kind {
                    let message = format!(
                        "Value of assignment does not match member type. Found {:?}. Expected {:?}",
                        kind,
                        member_type,
                    );

                    reporter.error_at(&message, error::CompilerErrorKind::TypeError, member_name);
                }


                value.type_kind = kind;
            }

            let Some(kind) = type_info.get(&name.value) else {
                let message = format!("Failed to get type for '{}'", &name.value);
                return Err(reporter.error_at(&message, error::CompilerErrorKind::TypeError, name))
            };

            kind.clone()
        }
    };

    Ok(kind)
}

fn type_from_identifier(token: &scan::Token, type_info: &scope::Module<TypeKind>) -> TypeKind
{
    match token.value.as_str() {
        "i32"    => TypeKind::I32,
        "string" => TypeKind::String { len: token.value.len() },
        "bool"   => TypeKind::Bool,
        _        => type_info.get(&token.value).unwrap().clone()
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
