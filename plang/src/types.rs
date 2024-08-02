use std::collections::BTreeSet;

use crate::{ast, error, scan, scope, source};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind
{
    Unknown,

    Unit,

    Bool,

    I32,

    String 
    { 
        len: usize 
    },

    Function 
    {
        value: Function,
    },

    Closure 
    {
        captured_kinds: Vec<Box<TypeKind>>,
        parameter_kinds: Vec<Box<TypeKind>>,
        return_kind: Box<TypeKind>,
    },

    Struct 
    {
        value: Struct,
    },

    Reference 
    {
        underlying: Box<TypeKind>,
    },
}

#[derive(Debug)]
pub struct InvalidKindError(());

impl TypeKind 
{
    pub fn as_struct(&self) -> Result<&Struct, InvalidKindError>
    {
        match self {
            TypeKind::Struct { value } => Ok(value),

            TypeKind::Reference { underlying } => {
                let TypeKind::Struct { value } = underlying.as_ref() else {
                    return Err(InvalidKindError(()))
                };

                Ok(value)
            }

            _ => Err(InvalidKindError(()))
        }
    }

    pub fn as_primitive(self: &TypeKind) -> &TypeKind
    {
        if let TypeKind::Reference { underlying } = self {
            return underlying.as_primitive()
        }

        self
    }

    pub fn as_function(&self) -> Result<&Function, InvalidKindError>
    {
        match self {
            TypeKind::Function { value } => Ok(value),

            TypeKind::Reference { underlying } => {
                let TypeKind::Function { value } = underlying.as_ref() else {
                    return Err(InvalidKindError(()))
                };

                Ok(value)
            }

            _ => Err(InvalidKindError(()))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function
{
    pub parameter_kinds: Vec<Box<TypeKind>>,
    pub return_kind: Box<TypeKind>,
    pub variadic: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct
{
    pub name: String,
    pub member_names: Vec<String>,
    pub member_types: Vec<Box<TypeKind>>,
}

pub struct Typer<'a>
{
    reporter: &'a mut error::Reporter,
    type_info: scope::Module<TypeKind>,
    source: &'a source::Source,
}

impl <'a> Typer<'a>
{
    pub fn new(source: &'a source::Source, reporter: &'a mut error::Reporter) -> Self
    {
        Self {
            reporter,
            type_info: scope::Module::new(),
            source,
        }
    }

    pub fn infer_types(mut self, mut program: Vec<ast::Node>) -> (Vec<ast::Node>, scope::Module<TypeKind>)
    {
        self.type_info.begin_scope();

        self.handle_native_functions();

        for stmt in &mut program {
            let ast::Node::Stmt(stmt) = stmt else {
                continue
            };

            let _ = self.match_statement(stmt);
        }

        self.type_info.end_scope();

        (program, self.type_info)
    }

    pub fn handle_native_functions(&mut self)
    {
        let function = Function {
            parameter_kinds: vec![
                Box::new
                (
                    TypeKind::Reference {
                        underlying: Box::new(TypeKind::String{ len: 0 }) 
                    }
                )
            ],
            return_kind: Box::new(TypeKind::I32),
            variadic: true,
        };

        let printf = TypeKind::Function {
            value: function,
        };
        self.type_info.add_to_current("printf", printf);
    }

    pub fn match_statement(&mut self, stmt: &mut ast::Stmt) -> Result<ast::Stmt, error::Error>
    {
        match stmt {
            ast::Stmt::Function { name, params, return_type, param_types, body } => {
                let function_name = self.source.token_value(name);

                let parameter_kinds: Vec<Box<TypeKind>> = param_types
                    .iter()
                    .map(|t| self.type_from_identifier(t))
                    .map_while(std::result::Result::ok)
                    .map(Box::new)
                    .collect();

                let defined_kind = match return_type {
                    Some(return_type) => self.type_from_identifier(return_type),
                    None              => Ok(TypeKind::Unit)
                }?;

                let function = Function {
                    parameter_kinds,
                    return_kind: Box::new(defined_kind.clone()),
                    variadic: false,
                };
                let kind = TypeKind::Function {
                    value: function,
                };

                self.type_info.add_to_current(function_name, kind);

                self.type_info.begin_scope();

                for i in 0..params.len() {
                    let param      = &params[i];
                    let param_type = &param_types[i];
                    let kind       = self.type_from_identifier(param_type);
                    let param_name = self.source.token_value(param);

                    if let Ok(kind) = kind {
                        self.type_info.add_to_current(param_name, kind);
                        continue
                    };

                    let message = format!("Failed to get type for {param_name}");
                    let _ = self.reporter.error_at(&message, error::Kind::TypeError, param);
                }

                for statement in body.iter_mut() {
                    let _ = self.match_statement(statement);
                }

                let return_kinds         = return_type_analysis(body)?;
                let returns_defined_type = return_kinds.iter().all(|k| k == &defined_kind);

                if !returns_defined_type {
                    let message = format!("Expected return value matching defined type: {defined_kind:?}. Found no value.");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name))
                }

                self.type_info.end_scope();
            },

            ast::Stmt::Struct { name, members, member_types } => {
                let member_types: Vec<Box<TypeKind>> = member_types
                    .iter()
                    .map(|t| self.type_from_identifier(t))
                    .map_while(std::result::Result::ok)
                    .map(Box::new)
                    .collect();

                let member_names = members
                    .iter()
                    .map(|m| self.source.token_value(m).to_owned())
                    .collect();

                let struct_type_name = self.source.token_value(name);

                let kind = TypeKind::Struct { 
                    value: Struct {
                        name: struct_type_name.to_owned(),
                        member_names,
                        member_types,
                    } 
                };

                self.type_info.add_to_current(struct_type_name, kind);
            }

            ast::Stmt::Var { name, initializer, type_name } => {
                let index = self.type_info.add_to_current(self.source.token_value(name), TypeKind::Unknown);
                let kind  = self.match_expression(&mut initializer.value)?;

                if let Some(type_name) = type_name {
                    let defined_type = self.type_from_identifier(type_name)?;

                    if defined_type != kind {
                        let message = format!("Defined type '{defined_type:?}' does not match expression type '{kind:?}'");
                        return Err(self.reporter.error_at(&message, error::Kind::TypeError, type_name));
                    }
                }

                initializer.type_kind = kind.clone();
                self.type_info.update_in_current(index, kind);
            }

            ast::Stmt::Const { name, initializer, type_name } => {
                let index = self.type_info.add_to_current(self.source.token_value(name), TypeKind::Unknown);
                let kind  = self.match_expression(&mut initializer.value)?;

                if let Some(type_name) = type_name {
                    let defined_type = self.type_from_identifier(type_name)?;

                    if defined_type != kind {
                        let message = format!("Defined type '{defined_type:?}' does not match expression type '{kind:?}'");
                        self.reporter.error_at(&message, error::Kind::TypeError, type_name);
                    }
                }

                initializer.type_kind = kind.clone();
                self.type_info.update_in_current(index, kind);
            }

            ast::Stmt::For { token, initializer, condition, advancement, body } => {
                let _ = self.match_statement(initializer);

                let condition_type = self.match_expression(&mut condition.value)?;
                if condition_type != TypeKind::Bool {
                    let message = format!("'for' condition type must be a boolean. Found type: {condition_type:?}");
                    self.reporter.error_at(&message, error::Kind::TypeError, token);
                }

                condition.type_kind = condition_type;

                let _ = self.match_statement(advancement);

                for stmt in body {
                    let _ = self.match_statement(stmt);
                }
            }

            ast::Stmt::While { token, condition, body } => {
                let condition_type = self.match_expression(&mut condition.value)?;
                if condition_type != TypeKind::Bool {
                    let message = format!("'while' condition type must be a boolean. Found type: {condition_type:?}");
                    self.reporter.error_at(&message, error::Kind::TypeError, token);
                }

                condition.type_kind = condition_type;

                for stmt in body {
                    let _ = self.match_statement(stmt);
                }
            }

            ast::Stmt::Expr { expr } => expr.type_kind = self.match_expression(&mut expr.value)?,
        }

        Ok(stmt.to_owned())
    }

    pub fn match_expression(&mut self, expr: &mut ast::Expr) -> Result<TypeKind, error::Error>
    {
        let kind = match expr {
            ast::Expr::Bad { .. } => TypeKind::Unknown,

            ast::Expr::Block { value, statements, .. } => {
                self.type_info.begin_scope();

                for statement in statements {
                    let _ = self.match_statement(statement);
                }

                let kind = if let Some(value) = value {
                    let kind        = self.match_expression(&mut value.value)?;
                    value.type_kind = kind.clone();
                    kind
                } else {
                    TypeKind::Unit
                };

                self.type_info.end_scope();

                kind
            }

            ast::Expr::If { token, conditions, branches } => {
                for condition in conditions.iter_mut() {
                    let condition_type     = self.match_expression(&mut condition.value);
                    let Ok(condition_type) = condition_type else {
                        let _ = self.reporter.error_at("Failed to type condition.", error::Kind::TypeError, token);
                        continue
                    };

                    if condition_type != TypeKind::Bool {
                        let message = format!("'if' condition type must be a boolean. Found type: {condition_type:?}");
                        let _ = self.reporter.error_at(&message, error::Kind::TypeError, token);
                        continue
                    }

                    condition.type_kind = condition_type;
                }

                for branch in branches.iter_mut() {
                    let branch_type     = self.match_expression(&mut branch.value);
                    let Ok(branch_type) = branch_type else {
                        let _ = self.reporter.error_at("Failed to type if branch.", error::Kind::TypeError, token);
                        continue
                    };

                   branch.type_kind = branch_type;
                }

                let first = branches[0].type_kind.clone();
                let same  = branches.iter().map(|b| &b.type_kind).all(|t| t == &first);
                if !same {
                    return Err(self.reporter.error_at("If block types diverge.", error::Kind::TypeError, token))
                }

                first
            },

            ast::Expr::Binary { left, right, operator } => {
                let left_type  = self.match_expression(&mut left.value)?;
                let right_type = self.match_expression(&mut right.value)?;

                if left_type.as_primitive() != right_type.as_primitive() {
                    let message = format!
                    (
                        "Binary operation between incompatible types. Left: {left_type:?} Right: {right_type:?}"
                    );
                    self.reporter.error_at(&message, error::Kind::TypeError, operator);
                }

                let kind = match operator.kind {
                    scan::TokenKind::Plus
                    | scan::TokenKind::Minus
                    | scan::TokenKind::Star
                    | scan::TokenKind::Slash
                    | scan::TokenKind::Ampersand
                    | scan::TokenKind::Pipe
                    | scan::TokenKind::RightAngleRightAngle
                    | scan::TokenKind::LeftAngleLeftAngle
                    | scan::TokenKind::Caret
                        => left_type.clone(),

                    scan::TokenKind::EqualEqual
                    | scan::TokenKind::BangEqual
                    | scan::TokenKind::GreaterEqual
                    | scan::TokenKind::LessEqual
                    | scan::TokenKind::LeftAngle
                    | scan::TokenKind::RightAngle
                    | scan::TokenKind::AmpersandAmpersand
                    | scan::TokenKind::PipePipe
                        => TypeKind::Bool,

                    scan::TokenKind::PlusEqual 
                    | scan::TokenKind::MinusEqual => TypeKind::Unit,

                    _ => {
                        let message = format!("Unrecognized binary expression token. {:?}", operator.kind);
                        return Err(self.reporter.error_at(&message, error::Kind::TypeError, operator))
                    }
                };

                left.type_kind  = left_type;
                right.type_kind = right_type;

                kind
            },

            ast::Expr::Unary { operator, expr } => {
                let kind = self.match_expression(&mut expr.value)?;
                expr.type_kind = kind;

                match operator.kind {
                    scan::TokenKind::Minus => {
                        if expr.type_kind != TypeKind::I32 {
                            let message = format!("'-' operand type must be a number. Found type: {:?}", expr.type_kind);
                            self.reporter.error_at(&message, error::Kind::TypeError, operator);
                        }

                        expr.type_kind.clone()
                    }
                    scan::TokenKind::Bang => {
                        if expr.type_kind != TypeKind::Bool {
                            let message = format!("Negation operand type must be a boolean. Found type: {:?}", expr.type_kind);
                            self.reporter.error_at(&message, error::Kind::TypeError, operator);
                        }

                        TypeKind::Bool
                    }
                    _ => TypeKind::Unit
                }
            }

            // TODO: fn token_type can return error. Refactor.
            ast::Expr::Literal { value } => self.token_type(value),

            ast::Expr::Variable { name } => {
                let variable_name = self.source.token_value(name);

                let Some(kind) = self.type_info.get(variable_name) else {
                    let message = format!("Failed to get type info for {variable_name}");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name))
                };

                kind.clone()
            }

            ast::Expr::Assignment { left, right } => {
                let right_kind  = self.match_expression(&mut right.value)?;
                right.type_kind = right_kind.clone();

                let left_kind = self.match_expression(&mut left.value)?;
                left.type_kind = left_kind.clone();

                TypeKind::Unit
            },

            ast::Expr::MemberAccess { instance_name, member_name } => {
                let instance = self.source.token_value(instance_name);
                let Some(instance_type) = self.type_info.get(instance) else {
                    let message = format!("Failed to get type info for {instance}");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, instance_name))
                };

                let Ok(struct_value) = instance_type.as_struct() else {
                    let message = format!("Instance type is not a struct. Found: {instance_type:?}");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, instance_name));
                };

                let member      = self.source.token_value(member_name);
                let index       = struct_value.member_names.iter().position(|n| n == member);
                let Some(index) = index else {
                    let message = format!("{member} is not a member of {instance}");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, member_name));
                };

                *struct_value.member_types[index].clone()
            }

            ast::Expr::Return { value, .. } => {
                let value_kind = self.match_expression(&mut value.value)?;
                value.type_kind = value_kind;

                TypeKind::Unit
            }

            ast::Expr::Call { name, arguments } => {
                for arg in arguments.iter_mut() {
                    let kind     = self.match_expression(&mut arg.value);
                    let Ok(kind) = kind else { continue };

                    arg.type_kind = kind; 
                }

                let function_name = self.source.token_value(name);

                let Some(TypeKind::Function { value: Function { return_kind, ..  }}) = self.type_info.get(function_name) else {
                    let message = format!("'{function_name}' is not a function.");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name));
                };

                *return_kind.clone()
            },

            ast::Expr::Function { right_paren, params, param_types, body, return_type, .. } => {
                self.type_info.begin_scope();

                let closed_variables     = self.captured_variables();
                // Remove self (which is the last captured variable).
                let closed_variables     = closed_variables[..closed_variables.len()-1].to_vec();
                let mut closed_variables = closed_variables
                    .into_iter()
                    .map(|(n, t)| {
                        let type_kind = TypeKind::Reference { underlying: Box::new(t.clone()) };
                        (n, type_kind)
                    })
                    .collect::<Vec<(String, TypeKind)>>();

                let mut parameter_kinds: Vec<(String, TypeKind)> = param_types
                    .iter()
                    .enumerate()
                    .map(|(i, t)|  {
                        let name      = self.source.token_value(&params[i]).to_owned();
                        let type_kind = self.type_from_identifier(t)?;

                        let result = (name, type_kind);

                        Ok::<(String, TypeKind), error::Error>(result)
                    })
                    .map_while(Result::ok)
                    .collect();

                let total_values_count = params.len() + closed_variables.len();
                let mut closed_params  = Vec::with_capacity(total_values_count);

                closed_params.append(&mut parameter_kinds);
                closed_params.append(&mut closed_variables);

                for (param, param_type) in &closed_params {
                    self.type_info.add_to_current(param, param_type.clone());
                }

                for statement in body.iter_mut() {
                    let _ = self.match_statement(statement);
                }

                let defined_kind = match return_type {
                    Some(return_type) => self.type_from_identifier(return_type),
                    None              => Ok(TypeKind::Unit)
                }?;

                let return_kinds         = return_type_analysis(body)?;
                let returns_defined_type = return_kinds.iter().all(|k| k == &defined_kind);

                if !returns_defined_type {
                    let message = format!("Expected return value matching defined type: {defined_kind:?}. Found no value.");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, right_paren))
                }
                
                let return_kind = Box::new(defined_kind);

                self.type_info.end_scope();

                let parameter_kinds = closed_params
                    .iter()
                    .map(|(_, t)| t)
                    .cloned()
                    .map(Box::new)
                    .collect();

                TypeKind::Function { 
                    value: Function { parameter_kinds, return_kind, variadic: false }
                }
            },

            ast::Expr::Struct { name, values, members } => {
                let instance = self.source.token_value(name);
                let Some(instance_type) = self.type_info.get(instance) else {
                    let message = format!("Failed to find type {instance}.");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name));
                };

                let instance_type = instance_type.clone();

                let Ok(struct_value) = instance_type.as_struct() else {
                    let message = format!("Instance type is not a struct. Found: {name:?}");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name));
                };
                
                for (i, value) in values.iter_mut().enumerate() {
                    let kind     = self.match_expression(&mut value.value);
                    let Ok(kind) = kind else { continue };

                    let member_name = &members[i];

                    let member = self.source.token_value(member_name);
                    let Some(index) = struct_value.member_names.iter().position(|n| n == member) else {
                        let message = format!("{member} is not a member of {name:?}");
                        self.reporter.error_at(&message, error::Kind::TypeError, member_name);

                        continue
                    };

                    let member_type = &struct_value.member_types[index];
                    if member_type.as_ref() != &kind {
                        let message = format!
                        (
                            "Value of assignment does not match member type. Found {kind:?}. Expected {member_type:?}",
                        );
                        self.reporter.error_at(&message, error::Kind::TypeError, member_name);

                        continue
                    }

                    value.type_kind = kind;
                }

                let struct_name = self.source.token_value(name);
                let Some(kind) = self.type_info.get(struct_name) else {
                    let message = format!("Failed to get type for '{struct_name}'");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name))
                };

                kind.clone()
            }
        };

        Ok(kind)
    }

    fn type_from_identifier(&mut self, token: &scan::Token) -> Result<TypeKind, error::Error>
    {
        let token_value = self.source.token_value(token);

        let kind = match token_value {
            "i32"    => TypeKind::I32,
            "string" => TypeKind::String { len: token_value.len() },
            "bool"   => TypeKind::Bool,
            _        => {
                let kind = self.type_info.get(token_value);
                let Some(kind) = kind else {
                    let message = format!("Failed to find type of {token_value}.");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, token))
               };

                kind.clone()
            }
        };

        Ok(kind)
    }

    fn token_type(&self, token: &scan::Token) -> TypeKind
    {
        match token.kind {
            scan::TokenKind::Semicolon => TypeKind::Unit,

            scan::TokenKind::True | scan::TokenKind::False  => TypeKind::Bool,

            _ => {
                let token = self.source.token_value(token);

                if token.starts_with('\"') {
                    TypeKind::String { len: token.len() }
                } else {
                    let first = token.as_bytes()[0] as char;
                    if !char::is_numeric(first) {
                        return TypeKind::Unknown
                    }

                    let Ok(_) = token.parse::<i32>() else {
                        return TypeKind::Unknown
                    };

                    TypeKind::I32
                }
            }
        }
    }

    pub fn captured_variables(&self) -> Vec<(String, TypeKind)>
    {
        let scope = self.type_info.current_scope();

        let global_scope = &self.type_info.scopes[0];
        let globals      = global_scope.names.iter().map(std::string::String::as_str).collect::<Vec<&str>>();
        let to_remove    = BTreeSet::<&str>::from_iter(globals);

        let capacity = scope.names.len() - global_scope.names.len();

        let mut vars: Vec<(String, TypeKind)> = Vec::with_capacity(capacity);

        for i in 0..scope.names.len() {
            let name = &scope.names[i];
            if to_remove.contains(name.as_str()) { continue }

            let value = &scope.values[i];

            vars.push((name.clone(), value.clone()));
        }

        vars
    }
}

pub fn return_type_analysis
(
    function_body: &[Box<ast::Stmt>],
) -> Result<Vec<TypeKind>, error::Error>
{
    let mut return_kinds = Vec::with_capacity(128);

    for statement in function_body {
        let kind = return_type_kind(statement);
        return_kinds.push(kind);
    }

    let last = function_body.last().map(|s| s.as_ref().clone());
    let last = match last {
        Some(ast::Stmt::Expr { expr }) => {
            let value = if let ast::Expr::Return { value, .. } = &expr.value {
                value.type_kind.clone()
            } else {
                expr.type_kind.clone()
            };

            Some(value)
        }

        _ => None
    };

    return_kinds.push(last);

    Ok
    (
        return_kinds.into_iter().flatten().collect()
    )
}

fn return_type_kind(statement: &ast::Stmt) -> Option<TypeKind>
{
    let ast::Stmt::Expr { expr } = statement else {
        return None
    };

    let ast::Expr::Return { value, .. } = &expr.value else {
        return None
    };

    Some(value.type_kind.clone())
}

