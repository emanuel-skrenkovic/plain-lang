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
        parameter_kinds: Vec<Box<TypeKind>>,
        return_kind: Box<TypeKind>,
        variadic: bool,
    },

    Closure 
    {
        captured_kinds: Vec<Box<TypeKind>>,
        parameter_kinds: Vec<Box<TypeKind>>,
        return_kind: Box<TypeKind>,
    },

    Struct 
    {
        name: String,
        member_names: Vec<String>,
        member_types: Vec<Box<TypeKind>>,
    },

    Reference 
    {
        underlying: Box<TypeKind>,
    },
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
        let _ = self.infer_global_types(&mut program);

        for stmt in &mut program {
            let ast::Node::Stmt(stmt) = stmt else {
                continue
            };

            let _ = self.match_statement(stmt);
        }

        self.type_info.end_scope();

        (program, self.type_info)
    }

    pub fn infer_global_types(&mut self, program: &mut [ast::Node]) -> Result<(), error::Error>
    {
        for node in program.iter_mut() {
            let ast::Node::Stmt(stmt) = node else {
                continue
            };

            match stmt {
                ast::Stmt::Function { name, return_type, param_types, .. } => {
                    let parameter_kinds: Vec<Box<TypeKind>> = param_types
                        .iter()
                        .map(|t| self.type_from_identifier(t))
                        .map_while(std::result::Result::ok)
                        .map(Box::new)
                        .collect();

                    let return_kind = match return_type {
                        Some(return_type) => self.type_from_identifier(return_type)?,
                        None              => TypeKind::Unit,
                    };
                    let kind = TypeKind::Function {
                        parameter_kinds,
                        return_kind: Box::new(return_kind),
                        variadic: false,
                    };

                    self.type_info.add_to_current(self.source.token_value(name), kind);
                }

                ast::Stmt::Const { name, initializer, type_name } => {
                    let kind = self.match_expression(&mut initializer.value)?;

                    if let Some(type_name) = type_name {
                        let defined_type = self.type_from_identifier(type_name)?;

                        if defined_type != kind {
                            let message = format!("Defined type '{defined_type:?}' does not match expression type '{kind:?}'");
                            self.reporter.error_at(&message, error::Kind::TypeError, name);
                        }
                    }

                    self.type_info.add_to_current(self.source.token_value(name), kind);
                }

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

                    let kind = TypeKind::Struct {
                        name: self.source.token_value(name).to_owned(),
                        member_names,
                        member_types,
                    };

                    self.type_info.add_to_current(self.source.token_value(name), kind);
                }

                _ => ()
            }
        }

        Ok(())
    }

    pub fn handle_native_functions(&mut self)
    {
        let printf = TypeKind::Function {
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
        self.type_info.add_to_current("printf", printf);
    }

    pub fn match_statement(&mut self, stmt: &mut ast::Stmt) -> Result<ast::Stmt, error::Error>
    {
        match stmt {
            ast::Stmt::Function { name, params, return_type, param_types, body } => {
                self.type_info.begin_scope();

                for i in 0..params.len() {
                    let param      = &params[i];
                    let param_type = &param_types[i];
                    let kind       = self.type_from_identifier(param_type);

                    if let Ok(kind) = kind {
                        self.type_info.add_to_current(self.source.token_value(param), kind);
                        continue
                    };

                    let message = format!("Failed to get type for {}", self.source.token_value(param));
                    let _ = self.reporter.error_at(&message, error::Kind::TypeError, param);
                }

                for statement in body.iter_mut() {
                    let _ = self.match_statement(statement);
                }

                let defined_kind = match return_type {
                    Some(return_type) => self.type_from_identifier(return_type),
                    None              => Ok(TypeKind::Unit)
                }?;

                let last = body.last_mut().map(std::convert::AsMut::as_mut);

                let return_kind = match (&defined_kind, last) {
                    // If we find a return value, match its type against the defined type.
                    (_, Some(ast::Stmt::Expr { expr })) => 'expression: {
                        let returned_kind = self.match_expression(&mut expr.value)?;    
                        
                        if returned_kind != defined_kind {
                            let message = format!("Expected return value of {defined_kind:?} found {returned_kind:?}");
                            let error   = self.reporter.error_at(&message, error::Kind::TypeError, name);
                            break 'expression Err(error)
                        }

                        Ok(defined_kind)    
                    }

                    // If we find no value, and the return type is Unit, then it is correct.
                    (TypeKind::Unit, _) => Ok(TypeKind::Unit),

                    // If we find expect a return value other than Unit, and we find no value, then
                    // it is an error.
                    (_, _) => {
                        let message = format!("Expected return value matching defined type: {defined_kind:?}. Found no value.");
                        Err(self.reporter.error_at(&message, error::Kind::TypeError, name))
                    }
                }?;

                let specified_return_kind = match return_type {
                    Some(return_type) => self.type_from_identifier(return_type)?,
                    None              => TypeKind::Unit,
                };

                if return_kind != specified_return_kind {
                    let message = format!
                    (
                        "Returned value does not match function definition.Function: {} Value type: {:?} Return type: {:?}", 
                        self.source.token_value(name),
                        return_kind, 
                        return_type,
                    );
                    self.reporter.error_at
                    (
                        &message, 
                        error::Kind::TypeError, 
                        return_type.as_ref().unwrap_or(name),
                    );
                }

                self.type_info.end_scope();

                let parameter_kinds: Vec<Box<TypeKind>> = param_types
                    .iter()
                    .map(|t| self.type_from_identifier(t))
                    .map_while(std::result::Result::ok)
                    .map(Box::new)
                    .collect();

                let kind = TypeKind::Function {
                    parameter_kinds,
                    return_kind: Box::new(return_kind),
                    variadic: false,
                };

                self.type_info.add_to_current(self.source.token_value(name), kind);
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

                let kind = TypeKind::Struct {
                    name: self.source.token_value(name).to_owned(),
                    member_names,
                    member_types,
                };

                self.type_info.add_to_current(self.source.token_value(name), kind);
            }

            ast::Stmt::Declaration { initializer, .. } => {
                let kind = self.match_expression(&mut initializer.value)?;
                initializer.type_kind = kind;
            },

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

            _ => ()
        }

        Ok(stmt.to_owned())
    }

    pub fn match_expression
    (
        &mut self,
        expr: &mut ast::Expr,
    ) -> Result<TypeKind, error::Error>
    {
        let kind = match expr {
            ast::Expr::Bad { .. } => TypeKind::Unknown,

            ast::Expr::Block { value, .. } => {
                let kind        = self.match_expression(&mut value.value)?;
                value.type_kind = kind.clone();
                kind
            }

            ast::Expr::If { token, condition, then_value, else_value, .. } => {
                let condition_type   = self.match_expression(&mut condition.value)?;
                let then_branch_type = self.match_expression(&mut then_value.value)?;
                let else_branch_type = self.match_expression(&mut else_value.value)?;

                if then_branch_type != else_branch_type {
                    self.reporter.error_at("Incompatible types between branches", error::Kind::TypeError, token);
                }

                if condition_type != TypeKind::Bool {
                    let message = format!("'if' condition type must be a boolean. Found type: {condition_type:?}");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, token));
                }

                let kind = then_branch_type.clone();

                condition.type_kind  = condition_type;
                then_value.type_kind = then_branch_type;
                else_value.type_kind = else_branch_type;

                kind
            },

            ast::Expr::Binary { left, right, operator } => {
                let left_type  = self.match_expression(&mut left.value)?;
                let right_type = self.match_expression(&mut right.value)?;

                if left_type != right_type {
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
                        => left_type.clone(),

                    scan::TokenKind::EqualEqual
                    | scan::TokenKind::BangEqual
                    | scan::TokenKind::GreaterEqual
                    | scan::TokenKind::LessEqual
                    | scan::TokenKind::LeftAngle
                    | scan::TokenKind::RightAngle
                        => TypeKind::Bool,

                    _ => {
                        let message = format!("Unrecognized binary expression token. {:?}", operator.kind);
                        return Err(self.reporter.error_at(&message, error::Kind::TypeError, operator))
                    }
                };

                left.type_kind  = left_type;
                right.type_kind = right_type;

                kind
            },

            // TODO: fn token_type can return error. Refactor.
            ast::Expr::Literal { value } => self.token_type(value),

            ast::Expr::Variable { name } 
                => {
                let Some(kind) = self.type_info.get(self.source.token_value(name)) else {
                    let message = format!("Failed to get type info for {}", self.source.token_value(name));
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name))
                };

                kind.clone()
            }

            ast::Expr::Assignment { value, .. } => {
                let kind        = self.match_expression(&mut value.value)?;
                value.type_kind = kind.clone();

                TypeKind::Unit
            },

            ast::Expr::MemberAssignment { instance_name, member_name, value } => {
                let kind = self.match_expression(&mut value.value)?;
                value.type_kind = kind.clone();

                let Some(instance_type) = self.type_info.get(self.source.token_value(instance_name)) else {
                    let message = format!("Failed to find type name of {}.", self.source.token_value(instance_name));
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, member_name));
                };

                let TypeKind::Struct { name, member_names, member_types, .. } = instance_type else {
                    let message = format!("Instance type is not a struct. Found: {instance_type:?}");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, instance_name));
                };

                let Some(index) = member_names.iter().position(|n| n == self.source.token_value(member_name)) else {
                    let message = format!("{} is not a member of {}", self.source.token_value(member_name), name);
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, member_name));
                };

                let member_type = &member_types[index];
                if member_type.as_ref() != &kind {
                    let message = format!
                    (
                        "Value of assignment does not match member type. Found {kind:?}. Expected {member_type:?}"
                    );
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, member_name))
                }

                TypeKind::Unit
            }

            ast::Expr::MemberAccess { instance_name, member_name } => {
                let Some(instance_type) = self.type_info.get(self.source.token_value(instance_name)) else {
                    let message = format!("Failed to get type info for {}", self.source.token_value(instance_name));
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, instance_name))
                };

                let TypeKind::Struct { name, member_names, member_types, .. } = instance_type else {
                    let message = format!("Instance type is not a struct. Found: {instance_type:?}");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, instance_name));
                };

                let index = member_names.iter().position(|n| n == self.source.token_value(member_name));
                let Some(index) = index else {
                    let message = format!("{} is not a member of {}", self.source.token_value(member_name), name);
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, member_name));
                };

                *member_types[index].clone()
            }
            ast::Expr::Logical => todo!(),

            ast::Expr::Call { name, arguments } => {
                for arg in arguments.iter_mut() {
                    let kind     = self.match_expression(&mut arg.value);
                    let Ok(kind) = kind else { continue };

                    arg.type_kind = kind; 
                }

                let Some(TypeKind::Function { return_kind, .. }) = self.type_info.get(self.source.token_value(name)) else {
                    let message = format!("'{}' is not a function.", self.source.token_value(name));
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name));
                };

                *return_kind.clone()
            },

            ast::Expr::Function { right_paren, param_types, body, return_type, .. } => {
                // TODO: handle captured variables as well.
                self.type_info.begin_scope();

                let parameter_kinds: Vec<Box<TypeKind>> = param_types
                    .iter()
                    .map(|t| self.type_from_identifier(t))
                    .map_while(std::result::Result::ok)
                    .map(Box::new)
                    .collect();

                for statement in body.iter_mut() {
                    let _ = self.match_statement(statement);
                }

                let defined_kind = match return_type {
                    Some(return_type) => self.type_from_identifier(return_type),
                    None              => Ok(TypeKind::Unit)
                }?;

                let last = body.last_mut().map(std::convert::AsMut::as_mut);

                let return_kind = match (&defined_kind, last) {
                    // If we find a return value, match its type against the defined type.
                    (_, Some(ast::Stmt::Expr { expr })) => 'expression: {
                        let returned_kind = self.match_expression(&mut expr.value)?;    
                        
                        if returned_kind != defined_kind {
                            let message = format!("Expected return value of {defined_kind:?} found {returned_kind:?}");
                            let error   = self.reporter.error_at(&message, error::Kind::TypeError, right_paren);
                            break 'expression Err(error)
                        }

                        Ok(defined_kind)    
                    }

                    // If we find no value, and the return type is Unit, then it is correct.
                    (TypeKind::Unit, _) => Ok(TypeKind::Unit),

                    // If we find expect a return value other than Unit, and we find no value, then
                    // it is an error.
                    (_, _) => {
                        let message = format!("Expected return value matching defined type: {defined_kind:?}. Found no value.");
                        Err(self.reporter.error_at(&message, error::Kind::TypeError, right_paren))
                    }
                }?;

                let return_kind = Box::new(return_kind);

                self.type_info.end_scope();

                TypeKind::Function { parameter_kinds, return_kind, variadic: false }
            },

            ast::Expr::Struct { name, values, members } => {
                let Some(instance_type) = self.type_info.get(self.source.token_value(name)) else {
                    let message = format!("Failed to find type {}.", self.source.token_value(name));
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name));
                };

                let instance_type = instance_type.clone();

                let TypeKind::Struct { member_names, member_types, .. } = instance_type else {
                    let message = format!("Instance type is not a struct. Found: {name:?}");
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name));
                };

                for (i, value) in values.iter_mut().enumerate() {
                    let kind     = self.match_expression(&mut value.value);
                    let Ok(kind) = kind else { continue };

                    let member_name = &members[i];

                    let Some(index) = member_names.iter().position(|n| n == self.source.token_value(member_name)) else {
                        let message = format!("{} is not a member of {:?}", self.source.token_value(member_name), name);
                        self.reporter.error_at(&message, error::Kind::TypeError, member_name);

                        continue
                    };

                    let member_type = &member_types[index];
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

                let Some(kind) = self.type_info.get(self.source.token_value(name)) else {
                    let message = format!("Failed to get type for '{}'", self.source.token_value(name));
                    return Err(self.reporter.error_at(&message, error::Kind::TypeError, name))
                };

                kind.clone()
            }
        };

        Ok(kind)
    }

    fn type_from_identifier(&mut self, token: &scan::Token) -> Result<TypeKind, error::Error>
    {
        let kind = match self.source.token_value(token) {
            "i32"    => TypeKind::I32,
            "string" => TypeKind::String { len: self.source.token_value(token).len() },
            "bool"   => TypeKind::Bool,
            _        => {
                let kind = self.type_info.get(self.source.token_value(token));
                let Some(kind) = kind else {
                    let message = format!("Failed to find type of {}.", self.source.token_value(token));
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
}


