use crate::{scan, types};


#[derive(Debug, Clone)]
pub enum Node
{
    Stmt(Stmt),

    Expr
    {
        value: Expr,
        type_kind: types::TypeKind,
    },
}

#[derive(Debug, Clone)]
pub struct ExprInfo
{
    pub value: Expr,
    pub type_kind: types::TypeKind,
}

impl ExprInfo
{
    pub fn new(value: Expr) -> Self
    {
        Self {
            value,
            type_kind: types::TypeKind::Unknown,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt
{
    Function
    {
        name: scan::Token,
        params: Vec<scan::Token>,
        return_type: scan::Token,
        param_types: Vec<scan::Token>,
        body: Vec<Box<Stmt>>,
    },

    Declaration
    {
        name: scan::Token,
        initializer: Box<ExprInfo>,
    },

    Block
    {
        statements: Vec<Box<Stmt>>,
    },

    Var
    {
        name: scan::Token,
        initializer: Box<ExprInfo>,
    },

    Const
    {
        name: scan::Token,
        initializer: Box<ExprInfo>,
    },

    For {
        initializer: Box<Stmt>,
        condition: Box<ExprInfo>,
        advancement: Box<Stmt>,
        body: Vec<Box<Stmt>>,
    },

    While
    {
        condition: Box<ExprInfo>,
        body: Vec<Box<Stmt>>,
    },

    Unary { },

    Return { },

    Expr
    {
        expr: Box<ExprInfo>,
    }
}

#[derive(Debug, Clone)]
pub enum Expr
{
    Bad
    {
        token: scan::Token,
    },

    Block
    {
        statements: Vec<Box<Stmt>>,
        value: Box<ExprInfo>,
    },

    If
    {
        condition: Box<ExprInfo>,
        then_branch: Vec<Box<Stmt>>,
        then_value: Box<ExprInfo>,
        else_branch: Vec<Box<Stmt>>,
        else_value: Box<ExprInfo>,
    },

    Binary
    {
        left: Box<ExprInfo>,
        right: Box<ExprInfo>,
        operator: scan::Token
    },

    Literal
    {
        value: scan::Token,
    },

    Variable
    {
        name: scan::Token,
    },

    Assignment
    {
        name: scan::Token,
        value: Box<ExprInfo>,
    },

    Logical,

    Call
    {
        name: scan::Token,
        arguments: Vec<Box<ExprInfo>>,
    },

    Function
    {
        params: Vec<scan::Token>,
        return_type: scan::Token,
        param_types: Vec<scan::Token>,
        body: Vec<Box<Stmt>>,
    },
}

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
