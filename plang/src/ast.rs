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
pub enum Stmt
{
    Function
    {
        name: scan::Token,
        params: Vec<scan::Token>,
        param_types: Vec<scan::Token>,
        body: Vec<Box<Stmt>>,
    },

    Declaration
    {
        name: scan::Token,
        initializer: Box<Expr>,
    },

    Block
    {
        statements: Vec<Box<Stmt>>,
    },

    Var
    {
        name: scan::Token,
        initializer: Box<Expr>,
    },

    Const
    {
        name: scan::Token,
        initializer: Box<Expr>,
    },

    For {
        initializer: Box<Stmt>,
        condition: Box<Expr>,
        advancement: Box<Stmt>,
        body: Vec<Box<Stmt>>,
    },

    While
    {
        condition: Box<Expr>,
        body: Vec<Box<Stmt>>,
    },

    Unary { },

    Return { },

    Expr
    {
        expr: Box<Expr>,
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
        value: Box<Expr>,
    },

    If
    {
        condition: Box<Expr>,
        then_branch: Vec<Box<Stmt>>,
        then_value: Box<Expr>,
        else_branch: Vec<Box<Stmt>>,
        else_value: Box<Expr>,
    },

    Binary
    {
        left: Box<Expr>,
        right: Box<Expr>,
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
        value: Box<Expr>,
    },

    Logical,

    Call
    {
        name: scan::Token,
        arguments: Vec<Box<Expr>>,
    },

    Function
    {
        params: Vec<scan::Token>,
        param_types: Vec<scan::Token>,
        body: Vec<Box<Stmt>>,
    },
}

pub fn bad_expr(token: scan::Token) -> Node
{
    Node::Expr {
        value: Expr::Bad { token },
        type_kind: types::TypeKind::Unknown,
    }
}

pub fn block_expr(statements: Vec<Box<Stmt>>, value: Box<Expr>) -> Node
{
    Node::Expr {
        value: Expr::Block { statements, value },
        type_kind: types::TypeKind::Unknown,
    }
}

pub fn if_expr
(
    condition: Box<Expr>,
    then_branch: Vec<Box<Stmt>>,
    then_value: Box<Expr>,
    else_branch: Vec<Box<Stmt>>,
    else_value: Box<Expr>,
) -> Node
{
     Node::Expr {
        value: Expr::If { condition, then_branch, then_value, else_branch, else_value },
        type_kind: types::TypeKind::Unknown,
    }
}

pub fn binary_expr
(
    left: Box<Expr>,
    right: Box<Expr>,
    operator: scan::Token
) -> Node
{
     Node::Expr {
        value: Expr::Binary { left, right, operator },
        type_kind: types::TypeKind::Unknown,
    }
}

pub fn literal_expr(value: scan::Token) -> Node
{
    Node::Expr {
        value: Expr::Literal { value },
        type_kind: types::TypeKind::Unknown,
    }
}

pub fn variable_expr(token: scan::Token) -> Node
{
    Node::Expr {
        value: Expr::Variable { name: token },
        type_kind: types::TypeKind::Unknown,
    }
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
