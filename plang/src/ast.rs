use crate::scan;


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
        statements: Vec<Box<Stmt>>
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
