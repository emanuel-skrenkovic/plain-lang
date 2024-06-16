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

// TODO: AST transformations.
pub trait Transformer
{
    fn transform(nodes: Vec<Node>) -> Vec<Node>;
}

pub struct MainTransformer { }

impl Transformer for MainTransformer
{
    fn transform(nodes: Vec<Node>) -> Vec<Node>
    {
        use std::collections::VecDeque;
        let mut q: VecDeque<Node> = VecDeque::new();

        let main_name = "main";

        for node in nodes.into_iter() {
            match &node {
                Node::Stmt(Stmt::Function { name, .. }) => {
                    if name.value == main_name {
                        q.push_front(node);
                    } else {
                        q.push_back(node);
                    }
                }
                _ => q.push_back(node)
            }
        }

        q.into()
    }
}

pub struct GlobalsHoistingTransformer { }

impl GlobalsHoistingTransformer
{
    fn match_statements(statements: &[Box<Stmt>], deps: &mut Vec<String>)
    {
        for stmt in statements.iter().map(|s| s.as_ref()) {
            match stmt {
                Stmt::Function { name: _, body, .. } => {
                    let mut nested_deps = Vec::with_capacity(1024);
                    Self::match_statements(body, &mut nested_deps);
                    deps.append(&mut nested_deps);
                }

                Stmt::Const { initializer, .. } => {
                    Self::match_expression(&initializer.value, deps);
                }

                Stmt::Expr { expr } => {
                    Self::match_expression(&expr.value, deps);
                }

                _ => ()
            }
        }
    }

    fn match_expression(expr: &Expr, deps: &mut Vec<String>)
    {
        match expr {
            Expr::Bad { token: _ } => (),

            Expr::Block { statements, value: _ } => {
                Self::match_statements(statements, deps);
            },

            Expr::If { condition, then_branch, then_value: _, else_branch, else_value: _ } => {
                Self::match_expression(&condition.value, deps);
                Self::match_statements(then_branch, deps);
                Self::match_statements(else_branch, deps);
            },

            Expr::Binary { left, right, operator: _ } => {
                Self::match_expression(&left.value, deps);
                Self::match_expression(&right.value, deps);
            },

            Expr::Literal { value: _ } => (),

            Expr::Variable { name: _ } => {
                // TODO: later
                // deps.push(name.value.clone());
            },

            Expr::Assignment { name: _, value } => {
                Self::match_expression(&value.value, deps);
            },

            Expr::Logical => (),

            Expr::Call { name, arguments } => {
                for arg in arguments {
                    Self::match_expression(&arg.value, deps);
                }
                deps.push(name.value.clone());
            },

            Expr::Function { params: _, return_type: _, param_types: _, body } => {
                Self::match_statements(body, deps);
            },
        }
    }
}

impl Transformer for GlobalsHoistingTransformer
{
    fn transform(nodes: Vec<Node>) -> Vec<Node>
    {
        use std::collections::VecDeque;

        let nodes_count = nodes.len();

        let mut declarations: Vec<String>      = Vec::with_capacity(nodes_count);
        let mut dependencies: Vec<Vec<String>> = Vec::with_capacity(nodes_count);
        let mut degrees: Vec<usize>              = Vec::with_capacity(nodes_count);

        // Build the dependency graph.

        for node in nodes.iter() {
            if let Node::Stmt(Stmt::Function { name, body, .. }) = &node {
                let mut deps = Vec::with_capacity(1024);
                Self::match_statements(body, &mut deps);

                declarations.push(name.value.clone());
                dependencies.push(deps);
                degrees.push(0);
            }
        }

        // Topological sort over the dependency graph.

        for i in 0..dependencies.len() {
            degrees[i] += dependencies[i].len();
        }

        let mut q: VecDeque<String> = VecDeque::with_capacity(nodes_count);

        for i in 0..dependencies.len() {
            if degrees[i] != 0 { continue }

            let declaration = &declarations[i];
            q.push_back(declaration.clone());
        }

        let mut order: Vec<String> = Vec::with_capacity(dependencies.len());

        while !q.is_empty() {
            let function = q.pop_front().expect("Expect next in queue.");
            order.push(function.clone());

            let index = declarations.iter().position(|d| d == &function).unwrap();

            for (i, deps) in dependencies.iter().enumerate() {
                if i == index                { continue }
                if !deps.contains(&function) { continue }

                degrees[i] -= 1;

                let dep = &declarations[i];
                if degrees[i] == 0 && !order.contains(dep) {
                    order.push(dep.clone());
                }
            }
        }

        let mut rest = declarations
            .iter()
            .filter(|name| !order.contains(name))
            .cloned()
            .collect();

        order.append(&mut rest);

        // After we get the order we can sort the root AST nodes in the proper order.

        let mut nodes = nodes.to_owned();
        nodes.sort_by(|a, b| {
            // TODO: for now we assume all root level nodes are functions.
            let a_name = match a {
                Node::Stmt(Stmt::Function { name, .. }) => &name.value,
                _ => unreachable!(),
            };

            let b_name = match b {
                Node::Stmt(Stmt::Function { name, .. }) => &name.value,
                _ => unreachable!(),
            };

            let a_pos= order.iter().position(|n| n == a_name).expect("Expect defined order.");
            let b_pos= order.iter().position(|n| n == b_name).expect("Expect defined order.");

            a_pos.cmp(&b_pos)
        });

        nodes
    }
}
