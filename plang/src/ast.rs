use std::collections::VecDeque;
use crate::{scan, context, types};


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
    Struct 
    {
        name: scan::TokenId,
        members: Vec<scan::TokenId>,
        member_types: Vec<scan::TokenId>,
    },

    Function
    {
        name: scan::TokenId,
        params: Vec<scan::TokenId>,
        return_type: Option<scan::TokenId>,
        param_types: Vec<scan::TokenId>,
        body: Vec<Box<Stmt>>,
    },

    ReceiverFunction
    {
        receiver_type_name: scan::TokenId,
        name: scan::TokenId,
        params: Vec<scan::TokenId>,
        return_type: Option<scan::TokenId>,
        param_types: Vec<scan::TokenId>,
        body: Vec<Box<Stmt>>,
    },

    Var
    {
        name: scan::TokenId,
        type_name: Option<scan::TokenId>,
        initializer: Box<ExprInfo>,
    },

    Const
    {
        name: scan::TokenId,
        type_name: Option<scan::TokenId>,
        initializer: Box<ExprInfo>,
    },

    For 
    {
        token: scan::TokenId,
        initializer: Box<Stmt>,
        condition: Box<ExprInfo>,
        advancement: Box<Stmt>,
        body: Vec<Box<Stmt>>,
    },

    While
    {
        token: scan::TokenId,
        condition: Box<ExprInfo>,
        body: Vec<Box<Stmt>>,
    },

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
        token: scan::TokenId,
    },

    Block
    {
        left_bracket: scan::TokenId,
        right_bracket: scan::TokenId,
        statements: Vec<Box<Stmt>>,
        value: Option<Box<ExprInfo>>,
    },

    If
    {
        token: scan::TokenId,
        conditions: Vec<Box<ExprInfo>>,
        branches: Vec<Box<ExprInfo>>,
    },

    Binary
    {
        left: Box<ExprInfo>,
        right: Box<ExprInfo>,
        operator: scan::TokenId
    },

    Unary 
    {
        operator: scan::TokenId,
        expr: Box<ExprInfo>,
    },

    Literal
    {
        value: scan::TokenId, 
    },

    Variable
    {
        name: scan::TokenId,
    },

    Assignment
    {
        left: Box<ExprInfo>,
        right: Box<ExprInfo>,
    },

    MemberAccess
    {
        instance_name: scan::TokenId,
        member_name: scan::TokenId,
    },

    Return 
    {
        token: scan::TokenId,

        // TODO: this should be Option<T> because return; is viable
        // in functions returning nothing.
        // Time to decide on Unit vs Void.
        value: Box<ExprInfo>,
    },

    Call
    {
        name: scan::TokenId,
        arguments: Vec<Box<ExprInfo>>,
    },

    ReceiverCall
    {
        receiver_name: scan::TokenId,
        name: scan::TokenId,
        arguments: Vec<Box<ExprInfo>>,
    },

    Function
    {
        left_paren: scan::TokenId,
        right_paren: scan::TokenId,
        params: Vec<scan::TokenId>,
        return_type: Option<scan::TokenId>,
        param_types: Vec<scan::TokenId>,
        body: Vec<Box<Stmt>>,
    },

    Struct 
    {
        name: scan::TokenId,
        members: Vec<scan::TokenId>,
        values: Vec<Box<ExprInfo>>,
    }
}

#[derive(Debug)]
struct DependencyGraph<'a>
{
    nodes: Vec<&'a str>,
    edges: Vec<Vec<&'a str>>,
    connections: Vec<usize>,
}

pub struct GlobalsHoistingTransformer { }

impl GlobalsHoistingTransformer
{
    pub fn transform(ctx: &context::Context, nodes: &mut [Node])
    {
        // First we build the dependency graph.
        let mut graph = Self::build_dependency_graph(ctx, nodes);

        // Then we use topological sort to find the correct declaration order.
        let order = Self::topological_sort(&mut graph);

        // After we get the order we can sort the root AST nodes accordingly.
        nodes.sort_by(|a, b| {
            // TODO: for now we assume all root level nodes are functions.
            let a_name = match a {
                Node::Stmt(
                    Stmt::Struct { name, .. } | Stmt::Function { name, .. } | Stmt::ReceiverFunction { name, .. } | Stmt::Const { name, .. }
                ) => ctx.token_value(*name),
                _ => unreachable!(),
            };

            let b_name = match b {
                Node::Stmt(
                    Stmt::Struct { name, .. } | Stmt::Function { name, .. } | Stmt::ReceiverFunction { name, .. } | Stmt::Const { name, .. }
                ) => ctx.token_value(*name),
                _ => unreachable!(),
            };

            let a_pos = order.iter().position(|n| n == &a_name).expect("Expect defined order.");
            let b_pos = order.iter().position(|n| n == &b_name).expect("Expect defined order.");

            a_pos.cmp(&b_pos)
        });
    }
    fn build_dependency_graph<'a>(ctx: &'a context::Context, nodes: &[Node]) -> DependencyGraph<'a>
    {
        let nodes_count = nodes.len();

        let mut declarations: Vec<&'a str>      = Vec::with_capacity(nodes_count);
        let mut dependencies: Vec<Vec<&'a str>> = Vec::with_capacity(nodes_count);
        let mut degrees: Vec<usize>             = Vec::with_capacity(nodes_count);

        for node in nodes {
            match node {
                Node::Stmt(Stmt::Struct { name, member_types, .. }) => {
                    declarations.push(ctx.token_value(*name));

                    let mut deps: Vec<&'a str> = member_types
                        .iter()
                        .map(|t| ctx.token_value(*t))
                        .collect();
                    deps.dedup();

                    dependencies.push(deps);
                    degrees.push(0);
                }

                Node::Stmt(Stmt::Function { name, body, param_types, .. }) => {
                    let mut deps = Vec::with_capacity(64);
                    
                    deps.append
                    (
                        &mut param_types
                                .iter()
                                .map(|t| ctx.token_value(*t))
                                .collect()
                    );

                    Self::match_statements(ctx, body, &mut deps);

                    declarations.push(ctx.token_value(*name));
                    dependencies.push(deps);
                    degrees.push(0);
                }

                Node::Stmt(Stmt::ReceiverFunction { name, body, param_types, .. }) => {
                    let mut deps = Vec::with_capacity(64);
                    
                    deps.append
                    (
                        &mut param_types
                                .iter()
                                .map(|t| ctx.token_value(*t))
                                .collect()
                    );

                    Self::match_statements(ctx, body, &mut deps);

                    declarations.push(ctx.token_value(*name));
                    dependencies.push(deps);
                    degrees.push(0);
                }

                Node::Stmt(Stmt::Const { name, initializer, .. }) => {
                    let mut deps = Vec::with_capacity(64);
                    Self::match_expression(ctx, &initializer.value, &mut deps);

                    declarations.push(ctx.token_value(*name));
                    dependencies.push(deps);
                    degrees.push(0);
                }

                _ => ()
            }
        }

        DependencyGraph {
            nodes: declarations,
            edges: dependencies,
            connections: degrees,
        }
    }

    // Topological sort over the dependency graph.
    // There will be "unsolvable" orders because of which we need
    // to forward declare global scope stuff.
    fn topological_sort<'a>(graph: &'a mut DependencyGraph) -> Vec<&'a str>
    {
        let count = graph.nodes.len();

        for i in 0..count {
            graph.connections[i] += graph.edges[i].len();
        }

        // TODO: Fix this. It doesn't really work for hoisting.
        let mut q: Vec<usize> = Vec::with_capacity(count);

        for i in 0..count {
            q.push(i);
        }

        let mut q: VecDeque<usize> = VecDeque::from(q);
        let mut order: Vec<&str>   = Vec::with_capacity(count);

        while !q.is_empty() {
            let i = q.pop_front().expect("Expect next in queue.");

            let node = &graph.nodes[i];
            order.push(node);

            for (j, deps) in graph.edges.iter().enumerate() {
                if j == i               { continue }
                if !deps.contains(node) { continue }

                graph.connections[j] -= 1;

                let dep = &graph.nodes[j];
                if graph.connections[j] == 0 && !order.contains(dep) {
                    order.push(dep);
                }
            }
        }

        let mut rest = graph.nodes
            .iter()
            .filter(|name| !order.contains(name))
            .copied()
            .collect();

        order.append(&mut rest);

        order
    }

    fn match_statements<'a>(ctx: &'a context::Context, statements: &[Box<Stmt>], deps: &mut Vec<&'a str>)
    {
        for stmt in statements.iter().map(std::convert::AsRef::as_ref) {
            match stmt {
                Stmt::Struct { member_types, .. } => {
                    let mut type_names = member_types
                        .iter()
                        .map(|t| ctx.token_value(*t))
                        .collect();
                    deps.append(&mut type_names);
                }

                Stmt::Function { body, param_types, .. } => {
                    let mut nested_deps = Vec::with_capacity(64);

                    nested_deps.append
                    (
                        &mut param_types
                            .iter()
                            .map(|t| ctx.token_value(*t))
                            .collect()
                    );
                    Self::match_statements(ctx, body, &mut nested_deps);

                    deps.append(&mut nested_deps);
                }

                Stmt::Var { initializer, .. } | Stmt::Const { initializer, .. } 
                    => Self::match_expression(ctx, &initializer.value, deps),

                Stmt::Expr { expr } => Self::match_expression(ctx, &expr.value, deps),

                _ => ()
            }
        }
    }

    fn match_expression<'a>(ctx: &'a context::Context, expr: &Expr, deps: &mut Vec<&'a str>)
    {
        match expr {
            Expr::Block { statements, value, .. } => {
                Self::match_statements(ctx, statements, deps);

                if let Some(value) = value {
                    Self::match_expression(ctx, &value.value, deps);
                }
            },

            Expr::If { conditions, branches, .. } => {
                for condition in conditions {
                    Self::match_expression(ctx, &condition.value, deps);
                }
                
                for branch in branches {
                    let Expr::Block { statements, value, .. } = &branch.value else {
                        panic!()
                    };

                    Self::match_statements(ctx, statements, deps);

                    if let Some(value) = value {
                        Self::match_expression(ctx, &value.value, deps);
                    }
                }
            },

            Expr::Binary { left, right, .. } => {
                Self::match_expression(ctx, &left.value, deps);
                Self::match_expression(ctx, &right.value, deps);
            },

            // TODO: later
            Expr::Variable { name, .. } => deps.push(ctx.token_value(*name)),

            Expr::Assignment { left, right } => {
                Self::match_expression(ctx, &right.value, deps);
                Self::match_expression(ctx, &left.value, deps);
            }

            Expr::Call { name, arguments } => {
                for arg in arguments {
                    Self::match_expression(ctx, &arg.value, deps);
                }
                deps.push(ctx.token_value(*name));
            },

            Expr::Function { body, .. } => Self::match_statements(ctx, body, deps),

            Expr::Struct { name, values, .. } => {
                deps.push(ctx.token_value(*name));
                for value in values {
                    Self::match_expression(ctx, &value.value, deps);
                }
            }

            _ => ()
        }
    }
}

