use std::collections::VecDeque;
use crate::{scan, context};


pub type NodeId = usize;

#[derive(Debug, Clone)]
pub enum Node
{
    Stmt(Stmt),
    Expr(Expr),
}

impl Node
{
    pub fn expr_node(value: Expr) -> Node
    {
        Node::Expr(value)
    }

    pub fn stmt_node(value: Stmt) -> Node
    {
        Node::Stmt(value)
    }

    pub fn as_expr(&self) -> Result<&Expr, ()>
    {
        if let Node::Expr(value) = self {
            return  Ok(value)
        };

        Err(())
    }

    pub fn as_stmt(&self) -> Result<&Stmt, ()>
    {
        if let Node::Stmt(stmt) = self {
            return  Ok(stmt)
        };

        Err(())
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
        body: Vec<NodeId>,
    },

    Var
    {
        name: scan::TokenId,
        type_name: Option<scan::TokenId>,
        initializer: NodeId,
    },

    Const
    {
        name: scan::TokenId,
        type_name: Option<scan::TokenId>,
        initializer: NodeId,
    },

    For 
    {
        token: scan::TokenId,
        initializer: NodeId,
        condition: NodeId,
        advancement: NodeId,
        body: Vec<NodeId>,
    },

    While
    {
        token: scan::TokenId,
        condition: NodeId,
        body: Vec<NodeId>,
    },

    Expr
    {
        expr: NodeId,
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
        statements: Vec<NodeId>,
        value: Option<NodeId>,
    },

    If
    {
        token: scan::TokenId,
        conditions: Vec<NodeId>,
        branches: Vec<NodeId>,
    },

    Binary
    {
        left: NodeId,
        right: NodeId,
        operator: scan::TokenId
    },

    Unary 
    {
        operator: scan::TokenId,
        expr: NodeId,
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
        left: NodeId,
        right: NodeId,
    },

    MemberAccess
    {
        left: NodeId,
        right: scan::TokenId,
    },

    Index
    {
        container: NodeId,
        value: NodeId,
    },

    Return 
    {
        token: scan::TokenId,

        // TODO: this should be Option<T> because return; is viable
        // in functions returning nothing.
        // Time to decide on Unit vs Void.
        value: NodeId,
    },

    Call
    {
        name: scan::TokenId,
        arguments: Vec<NodeId>,
    },

    ReceiverCall
    {
        receiver: NodeId,
        name: scan::TokenId,
        arguments: Vec<NodeId>,
    },

    Function
    {
        left_paren: scan::TokenId,
        right_paren: scan::TokenId,
        params: Vec<scan::TokenId>,
        return_type: Option<scan::TokenId>,
        param_types: Vec<scan::TokenId>,
        body: Vec<NodeId>,
    },

    Struct 
    {
        name: scan::TokenId,
        members: Vec<scan::TokenId>,
        values: Vec<NodeId>,
    },

    Slice
    {
        type_name: scan::TokenId,
        initial_values: Vec<NodeId>,
    },
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
                    Stmt::Struct { name, .. } | Stmt::Function { name, .. } | Stmt::Const { name, .. }
                ) => ctx.token_value(*name),
                _ => return std::cmp::Ordering::Equal,
            };

            let b_name = match b {
                Node::Stmt(
                    Stmt::Struct { name, .. } | Stmt::Function { name, .. } | Stmt::Const { name, .. }
                ) => ctx.token_value(*name),
                _ => return std::cmp::Ordering::Equal,
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

        for i in 0..nodes.len() {
            let node = &nodes[i];
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

                    Self::match_statements(ctx, nodes, body, &mut deps);

                    declarations.push(ctx.token_value(*name));
                    dependencies.push(deps);
                    degrees.push(0);
                }

                Node::Stmt(Stmt::Const { name, initializer, .. }) => {
                    let mut deps = Vec::with_capacity(64);
                    let initializer_expr = &nodes[*initializer];
                    Self::match_expression(ctx, nodes, initializer_expr.as_expr().unwrap(), &mut deps);

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

    fn match_statements<'a>(ctx: &'a context::Context, nodes: &[Node], indices: &[usize], deps: &mut Vec<&'a str>)
    {
        for i in indices {
            let stmt = &nodes[*i];
        // for stmt in statements.iter().map(std::convert::AsRef::as_ref) {
            match stmt.as_stmt().unwrap() {
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
                    Self::match_statements(ctx, nodes, body, &mut nested_deps);

                    deps.append(&mut nested_deps);
                }

                Stmt::Var { initializer, .. } | Stmt::Const { initializer, .. } 
                    => {
                    let initializer_expr = &nodes[*initializer];
                    Self::match_expression(ctx, nodes, initializer_expr.as_expr().unwrap(), deps);
                }

                Stmt::Expr { expr } => {
                    let expr = &nodes[*expr];
                    Self::match_expression(ctx, nodes, expr.as_expr().unwrap(), deps);
                }

                _ => ()
            }
        }
    }

    fn match_expression<'a>(ctx: &'a context::Context, nodes: &[Node], expr: &Expr, deps: &mut Vec<&'a str>)
    {
        match expr {
            Expr::Block { statements, value, .. } => {
                Self::match_statements(ctx, nodes, statements, deps);

                if let Some(value) = value {
                    let expr = &nodes[*value].as_expr().unwrap();
                    Self::match_expression(ctx, nodes, expr, deps);
                }
            },

            Expr::If { conditions, branches, .. } => {
                for condition in conditions {
                    let expr = &nodes[*condition].as_expr().unwrap();
                    Self::match_expression(ctx, nodes, expr, deps);
                }
                
                for branch in branches {
                    let branch = &nodes[*branch].as_expr().unwrap();
                    let Expr::Block { statements, value, .. } = branch else {
                        panic!()
                    };

                    Self::match_statements(ctx, nodes, statements, deps);

                    if let Some(value) = value {
                        let expr = &nodes[*value].as_expr().unwrap();
                        Self::match_expression(ctx, nodes, expr, deps);
                    }
                }
            },

            Expr::Binary { left, right, .. } => {
                let left  = &nodes[*left].as_expr().unwrap();
                let right = &nodes[*right].as_expr().unwrap();

                Self::match_expression(ctx, nodes, left, deps);
                Self::match_expression(ctx, nodes, right, deps);
            },

            // TODO: later
            Expr::Variable { name, .. } => deps.push(ctx.token_value(*name)),

            Expr::Assignment { left, right } => {
                let left  = &nodes[*left].as_expr().unwrap();
                let right = &nodes[*right].as_expr().unwrap();

                Self::match_expression(ctx, nodes, right, deps);
                Self::match_expression(ctx, nodes, left, deps);
            }

            Expr::Call { name, arguments } => {
                for arg in arguments {
                    let arg_expr = &nodes[*arg].as_expr().unwrap();
                    Self::match_expression(ctx, nodes, arg_expr, deps);
                }
                deps.push(ctx.token_value(*name));
            },

            Expr::Function { body, .. } => Self::match_statements(ctx, nodes, body, deps),

            Expr::Struct { name, values, .. } => {
                deps.push(ctx.token_value(*name));
                for value in values {
                    let val_expr = &nodes[*value].as_expr().unwrap();
                    Self::match_expression(ctx, nodes, val_expr, deps);
                }
            }

            _ => ()
        }
    }
}

