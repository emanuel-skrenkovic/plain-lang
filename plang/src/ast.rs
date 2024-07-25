use std::collections::VecDeque;
use crate::{scan, source, types};


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
        name: scan::Token,
        members: Vec<scan::Token>,
        member_types: Vec<scan::Token>,
    },

    Function
    {
        name: scan::Token,
        params: Vec<scan::Token>,
        return_type: Option<scan::Token>,
        param_types: Vec<scan::Token>,
        body: Vec<Box<Stmt>>,
    },

    Var
    {
        name: scan::Token,
        type_name: Option<scan::Token>,
        initializer: Box<ExprInfo>,
    },

    Const
    {
        name: scan::Token,
        type_name: Option<scan::Token>,
        initializer: Box<ExprInfo>,
    },

    For 
    {
        token: scan::Token,
        initializer: Box<Stmt>,
        condition: Box<ExprInfo>,
        advancement: Box<Stmt>,
        body: Vec<Box<Stmt>>,
    },

    While
    {
        token: scan::Token,
        condition: Box<ExprInfo>,
        body: Vec<Box<Stmt>>,
    },

    Unary { },

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
        left_bracket: scan::Token,
        right_bracket: scan::Token,
        statements: Vec<Box<Stmt>>,
        value: Box<ExprInfo>,
    },

    If
    {
        token: scan::Token,
        conditions: Vec<Box<ExprInfo>>,
        branches: Vec<Box<ExprInfo>>,
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

    MemberAssignment
    {
        instance_name: scan::Token,
        member_name: scan::Token,
        value: Box<ExprInfo>,
    },

    MemberAccess
    {
        instance_name: scan::Token,
        member_name: scan::Token,
    },

    Logical,

    Return 
    {
        token: scan::Token,
        value: Box<ExprInfo>,
    },

    Call
    {
        name: scan::Token,
        arguments: Vec<Box<ExprInfo>>,
    },

    Function
    {
        left_paren: scan::Token,
        right_paren: scan::Token,
        params: Vec<scan::Token>,
        return_type: Option<scan::Token>,
        param_types: Vec<scan::Token>,
        body: Vec<Box<Stmt>>,
    },

    Struct 
    {
        name: scan::Token,
        members: Vec<scan::Token>,
        values: Vec<Box<ExprInfo>>,
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

pub trait Transformer
{
    fn transform(source: &source::Source, nodes: Vec<Node>) -> Vec<Node>;
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
    fn build_dependency_graph<'a>(source: &'a source::Source, nodes: &[Node]) -> DependencyGraph<'a>
    {
        let nodes_count = nodes.len();

        let mut declarations: Vec<&'a str>      = Vec::with_capacity(nodes_count);
        let mut dependencies: Vec<Vec<&'a str>> = Vec::with_capacity(nodes_count);
        let mut degrees: Vec<usize>             = Vec::with_capacity(nodes_count);

        for node in nodes {
            match node {
                Node::Stmt(Stmt::Struct { name, member_types, .. }) => {
                    declarations.push(source.token_value(name));

                    let mut deps: Vec<&'a str> = member_types
                        .iter()
                        .map(|t| source.token_value(t))
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
                                .map(|t| source.token_value(t))
                                .collect()
                    );

                    Self::match_statements(source, body, &mut deps);

                    declarations.push(source.token_value(name));
                    dependencies.push(deps);
                    degrees.push(0);
                }

                Node::Stmt(Stmt::Const { name, initializer, .. }) => {
                    let mut deps = Vec::with_capacity(64);
                    Self::match_expression(source, &initializer.value, &mut deps);

                    declarations.push(source.token_value(name));
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

        let mut q: Vec<(usize, usize)> = Vec::with_capacity(count);

        for i in 0..count {
            q.push((i, graph.connections[i]));
        }

        // Sort by number of connections, from least to most.
        q.sort_by(|a, b| a.1.cmp(&b.1)); 

        let mut q: VecDeque<usize> = q.into_iter().map(|(x, _)| x).collect();
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

    fn match_statements<'a>(source: &'a source::Source, statements: &[Box<Stmt>], deps: &mut Vec<&'a str>)
    {
        for stmt in statements.iter().map(std::convert::AsRef::as_ref) {
            match stmt {
                Stmt::Struct { member_types, .. } => {
                    let mut type_names = member_types
                        .iter()
                        .map(|t| source.token_value(t))
                        .collect();
                    deps.append(&mut type_names);
                }

                Stmt::Function { body, param_types, .. } => {
                    let mut nested_deps = Vec::with_capacity(64);

                    nested_deps.append
                    (
                        &mut param_types
                            .iter()
                            .map(|t| source.token_value(t))
                            .collect()
                    );
                    Self::match_statements(source, body, &mut nested_deps);

                    deps.append(&mut nested_deps);
                }

                Stmt::Var { initializer, .. } | Stmt::Const { initializer, .. } 
                    => Self::match_expression(source, &initializer.value, deps),

                Stmt::Expr { expr } => Self::match_expression(source, &expr.value, deps),

                _ => ()
            }
        }
    }

    fn match_expression<'a>(source: &'a source::Source, expr: &Expr, deps: &mut Vec<&'a str>)
    {
        match expr {
            Expr::Block { statements, value, .. } => {
                Self::match_statements(source, statements, deps);
                Self::match_expression(source, &value.value, deps);
            },

            Expr::If { conditions, branches, .. } => {
                for condition in conditions {
                    Self::match_expression(source, &condition.value, deps);
                }
                
                for branch in branches {
                    let Expr::Block { statements, value, .. } = &branch.value else {
                        panic!()
                    };

                    Self::match_statements(source, statements, deps);
                    Self::match_expression(source, &value.value, deps);
                }
            },

            Expr::Binary { left, right, .. } => {
                Self::match_expression(source, &left.value, deps);
                Self::match_expression(source, &right.value, deps);
            },

            // TODO: later
            Expr::Variable { name, .. } => deps.push(source.token_value(name)),

            Expr::Assignment { value, .. } => Self::match_expression(source, &value.value, deps),

            Expr::Call { name, arguments } => {
                for arg in arguments {
                    Self::match_expression(source, &arg.value, deps);
                }
                deps.push(source.token_value(name));
            },

            Expr::Function { body, .. } => Self::match_statements(source, body, deps),

            Expr::Struct { name, values, .. } => {
                deps.push(source.token_value(name));
                for value in values {
                    Self::match_expression(source, &value.value, deps);
                }
            }

            _ => ()
        }
    }
}

impl Transformer for GlobalsHoistingTransformer
{
    fn transform(source: &source::Source, nodes: Vec<Node>) -> Vec<Node>
    {
        // First we build the dependency graph.
        let mut graph = Self::build_dependency_graph(source, &nodes);

        // Then we use topological sort to find the correct declaration order.
        let order = Self::topological_sort(&mut graph);

        // After we get the order we can sort the root AST nodes accordingly.
        let mut nodes = nodes.clone();
        nodes.sort_by(|a, b| {
            // TODO: for now we assume all root level nodes are functions.
            let a_name = match a {
                Node::Stmt(
                    Stmt::Struct { name, .. } | Stmt::Function { name, .. } | Stmt::Const { name, .. }
                ) => source.token_value(name),
                _ => unreachable!(),
            };

            let b_name = match b {
                Node::Stmt(
                    Stmt::Struct { name, .. } | Stmt::Function { name, .. } | Stmt::Const { name, .. }
                ) => source.token_value(name),
                _ => unreachable!(),
            };

            let a_pos= order.iter().position(|n| n == &a_name).expect("Expect defined order.");
            let b_pos= order.iter().position(|n| n == &b_name).expect("Expect defined order.");

            a_pos.cmp(&b_pos)
        });

        nodes
    }
}
