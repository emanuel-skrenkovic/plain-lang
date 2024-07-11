use std::collections::VecDeque;
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
        type_name: Option<scan::Token>,
        initializer: Box<ExprInfo>,
    },

    Const
    {
        name: scan::Token,
        type_name: Option<scan::Token>,
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

    MemberAccess
    {
        instance_name: scan::Token,
        member_name: scan::Token,
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
    fn transform(nodes: Vec<Node>) -> Vec<Node>;
}

#[derive(Debug)]
struct DependencyGraph
{
    nodes: Vec<String>,
    edges: Vec<Vec<String>>,
    connections: Vec<usize>,
}

pub struct GlobalsHoistingTransformer { }

impl GlobalsHoistingTransformer
{
    fn build_dependency_graph(nodes: &[Node]) -> DependencyGraph
    {
        let nodes_count = nodes.len();

        let mut declarations: Vec<String>      = Vec::with_capacity(nodes_count);
        let mut dependencies: Vec<Vec<String>> = Vec::with_capacity(nodes_count);
        let mut degrees: Vec<usize>            = Vec::with_capacity(nodes_count);

        for node in nodes.iter() {
            match node {
                Node::Stmt(Stmt::Struct { name, member_types, .. }) => {
                    declarations.push(name.value.clone());

                    let mut deps: Vec<String> = member_types
                        .iter()
                        .map(|t| t.value.clone())
                        .collect();
                    deps.dedup();

                    dependencies.push(deps);
                    degrees.push(0);
                }

                Node::Stmt(Stmt::Function { name, body, param_types, .. }) => {
                    let mut deps = Vec::with_capacity(1024);
                    
                    deps.append(&mut param_types.iter().map(|t| t.value.clone()).collect());

                    Self::match_statements(body, &mut deps);

                    declarations.push(name.value.clone());
                    dependencies.push(deps);
                    degrees.push(0);
                }

                Node::Stmt(Stmt::Const { name, initializer, .. }) => {
                    let mut deps = Vec::with_capacity(1024);
                    Self::match_expression(&initializer.value, &mut deps);

                    declarations.push(name.value.clone());
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
    fn topological_sort(graph: &mut DependencyGraph) -> Vec<String>
    {
        let count = graph.nodes.len();

        for i in 0..count {
            graph.connections[i] += graph.edges[i].len();
        }

        let mut q: VecDeque<usize> = VecDeque::with_capacity(count);

        for i in 0..count {
            if graph.connections[i] != 0 { continue }
            q.push_back(i);
        }

        let mut order: Vec<String> = Vec::with_capacity(count);

        while !q.is_empty() {
            let i = q.pop_front().expect("Expect next in queue.");

            let node = &graph.nodes[i];
            order.push(node.clone());

            for (j, deps) in graph.edges.iter().enumerate() {
                if j == i               { continue }
                if !deps.contains(node) { continue }

                graph.connections[j] -= 1;

                let dep = &graph.nodes[j];
                if graph.connections[j] == 0 && !order.contains(dep) {
                    order.push(dep.clone());
                }
            }
        }

        let mut rest = graph.nodes
            .iter()
            .filter(|name| !order.contains(name))
            .cloned()
            .collect();

        order.append(&mut rest);

        order
    }

    fn match_statements(statements: &[Box<Stmt>], deps: &mut Vec<String>)
    {
        for stmt in statements.iter().map(|s| s.as_ref()) {
            match stmt {
                Stmt::Struct { member_types, .. } => {
                    let mut type_names = member_types
                        .iter()
                        .map(|t| t.value.clone())
                        .collect();
                    deps.append(&mut type_names);
                }

                Stmt::Function { body, param_types, .. } => {
                    let mut nested_deps = Vec::with_capacity(1024);

                    nested_deps.append(&mut param_types.iter().map(|t| t.value.clone()).collect());
                    Self::match_statements(body, &mut nested_deps);

                    deps.append(&mut nested_deps);
                }

                Stmt::Var { initializer, .. } => Self::match_expression(&initializer.value, deps),

                Stmt::Const { initializer, .. } => Self::match_expression(&initializer.value, deps),

                Stmt::Expr { expr } => Self::match_expression(&expr.value, deps),

                _ => ()
            }
        }
    }

    fn match_expression(expr: &Expr, deps: &mut Vec<String>)
    {
        match expr {
            Expr::Block { statements, value } => {
                Self::match_statements(statements, deps);
                Self::match_expression(&value.value, deps);
            },

            Expr::If { condition, then_branch, else_branch, .. } => {
                Self::match_expression(&condition.value, deps);
                Self::match_statements(then_branch, deps);
                Self::match_statements(else_branch, deps);
            },

            Expr::Binary { left, right, .. } => {
                Self::match_expression(&left.value, deps);
                Self::match_expression(&right.value, deps);
            },

            // TODO: later
            Expr::Variable { name, .. } => deps.push(name.value.clone()),

            Expr::Assignment { value, .. } => Self::match_expression(&value.value, deps),

            Expr::Call { name, arguments } => {
                for arg in arguments {
                    Self::match_expression(&arg.value, deps);
                }
                deps.push(name.value.clone());
            },

            Expr::Function { body, .. } => Self::match_statements(body, deps),

            Expr::Struct { name, values, .. } => {
                deps.push(name.value.clone());
                for value in values {
                    Self::match_expression(&value.value, deps)
                }
            }

            _ => ()
        }
    }
}

impl Transformer for GlobalsHoistingTransformer
{
    fn transform(nodes: Vec<Node>) -> Vec<Node>
    {
        // First we build the dependency graph.
        let mut graph = Self::build_dependency_graph(&nodes);

        println!("{:#?}", graph);

        // Then we use topological sort to find the correct declaration order.
        let order = Self::topological_sort(&mut graph);

        println!("{:#?}", order);

        // After we get the order we can sort the root AST nodes accordingly.
        let mut nodes = nodes.to_owned();
        nodes.sort_by(|a, b| {
            // TODO: for now we assume all root level nodes are functions.
            let a_name = match a {
                Node::Stmt(Stmt::Struct { name, .. })   => &name.value,
                Node::Stmt(Stmt::Function { name, .. }) => &name.value,
                Node::Stmt(Stmt::Const { name, .. })    => &name.value,
                _ => unreachable!(),
            };

            let b_name = match b {
                Node::Stmt(Stmt::Struct { name, .. })   => &name.value,
                Node::Stmt(Stmt::Function { name, .. }) => &name.value,
                Node::Stmt(Stmt::Const { name, .. })    => &name.value,
                _ => unreachable!(),
            };

            let a_pos= order.iter().position(|n| n == a_name).expect("Expect defined order.");
            let b_pos= order.iter().position(|n| n == b_name).expect("Expect defined order.");

            a_pos.cmp(&b_pos)
        });

        nodes
    }
}
