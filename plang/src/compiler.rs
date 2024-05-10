use std::fmt;

use crate::block;
use crate::scan;
/*
    case ClassStmt classStmt:
    case IfStmt ifStmt:
    case DeclarationStmt declarationStmt:
    case VariableStmt variableStmt:
    case BlockStmt blockStmt:
    case ExpressionStmt exprStmt:
    case PrintStmt printStmt:
    case WhileStmt whileStmt:
    case LoopControlStmt loopControlStmt:
    case FunctionStmt functionStmt:
    case ReturnStmt returnStmt:
 */

pub struct Ast
{
    pub token: scan::Token,
    pub nodes: Vec<Node>,
}

pub enum Node
{
    Stmt,
    Expr,
}

#[derive(Debug, Clone)]
pub enum Stmt
{
    Function {
        params: scan::Token,
        body: Vec<Box<Stmt>>,
    },

    Declaration {
        name: scan::Token,
        initializer: Box<Expr>,
    },

    Block {
        statements: Vec<Box<Stmt>>
    },

    Var {
        name: scan::Token,
        initializer: Box<Expr>,
    },

    Const {
        name: scan::Token,
        initializer: Box<Expr>,
    },

    For { },

    While {
        condition: Box<Expr>,
        body: Vec<Box<Stmt>>,
    },

    Return { },

    Expr {
        expr: Box<Expr>,
    }
}

#[derive(Debug, Clone)]
pub enum Expr
{
    Bad {
        token: scan::Token,
    },

    Block {
        statements: Vec<Box<Stmt>>,
        value: Box<Expr>,
    },

    If {
        condition: Box<Expr>,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },

    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        operator: scan::Token
    },

    Grouping,

    Literal {
        value: block::Value,
    },

    Variable {
        name: scan::Token,
    },

    Unary,

    Assignment,

    Logical,

    Call {
        arguments: Vec<Box<Expr>>,
    },

    Function,
}

// //

#[derive(Debug)]
pub enum CompilerErrorKind
{
    ParseError
}

#[derive(Debug)]
pub struct CompilerError
{
    pub line: usize,
    pub source_line: String,
    pub token: String,
    pub msg: String,
    pub kind: CompilerErrorKind
}

impl fmt::Display for CompilerError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        write!(
            f,
            "\n{} | {:?}: {}\nat token: '{}'\nline: {}",
            self.line,
            self.kind,
            self.msg,
            self.token,
            self.source_line,
        )
    }
}

#[repr(u8)]
#[derive(Copy, Clone, PartialOrd, PartialEq)]
enum Precedence
{
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary
}

impl Precedence
{
    pub fn discriminator(self) -> u8
    {
        self as u8
    }
}

impl TryFrom<u8> for Precedence
{
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error>
    {
        match v {
            x if x == Precedence::None as u8       => Ok(Precedence::None),
            x if x == Precedence::Assignment as u8 => Ok(Precedence::Assignment),
            x if x == Precedence::Or as u8         => Ok(Precedence::Or),
            x if x == Precedence::And as u8        => Ok(Precedence::And),
            x if x == Precedence::Equality as u8   => Ok(Precedence::Equality),
            x if x == Precedence::Comparison as u8 => Ok(Precedence::Comparison),
            x if x == Precedence::Term as u8       => Ok(Precedence::Term),
            x if x == Precedence::Factor as u8     => Ok(Precedence::Factor),
            x if x == Precedence::Unary as u8      => Ok(Precedence::Unary),
            x if x == Precedence::Call as u8       => Ok(Precedence::Call),
            x if x == Precedence::Primary as u8    => Ok(Precedence::Primary),
            _ => Err(()),
        }
    }
}

type ParseFn = fn(&mut Compiler) -> Expr;

#[derive(Copy, Clone)]
struct ParseRule
{
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence
}

static RULES: [ParseRule; 41] =
[
    ParseRule { prefix: Some(Compiler::function_expression), infix: Some(Compiler::function_invocation), precedence: Precedence::Call }, // LeftParen
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // RightParen
    ParseRule { prefix: Some(Compiler::block_expression),    infix: None,                                precedence: Precedence::None }, // LeftBracket
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // RightBracket
    ParseRule { prefix: None,                                infix: Some(Compiler::binary),              precedence: Precedence::Comparison }, // LeftAngle
    ParseRule { prefix: None,                                infix: Some(Compiler::binary),              precedence: Precedence::Comparison }, // RightAngle
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Questionmark
    ParseRule { prefix: Some(Compiler::semicolon),           infix: None,                                precedence: Precedence::None }, // Semicolon
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Colon
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // ColonColon
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // ColonEquals
    ParseRule { prefix: None,                                infix: Some(Compiler::binary),              precedence: Precedence::Term }, // Plus
    ParseRule { prefix: None,                                infix: Some(Compiler::binary),              precedence: Precedence::Term }, // Minus
    ParseRule { prefix: None,                                infix: Some(Compiler::binary),              precedence: Precedence::Factor }, // Star
    ParseRule { prefix: None,                                infix: Some(Compiler::binary),              precedence: Precedence::Factor }, // Slash
    ParseRule { prefix: None,                                infix: Some(Compiler::pipe),                precedence: Precedence::Call }, // Pipe
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Comma
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Bang
    ParseRule { prefix: None,                                infix: Some(Compiler::binary),              precedence: Precedence::Equality }, // BandEqual
    ParseRule { prefix: None,                                infix: Some(Compiler::binary),              precedence: Precedence::Equality }, // EqualEqual
    ParseRule { prefix: None,                                infix: Some(Compiler::binary),              precedence: Precedence::Comparison }, // GreaterEqual
    ParseRule { prefix: None,                                infix: Some(Compiler::binary),              precedence: Precedence::Comparison }, // LessEqual
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Equal
    ParseRule { prefix: Some(Compiler::literal),             infix: None,                                precedence: Precedence::None }, // True
    ParseRule { prefix: Some(Compiler::literal),             infix: None,                                precedence: Precedence::None }, // False
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // This
    ParseRule { prefix: Some(Compiler::_if),                 infix: None,                                precedence: Precedence::None }, // If
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Else
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Break
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Continue
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Switch
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Case
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // For
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // While
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Func
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Struct
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Interface
    ParseRule { prefix: Some(Compiler::literal),             infix: None,                                precedence: Precedence::None }, // Literal
    ParseRule { prefix: Some(Compiler::variable),            infix: None,                                precedence: Precedence::None }, // Identifier
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }, // Error
    ParseRule { prefix: None,                                infix: None,                                precedence: Precedence::None }  // End
];

fn get_rule(token_kind: scan::TokenKind) -> ParseRule
{
    RULES[token_kind as usize]
}

pub struct Parser
{
    // Also keep the source in the parser for better error reporting.
    // Feel like this solution is bad, and that I should avoid cloning the source so much.
    // Got no other ideas, just feels that way currently.
    source: String,
    tokens: Vec<scan::Token>,
    scanned_tokens: Vec<scan::Token>,

    current_index: usize,

    current: scan::Token,
    previous: scan::Token,

    panic: bool,
    error: bool
}

impl Parser
{
    #[must_use]
    pub fn new(source: String, tokens: Vec<scan::Token>) -> Parser
    {
        Parser {
            source,
            tokens,
            scanned_tokens: Vec::with_capacity(1024 * 8),
            current_index: 0,
            current: scan::Token::default(),
            previous: scan::Token::default(),
            panic: false,
            error: false
        }
    }

    fn advance(&mut self) -> Result<(), CompilerError>
    {
        self.previous = self.current.clone();

        if matches!(self.current.kind, scan::TokenKind::End) {
            return Ok(())
        }

        if self.current_index >= self.tokens.len() {
            return Ok(())
        }

        self.current = self.tokens[self.current_index].clone();

        if matches!(self.current.kind, scan::TokenKind::End) {
            return Ok(())
        }

        self.current_index += 1;
        self.scanned_tokens.push(self.current.clone());

        if !matches!(self.current.kind, scan::TokenKind::Error) {
            return Ok(())
        }

        Err(self.error_at("Scanner error.", &self.current.clone()))
    }

    fn match_token(&mut self, token_kind: scan::TokenKind) -> Result<bool, CompilerError>
    {
        if self.current.kind.discriminant() != token_kind.discriminant() {
            return Ok(false)
        }

        self.advance()?;
        Ok(true)
    }

    fn check_token(&self, token_kind: scan::TokenKind) -> bool
    {
        self.current.kind.discriminant() == token_kind.discriminant()
    }

    fn consume(&mut self, token_kind: scan::TokenKind, error_message: &str) -> Result<(), CompilerError>
    {
        match self.match_token(token_kind) {
            Ok(value) => match value {
                true => Ok(()),
                false => Err(self.error_at(error_message, &self.current.clone()))
            }
            Err(err) => Err(err)
        }
    }

    fn error_at(&mut self, message: &str, token: &scan::Token) -> CompilerError
    {
        self.panic = true;
        self.error = true;

        let lines: Vec<String> = self
            .source
            .lines()
            .map(String::from)
            .collect();

        CompilerError {
            msg: message.to_owned(),
            line: token.line,
            source_line: lines[token.line - 1].clone(),
            token: token.value.clone(),
            kind: CompilerErrorKind::ParseError,
        }
    }

    // TODO: horrible and I should be publicly shamed, doesn't even work oh my gosh how embarrassing oh my
    fn peek(&self, diff: i32) -> Option<&scan::Token>
    {
        let index = self.scanned_tokens.len() - 1;
        let index = index as i32 + diff;

        if index < 0 || index > self.scanned_tokens.len() as i32 {
            return None
        }

        let index = index as usize;

        Some(&self.scanned_tokens[index])
    }
}

#[derive(Clone, Debug)]
pub struct Variable
{
    pub name: scan::Token,
    pub mutable: bool,
    pub defined: bool,
    pub type_name: Option<String>,
}

#[derive(Clone, Debug)]
pub struct Scope
{
    pub index: usize,
    pub path: Vec<usize>,
    pub variables: Vec<Variable>,
    pub scope_depth: usize,
    pub parent_function: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct Closure
{
    pub function: Function,
    pub upvalues: Vec<usize>, // TOOD: maybe
    pub scopes: Vec<Scope>,
    pub scope_depth: usize,
}

// TODO: I think I should start focusing on the LLVM implementation rather than the
// custom interpreter. This was always going to be a compiled language, and it is likely
// that the semantics of the LLVM implementation will stay more relevant.
#[derive(Clone, Debug)]
pub struct Function
{
    // pub parent_scope: usize,
    pub parent_function_index: Option<usize>,

    pub block: block::Block,

    pub scopes: Vec<usize>,
    pub scope_depth: usize,
}

impl Function
{
    pub fn new(entry_scope_index: usize, parent_function_index: Option<usize>) -> Self
    {
        Self {
            parent_function_index,
            block: block::Block::new(1024),
            scope_depth: 0,
            scopes: vec![entry_scope_index],
        }
    }
}

// I feel like I have to simulate relational data with functions and scopes.

#[derive(Clone, Debug)]
pub struct Program
{
    pub block: block::Block,
    pub scopes: Vec<Scope>,
    pub functions: Vec<Function>,
}

impl Default for Program
{
    fn default() -> Self
    {
        Self {
            block: block::Block::new(1024),

            scopes: vec!
            [
                Scope {
                    index: 0,
                    path: vec![],
                    variables: vec![],
                    scope_depth: 0,
                    parent_function: None,
                }
            ],

            functions: vec![],
        }
    }
}

pub struct Compiler
{
    // Keep the source for error reporting to be better.
    source: String,
    parser: Parser,

    program: Program,

    current_scope_index: usize,
    current_function: Option<usize>,

    stack: Vec<Expr>,

    pub errors: Vec<CompilerError>,
}

impl Compiler
{
    #[must_use]
    pub fn new(source: String) -> Compiler
    {
        let source_clone = source.clone();
        Compiler {
            source,
            parser: Parser::new(source_clone, vec![]),
            program: Program::default(),

            current_scope_index: 0,
            current_function: None,

            stack: Vec::with_capacity(1024),

            errors: vec![],
        }
    }

    pub fn compile(mut self, tokens: Vec<scan::Token>) -> Result<Program, Vec<CompilerError>>
    {
        self.parser = Parser::new(self.source.clone(), tokens);
        let _ = self.parser.advance().map_err(|e| self.error(e));

        // TODO: should probably enclose program itself.

        let mut statements: Vec<Stmt> = Vec::with_capacity(1024 * 8);

        loop {
            match self.parser.current.kind {
                scan::TokenKind::End => break,
                _                    => {
                    statements.push(self.declaration())
                }
            }
        }

        if self.parser.panic {
            return Err(self.errors)
        }

        // println!("{:#?}", statements);

        Ok(self.program)
    }

    // TOOD: Currently we are experienceing the "main" problem.
    // Getting the current executing function does not work because I don't expect a function to
    // be there already, since globals and type definitions do/will exist.
    // But that means that the implicit assumption that we can get "current()" does not hold
    // and stuff breaks.
    // I have to find a way to deal with these two separate lexical scopes - global and within a function.
    fn current(&self) -> &Function
    {
        // TODO: Remove unwrap, I think this break stuff!
        &self.program.functions[self.current_function.unwrap()]
    }

    fn current_mut(&mut self) -> &mut Function
    {
        // TODO: Remove unwrap, I think this break stuff!
        &mut self.program.functions[self.current_function.unwrap()]
    }

    fn current_scope(&self) -> &Scope
    {
        let index = self.current_scope_index;
        &self.program.scopes[index]
    }

    fn current_scope_mut(&mut self) -> &mut Scope
    {
        let scope = self.current_scope_index;
        &mut self.program.scopes[scope]
    }

    fn is_at_end(&self) -> bool
    {
        self.parser.current.kind.discriminant() == scan::TokenKind::End.discriminant()
    }

    fn parse_precedence(&mut self, prec: Precedence)
    {
        let _ = self.parser.advance().map_err(|e| self.error(e));

        match get_rule(self.parser.previous.kind).prefix {
            Some(prefix) => {
                let prefix_expr = prefix(self);
                self.stack.push(prefix_expr);
            },
            _ => {
                if self.is_at_end() { return }

                panic!("{}", self.parser.error_at("Expect expression.", &self.parser.current.clone()))
            }
        }

        while prec.discriminator() <= get_rule(self.parser.current.kind).precedence.discriminator() {
            let _ = self.parser.advance().map_err(|e| self.error(e));

            let Some(infix_rule) = get_rule(self.parser.previous.kind).infix else {
                self.error_at("Failed to get infix rule.", &self.parser.previous.clone());
                return
            };

            let infix_rule = infix_rule(self);
            self.stack.push(infix_rule);
        }
    }

    fn expression_statement(&mut self) -> Stmt
    {
        Stmt::Expr {
            expr: Box::new(self.expression()),
        }
    }

    fn expression(&mut self) -> Expr
    {
        self.parse_precedence(Precedence::Assignment);
        // TODO
        self.stack.pop().unwrap()
    }

    fn block_expression(&mut self) -> Expr
    {
        self.begin_scope();

        let mut statements = vec![];

        // Compile code until the end of the block or the end of the program is reached.
        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            let statement = self.declaration();
            statements.push(Box::new(statement));
        }

        // Blocks are expressions - this captures if the block contains a value,
        // or returns 'Unit'.
        // If the final statement in the block is a semicolon, then treat it
        // like a value-less block, else, return the last value in the block.
        // let has_value = !matches!(self.parser.previous.kind, scan::TokenKind::Semicolon);

        self.consume(scan::TokenKind::RightBracket, "Expect '}' at the end of a block expression.");
        self.match_token(scan::TokenKind::Semicolon);

        self.end_scope();
        Expr::Block {
            statements,
            // TODO: actual expression
            value: Box::new(Expr::Literal { value: block::Value::Unit })
        }
    }

    // TODO: this is the result of bad design. I need to use block expression
    // code when parsing both block expressions and function (valued block expressions?).
    // Since the 'block_expression' method calls end scope, and thus consumes the closure,
    // that code cannot be reused for function declarations.
    // This is bad and it should be implemented in more reusable, functional way.
    fn code_block(&mut self)
    {
        // Compile code until the end of the block or the end of the program is reached.
        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            self.declaration();
        }

        // Blocks are expressions - this captures if the block contains a value,
        // or returns 'Unit'.
        // If the final statement in the block is a semicolon, then treat it
        // like a value-less block, else, return the last value in the block.
        // let has_value = !matches!(self.parser.previous.kind, scan::TokenKind::Semicolon);
        // let count = if has_value && count > 0 { count - 1 } else { count };

        // TODO: should probably pop the values that are not the result of the expression.

        self.consume(scan::TokenKind::RightBracket, "Expect '}' at the end of a block expression.");
    }

    fn binary(&mut self) -> Expr
    {
        let operator   = self.parser.previous.clone();
        let parse_rule = get_rule(operator.kind);

        self.parse_precedence(
            Precedence::try_from(parse_rule.precedence.discriminator() + 1)
                .unwrap()
        );

        let right = self.stack.pop().unwrap();
        let left  = self.stack.pop().unwrap();

        let expr = Expr::Binary {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        };

        // match operator2 {
        //     scan::TokenKind::LeftAngle    => { Expr::Binary { left, right, operator }; self.emit_byte(block::Op::Less); }
        //     scan::TokenKind::RightAngle   => { self.emit_byte(block::Op::Greater); }
        //     scan::TokenKind::BangEqual    => { self.emit_bytes(block::Op::Equal, block::Op::Not); }
        //     scan::TokenKind::EqualEqual   => { self.emit_byte(block::Op::Equal); }
        //     scan::TokenKind::GreaterEqual => { self.emit_byte(block::Op::GreaterEqual); }
        //     scan::TokenKind::LessEqual    => { self.emit_byte(block::Op::LessEqual); }
        //     scan::TokenKind::Plus         => { self.emit_byte(block::Op::Add); }
        //     scan::TokenKind::Minus        => { self.emit_byte(block::Op::Subtract); }
        //     scan::TokenKind::Star         => { self.emit_byte(block::Op::Multiply); }
        //     scan::TokenKind::Slash        => { self.emit_byte(block::Op::Divide); }
        //     scan::TokenKind::Pipe         => { self.pipe(); },
        //     _ => ()
        // };

        self.match_token(scan::TokenKind::Semicolon);
        expr
    }

    fn literal(&mut self) -> Expr
    {
        let expr = match self.parser.previous.kind {
            scan::TokenKind::True  => Expr::Literal { value: block::Value::Bool { val: true } },
            scan::TokenKind::False => Expr::Literal { value: block::Value::Bool { val: false } },
            _ => {
                // Determine the type of the literal and create a matching value.
                let t = self.parser.previous.value.clone();

                // I don't like if else-if chains. >:(
                if t.starts_with('\"') {
                    // Removing the first and last chars because the token value contains the
                    // starting and ending quotes.
                    let val = t.clone()[1..t.len()-1].to_string();
                    Expr::Literal {
                        value: block::Value::String { val },
                    }
                } else if let Some(first) = t.chars().nth(0) {
                    // This is incorrect - should be part of the outer if-else conditions.
                    if char::is_numeric(first) {
                        let Ok(val) = t.parse::<i32>() else {
                            return self.error_at("Expected number.", &self.parser.previous.clone());
                        };

                        Expr::Literal {
                            value: block::Value::Number { val },
                        }
                    } else {
                        return self.error_at("Failed to parse token as a literal.", &self.parser.previous.clone());
                    }
                } else {
                    return self.error_at("Failed to parse token as a literal.", &self.parser.previous.clone());
                }
            }
        };

        // Eat the semicolon only if present;
        self.match_token(scan::TokenKind::Semicolon);
        expr
    }

    fn declaration(&mut self) -> Stmt
    {
        match self.parser.current.kind {
            scan::TokenKind::Func => {
                let _ = self.parser.advance().map_err(|e| self.error(e));
                self.function_decl()
            }
            scan::TokenKind::While => {
                let _ = self.parser.advance().map_err(|e| self.error(e));
                self._while()
            },
            scan::TokenKind::For => {
                let _ = self.parser.advance().map_err(|e| self.error(e));
                self._for()
            }
            scan::TokenKind::Identifier => {
                self.variable_statement().unwrap_or_else(|| self.expression_statement())
            }
            _ => self.expression_statement(),
        }
    }

    fn variable_statement(&mut self) -> Option<Stmt>
    {
        let Some(next) = self.parser.peek(1) else {
            return None
        };

        let mut maybe_type_name: Option<String> = None;

        let next_kind       = next.kind.discriminant();
        let type_definition = next_kind == scan::TokenKind::Colon.discriminant();
        let immutable       = next_kind == scan::TokenKind::ColonColon.discriminant();
        let mutable         = next_kind == scan::TokenKind::ColonEquals.discriminant();

        if type_definition {
            let _ = self.match_token(scan::TokenKind::Colon);
            self.consume(scan::TokenKind::Identifier, "Expected identifier");

            maybe_type_name = Some(self.parser.previous.clone().value);

            if !self.match_token(scan::TokenKind::Equal) {
                return Some
                    (
                        Stmt::Expr {
                            expr: Box::new(self.error_at("Expected token '='.", &self.parser.current.clone()))
                        }
                    )
            }
        } else if mutable || immutable {
            self.match_token(
                if mutable { scan::TokenKind::ColonEquals } else { scan::TokenKind::ColonColon }
            );
        } else {
            return None
        }

        let _ = self.parser.advance().map_err(|e| self.error(e));

        let variable_token = self.parser.previous.clone();
        let variable_name  = variable_token.value.clone();

        if self.variable_exists(&variable_name) {
            // even more #horribleways
            return Some
            (
                Stmt::Expr {
                    expr: Box::new
                    (
                        self.error_at
                        (
                            &format!("Cannot redeclare variable with name '{}'.", &variable_token),
                            &variable_token.clone()
                        )
                    )
                }
            )
        }





        let initializer = self.expression();

        let index = self.declare_variable(variable_token.clone(), mutable, maybe_type_name);
        self.variable_declaration(index);
        self.variable_definition(index);

        self.match_token(scan::TokenKind::Semicolon);
        let stmt = if mutable {
            Stmt::Var {
                name: variable_token,
                initializer: Box::new(initializer),
            }
        } else {
            Stmt::Const {
                name: variable_token,
                initializer: Box::new(initializer),
            }
        };

        return Some(stmt)
    }

    // If the variable type is specified, then assign the variable type here.
    // Otherwise, infer the type (if possible) from the value assigned to the variable
    // during type checking.
    // I can already see problems forming with this inference system. sadface
    fn variable(&mut self) -> Expr
    {
        let variable_token = self.parser.previous.clone();

        // This handles the case where the variable is used as an expression as opposed to
        // being defined (which the code above handles).

        // TODO: bring this back?
        // if let Some(next) = self.parser.peek(0) {
        //     if next.kind.discriminant() == scan::TokenKind::LeftParen.discriminant() {
        //         // This is a function call, do nothing in this case, 'call' will handle it.
        //         self.match_token(scan::TokenKind::Semicolon);
        //         return todo!()
        //     }
        // }

        if self.parse_variable().is_none() {
            return self
                .error_at(&format!("Variable '{}' is not declared.", &self.parser.previous.value), &self.parser.previous.clone());
        }

        self.match_token(scan::TokenKind::Semicolon);
        Expr::Variable {
            name: variable_token,
        }
    }

    fn function(&mut self, name: &str) -> block::Value
    {
        self.consume(scan::TokenKind::LeftParen, "Expected '(' after function declaration.");
        self.begin_function();

        let mut arity = 0;
        // If open paren is not followed by closed paren, then parse the parameters.
        if !self.parser.check_token(scan::TokenKind::RightParen) {
            loop {
                arity += 1;

                self.consume(
                    scan::TokenKind::Identifier,
                    "Expect parameter identifier after '('."
                );

                let parameter_name_token = self.parser.previous.clone();

                // TODO: type
                let variable_key = self.declare_variable(parameter_name_token, false, None);
                self.variable_declaration(variable_key);

                if !self.match_token(scan::TokenKind::Comma) {
                    break
                }
            }
        }

        self.consume(scan::TokenKind::RightParen, "Expected ')' after function parameters.");

        // Parse the block expression that defines the function.
        self.consume(scan::TokenKind::LeftBracket, "Expected '{' before function body.");

        let mut count = 0;
        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            self.declaration();
            count += 1;
        }

        let has_value = !matches!(self.parser.previous.kind, scan::TokenKind::Semicolon);
        let count     = if has_value && count > 0 { count - 1 } else { count };

        self.match_token(scan::TokenKind::RightBracket);
        self.match_token(scan::TokenKind::Semicolon);

        let function_code = self.end_function();
        block::Value::Function {
            name: name.to_owned(),
            arity,
            closure: block::Closure { code: function_code },

            // TODO: type
            argument_type_names: vec![],
            return_type_name: "TODO: FIXME".to_string(),
        }
    }

    fn function_expression(&mut self) -> Expr
    {
        self.begin_function();

        // a :: ()
        // ^ __
        // 3 21
        let function_token = self.parser.peek(-3).unwrap().clone();
        let function_name = function_token.value.clone();

        let mut argument_type_names: Vec<Option<String>> = vec![];

        let mut arity = 0;
        if !self.parser.check_token(scan::TokenKind::RightParen) {
            loop {
                arity += 1;

                self.consume(scan::TokenKind::Identifier, "Expect parameter identifier after '('.");

                let parameter_name_token = self.parser.previous.clone();

                let maybe_type_name: Option<String> = if self.match_token(scan::TokenKind::Colon) {
                    self.consume(scan::TokenKind::Identifier, "Expected identifier");
                    Some(self.parser.previous.clone().value)
                } else {
                    None
                };

                argument_type_names.push(maybe_type_name.clone());
                // Ooopsy whoopsy dooopsy hooopsy we just made it mutable.
                self.declare_variable(parameter_name_token, true, maybe_type_name);
                // self.variable_declaration(variable_key);

                if !self.match_token(scan::TokenKind::Comma) { break }
            }
        }

        self.consume(scan::TokenKind::RightParen, "Expect ')' after end of lambda parameters.");

        let return_type_name: Option<String> = {
            // Like with variables, if the type is defined here, fill out the return type of
            // the function at this point. Otherwise, infer the type during type checking.
            // (Again, I see a lot of problems potentially popping up regarding type inference.)
            if self.match_token(scan::TokenKind::Colon) {
                if !self.match_token(scan::TokenKind::Identifier) {
                    return self.error_at("Expected type identifier.", &self.parser.current.clone());
                }
                Some(self.parser.previous.clone().value)
            } else {
                None
            }
        };

        self.consume(scan::TokenKind::LeftBracket, "Expect token '{' after function definition.");

        let count = {
            let mut count = 0;
            while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
                self.declaration();
                count += 1;
            }

            // This does not denote if we return a value or not. Fix it!
            let has_value = !matches!(self.parser.previous.kind, scan::TokenKind::Semicolon);

            if has_value && count > 0 { count - 1 }
            else                      { count }
        };

        self.consume(scan::TokenKind::RightBracket, "Expect '}' at the end of a block expression.");

        // TODO: I think I should just pass on the token here instead of having
        // the name in a string like a dummy.
        let return_type_name = if let Some(type_name) = return_type_name { type_name.clone() }
                               else                                      { "unit".to_string() };

        let code = self.end_function();
        todo!()
    }

    fn function_decl(&mut self) -> Stmt
    {
        self.consume(scan::TokenKind::Identifier, "Cannot declare function without name.");

        let function_token  = self.parser.previous.clone();
        let function_token2 = function_token.clone();
        let function_name   = function_token.value.clone();

        if self.variable_exists(&function_name) {
            return Stmt::Expr {
                expr: Box::new(self.error_at(&format!("Cannot redeclare function with name '{}'", function_name), &function_token))
            }
        }

        // Compile the expression and then jump after the block
        // to avoid executing the code during function _declaration_.
        let variable_key = self.declare_variable(function_token, false, None);
        self.variable_declaration(variable_key);

        let function = self.function(&function_name);

        self.variable_definition(variable_key);

        Stmt::Declaration {
            name: function_token2,
            initializer: todo!(),
        }
    }

    fn declare_variable(&mut self, token: scan::Token, mutable: bool, type_name: Option<String>) -> u8
    {
        let variable_name = token.value.clone();

        let variable = Variable {
            name: token,
            mutable,
            defined: true,
            type_name,
        };

        // TODO: maybe have the insert return the index.
        self.current_scope_mut().variables.push(variable);
        let variable_index = self
            .current_scope()
            .variables
            .iter()
            .position(|v| v.name.value == variable_name)
            .unwrap();
        // I guess unwrap is ok here. I literally just inserted the thing in the line above.

        variable_index as u8
    }

    fn find_variable_by_name(&self, name: &str) -> Option<(Option<usize>, usize, usize)>
    {
        let current_scope = self.current_scope();

        let variable_index = current_scope
            .variables
            .iter()
            .position(|v| v.name.value == name);

        if let Some(position) = variable_index {
            return Some((self.current_function, self.current_scope_index, position));
        }

        for scope_index in &current_scope.path {
            let scope = &self.program.scopes[*scope_index];
            let variable_index = scope
                .variables
                .iter()
                .position(|v| v.name.value == name);

            if let Some(position) = variable_index {
                return Some((scope.parent_function, *scope_index, position))
            }
        }

        None
    }

    fn parse_variable(&mut self) -> Option<u8>
    {
        let previous = self.parser.previous.value.clone();
        let (function_index, scope, index) = self.find_variable_by_name(&previous)?;

        let scope_distance = self.function_distance(self.current_function, function_index);

        // If the following token is '=', then it's an assignment.
        if self.match_token(scan::TokenKind::Equal) {
            if scope_distance == 0 {
                if !self.program.scopes[scope].variables[index].mutable {
                    self.parser
                        .error_at("Cannot reassign value of an immutable variable.", &self.parser.previous.clone());
                }

                self.expression();
                // self.emit_byte(block::Op::SetLocal);
            } else {
                // Emit program distance -> vm will move frames by this distance.
                // self.emit_byte(block::Op::SetUpvalue);
                // self.emit(scope_distance as u8);
            }
        } else if scope_distance == 0 {
            // self.emit_byte(block::Op::GetLocal);
        } else {
            // Emit program distance -> vm will move frames by this distance.
            // self.emit_byte(block::Op::GetUpvalue);
            // self.emit(scope_distance as u8);
        }

        // self.emit(index as u8);
        Some(index as u8)
    }

    fn variable_exists(&self, name: &str) -> bool
    {
        let current_scope = self.current_scope();

        let variable_index = current_scope
            .variables
            .iter()
            .position(|v| v.name.value == name);

        if variable_index.is_some() { return true }

        for scope_index in &current_scope.path {
            let scope = &self.program.scopes[*scope_index];
            let variable_index = scope
                .variables
                .iter()
                .position(|v| v.name.value == name);

            if variable_index.is_some() { return true }
        }

        false
    }

    fn variable_declaration(&mut self, variable_key: u8)
    {
        self.current_scope_mut()
            .variables
            .get_mut(variable_key as usize)
            .unwrap()
            .defined = false;

        // self.emit_byte(block::Op::DeclareVariable);
        // self.emit(variable_key);
        // self.emit(self.current_scope_index as u8);
    }

    fn variable_definition(&mut self, variable_key: u8)
    {
        self.current_scope_mut()
            .variables
            .get_mut(variable_key as usize)
            .unwrap()
            .defined = true;

        // self.emit_byte(block::Op::SetLocal);
        // self.emit(variable_key);
    }

    fn _if(&mut self) -> Expr
    {
        let condition = self.expression();

        let then_branch = self.declaration();

        let else_branch = if self.match_token(scan::TokenKind::Else) { Some(self.declaration()) }
                          else                                       { None };

        Expr::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        }
    }

    fn _while(&mut self) -> Stmt
    {
        let condition = self.expression();

        // Body
        self.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        let mut body = vec![];

        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            let stmt = self.declaration();
            body.push(Box::new(stmt));
        }

        self.match_token(scan::TokenKind::RightBracket);

        Stmt::While {
            condition: Box::new(condition),
            body,
        }
    }

    // In this implementation, all the parts of a for
    // loop declaration are required. While and iterators (when I get to that)
    // will make up for everything.
    fn _for(&mut self) -> Stmt
    {
        // loop variable

        // TODO: think about this if condition.
        // let variable_expr = if self.match_token(scan::TokenKind::Identifier) { self.variable_statement(); }
        //                     else                                             { self.expression() };

        // TODO: no unwrap
        let variable = self.variable_statement().unwrap();

        // end loop variables

        // condition

        let condition_expr = self.expression();

        // end condition

        // advancement statement

        let advancement_expr = self.expression();

        // end advancement statement

        // body

        self.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        let mut body: Vec<Stmt> = vec![variable];

        // Compile code until the end of the block or the end of the program is reached.
        let mut count = 0;
        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            body.push(self.declaration());
            count += 1;
        }

        body.push(Stmt::Expr { expr: Box::new(advancement_expr) });

        self.match_token(scan::TokenKind::RightBracket);

        // end body

        Stmt::While {
            condition: Box::new(condition_expr),
            body: body.into_iter().map(Box::new).collect(),
        }
    }

    fn function_invocation(&mut self) -> Expr
    {
        let Some(token) = self.parser.peek(-2) else {
            return self
                .error_at("Failed to parse function - no function name found.", &self.parser.previous.clone());
        };

        let function_name_token = token.clone();
        let function_name       = function_name_token.value.clone();

        let mut arguments: Vec<Expr> = vec![];

        let mut arguments_count: usize = 0;
        if !self.parser.check_token(scan::TokenKind::RightParen) {
            loop {
                arguments_count += 1;
                arguments.push(self.expression());

                if !self.match_token(scan::TokenKind::Comma) { break }
            }
        }
        self.consume(scan::TokenKind::RightParen, "Expect ')' after function arguments.");

        let Some((function_index, _, index)) = self.find_variable_by_name(&function_name) else {
            return self
                .error_at(&format!("Failed to find function '{}'.", &function_name), &function_name_token.clone());
        };

        let scope_distance = self.function_distance(self.current_function, function_index);

        let block = if let Some(variable_function_index) = function_index {
            &self.program.functions[variable_function_index].block
        } else {
            &self.program.block
        };

        // Get the scope of the variable, then find it by name in the scopes constants.
        let function = block
            .constants
            .iter()
            .find(|c| match c {
                block::Value::Function { name, .. } => name == &function_name,
                _ => false
            });

        // This is stupid - if the function is indirected through a variable,
        // then this doesn't work. Because of that, we have no way of checking the
        // function arity in that case.
        let Some(block::Value::Function { name, arity, .. }) = function else {
            return self
                .error_at(&format!("Function with name '{}' not in scope", &function_name), &function_name_token.clone());
        };

        if arity != &arguments_count {
            let error_message = format!(
                "Number of passed arguments '{}' to function '{}' does not match function arity '{}'.",
                arguments_count,
                name,
                arity
            );

            return self.error_at(&error_message, &function_name_token.clone());
        }

        self.match_token(scan::TokenKind::Semicolon);

        Expr::Call {
            arguments: arguments.into_iter().map(Box::new).collect(),
        }
    }

    // TODO: So far, piping into a function is only supported with functions
    // with arity of 1.
    // Because of the different order of operations when using the pipe operator as opposed to
    // regular function invocation, I've had to change the way functions are being called.
    // Previously, the function was pulled back into the stack using Op::GetLocal/Op::GetUpvalue,
    // and after that the arguments were pulled onto the stack. Now, the Op::GetLocal/Op::GetUpvalue
    // functionality is baked into Op::Call. This way, the runtime operations regarding function calls
    // do not depend on that order of values on the stack, instead they reach into the exact stack
    // position to get the function value.
    // Again, this makes things way easier when working with different execution orders, such as
    // when piping things into functions (as arguments are parsed in a different order from regular function
    // calls, again again).
    fn pipe(&mut self) -> Expr
    {
        if self.parser.previous.kind.discriminant() != scan::TokenKind::Pipe.discriminant() {
            return self.error_at
            (
                &format!("Expected infix token '{}' found '{}'", "|>", self.parser.previous.value),
                &self.parser.previous.clone()
            );
        }

        let Some(function_name) = self.parser.peek(0) else {
            return self
                .error_at("Failed to parse function name - no function name found.", &self.parser.current.clone());
        };

        let Some((function_index, _, index)) = self.find_variable_by_name(&function_name.value) else {
            return self
                .error_at(&format!("Failed to find function '{}'.", &function_name.value), &function_name.clone());
        };

        let block = if let Some(variable_function_index) = function_index {
            &self.program.functions[variable_function_index].block
        } else {
            &self.program.block
        };

        // Get the scope of the variable, then find it by name in the scopes constants.
        let function = block
            .constants
            .iter()
            .find(|c| match c {
                block::Value::Function { name, .. } => name == &function_name.value,
                _ => false
            });

        let Some(block::Value::Function { arity, ..}) = function else {
            return self
                .error_at(&format!("Function with name '{}' not in scope", &function_name.value), &function_name.clone())
        };

        if arity != &1 {
            return self
                .error_at("Function must take only one argument to be able to work with pipes.", &function_name.clone());
        }

        self.match_token(scan::TokenKind::Semicolon);
        todo!()
    }

    fn begin_scope(&mut self) -> usize
    {
        let parent_scope = self.current_scope();

        // New scope path will contain the parent as well, so extending with the
        // index of the parent.
        let mut new_scope_path = Vec::with_capacity(parent_scope.path.len() + 1);
        new_scope_path.extend_from_slice(&parent_scope.path);
        new_scope_path.push(parent_scope.index);

        let new_scope = Scope {
            index: 0,
            path: new_scope_path,
            variables: vec![],
            scope_depth: parent_scope.scope_depth + 1,
            parent_function: self.current_function,
        };

        // Push the new scope and get its index. Use it as the ID of the Scope struct.
        self.program.scopes.push(new_scope);

        let new_scope_index = self.program.scopes.len() - 1;
        self.program.scopes[new_scope_index].index = new_scope_index;

        self.current_scope_index = new_scope_index;

        new_scope_index
    }

    fn end_scope(&mut self)
    {
        let scope                = self.current_scope();
        let parent_scope         = scope.path.last().unwrap();
        self.current_scope_index = *parent_scope;
    }

    fn begin_function(&mut self)
    {
        let scope_index = self.begin_scope();

        self.program.functions.push(Function::new(scope_index, self.current_function));
        self.current_function = Some(self.program.functions.len() - 1);
    }

    fn end_function(&mut self) -> block::Block
    {
        assert!(self.current_function.is_some());

        self.end_scope();

        let func         = self.current();
        let parent_index = func.parent_function_index;
        let block        = func.block.clone();

        self.current_function = parent_index;

        block
    }

    // TODO: not sure which version is correct, so I'm leaving this here until my
    // tiny brain gets going.
    // fn function_distance(&self, starting_index: Option<usize>, ending_index: Option<usize>) -> usize
    // {
    //     let mut distance = 0;
    //     let Some(starting_index) = starting_index else {
    //         return distance;
    //     };

    //     // TODO: this is a hack and a sign of the whole thing being a bit shit.
    //     if starting_index == 0 && ending_index.is_none() { return 0 }

    //     let mut current_function = &self.program.functions[starting_index];

    //     loop {
    //         let next = current_function.parent_function_index;

    //         if next.is_none() && ending_index.is_none()        { return distance + 1 }
    //         let Some(next_index) = next else                   { break };
    //         if ending_index.map_or(false, |e| e == next_index) { break };

    //         distance += 1;
    //         current_function = &self.program.functions[next_index];
    //     }

    //     distance
    // }

    // TODO: both of these are broken and do not take scopes (especially parameter scopes)
    // into account in any way.
    fn function_distance(&self, starting_index: Option<usize>, ending_index: Option<usize>) -> usize
    {
        let mut distance = 0;
        let Some(starting_index) = starting_index else {
            return 0
        };

        let mut current = &self.program.functions[starting_index];
        loop {
            let next = current.parent_function_index;
            let Some(next_index) = next else {
                break
            };

            distance += 1;

            if ending_index.map_or(false, |i| i == next_index) {
                return distance
            }

            current = &self.program.functions[next_index];
        }

        distance
    }

    fn semicolon(&mut self) -> Expr
    {
        self.match_token(scan::TokenKind::Semicolon);
        Expr::Literal { value: block::Value::Unit }
    }

    // This section basically implements the parser methods, the difference is that the
    // errors are pushed into the compiler error vec. This is so the caller doesn't need to
    // manually bother with this stuff all the time.
    fn match_token(&mut self, token_kind: scan::TokenKind) -> bool
    {
        if self.parser.current.kind.discriminant() != token_kind.discriminant() {
            return false
        }

        let _ = self.parser.advance().map_err(|e| self.error(e));
        true
    }

    fn consume(&mut self, token_kind: scan::TokenKind, error_message: &str)
    {
        let _ = self
            .parser
            .consume(token_kind, error_message)
            .map_err(|e| self.error(e));
    }

    //

    fn error_at(&mut self, msg: &str, token: &scan::Token) -> Expr
    {
        let err = self.parser.error_at(msg, token);
        self.errors.push(err);
        Expr::Bad { token: token.clone() }
    }

    fn error(&mut self, err: CompilerError)
    {
        self.errors.push(err)
    }
}
