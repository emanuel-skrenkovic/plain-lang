use std::fmt;
use crate::{ast, scan};


#[derive(Debug)]
pub enum CompilerErrorKind
{
    ParseError
}

#[derive(Debug)]
pub struct CompilerError
{
    pub line: usize,
    pub token_index: usize,
    pub source_line: String,
    pub token: String,
    pub msg: String,
    pub kind: CompilerErrorKind
}

impl fmt::Display for CompilerError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let token_len             = self.token.len();
        let token_underline_range = self.token_index..self.token_index + token_len + 1;
        let mut underline         = " ".repeat(self.source_line.len());

        underline.replace_range(token_underline_range, &"^".repeat(token_len));

        let line1 = format!("{line:<width$} {line_text}", line=format!("{}:", self.line), line_text=self.source_line, width=6);
        let line2 = format!("{line:<width$} {line_text}", line="", line_text=underline, width=6);

        write!(
            f,
            "Error: {}\n\n{}\n{}",
            self.msg,
            line1,
            line2,
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

type ParseFn = fn(&mut Compiler) -> ast::Expr;

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
        if matches!(self.current.kind, scan::TokenKind::End) {
            return Ok(())
        }

        if self.current_index >= self.tokens.len() {
            return Ok(())
        }

        std::mem::swap(&mut self.previous, &mut self.current);
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
            token_index: token.token_index,
            source_line: lines[token.line - 1].clone(),
            token: token.value.clone(),
            kind: CompilerErrorKind::ParseError,
        }
    }

    // TODO: horrible and I should be publicly shamed, doesn't even work oh my gosh how embarrassing oh my
    fn peek(&self, diff: i32) -> Option<&scan::Token>
    {
        let index = self.current_index - 1;
        let index = index as i32 + diff;

        if index < 0 || index > self.tokens.len() as i32 {
            return None
        }

        let index = index as usize;

        Some(&self.tokens[index])
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

    pub scopes: Vec<usize>,
    pub scope_depth: usize,
}

impl Function
{
    pub fn new(entry_scope_index: usize, parent_function_index: Option<usize>) -> Self
    {
        Self {
            parent_function_index,
            scope_depth: 0,
            scopes: vec![entry_scope_index],
        }
    }
}

// I feel like I have to simulate relational data with functions and scopes.

#[derive(Clone, Debug)]
pub struct Program
{
    pub scopes: Vec<Scope>,
    pub functions: Vec<Function>,
}

impl Default for Program
{
    fn default() -> Self
    {
        Self {
            scopes: vec!
            [
                Scope {
                    index: 0,
                    path: Vec::with_capacity(1024),
                    variables: Vec::with_capacity(1024),
                    scope_depth: 0,
                    parent_function: None,
                }
            ],

            functions: Vec::with_capacity(1024),
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

    stack: Vec<ast::Expr>,

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

    pub fn compile(mut self, tokens: Vec<scan::Token>) -> Result<Vec<ast::Node>, Vec<CompilerError>>
    {
        self.parser = Parser::new(self.source.clone(), tokens);
        let _ = self.parser.advance().map_err(|e| self.error(e));

        // TODO: should probably enclose program itself.

        let mut nodes: Vec<ast::Node> = Vec::with_capacity(1024 * 8);

        loop {
            match self.parser.current.kind {
                scan::TokenKind::End => break,
                _ => {
                    let decl = self.declaration();
                    nodes.push(ast::Node::Stmt(decl));
                }
            }
        }

        if self.parser.panic {
            return Err(self.errors)
        }

        Ok(nodes)
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

    fn expression_statement(&mut self) -> ast::Stmt
    {
        ast::Stmt::Expr {
            expr: Box::new(ast::ExprInfo::new(self.expression())),
        }
    }

    fn expression(&mut self) -> ast::Expr
    {
        self.parse_precedence(Precedence::Assignment);
        // TODO
        self.stack.pop().unwrap()
    }

    fn block_expression(&mut self) -> ast::Expr
    {
        let (statements, value) = self.block();
        let value = ast::ExprInfo::new(value);
        let value = Box::new(value);
        ast::Expr::Block { statements, value }
    }

    fn binary(&mut self) -> ast::Expr
    {
        let operator   = self.parser.previous.clone();
        let parse_rule = get_rule(operator.kind);

        let left = self.stack.pop().unwrap();

        self.parse_precedence(
            Precedence::try_from(parse_rule.precedence.discriminator() + 1)
                .unwrap()
        );

        let right = self.stack.pop().unwrap();

        let left  = ast::ExprInfo::new(left);
        let right = ast::ExprInfo::new(right);

        let expr = ast::Expr::Binary {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        };

        self.match_token(scan::TokenKind::Semicolon);
        expr
    }

    fn literal(&mut self) -> ast::Expr
    {
        let expr = ast::Expr::Literal { value: self.parser.previous.clone() };

        // Eat the semicolon only if present;
        self.match_token(scan::TokenKind::Semicolon);
        expr
    }

    fn declaration(&mut self) -> ast::Stmt
    {
        match self.parser.current.kind {
            scan::TokenKind::While => {
                let _ = self.parser.advance().map_err(|e| self.error(e));
                self._while()
            },
            scan::TokenKind::For => {
                let _ = self.parser.advance().map_err(|e| self.error(e));
                self._for()
            }
            scan::TokenKind::Identifier => {
                self.declaration_statement().unwrap_or_else(|| self.expression_statement())
            }
            _ => self.expression_statement(),
        }
    }

    fn declaration_statement(&mut self) -> Option<ast::Stmt>
    {
        let next = self.parser.peek(1)?;

        let next_kind = next.kind.discriminant();

        let type_definition = next_kind == scan::TokenKind::Colon.discriminant();
        let immutable       = next_kind == scan::TokenKind::ColonColon.discriminant();
        let mutable         = next_kind == scan::TokenKind::ColonEquals.discriminant();

        if !(type_definition || immutable || mutable) {
            return None
        }

        let variable_token = self.parser.current.clone();
        let variable_name  = variable_token.value.clone();

        let _ = self.parser.advance().map_err(|e| self.error(e));

        let mut maybe_type_name: Option<String> = None;
        if type_definition {
            let _ = self.match_token(scan::TokenKind::Colon);
            self.consume(scan::TokenKind::Identifier, "Expected identifier");

            maybe_type_name = Some(self.parser.previous.clone().value);

            if !self.match_token(scan::TokenKind::Equal) {
                let error = self.error_at("Expected token '='.", &self.parser.current.clone());
                let error = ast::ExprInfo::new(error);
                return Some(ast::Stmt::Expr { expr: Box::new(error) })
            }
        } else if mutable || immutable {
            let mutable = if mutable { scan::TokenKind::ColonEquals } else { scan::TokenKind::ColonColon };
            self.consume(mutable, "Expected ':=' or '::' after identifier.");
        } else {
            return None
        }

        // Function declaration.
        if self.parser.current.kind.discriminant() == scan::TokenKind::LeftParen.discriminant() && self.current_scope_index == 0 {
            // Cannot use match here because this is only for global scope - we could
            // unintentionally advance.
            let _ = self.parser.advance().map_err(|e| self.error(e));

            // Declare self first to allow recursion.
            let index = self.declare_variable(variable_token.clone(), false, maybe_type_name);

            let (params, param_types, body) = self.function();

            self.variable_declaration(index);
            self.variable_definition(index);

            return Some(ast::Stmt::Function { name: variable_token, params, param_types, body })
        }

        if self.variable_exists(&variable_name) {
            // even more #horribleways
            let error = self.error_at
            (
                &format!("Cannot redeclare variable with name '{}'.", &variable_token.value),
                &variable_token.clone()
            );
            let error = ast::ExprInfo::new(error);
            return Some(ast::Stmt::Expr { expr: Box::new(error) })
        }

        let initializer = self.expression();

        let index = self.declare_variable(variable_token.clone(), mutable, maybe_type_name);
        self.variable_declaration(index);
        self.variable_definition(index);

        self.match_token(scan::TokenKind::Semicolon);

        let initializer = Box::new(ast::ExprInfo::new(initializer));
        let stmt = if mutable { ast::Stmt::Var { name: variable_token, initializer } }
                   else       { ast::Stmt::Const { name: variable_token, initializer } };

        Some(stmt)
    }

    fn variable_statement(&mut self) -> Option<ast::Stmt>
    {
        let next = self.parser.peek(1)?;

        let next_kind = next.kind.discriminant();

        let type_definition = next_kind == scan::TokenKind::Colon.discriminant();
        let immutable       = next_kind == scan::TokenKind::ColonColon.discriminant();
        let mutable         = next_kind == scan::TokenKind::ColonEquals.discriminant();

        if !(type_definition || immutable || mutable) {
            return None
        }

        let variable_token = self.parser.current.clone();
        let variable_name  = variable_token.value.clone();

        let _ = self.parser.advance().map_err(|e| self.error(e));

        let mut maybe_type_name: Option<String> = None;
        if type_definition {
            let _ = self.match_token(scan::TokenKind::Colon);
            self.consume(scan::TokenKind::Identifier, "Expected identifier");

            maybe_type_name = Some(self.parser.previous.clone().value);

            if !self.match_token(scan::TokenKind::Equal) {
                let error = self.error_at("Expected token '='.", &self.parser.current.clone());
                let error = ast::ExprInfo::new(error);
                return Some(ast::Stmt::Expr { expr: Box::new(error) })
            }
        } else if mutable || immutable {
            self.match_token(
                if mutable { scan::TokenKind::ColonEquals } else { scan::TokenKind::ColonColon }
            );
        } else {
            return None
        }

        if self.variable_exists(&variable_name) {
            // even more #horribleways
            let error = self.error_at
            (
                &format!("Cannot redeclare variable with name '{}'.", &variable_token),
                &variable_token.clone()
            );
            let error = ast::ExprInfo::new(error);
            return Some(ast::Stmt::Expr { expr: Box::new(error) })
        }

        let initializer = self.expression();
        let initializer = ast::ExprInfo::new(initializer);

        let index = self.declare_variable(variable_token.clone(), mutable, maybe_type_name);
        self.variable_declaration(index);
        self.variable_definition(index);

        self.match_token(scan::TokenKind::Semicolon);
        let stmt = if mutable {
            ast::Stmt::Var {
                name: variable_token,
                initializer: Box::new(initializer),
            }
        } else {
            ast::Stmt::Const {
                name: variable_token,
                initializer: Box::new(initializer),
            }
        };

        Some(stmt)
    }

    // If the variable type is specified, then assign the variable type here.
    // Otherwise, infer the type (if possible) from the value assigned to the variable
    // during type checking.
    // I can already see problems forming with this inference system. sadface
    fn variable(&mut self) -> ast::Expr
    {
        let name = self.parser.previous.clone();

        if name.value != "printf" && self.parse_variable().is_none() {
            return self
                .error_at(&format!("Variable '{}' is not declared.", &self.parser.previous.value), &self.parser.previous.clone());
        }

        // If the next token is equal, handle assignment expression.
        if self.parser.previous.kind.discriminant() == scan::TokenKind::Equal.discriminant() {
            let value_expr = self.expression();
            let value_expr = ast::ExprInfo::new(value_expr);
            return ast::Expr::Assignment {
                name,
                value: Box::new(value_expr),
            }
        }

        // Handles variable expression here.
        self.match_token(scan::TokenKind::Semicolon);
        ast::Expr::Variable { name }
    }

    fn function_expression(&mut self) -> ast::Expr
    {
        let (params, param_types, body) = self.function();
        ast::Expr::Function { params, param_types, body }
    }

    fn function(&mut self) -> (Vec<scan::Token>, Vec<scan::Token>, Vec<Box<ast::Stmt>>)
    {
        self.begin_function();

        // a :: ()
        // ^ __
        // 3 21
        // let function_token = self.parser.peek(-3).unwrap().clone();
        // let function_name = function_token.value.clone();

        let mut argument_type_names = Vec::with_capacity(512);
        let mut params              = Vec::with_capacity(512);

        if !self.parser.check_token(scan::TokenKind::RightParen) {
            loop {
                self.consume(scan::TokenKind::Identifier, "Expect parameter identifier after '('.");

                let parameter_name_token = self.parser.previous.clone();
                params.push(parameter_name_token.clone());

                self.consume(scan::TokenKind::Colon, "Expect type definition");
                self.consume(scan::TokenKind::Identifier, "Expected type identifier");

                let type_name = &self.parser.previous;
                argument_type_names.push(type_name.clone());

                // Ooopsy whoopsy dooopsy hooopsy we just made it mutable.
                self.declare_variable(parameter_name_token, true, Some(type_name.value.clone()));
                // self.variable_declaration(variable_key);

                if !self.match_token(scan::TokenKind::Comma) { break }
            }
        }

        self.consume(scan::TokenKind::RightParen, "Expect ')' after end of lambda parameters.");

        // let return_type_name: Option<String> = {
        //     // Like with variables, if the type is defined here, fill out the return type of
        //     // the function at this point. Otherwise, infer the type during type checking.
        //     // (Again, I see a lot of problems potentially popping up regarding type inference.)
        //     if self.match_token(scan::TokenKind::Colon) {
        //         if !self.match_token(scan::TokenKind::Identifier) {
        //             return self.error_at("Expected type identifier.", &self.parser.current.clone());
        //         }
        //         Some(self.parser.previous.clone().value)
        //     } else {
        //         None
        //     }
        // };

        self.consume(scan::TokenKind::LeftBracket, "Expect token '{' after function definition.");

        let mut body = Vec::with_capacity(512);

        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            let stmt = self.declaration();
            body.push(Box::new(stmt));
        }

        // This does not denote if we return a value or not. Fix it!
        // let has_value = !matches!(self.parser.previous.kind, scan::TokenKind::Semicolon);

        self.consume(scan::TokenKind::RightBracket, "Expect '}' at the end of a block expression.");

        // TODO: I think I should just pass on the token here instead of having
        // the name in a string like a dummy.
        // let return_type_name = if let Some(type_name) = return_type_name { type_name.clone() }
        //                        else                                      { "unit".to_string() };

        let _ = self.end_function();

        (params, argument_type_names, body)
    }

    fn block(&mut self) -> (Vec<Box<ast::Stmt>>, ast::Expr)
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
        let has_value = !matches!(self.parser.previous.kind, scan::TokenKind::Semicolon);

        self.consume(scan::TokenKind::RightBracket, "Expect '}' at the end of a block expression.");
        self.match_token(scan::TokenKind::Semicolon);

        // TODO: need to take tokens into account here:
        // if the last expression in the block ends with a semicolon,
        // it is of Unit value;
        let expr = if has_value {
            let binding = statements.pop().unwrap();
            match binding.as_ref() {
                ast::Stmt::Expr { expr } => expr.to_owned().value,
                _                        => panic!(),
            }
        } else {
            ast::Expr::Literal { value: scan::Token::default() }
        };

        self.end_scope();
        (statements, expr)
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
        let previous                       = self.parser.previous.value.clone();
        let (function_index, scope, index) = self.find_variable_by_name(&previous)?;

        let scope_distance = self.function_distance(self.current_function, function_index);

        // If the following token is '=', then it's an assignment.
        if self.match_token(scan::TokenKind::Equal) && scope_distance == 0  {
            // If it is an assignment, and the variable is immutable -> compilation error.
            if !self.program.scopes[scope].variables[index].mutable {
                self.parser
                    .error_at("Cannot reassign value of an immutable variable.", &self.parser.previous.clone());
            }
        }

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
    }

    fn variable_definition(&mut self, variable_key: u8)
    {
        self.current_scope_mut()
            .variables
            .get_mut(variable_key as usize)
            .unwrap()
            .defined = true;
    }

    fn _if(&mut self) -> ast::Expr
    {
        let condition = self.expression();

        self.consume(scan::TokenKind::LeftBracket, "Expect '{");

        let (then_branch, then_value) = self.block();

        // This looks ugly. :(
        let (else_branch, else_value) = if self.match_token(scan::TokenKind::Else) {
            self.consume(scan::TokenKind::LeftBracket, "Expect '{");

            let (branch, value) = self.block();
            (branch, value)
        } else {
            (vec![], ast::Expr::Literal { value: scan::Token::default() })
        };

        let condition = ast::ExprInfo::new(condition);
        let then_value = Box::new(ast::ExprInfo::new(then_value));
        let else_value = Box::new(ast::ExprInfo::new(else_value));

        ast::Expr::If {
            condition: Box::new(condition),
            then_branch,
            then_value,
            else_branch,
            else_value,
        }
    }

    fn _while(&mut self) -> ast::Stmt
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

        let condition = ast::ExprInfo::new(condition);

        ast::Stmt::While {
            condition: Box::new(condition),
            body,
        }
    }

    // In this implementation, all the parts of a for
    // loop declaration are required. While and iterators (when I get to that)
    // will make up for everything.
    fn _for(&mut self) -> ast::Stmt
    {
        // TODO: no unwrap
        let variable       = self.variable_statement().unwrap();
        let condition_expr = self.expression();
        // TODO: needs to be just unary statement.
        let advancement    = self.declaration();

        self.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        let mut body: Vec<Box<ast::Stmt>> = vec![];

        // Compile code until the end of the block or the end of the program is reached.
        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            body.push(Box::new(self.declaration()));
        }

        self.consume(scan::TokenKind::RightBracket, "Expect '}' after the 'for' block.");

        // end body

        let condition_expr = ast::ExprInfo::new(condition_expr);

        ast::Stmt::For {
            initializer: Box::new(variable),
            condition: Box::new(condition_expr),
            advancement: Box::new(advancement),
            body,
        }
    }

    fn function_invocation(&mut self) -> ast::Expr
    {
        let Some(token) = self.parser.peek(-2) else {
            return self
                .error_at("Failed to parse function - no function name found.", &self.parser.previous.clone());
        };

        let function_name_token = token.clone();
        let function_name       = function_name_token.value.clone();

        let mut arguments: Vec<Box<ast::ExprInfo>> = vec![];

        if !self.parser.check_token(scan::TokenKind::RightParen) {
            loop {
                let expr = self.expression();
                let expr = ast::ExprInfo::new(expr);
                let expr = Box::new(expr);
                arguments.push(expr);

                if !self.match_token(scan::TokenKind::Comma) {
                    break
                }
            }
        }
        self.consume(scan::TokenKind::RightParen, "Expect ')' after function arguments.");

        if function_name != "printf" {
            let Some(_) = self.find_variable_by_name(&function_name) else {
                return self
                    .error_at(&format!("Failed to find function '{}'.", &function_name), &function_name_token.clone());
            };
        }

        self.match_token(scan::TokenKind::Semicolon);

        ast::Expr::Call { name: function_name_token, arguments }
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
    fn pipe(&mut self) -> ast::Expr
    {
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

    fn end_function(&mut self)
    {
        assert!(self.current_function.is_some());

        self.end_scope();

        let func         = self.current();
        let parent_index = func.parent_function_index;

        self.current_function = parent_index;
    }

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

    fn semicolon(&mut self) -> ast::Expr
    {
        self.match_token(scan::TokenKind::Semicolon);
        ast::Expr::Literal { value: scan::Token::default() }
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

    fn error_at(&mut self, msg: &str, token: &scan::Token) -> ast::Expr
    {
        let err = self.parser.error_at(msg, token);
        self.errors.push(err);
        ast::Expr::Bad { token: token.clone() }
    }

    fn error(&mut self, err: CompilerError)
    {
        self.errors.push(err)
    }
}
