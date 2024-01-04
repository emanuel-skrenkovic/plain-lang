use std::fmt;

use crate::block;
use crate::scan;

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

type ParseFn = fn(&mut Compiler);

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
    ParseRule { prefix: None,                                infix: Some(Compiler::left_angle),          precedence: Precedence::Comparison }, // LeftAngle
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
            scanned_tokens: vec![],
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

        loop {
            if matches!(self.current.kind, scan::TokenKind::End) {
                break
            }

            if self.current_index >= self.tokens.len() {
                break
            }

            self.current = self.tokens[self.current_index].clone();

            if matches!(self.current.kind, scan::TokenKind::End) {
                break
            }

            self.current_index += 1;
            self.scanned_tokens.push(self.current.clone());

            if !matches!(self.current.kind, scan::TokenKind::Error) {
                break;
            }

            return Err(self.error_at("Scanner error.", &self.current.clone()))
        }

        Ok(())
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

    // TODO: horrible and I should be publicly shamed
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

            errors: vec![],
        }
    }

    pub fn compile(mut self, tokens: Vec<scan::Token>) -> Result<Program, Vec<CompilerError>>
    {
        self.parser = Parser::new(self.source.clone(), tokens);
        let _ = self.parser.advance().map_err(|e| self.error(e));

        // TODO: should probably enclose program itself.

        loop {
            match self.parser.current.kind {
                scan::TokenKind::End => break,
                _                    => self.declaration()
            }
        }

        if self.parser.panic {
            return Err(self.errors)
        }

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
            Some(prefix) => prefix(self),
            _ => {
                if self.is_at_end() { return }

                self.parser.error_at("Expect expression.", &self.parser.current.clone());
                panic!()
            }
        }

        while prec.discriminator() <= get_rule(self.parser.current.kind).precedence.discriminator() {
            let _ = self.parser.advance().map_err(|e| self.error(e));

            let Some(infix_rule) = get_rule(self.parser.previous.kind).infix else {
                return self.error_at("Failed to get infix rule.", &self.parser.previous.clone());
            };

            infix_rule(self);
        }
    }

    fn expression_statement(&mut self)
    {
        self.expression();
    }

    fn expression(&mut self)
    {
        self.parse_precedence(Precedence::Assignment);
    }

    fn block_expression(&mut self)
    {
        self.begin_scope();

        self.code_block();
        self.match_token(scan::TokenKind::Semicolon);

        self.end_scope();
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

    fn binary(&mut self)
    {
        let operator   = self.parser.previous.kind;
        let parse_rule = get_rule(operator);

        self.parse_precedence(
            Precedence::try_from(parse_rule.precedence.discriminator() + 1)
                .unwrap()
        );

        match operator {
            scan::TokenKind::LeftAngle    => { self.emit_byte(block::Op::Less); }
            scan::TokenKind::RightAngle   => { self.emit_byte(block::Op::Greater); }
            scan::TokenKind::BangEqual    => { self.emit_bytes(block::Op::Equal, block::Op::Not); }
            scan::TokenKind::EqualEqual   => { self.emit_byte(block::Op::Equal); }
            scan::TokenKind::GreaterEqual => { self.emit_byte(block::Op::GreaterEqual); }
            scan::TokenKind::LessEqual    => { self.emit_byte(block::Op::LessEqual); }
            scan::TokenKind::Plus         => { self.emit_byte(block::Op::Add); }
            scan::TokenKind::Minus        => { self.emit_byte(block::Op::Subtract); }
            scan::TokenKind::Star         => { self.emit_byte(block::Op::Multiply); }
            scan::TokenKind::Slash        => { self.emit_byte(block::Op::Divide); }
            scan::TokenKind::Pipe         => { self.pipe(); },
            _ => ()
        };

        self.match_token(scan::TokenKind::Semicolon);
    }

    fn left_angle(&mut self)
    {
        // TODO generics
        if self.match_token(scan::TokenKind::Identifier) { self.match_token(scan::TokenKind::RightAngle); }
        else                                             { self.binary(); }
    }

    fn literal(&mut self)
    {
        match self.parser.previous.kind {
            scan::TokenKind::True  => self.emit_constant(block::Value::Bool { val: true }),
            scan::TokenKind::False => self.emit_constant(block::Value::Bool { val: false }),
            _ => {
                // Determine the type of the literal and create a matching value.
                let t = self.parser.previous.value.clone();

                // I don't like if else-if chains. >:(
                if t.starts_with("\"") {
                    // Removing the first and last chars because the token value contains the
                    // starting and ending quotes.
                    let val = t.clone()[1..t.len()-1].to_string();
                    self.emit_constant(block::Value::String { val });
                } else if let Some(first) = t.chars().nth(0) {
                    // This is incorrect - should be part of the outer if-else conditions.
                    if char::is_numeric(first) {
                        let Ok(val) = t.parse::<i32>() else {
                            return self.error_at("Expected number.", &self.parser.previous.clone());
                        };

                        self.emit_constant(block::Value::Number { val });
                    }
                } else {
                    return self.error_at("Failed to parse token as a literal.", &self.parser.previous.clone());
                }
            }
        }

        // Eat the semicolon only if present;
        self.match_token(scan::TokenKind::Semicolon);
    }

    fn declaration(&mut self)
    {
        match self.parser.current.kind {
            scan::TokenKind::Func => {
                let _ = self.parser.advance().map_err(|e| self.error(e));
                self.function_decl();
            }
            scan::TokenKind::While => {
                let _ = self.parser.advance().map_err(|e| self.error(e));
                self._while();
            },
            scan::TokenKind::For => {
                let _ = self.parser.advance().map_err(|e| self.error(e));
                self._for();
            }
            _ => self.expression_statement(),
        }
    }

    // If the variable type is specified, then assign the variable type here.
    // Otherwise, infer the type (if possible) from the value assigned to the variable
    // during type checking.
    // I can already see problems forming with this inferrence system. sadface
    fn variable(&mut self)
    {
        let variable_token = self.parser.previous.clone();
        let variable_name  = variable_token.value.clone();

        let mut maybe_type_name: Option<String> = None;

        let type_definition = self.parser.check_token(scan::TokenKind::Colon);
        let immutable       = self.parser.check_token(scan::TokenKind::ColonColon);
        let mutable         = self.parser.check_token(scan::TokenKind::ColonEquals);

        // We look at the next token to check if the variable is being declared,
        // or if it is being used.
        // If the next token is one of Token::Colon, Token::ColonColon or Token::ColonEquals,
        // then the variable is being declared.
        // Examples with completely random values:
        // a : number = 42069;
        // a :: 42069;
        // a := 42069;
        let variable_decl = type_definition || immutable || mutable;

        if type_definition {
            let _ = self.match_token(scan::TokenKind::Colon);
            self.consume(scan::TokenKind::Identifier, "Expected identifier");

            maybe_type_name = Some(self.parser.previous.clone().value);

            if !self.match_token(scan::TokenKind::Equal) {
                return self.error_at("Expected token '='.", &self.parser.current.clone());
            }
        } else if mutable || immutable {
            self.match_token(
                if mutable { scan::TokenKind::ColonEquals } else { scan::TokenKind::ColonColon }
            );
        }

        if variable_decl {
            if self.variable_exists(&variable_name) {
                return self.error_at
                (
                    &format!("Cannot redeclare variable with name '{}'.", &variable_token),
                    &variable_token.clone()
                );
            }

            self.expression();

            let index = self.declare_variable(variable_token, mutable, maybe_type_name);
            self.variable_declaration(index);
            self.variable_definition(index);

            self.match_token(scan::TokenKind::Semicolon);
            return
        }

        // This handles the case where the variable is used as an expression as opposed to
        // being defined (which the code above handles).
        if let Some(next) = self.parser.peek(0) {
            if next.kind.discriminant() == scan::TokenKind::LeftParen.discriminant() {
                // This is a function call, do nothing in this case, 'call' will handle it.
                self.match_token(scan::TokenKind::Semicolon);
                return
            }
        }

        if self.parse_variable().is_none() {
            return self
                .error_at(&format!("Variable '{}' is not declared.", &self.parser.previous.value), &self.parser.previous.clone());
        }

        self.match_token(scan::TokenKind::Semicolon);
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

        // TODO: think about semantics.
        // If the function returns nothing, return Unit instead.
        if !has_value { self.emit_constant(block::Value::Unit); }

        self.emit_return(count + arity);

        let function_code = self.end_function();
        let function = block::Value::Function {
            name: name.to_owned(),
            arity,
            closure: block::Closure { code: function_code },

            // TODO: type
            argument_type_names: vec![],
            return_type_name: "TODO: FIXME".to_string(),
        };

        function
    }

    fn function_expression(&mut self)
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
                self.declare_variable(parameter_name_token, false, maybe_type_name);

                if !self.match_token(scan::TokenKind::Comma) { break }
            }
        }

        self.consume(scan::TokenKind::RightParen, "Expect ')' after end of lambda parameters.");

        let mut return_type_name: Option<String> = None;

        // Like with variables, if the type is defined here, fill out the return type of
        // the function at this point. Otherwise, infer the type during type checking.
        // (Again, I see a lot of problems potentially popping up regarding type inference.)
        if self.match_token(scan::TokenKind::Colon) {
            if !self.match_token(scan::TokenKind::Identifier) {
                return self.error_at("Expected type identifier.", &self.parser.current.clone());
            }

            return_type_name = Some(self.parser.previous.clone().value);
        }

        self.consume(scan::TokenKind::LeftBracket, "Expect token '{' after function definition.");

        // TODO: should this be an expression?
        // self.expression();
        self.code_block();
        self.emit_return(arity);

        // TODO: I think I should just pass on the token here instead of having
        // the name in a string like a dummy.
        let return_type_name = if let Some(type_name) = return_type_name { type_name.clone() }
                               else                                      { "unit".to_string() };

        let expression_block = self.end_function();

        self.emit_constant
        (
            block::Value::Function {
                name: function_name,
                arity,
                closure: block::Closure { code: expression_block },
                argument_type_names,
                return_type_name,
            }
        );
    }

    fn function_decl(&mut self)
    {
        self.consume(scan::TokenKind::Identifier, "Cannot declare function without name.");

        let function_token = self.parser.previous.clone();
        let function_name  = function_token.value.clone();

        if self.variable_exists(&function_name) {
            return self
                .error_at(&format!("Cannot redeclare function with name '{}'", function_name), &function_token);
        }

        // Compile the expression and then jump after the block
        // to avoid executing the code during function _declaration_.
        let variable_key = self.declare_variable(function_token, false, None);
        self.variable_declaration(variable_key);

        let function = self.function(&function_name);
        self.emit_constant(function);

        self.variable_definition(variable_key);
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
            .position(|v| &v.name.value == name);

        if let Some(position) = variable_index {
            return Some((self.current_function, 0, position));
        }

        for scope_index in &current_scope.path {
            let scope = &self.program.scopes[*scope_index];
            let variable_index = scope
                .variables
                .iter()
                .position(|v| &v.name.value == name);

            if let Some(position) = variable_index {
                return Some((scope.parent_function, *scope_index, position))
            }
        }

        None
    }

    fn parse_variable(&mut self) -> Option<u8>
    {
        let previous = self.parser.previous.value.clone();
        let Some((function_index, scope, index)) = self.find_variable_by_name(&previous) else {
            return None
        };

        let scope_distance = self.function_distance(self.current_function, function_index);

        // If the following token is '=', then it's an assignment.
        if self.match_token(scan::TokenKind::Equal) {
            if scope_distance == 0 {
                if !self.program.scopes[scope].variables[index].mutable {
                    self.parser
                        .error_at("Cannot reassign value of an immutable variable.", &self.parser.previous.clone());
                }

                self.expression();
                self.emit_byte(block::Op::SetLocal);
            } else {
                // Emit program distance -> vm will move frames by this distance.
                self.emit_byte(block::Op::SetUpvalue);
                self.emit(scope_distance as u8);
            }
        } else {
            if scope_distance == 0 {
                self.emit_byte(block::Op::GetLocal);
            } else {
                // Emit program distance -> vm will move frames by this distance.
                self.emit_byte(block::Op::GetUpvalue);
                self.emit(scope_distance as u8);
            }
        }

        self.emit(index as u8);
        Some(index as u8)
    }

    fn variable_exists(&self, name: &str) -> bool
    {
        let current_scope = self.current_scope();

        let variable_index = current_scope
            .variables
            .iter()
            .position(|v| &v.name.value == name);

        if variable_index.is_some() { return true }

        for scope_index in &current_scope.path {
            let scope = &self.program.scopes[*scope_index];
            let variable_index = scope
                .variables
                .iter()
                .position(|v| &v.name.value == name);

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

        self.emit_byte(block::Op::DeclareVariable);
        self.emit(variable_key);
        // This is kinda poopy.
        self.emit(self.current_scope_index as u8);
    }

    fn variable_definition(&mut self, variable_key: u8)
    {
        self.current_scope_mut()
            .variables
            .get_mut(variable_key as usize)
            .unwrap()
            .defined = true;

        self.emit_byte(block::Op::SetLocal);
        self.emit(variable_key);
    }

    fn _if(&mut self)
    {
        self.expression();

        let then_jump = self.emit_jump(block::Op::CondJump);
        self.declaration();

        let else_jump = self.emit_jump(block::Op::Jump);
        self.patch_jump(then_jump);

        if self.match_token(scan::TokenKind::Else) {
            self.declaration();
        }

        self.patch_jump(else_jump);
    }

    fn _while(&mut self)
    {
        let loop_start = self.position();

        // TODO: I think I need to emit an operation here in order to know
        // I'm inside a loop for LLVM.
        self.emit_byte(block::Op::Loop);

        self.expression();

        // let break_jump = self.emit_jump(block::Op::CondJump);
        let break_jump = self.emit_jump(block::Op::LoopCondJump);

        // Body
        self.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        let mut values = 0;
        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            self.declaration();
            values += 1;
        }
        for _ in 0..values { self.emit_byte(block::Op::Pop); }

        self.match_token(scan::TokenKind::RightBracket);

        self.emit_loop(loop_start);
        self.patch_jump(break_jump);
    }

    // In this implementation, all the parts of a for
    // loop declaration are required. While and iterators (when I get to that)
    // will make up for everything.
    fn _for(&mut self)
    {
        // loop variable

        if self.match_token(scan::TokenKind::Identifier) { self.variable() }
        else                                             { self.expression() };

        let mut loop_start = self.position();

        // end loop variables

        // condition

        self.expression();
        // let exit_jump = self.emit_jump(block::Op::CondJump);
        let exit_jump = self.emit_jump(block::Op::LoopCondJump);

        // end condition

        // advancement statement

        let body_jump = self.emit_jump(block::Op::Jump);
        let advancement_start = self.position();
        self.expression();
        self.emit_byte(block::Op::Pop);

        self.emit_loop(loop_start);

        loop_start = advancement_start;
        self.patch_jump(body_jump);

        // end advancement statement

        // body

        self.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        // Compile code until the end of the block or the end of the program is reached.
        let mut count = 0;
        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            self.declaration();
            count += 1;
        }

        let has_value = !matches!(self.parser.previous.kind, scan::TokenKind::Semicolon);
        let count = if has_value && count > 0 { count - 1 } else { count };
        for _ in 0..count { self.emit_byte(block::Op::Pop); }

        self.match_token(scan::TokenKind::RightBracket);

        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);

        // end body

        self.emit_byte(block::Op::Pop);
    }

    fn function_invocation(&mut self)
    {
        let Some(token) = self.parser.peek(-2) else {
            return self
                .error_at("Failed to parse function - no function name found.", &self.parser.previous.clone());
        };

        let function_name_token = token.clone();
        let function_name       = function_name_token.value.clone();

        let mut arguments: usize = 0;
        if !self.parser.check_token(scan::TokenKind::RightParen) {
            loop {
                arguments += 1;
                self.expression();

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

        if arity != &arguments {
            let error_message = format!(
                "Number of passed arguments '{}' to function '{}' does not match function arity '{}'.",
                arguments,
                name,
                arity
            );

            return self.error_at(&error_message, &function_name_token.clone());
        }

        self.emit_byte(block::Op::Call);
        self.emit(scope_distance as u8);
        self.emit(index as u8);

        self.match_token(scan::TokenKind::Semicolon);
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
    fn pipe(&mut self)
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
                block::Value::Function { name, .. } => name == &function_name.value,
                _ => false
            });

        let Some(block::Value::Function { arity, ..}) = function else {
            return self
                .error_at(&format!("Function with name '{}' not in scope", &function_name.value), &function_name.clone());
        };

        if arity != &1 {
            return self
                .error_at("Function must take only one argument to be able to work with pipes.", &function_name.clone());
        }

        self.emit_byte(block::Op::Call);
        self.emit(scope_distance as u8);
        self.emit(index as u8);

        self.match_token(scan::TokenKind::Semicolon);
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

        // Push the new scope and get it's index. Use it as the ID of the Scope struct.
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
    // fn function_distance2(&self, starting_index: Option<usize>, ending_index: Option<usize>) -> usize
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

    fn semicolon(&mut self)
    {
        self.match_token(scan::TokenKind::Semicolon);
    }

    fn emit_return(&mut self, values_count: usize)
    {
        self.emit_byte(block::Op::Return);
        self.emit(values_count as u8);
    }

    fn patch(&mut self, index: usize, byte: u8)
    {
        let block = if self.current_function.is_none() { &mut self.program.block }
                    else                               { &mut self.current_mut().block };
        block.write_at(index, byte);
    }

    fn patch_jump(&mut self, index: usize)
    {
        let code_len = if self.current_function.is_none() { self.program.block.code.len() }
                       else                               { self.current_mut().block.code.len() };
        self.patch(index, (code_len - 1 - index) as u8);
    }

    fn emit_loop(&mut self, loop_start: usize)
    {
        let loop_end = if self.current_function.is_none() { self.program.block.code.len() }
                       else                               { self.current_mut().block.code.len() };
        self.emit_byte(block::Op::LoopJump);
        self.emit((loop_end - loop_start + 2) as u8);
    }

    fn emit_jump(&mut self, op: block::Op) -> usize
    {
        self.emit_byte(op);
        self.emit(0)
    }

    fn emit_byte(&mut self, op: block::Op) -> usize
    {
        let block = if self.current_function.is_none() { &mut self.program.block }
                    else                               { &mut self.current_mut().block };
        block.write_op(op)
    }

    fn emit(&mut self, byte: u8) -> usize
    {
        let block = if self.current_function.is_none() { &mut self.program.block }
                    else                               { &mut self.current_mut().block };

        block.write(byte)
    }

    fn emit_bytes(&mut self, a: block::Op, b: block::Op) -> usize
    {
        let block = if self.current_function.is_none() { &mut self.program.block }
                    else                               { &mut self.current_mut().block };

        block.write_op(a);
        block.write_op(b)
    }

    fn emit_constant(&mut self, value: block::Value)
    {
        let block = if self.current_function.is_none() { &mut self.program.block }
                    else                               { &mut self.current_mut().block };

        let i: u8 = block.write_constant(value);
        block.write_op(block::Op::Constant);
        block.write(i);
    }

    fn position(&self) -> usize
    {
        let block = if self.current_function.is_none() { &self.program.block }
                    else                               { &self.current().block };

        block.code.len()
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

    fn error_at(&mut self, msg: &str, token: &scan::Token)
    {
        let err = self.parser.error_at(msg, token);
        self.errors.push(err);
    }

    fn error(&mut self, err: CompilerError)
    {
        self.errors.push(err)
    }
}
