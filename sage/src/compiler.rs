use std::fmt;
use std::mem::discriminant;

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

static RULES: [ParseRule; 41] = [
    ParseRule { prefix: Some(Compiler::function_expression),           infix: Some(Compiler::function_invocation), precedence: Precedence::Call }, // LeftParen
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // RightParen
    ParseRule { prefix: Some(Compiler::block_expression),              infix: None,                                precedence: Precedence::None }, // LeftBracket
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // RightBracket
    ParseRule { prefix: None,                                          infix: Some(Compiler::left_angle),          precedence: Precedence::Comparison }, // LeftAngle
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Comparison }, // RightAngle
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Questionmark
    ParseRule { prefix: Some(Compiler::semicolon),                     infix: None,                                precedence: Precedence::None }, // Semicolon
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Colon
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // ColonColon
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // ColonEquals
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Term }, // Plus
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Term }, // Minus
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Factor }, // Star
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Factor }, // Slash
    ParseRule { prefix: None,                                          infix: Some(Compiler::pipe),                precedence: Precedence::Call }, // Pipe
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Comma
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Bang
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Equality }, // BandEqual
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Equality }, // EqualEqual
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Comparison }, // GreaterEqual
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Comparison }, // LessEqual
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Equal
    ParseRule { prefix: Some(Compiler::literal),                       infix: None,                                precedence: Precedence::None }, // True
    ParseRule { prefix: Some(Compiler::literal),                       infix: None,                                precedence: Precedence::None }, // False
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // This
    ParseRule { prefix: Some(Compiler::_if),                           infix: None,                                precedence: Precedence::None }, // If
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Else
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Break
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Continue
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Switch
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Case
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // For
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // While
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Func
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Struct
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Interface
    ParseRule { prefix: Some(Compiler::literal),                       infix: None,                                precedence: Precedence::None }, // Literal
    ParseRule { prefix: Some(Compiler::variable),                      infix: None,                                precedence: Precedence::None }, // Identifier
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Error
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }  // End
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

    fn advance(&mut self)
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

            self.error_at("Scanner error.", &self.current.clone());
        }
    }

    fn match_token(&mut self, token_kind: scan::TokenKind) -> bool
    {
        if discriminant(&self.current.kind) != discriminant(&token_kind) {
            return false
        }

        self.advance();
        true
    }

    fn check_token(&self, token_kind: scan::TokenKind) -> bool
    {
        discriminant(&self.current.kind) == discriminant(&token_kind)
    }

    // TODO: this needs to somehow push the error into the compiler.
    // Maybe the parser should hold its own errors. To think about.
    fn consume(&mut self, token_kind: scan::TokenKind, error_message: &str)
    {
        if !self.match_token(token_kind) {
            self.error_at(error_message, &self.current.clone());
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
    name: scan::Token,
    mutable: bool,
    defined: bool
}

#[derive(Clone, Debug)]
pub struct Scope
{
    pub index: usize,
    pub path: Vec<usize>,
    pub variables: Vec<Variable>,
    pub scope_depth: usize
}

#[derive(Clone)]
pub struct Program
{
    pub block: block::Block,
    pub scopes: Vec<Scope>,
    pub current_scope: usize,
    pub scope_depth: usize
}

impl Program
{
    fn new() -> Program
    {
        let starting_scope = Scope {
            index: 0,
            path: vec![],
            variables: vec![],
            scope_depth: 0,
        };
        Program {
            block: block::Block::new(1024),
            scopes: vec![starting_scope],
            current_scope: 0,
            scope_depth: 0
        }
    }

    fn current_scope(&self) -> &Scope
    {
        &self.scopes[self.current_scope]
    }

    fn current_scope_mut(&mut self) -> &mut Scope
    {
        &mut self.scopes[self.current_scope]
    }
}

pub struct Compiler
{
    // Keep the source for error reporting to be better.
    source: String,
    parser: Parser,
    programs: Vec<Program>,
    current_program: usize,

    pub errors: Vec<CompilerError>,
}

// TODO: refactor to pass the entire scanned/parsed data
// structure from parser/scanner.
// A data pipeline.
impl Compiler
{
    #[must_use]
    pub fn new(source: String) -> Compiler
    {
        let source_clone = source.clone();
        Compiler {
            source,
            parser: Parser::new(source_clone, vec![]),
            programs: vec![Program::new()],
            current_program: 0,
            errors: vec![],
        }
    }

    pub fn compile(&mut self, tokens: Vec<scan::Token>) -> Result<Program, ()>
    {
        self.parser = Parser::new(self.source.clone(), tokens);
        self.parser.advance();

        // TODO: should probably enclose program itself.

        loop {
            match self.parser.current.kind {
                scan::TokenKind::End => break,
                _              => self.declaration()
            }
        }

        if self.parser.panic {
            return Err(())
        }

        assert!(self.programs.len() == 1);
        Ok(self.programs
           .pop()
           .unwrap())
    }

    fn current(&self) -> &Program
    {
        &self.programs[self.current_program]
    }

    fn current_mut(&mut self) -> &mut Program
    {
        &mut self.programs[self.current_program]
    }

    fn is_at_end(&self) -> bool
    {
        discriminant(&self.parser.current.kind) == discriminant(&scan::TokenKind::End)
    }

    fn parse_precedence(&mut self, precedence: Precedence)
    {
        self.parser.advance();

        let prefix_rule  = get_rule(self.parser.previous.kind).prefix;

        match prefix_rule {
            Some(prefix) => prefix(self),
            _ => {
                if self.is_at_end() {
                    return
                }

                self.parser.error_at("Expect expression.", &self.parser.current.clone());
                panic!();
            }
        }

        while precedence.discriminator() <= get_rule(self.parser.current.kind)
                                                    .precedence
                                                    .discriminator() {
            self.parser.advance();

            let infix_rule = get_rule(self.parser.previous.kind).infix;
            infix_rule.unwrap()(self);
        }
    }

    fn expression_statement(&mut self)
    {
        self.expression();
        // TODO
    }

    fn expression(&mut self)
    {
        self.parse_precedence(Precedence::Assignment);
    }

    fn block_expression(&mut self)
    {
        self.begin_scope();

        // TODO
        self.code_block();
        self.parser.match_token(scan::TokenKind::Semicolon);

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

        // Blocks are expression - this captures if the block contains a value,
        // or returns 'Unit'.
        // If the final statement in the block is a semicolon, then treat it
        // like a value-less block, else, return the last value in the block.
        // let has_value = !matches!(self.parser.previous.kind, scan::TokenKind::Semicolon);
        // let count = if has_value && count > 0 { count - 1 } else { count };

        // TODO: should probably pop the values that are not the result of the expression.

        self.parser.consume(scan::TokenKind::RightBracket, "Expect '}' at the end of a block expression.");
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
            scan::TokenKind::LeftAngle    => self.emit_byte(block::Op::Less),
            scan::TokenKind::RightAngle   => self.emit_byte(block::Op::Greater),
            scan::TokenKind::BangEqual    => self.emit_bytes(block::Op::Equal, block::Op::Not),
            scan::TokenKind::EqualEqual   => self.emit_byte(block::Op::Equal),
            scan::TokenKind::GreaterEqual => self.emit_byte(block::Op::GreaterEqual),
            scan::TokenKind::LessEqual    => self.emit_byte(block::Op::LessEqual),
            scan::TokenKind::Plus         => self.emit_byte(block::Op::Add),
            scan::TokenKind::Minus        => self.emit_byte(block::Op::Subtract),
            scan::TokenKind::Star         => self.emit_byte(block::Op::Multiply),
            scan::TokenKind::Slash        => self.emit_byte(block::Op::Divide),
            scan::TokenKind::Pipe         => { self.pipe(); 0 },
            _ => { 0 }
        };

        self.parser.match_token(scan::TokenKind::Semicolon);
    }

    fn left_angle(&mut self)
    {
        if self.parser.match_token(scan::TokenKind::Identifier) {
            self.parser.match_token(scan::TokenKind::RightAngle);
            // TODO generics
        } else {
            self.binary();
        }
    }

    fn literal(&mut self)
    {
        match self.parser.previous.kind {
            scan::TokenKind::True  => self.emit_constant(block::Value::Bool{ val: true }),
            scan::TokenKind::False => self.emit_constant(block::Value::Bool { val: false }),

            _ => {
                let value = block::Value::Number {
                    val: self.parser.previous.value.parse::<i32>().unwrap()
                };
                self.emit_constant(value);
            }
        }

        // Eat the semicolon only if present;
        self.parser.match_token(scan::TokenKind::Semicolon);
    }

    fn declaration(&mut self)
    {
        match self.parser.current.kind {
            scan::TokenKind::Func => {
                self.parser.advance();
                self.function_decl();
            }
            scan::TokenKind::While => {
                self.parser.advance();
                self._while();
            },
            scan::TokenKind::For => {
                self.parser.advance();
                self._for();
            }
            _ => self.expression_statement(),
        }
    }

    fn variable(&mut self)
    {
        let immutable = self.parser.check_token(scan::TokenKind::ColonColon);
        let mutable   = self.parser.check_token(scan::TokenKind::ColonEquals);

        if mutable || immutable {
            let variable_token = self.parser.previous.clone();
            let variable_name  = variable_token.value.clone();

            self.parser.match_token(
                if mutable { scan::TokenKind::ColonEquals } else { scan::TokenKind::ColonColon }
            );

            if self.variable_exists(&variable_name) {
                return self
                    .error_at(&format!("Cannot redeclare variable with name '{}'.", &variable_token), &variable_token.clone());
            }

            self.expression();

            let index = self.declare_variable(variable_token, mutable);
            self.variable_definition(index);

            self.parser.match_token(scan::TokenKind::Semicolon);
            return
        }

        if let Some(next) = self.parser.peek(0) {
            if discriminant(&next.kind) == discriminant(&scan::TokenKind::LeftParen) {
                // This is a function call, do nothing in this case, 'call' will handle it.
                self.parser.match_token(scan::TokenKind::Semicolon);
                return
            }
        }

        if self.parse_variable().is_none() {
            return self
                .error_at(&format!("Variable '{}' is not declared.", &self.parser.previous.value), &self.parser.previous.clone());
        }

        self.parser.match_token(scan::TokenKind::Semicolon);
    }

    fn function(&mut self, name: &str) -> block::Value
    {
        self.parser.consume(scan::TokenKind::LeftParen, "Expected '(' after function declaration.");
        self.begin_function();

        let mut arity = 0;
        // If open paren is not followed by closed paren, then parse the parameters.
        if !self.parser.check_token(scan::TokenKind::RightParen) {
            loop {
                arity += 1;

                self.parser.consume(
                    scan::TokenKind::Identifier,
                    "Expect parameter identifier after '('."
                );

                let parameter_name_token = self.parser.previous.clone();
                self.declare_variable(parameter_name_token, false);

                if !self.parser.match_token(scan::TokenKind::Comma) {
                    break
                }
            }
        }

        self.parser.consume(scan::TokenKind::RightParen, "Expected ')' after function parameters.");

        // Parse the block expression that defines the function.
        self.parser.consume(scan::TokenKind::LeftBracket, "Expected '{' before function body.");

        let mut count = 0;
        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            self.declaration();
            count += 1;
        }
        let has_value = !matches!(self.parser.previous.kind, scan::TokenKind::Semicolon);
        let count = if has_value && count > 0 { count - 1 } else { count };

        self.parser.match_token(scan::TokenKind::RightBracket);
        self.parser.match_token(scan::TokenKind::Semicolon);

        // TODO: think about semantics.
        // If the function returns nothing, return Unit instead.
        if !has_value { self.emit_constant(block::Value::Unit); }

        self.emit_return(count + arity);

        let function_code = self.end_function();
        let function = block::Value::Function {
            name: name.to_owned(),
            arity,
            closure: block::Closure { code: function_code },
        };

        function
    }

    fn function_expression(&mut self)
    {
        self.begin_function();

        // a :: ()
        // ^ __
        // 3 21
        let variable_name = self.parser
            .peek(-3)
            .clone()
            .unwrap()
            .value
            .clone();

        let mut arity = 0;
        if !self.parser.check_token(scan::TokenKind::RightParen) {
            loop {
                arity += 1;

                self.parser.consume(scan::TokenKind::Identifier, "Expect parameter identifier after '('.");

                let parameter_name_token = self.parser.previous.clone();
                self.declare_variable(parameter_name_token, false);

                if !self.parser.match_token(scan::TokenKind::Comma) {
                    break
                }
            }
        }

        self.declare_variable(self.parser.previous.clone(), false);

        self.parser.consume(scan::TokenKind::RightParen, "Expect ')' after end of lambda parameters.");

        self.expression();
        self.emit_return(arity);

        let expression_block = self.end_function();
        let function = block::Value::Function {
            name: variable_name,
            arity,
            closure: block::Closure { code: expression_block },
        };

        self.emit_constant(function)
    }

    fn function_decl(&mut self)
    {
        self.parser.consume(scan::TokenKind::Identifier, "Cannot declare function without name.");

        let function_token = self.parser.previous.clone();
        let function_name = function_token.value.clone();

        if self.variable_exists(&function_name) {
            return self
                .error_at(&format!("Cannot redeclare function with name '{}'", function_name), &function_token);
        }

        // Compile the expression and then jump after the block
        // to avoid executing the code during function _declaration_.
        let variable_key = self.declare_variable(function_token, false);
        self.variable_declaration(variable_key);

        let function = self.function(&function_name);
        self.emit_constant(function);

        self.variable_definition(variable_key);
    }

    fn declare_variable(&mut self, token: scan::Token, mutable: bool) -> u8
    {
        let variable_name = token.value.clone();

        let variable = Variable {
            name: token,
            mutable,
            defined: true
        };

        self.current_mut()
            .current_scope_mut()
            .variables
            .push(variable);

        let variable_index = self.current()
            .current_scope()
            .variables
            .iter()
            .position(|v| v.name.value == variable_name)
            .unwrap();

        variable_index as u8
    }

    fn find_variable_by_name(&self, name: &str) -> Option<(usize, usize, usize)>
    {
        let mut current_program = self.current_program;

        loop {
            let program = &self.programs[current_program];

            let variable_index = program
                .current_scope()
                .variables
                .iter()
                .position(|v| v.name.value == name);

            if let Some(position) = variable_index {
                return Some((current_program, self.current().current_scope, position))
            }

            for s in program.current_scope().path.iter() {
                let scope = &program.scopes[*s];

                let variable_index = scope
                    .variables
                    .iter()
                    .position(|v| v.name.value == name);

                if let Some(position) = variable_index {
                    return Some((current_program, scope.index, position))
                }
            }

            if current_program == 0 { break }
            current_program -= 1;
        }

        None
    }

    fn parse_variable(&mut self) -> Option<u8>
    {
        let previous         = self.parser.previous.value.clone();
        let variable_indices = self.find_variable_by_name(&previous);

        let Some((program_idx, scope, index)) = variable_indices else {
            return None
        };

        // If the following token is '=', then it's an assignment.
        if self.parser.match_token(scan::TokenKind::Equal) {
            if program_idx == self.current_program {
                if !self.current().scopes[scope].variables[index].mutable {
                    self.parser
                        .error_at("Cannot reassign value of an immutable variable.", &self.parser.previous.clone());
                }

                self.expression();

                self.emit_byte(block::Op::SetLocal);
            } else {
                // Emit program distance -> vm will move frames by this distance.
                self.emit_byte(block::Op::SetUpvalue);
                let scope_distance = self.current_program - program_idx;
                self.emit(scope_distance as u8);
            }
        } else {
            if program_idx == self.current_program {
                self.emit_byte(block::Op::GetLocal);
            } else {
                // Emit program distance -> vm will move frames by this distance.
                self.emit_byte(block::Op::GetUpvalue);
                let scope_distance = self.current_program - program_idx;
                self.emit(scope_distance as u8);
            }
        }

        self.emit(index as u8);
        Some(index as u8)
    }

    fn variable_exists(&self, name: &str) -> bool
    {
        let mut current_program = self.current_program;

        loop {
            let program = &self.programs[current_program];

            let variable_index = program
                .current_scope()
                .variables
                .iter()
                .position(|v| v.name.value == name);

            if variable_index.is_some() {
                return true
            }

            for s in program.current_scope().path.iter() {
                let scope = &program.scopes[*s];

                let variable_index = scope
                    .variables
                    .iter()
                    .position(|v| v.name.value == name);

                if variable_index.is_some() {
                    return true
                }
            }

            if current_program == 0 { break }
            current_program -= 1;
        }

        false
    }

    fn variable_declaration(&mut self, variable_key: u8)
    {
        self.current_mut()
            .current_scope_mut()
            .variables
            .get_mut(variable_key as usize)
            .unwrap()
            .defined = false;

        self.emit_byte(block::Op::DeclareVariable);
    }

    fn variable_definition(&mut self, variable_key: u8)
    {
        self.current_mut()
            .current_scope_mut()
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

        if self.parser.match_token(scan::TokenKind::Else) {
            self.declaration();
        }

        self.patch_jump(else_jump);
    }

    fn _while(&mut self)
    {
        let loop_start = self.position();
        self.expression();

        let break_jump = self.emit_jump(block::Op::CondJump);

        // Body
        self.parser.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        let mut values = 0;
        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            self.declaration();
            values += 1;
        }
        for _ in 0..values { self.emit_byte(block::Op::Pop); }

        self.parser.match_token(scan::TokenKind::RightBracket);

        self.emit_loop(loop_start);
        self.patch_jump(break_jump);
    }

    // In this implementation, all the parts of a for
    // loop declaration are required. While and iterators (when I get to that)
    // will make up for everything.
    fn _for(&mut self)
    {
        // loop variable

        if self.parser.match_token(scan::TokenKind::Identifier) {
            self.variable();
        } else {
            self.expression();
        }

        let mut loop_start = self.position();

        // end loop variables

        // condition

        self.expression();
        let exit_jump = self.emit_jump(block::Op::CondJump);

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

        self.parser.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        // Compile code until the end of the block or the end of the program is reached.
        let mut count = 0;
        while !self.parser.check_token(scan::TokenKind::RightBracket) && !self.parser.check_token(scan::TokenKind::End) {
            self.declaration();
            count += 1;
        }

        let has_value = !matches!(self.parser.previous.kind, scan::TokenKind::Semicolon);
        let count = if has_value && count > 0 { count - 1 } else { count };
        for _ in 0..count { self.emit_byte(block::Op::Pop); }

        self.parser.match_token(scan::TokenKind::RightBracket);

        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);

        // end body

        self.emit_byte(block::Op::Pop);
    }

    fn function_invocation(&mut self)
    {
        let token = self.parser.peek(-2);
        if token.is_none() {
            // TODO: it's not really at self.parser.previous, but the one before that,
            // but since it could not get it, then I guess previous is the next best thing.
            return self
                .error_at("Failed to parse function - no function name found.", &self.parser.previous.clone());
        }

        // Unwrap + clone to deal with ownership fuckery.
        let function_name_token = token.unwrap().clone();

        let function_name = function_name_token.value.clone();

        let mut arguments: usize = 0;
        if !self.parser.check_token(scan::TokenKind::RightParen) {
            loop {
                arguments += 1;
                self.expression();

                if !self.parser.match_token(scan::TokenKind::Comma) { break }
            }
        }
        self.parser.consume(scan::TokenKind::RightParen, "Expect ')' after function arguments.");

        let Some((program_idx, scope, index)) = self.find_variable_by_name(&function_name) else {
            return self
                .error_at(&format!("Failed to find function '{}'.", &function_name), &function_name_token.clone());
        };

        // Get the scope of the variable, then find it by name in the scopes constants.
        let function = self.programs[scope]
            .block
            .constants
            .iter()
            .find(|c| match c {
                block::Value::Function { name, .. } => name == &function_name,
                _ => false
            });

        if let Some(block::Value::Function { name, arity, .. }) = function {
            if arity != &arguments {
                let error_message = format!(
                    "Number of passed arguments '{}' to function '{}' does not match function arity '{}'.",
                    arguments,
                    name,
                    arity
                );

                return self.error_at(&error_message, &function_name_token.clone());
            }
        }

        self.emit_byte(block::Op::Call);
        self.emit((self.current_program - program_idx) as u8);
        self.emit(index as u8);

        self.parser.match_token(scan::TokenKind::Semicolon);
    }

    // TODO: So far the piping into a function is only supported with function
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
        if discriminant(&self.parser.previous.kind) != discriminant(&scan::TokenKind::Pipe) {
            return self
                .error_at(
                    &format!("Expected infix token '{}' found '{}'", "|>", self.parser.previous.value),
                    &self.parser.previous.clone()
                );
        }

        let Some(function_name) = self.parser.peek(0) else {
            return self
                .error_at("Failed to parse function name - no function name found.", &self.parser.current.clone());
        };

        let Some((program_idx, scope, index)) = self.find_variable_by_name(&function_name.value) else {
            return self
                .error_at(&format!("Failed to find function '{}'.", &function_name.value), &function_name.clone());
        };

        // Get the scope of the variable, then find it by name in the scopes constants.
        let function = self.programs[scope]
            .block
            .constants
            .iter()
            .find(|c| match c {
                block::Value::Function { name, .. } => name == &function_name.value,
                _ => false
            });

        let Some(block::Value::Function { arity, ..}) = function else {
            // TODO: how are parse errors handled? How is compilation stopped? Do I even
            // stop compilation? Should I?
            return self
                .error_at(&format!("Function with name '{}' not in scope", &function_name.value), &function_name.clone());
        };

        if arity != &1 {
            return self
                .error_at("Function must take only one argument to be able to work with pipes.", &function_name.clone());
        }

        self.emit_byte(block::Op::Call);
        self.emit((self.current_program - program_idx) as u8);
        self.emit(index as u8);

        self.parser.match_token(scan::TokenKind::Semicolon);
    }

    fn begin_scope(&mut self)
    {
        self.current_mut().scope_depth += 1;

        let previous = &self.current().current_scope();

        let mut scope_path = previous.path.clone();
        scope_path.push(previous.index);

        let scope = Scope {
            index: self.current().scopes.len() - 1,
            path: scope_path,
            variables: vec![],
            scope_depth: self.current().scope_depth,
        };

        let idx = self.current().scopes.len();
        self.current_mut().scopes.push(scope);
        self.current_mut().current_scope = idx;
    }

    fn end_scope(&mut self)
    {
        self.current_mut().scope_depth -= 1;

        let scope_idx = self.current()
            .current_scope()
            .path
            .last()
            .unwrap_or(&0);

        self.current_mut().current_scope = *scope_idx;
    }

    fn begin_function(&mut self)
    {
        self.programs.push(Program::new());
        self.current_program = self.programs.len() - 1;
    }

    /// Gives away ownership of the block after the scope is finished.
    fn end_function(&mut self) -> block::Block
    {
        let nested = self.programs.pop().unwrap();
        self.current_program = self.programs.len() - 1;
        nested.block
    }

    fn semicolon(&mut self)
    {
        self.parser.match_token(scan::TokenKind::Semicolon);
    }

    fn emit_return(&mut self, values_count: usize)
    {
        self.emit_byte(block::Op::Return);
        self.emit(values_count as u8);
    }

    fn patch(&mut self, index: usize, byte: u8)
    {
        self.current_mut().block.write_at(index, byte);
    }

    fn patch_jump(&mut self, index: usize)
    {
        let code_len = self.current().block.code.len();
        self.patch(index, (code_len - 1 - index) as u8);
    }

    fn emit_loop(&mut self, loop_start: usize)
    {
        let loop_end = self.current().block.code.len();
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
        self.current_mut().block.write_op(op)
    }

    fn emit(&mut self, byte: u8) -> usize
    {
        self.current_mut().block.write(byte as u8)
    }

    fn emit_bytes(&mut self, a: block::Op, b: block::Op) -> usize
    {
        self.current_mut().block.write_op(a);
        self.current_mut().block.write_op(b)
    }

    fn emit_constant(&mut self, value: block::Value)
    {
        let i = self.current_mut().block.write_constant(value);

        self.current_mut().block.write_op(block::Op::Constant);
        self.current_mut().block.write(i as u8);
    }

    fn position(&self) -> usize
    {
        self.current().block.code.len()
    }

    fn error_at(&mut self, msg: &str, token: &scan::Token)
    {
        let err = self.parser.error_at(msg, token);
        self.errors.push(err);
    }
}
