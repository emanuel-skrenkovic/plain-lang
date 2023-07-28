use std::mem::discriminant;

use crate::block::{Block, Closure, Op, Value};
use crate::scan::{Token, TokenKind};

#[repr(u8)]
#[derive(Copy, Clone, PartialOrd, PartialEq)]
enum Precedence {
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

impl Precedence {
    pub fn discriminator(self) -> u8 {
        self as u8
    }
}

impl TryFrom<u8> for Precedence {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
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
struct ParseRule {
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
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Term }, // Plus
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Term }, // Minus
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Factor }, // Star
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Factor }, // Slash
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Comma
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Bang
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Equality }, // BandEqual
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Equality }, // EqualEqual
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Comparison }, // GreaterEqual
    ParseRule { prefix: None,                                          infix: Some(Compiler::binary),              precedence: Precedence::Comparison }, // LessEqual
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Equal
    ParseRule { prefix: Some(Compiler::literal),                       infix: None,                                precedence: Precedence::None }, // True
    ParseRule { prefix: Some(Compiler::literal),                       infix: None,                                precedence: Precedence::None }, // False
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Let
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Var
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // This
    ParseRule { prefix: Some(Compiler::_if),                           infix: None,                                precedence: Precedence::None }, // If
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Else
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Break
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Continue
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Switch
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Case
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // For
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // While
    ParseRule { prefix: Some(Compiler::anonymous_function_expression), infix: None,                                precedence: Precedence::None }, // Func
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Struct
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Interface
    ParseRule { prefix: Some(Compiler::literal),                       infix: None,                                precedence: Precedence::None }, // Literal
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // FatArrow
    ParseRule { prefix: Some(Compiler::variable),                      infix: None,                                precedence: Precedence::None }, // Identifier
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }, // Error
    ParseRule { prefix: None,                                          infix: None,                                precedence: Precedence::None }  // End
];

fn get_rule(token_kind: TokenKind) -> ParseRule {
    RULES[token_kind as usize]
}

pub struct Parser {
    tokens: Vec<Token>,
    scanned_tokens: Vec<Token>,

    current_index: usize,

    current: Token,
    previous: Token,

    panic: bool,
    error: bool
}

impl Parser {
    #[must_use]
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            scanned_tokens: vec![],
            current_index: 0,
            current: Token::default(),
            previous: Token::default(),
            panic: false,
            error: false
        }
    }

    fn advance(&mut self) {
        self.previous = self.current.clone();

        loop {
            if matches!(self.current.kind, TokenKind::End) {
                break
            }

            if self.current_index >= self.tokens.len() {
                break
            }

            self.current = self.tokens[self.current_index].clone();

            if matches!(self.current.kind, TokenKind::End) {
                break
            }

            self.current_index += 1;
            self.scanned_tokens.push(self.current.clone());

            if !matches!(self.current.kind, TokenKind::Error) {
                break;
            }

            self.error_at_current("Scanner error.");
        }
    }

    fn match_token(&mut self, token_kind: TokenKind) -> bool {
        if discriminant(&self.current.kind) != discriminant(&token_kind) {
            return false
        }

        self.advance();
        true
    }

    fn check_token(&self, token_kind: TokenKind) -> bool {
        discriminant(&self.current.kind) == discriminant(&token_kind)
    }

    fn consume(&mut self, token_kind: TokenKind, error_message: &str) {
        if !self.match_token(token_kind) {
            self.error_at_current(error_message);
        }
    }

    fn error_at_current(&mut self, message: &str) {
        self.panic = true;
        self.error = true;

        eprintln!(
            "[line {}] Error at token '{}\n{}'",
            self.current.line,
            self.current.value,
            message
        );
    }

    // TODO: horrible and I should be publicly shamed
    fn peek(&self, diff: i32) -> Option<&Token> {
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
pub struct Variable {
    name: Token,
    mutable: bool,
    scope: usize,
    defined: bool
}

#[derive(Clone)]
pub struct Program {
    pub block: Block,
    pub variables: Vec<Variable>,
}

impl Program {
    fn new() -> Program {
        Program {
            block: Block::new(1024),
            variables: vec![],
        }
    }
}

pub struct Compiler {
    // scanner: Scanner,
    parser: Parser,
    scopes: Vec<Program>,
    scope_depth: usize
}

// TODO: refactor to pass the entire scanned/parsed data
// structure from parser/scanner.
// A data pipeline.
impl Compiler {
    #[must_use]
    pub fn new() -> Compiler {
        Compiler {
            // scanner: Scanner::new(source),
            parser: Parser::new(vec![]),
            scopes: vec![Program::new()],
            scope_depth: 0
        }
    }

    pub fn compile(&mut self, tokens: Vec<Token>) -> Result<Program, ()> {
        self.parser = Parser::new(tokens);
        self.parser.advance();

        // TODO: should probably enclose program itself.

        loop {
            match self.parser.current.kind {
                TokenKind::End => break,
                _              => self.declaration()
            }
        }

        if self.parser.panic {
            return Err(())
        }

        assert!(self.scopes.len() == 1);
        Ok(self.scopes
           .pop()
           .unwrap())
    }

    fn current(&self) -> &Program {
        &self.scopes[self.scope_depth]
    }

    fn current_mut(&mut self) -> &mut Program {
        &mut self.scopes[self.scope_depth]
    }

    fn is_at_end(&self) -> bool {
        discriminant(&self.parser.current.kind) == discriminant(&TokenKind::End)
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.parser.advance();

        let prefix_rule  = get_rule(self.parser.previous.kind).prefix;

        match prefix_rule {
            Some(prefix) => prefix(self),
            _ => {
                if self.is_at_end() {
                    return
                }

                self.parser.error_at_current("Expect expression.");
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

    fn expression_statement(&mut self) {
        self.expression();
        // TODO
        // self.parser.match_token(TokenKind::Semicolon);
        // self.emit_byte(Op::Pop);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn block_expression(&mut self) {
        self.emit_byte(Op::Frame);

        // Reserve place for Closure value which will be written later.
        // Closure is written later because the code block needs to be populated
        // by nested code first.
        // TODO: make prettier
        let closure_index = self.current_mut().block.write_constant(Value::Unit);
        self.current_mut().block.write(closure_index as u8);

        // TODO
        let nested_block = self.code_block();
        self.parser.match_token(TokenKind::Semicolon);

        let closure = Closure { code: nested_block };
        self.patch_constant(closure_index as usize, Value::Closure { val: closure });
    }

    // TODO: this is the result of bad design. I need to use block expression
    // code when parsing both block expressions and function (valued block expressions?).
    // Since the 'block_expression' method calls end scope, and thus consumes the closure,
    // that code cannot be reused for function declarations.
    // This is bad and it should be implemented in more reusable, functional way.
    fn code_block(&mut self) -> Block {
        self.begin_scope();

        // Compile code until the end of the block or the end of the program is reached.
        let mut count = 0;
        while !self.parser.check_token(TokenKind::RightBracket) && !self.parser.check_token(TokenKind::End) {
            self.declaration();
            count += 1;
        }

        // Blocks are expression - this captures if the block contains a value,
        // or returns 'Unit'.
        // If the final statement in the block is a semicolon, then treat it
        // like a value-less block, else, return the last value in the block.
        let has_value = !matches!(self.parser.previous.kind, TokenKind::Semicolon);
        let count = if has_value && count > 0 { count - 1 } else { count };

        self.parser.match_token(TokenKind::RightBracket);
        // TODO: think about this
        // self.parser.consume(TokenKind::RightBracket, "Expect '}' at the end of a block expression.");

        self.emit_return(count);
        self.end_scope()
    }

    fn binary(&mut self) {
        let operator   = self.parser.previous.kind;
        let parse_rule = get_rule(operator);

        self.parse_precedence(
            Precedence::try_from(
                parse_rule.precedence.discriminator() + 1
            ).unwrap()
        );

        match operator {
            TokenKind::LeftAngle    => self.emit_byte(Op::Less),
            TokenKind::RightAngle   => self.emit_byte(Op::Greater),
            TokenKind::BangEqual    => self.emit_bytes(Op::Equal, Op::Not),
            TokenKind::EqualEqual   => self.emit_byte(Op::Equal),
            TokenKind::GreaterEqual => self.emit_byte(Op::GreaterEqual),
            TokenKind::LessEqual    => self.emit_byte(Op::LessEqual),
            TokenKind::Plus         => self.emit_byte(Op::Add),
            TokenKind::Minus        => self.emit_byte(Op::Subtract),
            TokenKind::Star         => self.emit_byte(Op::Multiply),
            TokenKind::Slash        => self.emit_byte(Op::Divide),
            _ => { 0 }
        };

        self.parser.match_token(TokenKind::Semicolon);
    }

    fn left_angle(&mut self) {
        if self.parser.match_token(TokenKind::Identifier) {
            self.parser.match_token(TokenKind::RightAngle);
            // TODO generics
        } else {
            self.binary();
        }
    }

    fn literal(&mut self) {
        match self.parser.previous.kind {
            TokenKind::True  => self.emit_constant(Value::Bool{ val: true }),
            TokenKind::False => self.emit_constant(Value::Bool { val: false }),

            _ => {
                let value = Value::Number {
                    val: self.parser.previous.value.parse::<i32>().unwrap()
                };
                self.emit_constant(value);
            }
        }

        // Eat the semicolon only if present;
        self.parser.match_token(TokenKind::Semicolon);
    }

    fn declaration(&mut self) {
        match self.parser.current.kind {
            TokenKind::Func => {
                self.parser.advance();
                self.function_decl();
            }
            TokenKind::Var => {
                self.parser.advance();
                self.var_decl();
            },
            TokenKind::Let => {
                self.parser.advance();
                self.let_decl();
            },
            TokenKind::While => {
                self.parser.advance();
                self._while();
            },
            TokenKind::For => {
                self.parser.advance();
                self._for();
            }
            _ => self.expression_statement(),
        }
    }

    fn variable(&mut self) {
        if self.parse_variable().is_none() {
            let previous = self.parser.previous.value.clone();
            self.parser.error_at_current(
                &format!("Variable '{}' is not declared.", &previous)
            );
        }

        self.parser.match_token(TokenKind::Semicolon);
    }

    fn function(&mut self, name: &str) -> Value {
        self.parser.consume(TokenKind::LeftParen, "Expected '(' after function declaration.");
        self.begin_scope();

        let mut arity = 0;
        // If open paren is not followed by closed paren, then parse the parameters.
        if !self.parser.check_token(TokenKind::RightParen) {
            loop {
                arity += 1;

                self.parser.consume(
                    TokenKind::Identifier,
                    "Expect parameter identifier after '('."
                );

                let parameter_name_token = self.parser.previous.clone();
                self.declare_variable(parameter_name_token, false);

                if !self.parser.match_token(TokenKind::Comma) {
                    break
                }
            }
        }

        self.parser.consume(TokenKind::RightParen, "Expected ')' after function parameters.");

        // Parse the block expression that defines the function.
        self.parser.consume(TokenKind::LeftBracket, "Expected '{' before function body.");

        let mut count = 0;
        while !self.parser.check_token(TokenKind::RightBracket) && !self.parser.check_token(TokenKind::End) {
            self.declaration();
            count += 1;
        }
        let has_value = !matches!(self.parser.previous.kind, TokenKind::Semicolon);
        let count = if has_value && count > 0 { count - 1 } else { count };

        self.parser.match_token(TokenKind::RightBracket);
        self.parser.match_token(TokenKind::Semicolon);

        // TODO: think about semantics.
        // If the function returns nothing, return Unit instead.
        if !has_value { self.emit_constant(Value::Unit); }

        // Pop all the values in the block (excluding the returning value, if preset),
        // as well as the arguments, and the function itself which was pulled onto the stack
        // again during function call.
        self.emit_return(count + arity + 1);

        let function_code = self.end_scope();
        let function = Value::Function {
            name: name.to_owned(),
            arity,
            closure: Closure { code: function_code },
        };

        function
    }

    fn function_expression(&mut self) {
        self.begin_scope();

        let mut arity = 0;
        if !self.parser.check_token(TokenKind::RightParen) {
            loop {
                arity += 1;

                self.parser.consume(
                    TokenKind::Identifier,
                    "Expect parameter identifier after '('."
                );

                let parameter_name_token = self.parser.previous.clone();
                self.declare_variable(parameter_name_token, false);

                if !self.parser.match_token(TokenKind::Comma) {
                    break
                }
            }
        }

        let variable_name = self.parser.previous.clone();
        self.declare_variable(variable_name, false);

        self.parser.consume(TokenKind::RightParen, "Expect ')' after end of lambda parameters.");
        self.parser.consume(TokenKind::FatArrow, "Expect '=>' when declaring lambda.");

        self.expression();
        self.emit_return(arity + 1);

        let expression_block = self.end_scope();
        let function = Value::Function {
            name: "lambda".to_owned(), // TODO: proper name
            arity,
            closure: Closure { code: expression_block },
        };

        self.emit_constant(function)

    }

    fn anonymous_function_expression(&mut self) {
        let function = self.function("anonumous");
        self.emit_constant(function);
    }

    fn function_decl(&mut self) {
        self.parser.consume(TokenKind::Identifier, "Cannot declare function without name.");

        let function_token = self.parser.previous.clone();
        let function_name = function_token.value.clone();

        if self.variable_exists(&function_name) {
            self.parser.error_at_current(
                &format!("Cannot redeclare function with name '{}'", function_name)
            );
        }

        // Compile the expression and then jump after the block
        // to avoid executing the code during function _declaration_.
        let variable_key = self.declare_variable(function_token, false);
        self.variable_declaration(variable_key);

        let function = self.function(&function_name);
        self.emit_constant(function);

        self.variable_definition(variable_key);
    }

    fn let_decl(&mut self) {
        self.parser.match_token(TokenKind::Identifier);

        let variable_token = self.parser.previous.clone();
        let variable_name  = variable_token.value.clone();

        if self.variable_exists(&variable_name) {
            self.parser.error_at_current(
                &format!("Cannot redeclare variable with name '{}'.", variable_name)
            );
        }

        self.parser.consume(TokenKind::Equal, "Expect definition as a part of let declaration.");

        let index = self.declare_variable(variable_token, false);

        self.expression();
        // self.parser.consume(TokenKind::Semicolon, "Expect ';' after variable 'let' declaration.");

        self.variable_definition(index);
    }

    fn var_decl(&mut self) {
        self.parser.match_token(TokenKind::Identifier);

        let variable_token = self.parser.previous.clone();
        let variable_name  = variable_token.value.clone();

        if self.variable_exists(&variable_name) {
            self.parser.error_at_current(
                &format!("Cannot redeclare variable with name '{}'.", variable_name)
            );
        }

        if self.parse_variable().is_some() {
            return
        }

        let index = self.declare_variable(variable_token, true);

        // If the variable declaration is followed by the '=' sign,
        // then it is defined and we should parse the definition expression.
        if self.parser.match_token(TokenKind::Equal) {
            self.expression();
            // self.parser.consume(TokenKind::Semicolon, "Expect ';' after variable definition.");
            self.variable_definition(index);
        } else {
            self.variable_declaration(index);
            self.parser.consume(TokenKind::Semicolon, "Expect ';' after variable declaration.");
        }
    }

    fn declare_variable(&mut self, token: Token, mutable: bool) -> u8 {
        let variable_name = token.value.clone();

        let variable = Variable {
            name: token,
            mutable,
            scope: self.scope_depth,
            defined: true
        };
        self.current_mut()
            .variables
            .push(variable);

        let variable_index = self.current_mut()
                                 .variables
                                 .iter()
                                 .position(|v| v.name.value == variable_name)
                                 .unwrap();

        variable_index as u8
    }

    fn find_variable_by_name(&self, name: &str) -> Option<(usize, usize)> {
        let mut current_scope = self.scope_depth;

        loop {
            let scope = &self.scopes[current_scope];

            let variable_index = scope
                .variables
                .iter()
                .position(|v| v.name.value == name);

            if let Some(position) = variable_index {
                return Some((current_scope, position))
            }

            if current_scope == 0 {
                break
            }

            current_scope -= 1;
        }

        None
    }

    fn parse_variable(&mut self) -> Option<u8> {
        let previous = self.parser.previous.value.clone();
        let variable_indices = self.find_variable_by_name(&previous);

        let Some((scope, index)) = variable_indices else { return None };

        // If the following token is '=', then it's an assignment.
        if self.parser.match_token(TokenKind::Equal) {
            if !self.scopes[scope].variables[index].mutable {
                self.parser.error_at_current("Cannot reassign value of an immutable variable.");
            }

            self.expression();

            if scope < self.scope_depth {
                // Emit scope distance -> vm will move frames by this distance.
                self.emit_byte(Op::SetUpvalue);
                let scope_distance = self.scope_depth - scope;
                self.emit(scope_distance as u8);
            } else {
                self.emit_byte(Op::SetLocal);
            }
        } else {
            if scope < self.scope_depth {
                // Emit scope distance -> vm will move frames by this distance.
                self.emit_byte(Op::GetUpvalue);
                let scope_distance = self.scope_depth - scope;
                self.emit(scope_distance as u8);
            } else {
                self.emit_byte(Op::GetLocal);
            }
        }

        self.emit(index as u8);
        Some(index as u8)
    }

    fn variable_exists(&self, name: &str) -> bool {
        self.current()
            .variables
            .iter()
            .any(|v| v.name.value == name && v.scope <= self.scope_depth)
    }

    fn variable_declaration(&mut self, variable_key: u8) {
        self.current_mut()
            .variables
            .get_mut(variable_key as usize)
            .unwrap().defined = false;

        self.emit_byte(Op::DeclareVariable);
    }

    fn variable_definition(&mut self, variable_key: u8) {
        self.current_mut()
            .variables
            .get_mut(variable_key as usize).unwrap().defined = true;

        self.emit_byte(Op::SetLocal);
        self.emit(variable_key);
    }

    fn _if(&mut self) {
        self.expression();

        let then_jump = self.emit_jump(Op::CondJump);
        self.declaration();

        let else_jump = self.emit_jump(Op::Jump);
        self.patch_jump(then_jump);

        if self.parser.match_token(TokenKind::Else) {
            self.declaration();
        }

        self.patch_jump(else_jump);
    }

    fn _while(&mut self) {
        let closure_index = self.start_closure();
        self.begin_scope();

        let loop_start = self.position();
        self.expression();

        let break_jump = self.emit_jump(Op::CondJump);

        // Body
        let mut values = 0;
        self.parser.consume(TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");
        while !self.parser.check_token(TokenKind::RightBracket) && !self.parser.check_token(TokenKind::End) {
            self.declaration();
            values += 1;
        }
        for _ in 0..values { self.emit_byte(Op::Pop); }

        self.parser.match_token(TokenKind::RightBracket);

        self.emit_loop(loop_start);
        self.patch_jump(break_jump);

        self.emit_return(0);

        let nested_block = self.end_scope();
        self.end_closure(closure_index as usize, nested_block);
    }

    // In this implementation, all the parts of a for
    // loop declaration are required. While and iterators (when I get to that)
    // will make up for everything.
    fn _for(&mut self) {
        let closure_index = self.start_closure();
        self.begin_scope();

        // loop variable

        if self.parser.match_token(TokenKind::Var) {
            self.var_decl();
        } else if self.parser.match_token(TokenKind::Let) {
            self.parser.error_at_current("'let' declaration as a part of for loop is not allowed.");
        } else {
            self.expression();
        }

        let mut loop_start = self.position();

        // end loop variables

        // condition

        self.expression();
        let exit_jump = self.emit_jump(Op::CondJump);

        // end condition

        // advancement statement

        let body_jump = self.emit_jump(Op::Jump);
        let advancement_start = self.position();
        self.expression();
        self.emit_byte(Op::Pop);

        self.emit_loop(loop_start);

        loop_start = advancement_start;
        self.patch_jump(body_jump);

        // end advancement statement

        // body

        self.parser.consume(TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        // Compile code until the end of the block or the end of the program is reached.
        let mut count = 0;
        while !self.parser.check_token(TokenKind::RightBracket) && !self.parser.check_token(TokenKind::End) {
            self.declaration();
            count += 1;
        }

        let has_value = !matches!(self.parser.previous.kind, TokenKind::Semicolon);
        let count = if has_value && count > 0 { count - 1 } else { count };
        for _ in 0..count { self.emit_byte(Op::Pop); }

        self.parser.match_token(TokenKind::RightBracket);

        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);

        // end body

        self.emit_return(0);

        let nested_block = self.end_scope();
        self.end_closure(closure_index as usize, nested_block);

        self.emit_byte(Op::Pop);
    }

    fn function_invocation(&mut self) {
        let function_name_token = self.parser.peek(-2);

        if function_name_token.is_none() {
            // TODO: error, probably
            return;
        }

        // Unwrapping to avoid borrowing. Need to get an mutable reference later in
        // parser.consume.
        let function_name_token = function_name_token.unwrap();
        let function_name = function_name_token.value.clone();

        let mut arguments: usize = 0;
        if !self.parser.check_token(TokenKind::RightParen) {
            loop {
                arguments += 1;
                self.expression();

                if !self.parser.match_token(TokenKind::Comma) {
                    break
                }
            }
        }
        self.parser.consume(TokenKind::RightParen, "Expect ')' after function arguments.");

        // Get the scope of the variable, then find it by name in the scopes constants.
        if let Some((scope, _)) = self.find_variable_by_name(&function_name) {
            let function = self.scopes[scope].block
                .constants
                .iter()
                .find(|c| {
                    match c {
                        Value::Function { name, arity: _, closure: _ } => name == &function_name,
                        _ => false
                    }
                });

            if let Some(Value::Function { name, arity, closure: _ }) = function {
                if arity != &arguments {
                    let error_message = format!(
                        "Number of passed arguments '{}' to function '{}' does not match function arity '{}'.",
                        arguments,
                        name,
                        arity
                    );

                    self.parser.error_at_current(&error_message);
                }
            }

            self.emit_byte(Op::Call);
            self.emit(arguments as u8);
        }

        self.parser.match_token(TokenKind::Semicolon);
    }

    fn start_closure(&mut self) -> u8 {
        self.emit_byte(Op::Frame);

        let closure_index = self.current_mut().block.write_constant(Value::Unit);
        self.current_mut().block.write(closure_index as u8);

        closure_index
    }

    fn end_closure(&mut self, closure_index: usize, block: Block) {
        let closure = Closure { code: block };
        self.patch_constant(
            closure_index,
            Value::Closure { val: closure }
        );
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
        self.scopes.push(Program::new());
    }

    /// Gives away ownership of the block after the scope is finished.
    fn end_scope(&mut self) -> Block {
        assert!(self.scope_depth > 0);
        assert!(!self.scopes.is_empty());

        self.scope_depth -= 1;
        let nested = self.scopes.pop().unwrap();

        nested.block
    }

    fn semicolon(&mut self) {
        self.parser.match_token(TokenKind::Semicolon);
    }

    fn emit_return(&mut self, values_count: usize) {
        self.emit_byte(Op::Return);
        self.emit(values_count as u8);
    }

    fn patch(&mut self, index: usize, byte: u8) {
        self.current_mut().block.write_at(index, byte);
    }

    fn patch_jump(&mut self, index: usize) {
        let code_len = self.current().block.code.len();
        self.patch(index, (code_len - 1 - index) as u8);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let loop_end = self.current().block.code.len();
        self.emit_byte(Op::LoopJump);
        self.emit((loop_end - loop_start + 2) as u8);
    }

    fn emit_jump(&mut self, op: Op) -> usize {
        self.emit_byte(op);
        self.emit(0)
    }

    fn emit_byte(&mut self, op: Op) -> usize {
        self.current_mut().block.write_op(op)
    }

    fn emit(&mut self, byte: u8) -> usize {
        self.current_mut().block.write(byte as u8)
    }

    fn emit_bytes(&mut self, a: Op, b: Op) -> usize {
        self.current_mut().block.write_op(a);
        self.current_mut().block.write_op(b)
    }

    fn emit_constant(&mut self, value: Value) {
        let i = self.current_mut().block.write_constant(value);

        self.current_mut().block.write_op(Op::Constant);
        self.current_mut().block.write(i as u8);
    }

    fn patch_constant(&mut self, index: usize, constant: Value) {
        self.current_mut()
            .block
            .write_constant_at(index, constant);
    }

    fn position(&self) -> usize {
        self.current().block.code.len()
    }
}

// #[cfg(test)]
// mod compiler_tests {
//     use super::*;
//
//     #[test]
//     fn add() {
//         // Arrange
//         let source = "5 + 6";
//
//         // Act
//         let program = compile_source(source).unwrap();
//
//         // Assert
//         let generated_code = program.block.code;
//
//         debug_assert!(!generated_code.is_empty());
//         debug_assert_eq!(generated_code, vec![8, 0, 8, 1, 4]);
//     }
//
//     #[test]
//     fn let_value() {
//         // Arrange
//         let source = "
//             let a = 5;
//             a;
//         ";
//
//         // Act
//         let program = compile_source(source).unwrap();
//
//         // Assert
//         let generated_code = program.block.code;
//
//         debug_assert!(!generated_code.is_empty());
//         debug_assert_eq!(generated_code, vec![8, 0, 15, 0, 16, 0]);
//     }
//
//     fn compile_source(source: &str) -> Result<Program, ()> {
//         let mut compiler = Compiler::new(source.to_owned());
//         let program = compiler.compile();
//         program
//     }
// }
