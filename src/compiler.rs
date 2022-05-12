use std::rc::Rc;
use std::cell::RefCell;
use std::mem::discriminant;

use crate::block::{Block, Op, Value};
use crate::scan::{Scanner, Token, TokenKind};
use crate::vm::VM;

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

static RULES: [ParseRule; 39] = [
    ParseRule { prefix: None, infix: Some(Compiler::function_invocation), precedence: Precedence::Call }, // LeftParen
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // RightParen
    ParseRule { prefix: Some(Compiler::block_expression), infix: None, precedence: Precedence::None }, // LeftBracket
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // RightBracket
    ParseRule { prefix: None, infix: Some(Compiler::left_angle), precedence: Precedence::Comparison }, // LeftAngle
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Comparison }, // RightAngle
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Questionmark
    ParseRule { prefix: Some(Compiler::unit), infix: None, precedence: Precedence::None }, // Semicolon
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Colon
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Term }, // Plus
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Term }, // Minus
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Factor }, // Star
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Factor }, // Slash
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Bang
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Equality }, // BandEqual
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Equality }, // EqualEqual
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Comparison }, // GreaterEqual
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Comparison }, // LessEqual
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Equal
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // True
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // False
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Let
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Var
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // This
    ParseRule { prefix: Some(Compiler::_if), infix: None, precedence: Precedence::None }, // If
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Else
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Break
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Continue
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Switch
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Case
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // For
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // While
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Func
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Struct
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Interface
    ParseRule { prefix: Some(Compiler::literal), infix: None, precedence: Precedence::None }, // Literal
    ParseRule { prefix: Some(Compiler::variable), infix: None, precedence: Precedence::None }, // Identifier
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Error
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }  // End
];

fn get_rule(token_kind: TokenKind) -> ParseRule {
    RULES[token_kind as usize]
}

pub struct Parser {
    current: Token,
    previous: Token,

    panic: bool,
    error: bool
}

impl std::default::Default for Parser {
    fn default() -> Self {
        Parser::new()
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            current:  Token { kind: TokenKind::Error, value: "".to_owned(), line: 0 },
            previous: Token { kind: TokenKind::Error, value: "".to_owned(), line: 0 },
            panic: false,
            error: false
        }
    }

    fn error_at_current(&mut self, message: &str) {
        self.panic = true;
        self.error = true;

        eprintln!("[line {}] Error at token '{}\n{}'", self.current.line,
                                                      self.current.value,
                                                      message);
    }
}

pub struct Variable {
    name: Token,
    mutable: bool,
    scope: usize,
    defined: bool
}

pub struct Compiler {
    source: String,
    scanner: Scanner,
    parser: Parser,
    block: Rc<RefCell<Block>>,
    variables: Vec<Variable>,
    scope_depth: usize
}

impl Compiler {
    pub fn new(source: String, block: Rc<RefCell<Block>>) -> Compiler {
        Compiler {
            source: source.clone(),
            scanner: Scanner::new(source),
            parser: Default::default(),
            block,
            variables: vec![],
            scope_depth: 0
        }
    }

    pub fn compile(&mut self) {
        self.advance();

        // TODO: check if correct
        self.emit_byte(Op::Frame);

        loop {
            match self.parser.current.kind {
                TokenKind::End => { break; }
                _              => { self.declaration(); }
            }
        }

        self.emit_byte(Op::Return);
    }

    fn advance(&mut self) {
        self.parser.previous = self.parser.current.clone();

        loop {
            self.parser.current = self.scanner.scan_token();

            if !matches!(self.parser.current.kind, TokenKind::Error) {
                break;
            }

            self.parser.error_at_current("Scanner error.");
        }
    }

    fn match_token(&mut self, token_kind: TokenKind) -> bool {
        if discriminant(&self.parser.current.kind) != discriminant(&token_kind) {
            return false
        }

        self.advance();
        true
    }

    fn consume(&mut self, token_kind: TokenKind, error_message: &str) {
        if !self.match_token(token_kind) {
            self.parser.error_at_current(error_message)
        }
    }

    fn check_token(&self, token_kind: TokenKind) -> bool {
        discriminant(&self.parser.current.kind) == discriminant(&token_kind)
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let prefix_rule  = get_rule(self.parser.previous.kind).prefix;

        if let Some(prefix) = prefix_rule {
            prefix(self);
        } else {
            self.parser.error_at_current("Prefix rule not defined.");
        }

        while precedence.discriminator() <= get_rule(self.parser.current.kind)
                                                    .precedence
                                                    .discriminator() {
            self.advance();

            let infix_rule = get_rule(self.parser.previous.kind).infix;
            infix_rule.unwrap()(self);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn block_expression(&mut self) {
        self.scope_depth += 1;

        self.emit_byte(Op::Frame);

        while !self.check_token(TokenKind::RightBracket) && !self.check_token(TokenKind::End) {
            self.declaration();
        }

        let has_value = !matches!(self.parser.previous.kind, TokenKind::Semicolon);

        self.consume(TokenKind::RightBracket, "Expect '}' at the end of a block expression.");
        self.match_token(TokenKind::Semicolon); // Only eat the semicolon if present.

        self.scope_depth -= 1;

        let count = self.variables
                        .iter()
                        .filter(|v| v.scope > self.scope_depth)
                        .count();

        if !has_value {
            for _ in 0..count {
                self.emit_byte(Op::Pop);
            }
        }

        self.emit_byte(Op::Return);
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
            TokenKind::LeftAngle    => { self.emit_byte(Op::Less); }
            TokenKind::RightAngle   => { self.emit_byte(Op::Greater); }
            TokenKind::BangEqual    => { self.emit_bytes(Op::Equal, Op::Not); }
            TokenKind::EqualEqual   => { self.emit_byte(Op::Equal); }
            TokenKind::GreaterEqual => { self.emit_byte(Op::GreaterEqual); }
            TokenKind::LessEqual    => { self.emit_byte(Op::LessEqual); }
            TokenKind::Plus         => { self.emit_byte(Op::Add); }
            TokenKind::Minus        => { self.emit_byte(Op::Subtract); }
            TokenKind::Star         => { self.emit_byte(Op::Multiply); }
            TokenKind::Slash        => { self.emit_byte(Op::Divide); }
            _ => { }
        }
    }

    fn left_angle(&mut self) {
        if self.match_token(TokenKind::Identifier) {
            self.match_token(TokenKind::RightAngle);
            // TODO generics
        } else {
            self.binary();
        }
    }

    fn literal(&mut self) {
        let value = Value::Number {
            val: self.parser.previous.value.parse::<i32>().unwrap()
        };
        self.emit_constant(value);
    }

    fn declaration(&mut self) {
        match self.parser.current.kind {
            TokenKind::Func => {
                self.advance();
                self.function_declaration();
            }
            TokenKind::Var => {
                self.advance();
                self.var_declaration();
            },
            TokenKind::Let => {
                self.advance();
                self.let_declaration();
            },
            TokenKind::While => {
                self.advance();
                self._while();
            },
            TokenKind::For => {
                self.advance();
                self._for();
            }
            _ => self.expression()
        }
    }

    fn variable(&mut self) {
        self.parse_variable();
    }

    fn function_declaration(&mut self) {
        self.consume(TokenKind::Identifier, "Cannot declare function without name.");

        let function_token = self.parser.previous.clone();
        let function_name = function_token.value.clone();

        self.consume(TokenKind::LeftParen, "Expected '(' after function name.");

        while self.match_token(TokenKind::Identifier) {
            // self.match_token(TokenKind::Comma);
        }

        self.consume(TokenKind::RightParen, "Expected ')' after function parameters.");

        self.variables.push(Variable {
            name:         function_token,
            mutable:      false,
            scope:        self.scope_depth, // TODO
            defined:      true
        });
        let function_variable_index = self.variables
                                 .iter()
                                 .position(|v| v.name.value == function_name);

        self.block_expression();
        self.variable_definition(function_variable_index.unwrap() as u8);
    }

    fn let_declaration(&mut self) {
        self.match_token(TokenKind::Identifier);

        let variable_token = self.parser.previous.clone();
        let variable_name  = variable_token.value.clone();

        let variable_exists = self.variables
                                  .iter()
                                  .any(|v| v.name.value == variable_name && v.scope <= self.scope_depth);
        if variable_exists {
            self.parser.error_at_current(&format!("Cannot redeclare variable with name '{}'.", variable_name));
        }

        self.consume(TokenKind::Equal, "Expect definition as a part of let declaration.");

        self.variables.push(Variable {
            name:         variable_token,
            mutable:      false,
            scope:        self.scope_depth, // TODO
            defined:      true
        });
        let variable_index = self.variables
                                 .iter()
                                 .position(|v| v.name.value == variable_name);

        self.expression();
        self.match_token(TokenKind::Semicolon);

        self.variable_definition(variable_index.unwrap() as u8);
    }

    fn var_declaration(&mut self) {
        self.match_token(TokenKind::Identifier);

        let variable_token = self.parser.previous.clone();
        let variable_name  = variable_token.value.clone();

        let variable_exists = self.variables
                                  .iter()
                                  .any(|v| v.name.value == variable_name && v.scope <= self.scope_depth);
        if variable_exists {
            self.parser.error_at_current(&format!("Cannot redeclare variable with name '{}'.", variable_name));
        }

        if self.parse_variable().is_some() {
            return
        }

        let mut variable = Variable {
            name:    variable_token,
            mutable: true,
            scope:   self.scope_depth, // TODO
            defined: false
        };

        let variable_index = (self.variables.len()) as u8; // TODO: becomes a mess later on.

        if self.match_token(TokenKind::Equal) {
            self.expression();
            self.match_token(TokenKind::Semicolon);

            variable.defined = true;

            self.variable_definition(variable_index);
        } else {
            self.emit_constant(Value::Unit);
            self.variable_definition(variable_index);
        }

        self.variables.push(variable);
    }

    fn parse_variable(&mut self) -> Option<u8> {
        let variable_index = self.variables
                                 .iter()
                                 .position(|v| v.name.value == self.parser.previous.value);

        if let Some(index) = variable_index {
            let set_op = if self.variables[index].scope < self.scope_depth {
                Op::SetUpvalue
            } else {
                Op::SetVariable
            };

            let get_op = if self.variables[index].scope < self.scope_depth {
                Op::GetUpvalue
            } else {
                Op::GetVariable
            };

            if self.match_token(TokenKind::Equal) {
                if !self.variables[index].mutable {
                    panic!("Cannot reassign value of an immutable variable.");
                }

                self.expression();
                self.emit_byte(set_op);
            } else {
                self.emit_byte(get_op);
            }

            self.emit(index as u8);
            return Some(index as u8)
        }

        None
    }

    fn variable_definition(&mut self, variable_key: u8) {
        self.emit_byte(Op::SetVariable);
        self.emit(variable_key);
    }

    fn _if(&mut self) {
        self.expression();

        let then_jump = self.emit_jump(Op::CondJump);
        self.declaration();

        let else_jump = self.emit_jump(Op::Jump);
        self.patch_jump(then_jump);

        if self.match_token(TokenKind::Else) {
            self.declaration();
        }

        self.patch_jump(else_jump);
    }

    fn _while(&mut self) {
        let loop_start = self.position();
        self.expression();

        let break_jump = self.emit_jump(Op::CondJump);
        self.declaration();
        self.emit_loop(loop_start);

        self.patch_jump(break_jump);
    }

    // In this implementation, all the parts of a for
    // loop declaration are required. While and iterators (when I get to that)
    // will make up for everything.
    fn _for(&mut self) {
        if self.match_token(TokenKind::Var) {
            self.var_declaration();
        } else if self.match_token(TokenKind::Let) {
            self.parser.error_at_current("'let' declaration as a part of for loop is not allowed.")
        } else {
            self.expression();
        }

        let mut loop_start = self.position();
        // condition
        self.expression();

        let exit_jump = self.emit_jump(Op::CondJump);
        // advancement statement
        let body_jump = self.emit_jump(Op::Jump);
        let advancement_start = self.position();
        self.expression();

        self.emit_loop(loop_start);

        loop_start = advancement_start;
        self.patch_jump(body_jump);

        // body
        self.block_expression();
        self.emit_loop(loop_start);
        self.patch_jump(exit_jump);
    }

    fn function_invocation(&mut self) {
        /*
        let block = Rc::new(RefCell::new(Block::new(256)));
        let mut compiler = Compiler::new(
            self.source.clone(),
            block.clone()
        );

        compiler.compile();

        let mut vm = VM::new(block);
        vm.interpret();
        */
        self.emit_byte(Op::Call);
    }

    fn unit(&mut self) {
        self.emit_byte(Op::Pop);
    }

    fn patch(&mut self, index: usize, byte: u8) {
        (*self.block).borrow_mut().write_at(index, byte);
    }

    fn patch_jump(&mut self, index: usize) {
        let code_len = (*self.block).borrow_mut().code.len();
        self.patch(index, (code_len - 1 - index) as u8);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        let loop_end = (*self.block).borrow_mut().code.len();
        self.emit_byte(Op::LoopJump);
        self.emit((loop_end - loop_start + 2) as u8);
    }

    fn emit_jump(&mut self, op: Op) -> usize {
        self.emit_byte(op);
        self.emit(0)
    }

    fn emit_byte(&mut self, op: Op) -> usize {
        (*self.block).borrow_mut().write_op(op)
    }

    fn emit(&mut self, byte: u8) -> usize {
        (*self.block).borrow_mut().write(byte as u8)
    }

    fn emit_bytes(&mut self, a: Op, b: Op) -> usize {
        (*self.block).borrow_mut().write_op(a);
        (*self.block).borrow_mut().write_op(b)
    }

    fn emit_constant(&mut self, value: Value) {
        let i = (*self.block).borrow_mut().write_constant(value);

        (*self.block).borrow_mut().write_op(Op::Constant);
        (*self.block).borrow_mut().write(i as u8);
    }

    fn position(&self) -> usize {
        (*self.block).borrow().code.len()
    }
}
