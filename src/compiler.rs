use std::rc::Rc;
use std::cell::RefCell;
use std::mem::discriminant;

use crate::block::{Block, Op, Value};
use crate::scan::{Scanner, Token, TokenKind};

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
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Semicolon
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
    scanner: Scanner,

    current: Token,
    previous: Token,

    panic: bool,
    error: bool
}

impl Parser {
    pub fn new(scanner: Scanner) -> Parser {
        Parser {
            scanner,
            current: Token::default(),
            previous: Token::default(),
            panic: false,
            error: false
        }
    }

    fn advance(&mut self) {
        self.previous = self.current.clone();

        loop {
            self.current = self.scanner.scan_token();

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

    fn consume(&mut self, token_kind: TokenKind, error_message: &str) {
        if !self.match_token(token_kind) {
            self.error_at_current(error_message)
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
    parser: Parser,
    block: Rc<RefCell<Block>>,
    variables: Vec<Variable>,
    scope_depth: usize
}

impl Compiler {
    pub fn new(source: String, block: Rc<RefCell<Block>>) -> Compiler {
        Compiler {
            parser: Parser::new(Scanner::new(source)),
            block,
            variables: vec![],
            scope_depth: 0
        }
    }

    pub fn compile(&mut self) {
        self.parser.advance();

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

    fn check_token(&self, token_kind: TokenKind) -> bool {
        discriminant(&self.parser.current.kind) == discriminant(&token_kind)
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.parser.advance();

        let prefix_rule  = get_rule(self.parser.previous.kind).prefix;

        if let Some(prefix) = prefix_rule {
            prefix(self);
        } else {
            self.parser.error_at_current("Prefix rule not defined.");
        }

        while precedence.discriminator() <= get_rule(self.parser.current.kind)
                                                    .precedence
                                                    .discriminator() {
            self.parser.advance();

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

        self.parser.consume(TokenKind::RightBracket, "Expect '}' at the end of a block expression.");
        self.parser.match_token(TokenKind::Semicolon); // Only eat the semicolon if present.

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
        let value = Value::Number {
            val: self.parser.previous.value.parse::<i32>().unwrap()
        };
        self.emit_constant(value);
    }

    fn declaration(&mut self) {
        match self.parser.current.kind {
            TokenKind::Func => {
                self.parser.advance();
                self.function_decl();
            }
            TokenKind::Var => {
                self.parser.advance();
                self.var_declaration();
            },
            TokenKind::Let => {
                self.parser.advance();
                self.let_declaration();
            },
            TokenKind::While => {
                self.parser.advance();
                self._while();
            },
            TokenKind::For => {
                self.parser.advance();
                self._for();
            }
            _ => self.expression()
        }
    }

    fn variable(&mut self) {
        self.parse_variable();
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

        self.parser.consume(TokenKind::LeftParen, "Expected '(' after function name.");

        while self.parser.match_token(TokenKind::Identifier) {
            // TODO: parameters
            // self.match_token(TokenKind::Comma);
        }

        self.parser.consume(TokenKind::RightParen, "Expected ')' after function parameters.");

        let index = self.declare_variable(function_token, false);

        self.block_expression();
        self.variable_definition(index);
    }

    fn let_declaration(&mut self) {
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
        self.parser.consume(TokenKind::Semicolon, "Expect ';' after variable declaration.");

        self.variable_definition(index);
    }

    fn var_declaration(&mut self) {
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

        if self.parser.match_token(TokenKind::Equal) {
            self.expression();
            self.parser.consume(TokenKind::Semicolon, "Expect ';' after variable declaration.");
            self.variable_definition(index);
        } else {
            self.variable_declaration(index);
        }
    }

    fn declare_variable(&mut self, token: Token, mutable: bool) -> u8 {
        let variable_name = token.value.clone();

        self.variables.push(Variable {
            name:         token,
            mutable,
            scope:        self.scope_depth, // TODO
            defined:      true
        });

        let variable_index = self.variables
                                 .iter()
                                 .position(|v| v.name.value == variable_name)
                                 .unwrap();

        variable_index as u8
    }

    fn parse_variable(&mut self) -> Option<u8> {
        let variable_index = self.variables
                                 .iter()
                                 .position(|v| v.name.value == self.parser.previous.value);

        if let Some(index) = variable_index {
            if self.parser.match_token(TokenKind::Equal) {
                if !self.variables[index].mutable {
                    panic!("Cannot reassign value of an immutable variable.");
                }

                let set_op = if self.variables[index].scope < self.scope_depth {
                    Op::SetUpvalue
                } else {
                    Op::SetVariable
                };

                self.expression();
                self.emit_byte(set_op);
            } else {
                let get_op = if self.variables[index].scope < self.scope_depth {
                    Op::GetUpvalue
                } else {
                    Op::GetVariable
                };

                self.emit_byte(get_op);
            }

            self.emit(index as u8);
            return Some(index as u8)
        }

        None
    }

    fn variable_exists(&self, name: &str) -> bool {
        self.variables
            .iter()
            .any(|v| v.name.value == name && v.scope <= self.scope_depth)
    }

    fn variable_declaration(&mut self, variable_key: u8) {
        let mut variable = self.variables.get_mut(variable_key as usize).unwrap();
        variable.defined = false;

        self.emit_byte(Op::DeclareVariable);
    }

    fn variable_definition(&mut self, variable_key: u8) {
        let mut variable = self.variables.get_mut(variable_key as usize).unwrap();
        variable.defined = true;

        self.emit_byte(Op::SetVariable);
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
        if self.parser.match_token(TokenKind::Var) {
            self.var_declaration();
        } else if self.parser.match_token(TokenKind::Let) {
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

    // TODO: don't know what to do with this or if it is
    // even needed.
    // fn unit(&mut self) {
    //     self.emit_byte(Op::Pop);
    // }

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
