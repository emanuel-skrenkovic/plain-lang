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
    ParseRule { prefix: None, infix: None, precedence: Precedence::Call }, // LeftParen
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // RightParen
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // LeftBracket
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
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // If
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

pub struct Global {
    name: Token,
    mutable: bool,
    global_type: String,
    scope: usize
}

pub struct Compiler {
    scanner: Scanner,
    parser: Parser,
    block: Rc<RefCell<Block>>,
    globals: Vec<Global>
}

impl Compiler {
    pub fn new(source: String, block: Rc<RefCell<Block>>) -> Compiler {
        Compiler {
            scanner: Scanner::new(source),
            parser: Parser::new(),
            block,
            globals: vec![]
        }
    }

    pub fn compile(&mut self) {
        self.advance();

        loop {
            match self.parser.current.kind {
                TokenKind::End => { break; }
                _              => { self.expression(); }
            }
        }
    }

    fn advance(&mut self) {
        self.parser.previous = self.parser.current.clone();

        loop {
            self.parser.current = self.scanner.scan_token();

            println!("{}", self.parser.current);

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

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let parse_rule = get_rule(self.parser.previous.kind);
        let prefix_rule = parse_rule.prefix;

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
        // self.emit_byte(Op::Pop);
    }

    fn binary(&mut self) {
        let operator = self.parser.previous.kind;
        let parse_rule = get_rule(operator);

        self.parse_precedence(
            Precedence::try_from(parse_rule.precedence.discriminator() + 1)
                .unwrap()
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
            _ => {}
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

    fn variable(&mut self) {
        if matches!(self.parser.previous.kind, TokenKind::Var) {
            self.var_declaration();
        } else {
            self.let_declaration();
        }
    }

    fn let_declaration(&mut self) {
        let global_name = self.parser.previous.clone();

        let variable_key = self.parse_variable(&global_name);
        if let Some(key) = variable_key {
            self.emit_byte(Op::GetVariable);
            self.emit(key as u8);

            return;
        }

        if !self.match_token(TokenKind::Equal) {
            self.parser.error_at_current("Expect definition as a part of let declaration.");
        }

        self.globals.push(Global {
            name:         global_name,
            mutable:      false,
            global_type: "string".to_owned(),
            scope:        0 // TODO
        });

        self.expression();
        self.match_token(TokenKind::Semicolon);

        self.variable_definition((self.globals.len() - 1) as u8);
    }

    fn var_declaration(&mut self) {
        todo!("Implement var declaration.")
    }

    fn variable_definition(&mut self, variable_key: u8) {
        self.emit_byte(Op::SetVariable);
        self.emit(variable_key);
    }

    fn parse_variable(&self, name: &Token) -> Option<usize> {
        self.globals
            .iter()
            .position(|g| g.name.value == name.value)

    }

    fn unit(&mut self) {
        self.emit_byte(Op::Pop);
        self.emit_constant(Value::Unit);
        self.emit_byte(Op::Pop);
    }

    fn emit_byte(&mut self, op: Op) {
        (*self.block).borrow_mut().write_op(op);
    }

    fn emit(&mut self, byte: u8) {
        (*self.block).borrow_mut().write(byte as u8);
    }

    fn emit_bytes(&mut self, a: Op, b: Op) {
        (*self.block).borrow_mut().write_op(a);
        (*self.block).borrow_mut().write_op(b);
    }

    fn emit_constant(&mut self, value: Value) {
        let i = (*self.block).borrow_mut().write_constant(value);

        (*self.block).borrow_mut().write_op(Op::Constant);
        (*self.block).borrow_mut().write(i as u8);
    }
}
