use std::rc::Rc;
use std::cell::RefCell;

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
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // LeftAngle
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // RightAngle
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Questionmark
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Semicolon
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Colon
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Term }, // Plus
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Term }, // Minus
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Factor }, // Star
    ParseRule { prefix: None, infix: Some(Compiler::binary), precedence: Precedence::Factor }, // Slash
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Bang
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // BandEqual
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // EqualEqual
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // GreaterEqual
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // LessEqual
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
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Identifier
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

    fn error_at_current(&mut self) {
        self.panic = true;
        self.error = true;

        println!("[line {}] Error at token '{}'", self.current.line,
                                                  self.current.value);
    }
}

pub struct Compiler {
    scanner: Scanner,
    parser: Parser,
    block: Rc<RefCell<Block>>
}

impl Compiler {
    pub fn new(source: String, block: Rc<RefCell<Block>>) -> Compiler {
        Compiler {
            scanner: Scanner::new(source),
            parser: Parser::new(),
            block
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

            println!("Token: {}", self.parser.current);

            if !matches!(self.parser.current.kind, TokenKind::Error) {
                break;
            }

            self.parser.error_at_current();
        }
    }

    fn match_token(&self, token_kind: TokenKind) -> bool {
        matches!(&self.parser.current.kind, token_kind)
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let parse_rule = get_rule(self.parser.previous.kind);
        let prefix_rule = parse_rule.prefix;

        if let Some(prefix) = prefix_rule {
            prefix(self);
        } else {
            self.parser.error_at_current();
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
        self.emit_byte(Op::Pop);
    }

    fn binary(&mut self) {
        let operator = self.parser.previous.kind;
        let parse_rule = get_rule(operator);

        self.parse_precedence(
            Precedence::try_from(parse_rule.precedence.discriminator() + 1)
                .unwrap()
        );

        match operator {
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

    fn literal(&mut self) {
        let value = Value::Number {
            val: self.parser.previous.value.parse::<i32>().unwrap()
        };
        self.emit_constant(value);
    }

    fn emit_byte(&mut self, op: Op) {
        (*self.block).borrow_mut().write_op(op);
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
