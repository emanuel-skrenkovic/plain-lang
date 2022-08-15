use std::mem::discriminant;

use crate::block::{Block, Closure, Op, Value};
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

static RULES: [ParseRule; 40] = [
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
    ParseRule { prefix: None, infix: None, precedence: Precedence::None }, // Comma
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

    scanned_tokens: Vec<Token>,

    current: Token,
    previous: Token,

    panic: bool,
    error: bool
}

impl Parser {
    #[must_use]
    pub fn new(scanner: Scanner) -> Parser {
        Parser {
            scanner,
            scanned_tokens: vec![],
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

#[derive(Clone)]
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
    parser: Parser,
    scopes: Vec<Program>,
    scope_depth: usize
}

impl Compiler {
    #[must_use]
    pub fn new(source: String) -> Compiler {
        Compiler {
            parser: Parser::new(Scanner::new(source)),
            scopes: vec![Program::new()],
            // current: Rc::new(RefCell::new(Program::new(None))),
            scope_depth: 0
        }
    }

    pub fn compile(&mut self) -> Program {
        self.parser.advance();

        // TODO: check if correct
        // self.emit_byte(Op::Frame);

        loop {
            match self.parser.current.kind {
                TokenKind::End => { break; }
                _              => { self.declaration(); }
            }
        }

        // self.emit_byte(Op::Return);

        // #horribleways
        assert!(self.scopes.len() == 1);
        self.scopes.pop().unwrap()
    }

    fn current(&self) -> &Program {
        &self.scopes[self.scope_depth]
    }

    fn current_mut(&mut self) -> &mut Program {
        &mut self.scopes[self.scope_depth]
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
        self.emit_byte(Op::Frame);

        // Reserve place for Closure value which will be written later.
        // Closure is written later because the code block needs to be populated
        // by nested code first.
        // TODO: make prettier
        let i = self.current_mut().block.write_constant(Value::Unit);
        self.current_mut().block.write(i as u8);

        // TODO
        self.begin_scope();
        self.code_block();
        self.emit_byte(Op::Return);
        // The nested block of code is fully compiled,
        // and the closure can be constructed.
        let nested_block = self.end_scope();

        let closure = Closure { code: nested_block };
        self.current_mut()
            .block
            .write_constant_at(i as usize, Value::Closure { val: closure });
    }

    // TODO: this is the result of bad design. I need to use block expression
    // code when parsing both block expressions and function (valued block expressions?).
    // Since the 'block_expression' method calls end scope, and thus consumes the closure,
    // that code cannot be reused for function declarations.
    // This is bad and it should be implemented in more reusable, functional way.
    fn code_block(&mut self) {
        // Compile code until the end of the block or the end of the program is reached.
        while !self.parser.check_token(TokenKind::RightBracket) && !self.parser.check_token(TokenKind::End) {
            self.declaration();
        }

        // Blocks are expression - this captures if the block contains a value,
        // or returns 'Unit'.
        // If the final statement in the block is a semicolon, then treat it
        // like a value-less block, else, return the last value in the block.
        let has_value = !matches!(self.parser.previous.kind, TokenKind::Semicolon);
        // If the block returns no value, all the nested values go out of scope
        // and should be popped.
        if !has_value {
            self.emit_byte(Op::Pop);
            // let count = self.current()
            //             .variables
            //             .iter()
            //             .filter(|v| v.scope > self.scope_depth)
            //             .count();

            // TODO: think about this. Is it necessary to pop all the values
            // that the closure creates, or only the last one which represents
            // its value?
            // for _ in 0..count {
            //     self.emit_byte(Op::Pop);
            // }
        }

        self.parser.consume(TokenKind::RightBracket, "Expect '}' at the end of a block expression.");
        self.parser.match_token(TokenKind::Semicolon); // Only eat the semicolon if present.
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

        // Compile the expression and then jump after the block
        // to avoid executing the code during function _declaration_.
        let variable_key = self.declare_variable(function_token, false);
        self.variable_declaration(variable_key);

        self.parser.consume(TokenKind::LeftParen, "Expected '(' after function name.");

        // TODO: Parse function arguments.
        let mut arity = 0;
        while self.parser.match_token(TokenKind::Identifier) {
            // TODO: parameters
            self.parser.match_token(TokenKind::Comma);
            arity += 1;
        }

        self.parser.consume(TokenKind::RightParen, "Expected ')' after function parameters.");

        // Parse the block expression that defines the function.
        self.parser.consume(TokenKind::LeftBracket, "Expected '{' before function body.");

        self.begin_scope();
        self.code_block();
        self.emit_byte(Op::Return);
        let function_code = self.end_scope();

        let function = Value::Function {
            name: function_name,
            arity,
            closure: Closure { code: function_code },
        };
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
        self.parser.consume(TokenKind::Semicolon, "Expect ';' after variable declaration.");

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
            self.parser.consume(TokenKind::Semicolon, "Expect ';' after variable definition.");
            self.variable_definition(index);
        } else {
            self.variable_declaration(index);
            self.parser.consume(TokenKind::Semicolon, "Expect ';' after variable declaration.");
        }
    }

    fn declare_variable(&mut self, token: Token, mutable: bool) -> u8 {
        let variable_name = token.value.clone();

        let scope_depth = self.scope_depth;
        self.current_mut()
            .variables
            .push(Variable {
                name: token,
                mutable,
                scope: scope_depth, // TODO
                defined: true
            });

        let variable_index = self.current_mut()
                                 .variables
                                 .iter()
                                 .position(|v| v.name.value == variable_name)
                                 .unwrap();

        variable_index as u8
    }

    fn parse_variable(&mut self) -> Option<u8> {
        let previous = self.parser.previous.value.clone();
        let variable_index = self.current_mut()
                                 .variables
                                 .iter()
                                 .position(|v| v.name.value == previous);

        if let Some(index) = variable_index {
            if self.parser.match_token(TokenKind::Equal) {
                if !self.current().variables[index].mutable {
                    panic!("Cannot reassign value of an immutable variable.");
                }

                let set_op = if self.current().variables[index].scope < self.scope_depth {
                    Op::SetUpvalue
                } else {
                    Op::SetVariable
                };

                self.expression();
                self.emit_byte(set_op);
            } else {
                let get_op = if self.current().variables[index].scope < self.scope_depth {
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
            self.var_decl();
        } else if self.parser.match_token(TokenKind::Let) {
            self.parser.error_at_current("'let' declaration as a part of for loop is not allowed.");
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

        let function_name_token = self.parser.peek(-2);

        if let Some(function_name) = function_name_token {
            let variable_index = self.current()
                                    .variables
                                    .iter()
                                    .position(|v| v.name.value == function_name.value);

            if let Some(index) = variable_index {
                self.emit_byte(Op::Call);
                self.emit(index as u8);
            }
        }
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

    fn position(&self) -> usize {
        self.current().block.code.len()
    }
}


#[cfg(test)]
mod compiler_tests {
    use super::*;

    #[test]
    fn add() {
        // Arrange
        let source = "5 + 6";

        // Act
        let program = compile_source(source);

        // Assert
        let generated_code = program.block.code;

        debug_assert!(!generated_code.is_empty());
        debug_assert_eq!(generated_code, vec![8, 0, 8, 1, 4]);
    }

    #[test]
    fn let_value() {
        // Arrange
        let source = "
            let a = 5;
            a;
        ";

        // Act
        let program = compile_source(source);

        // Assert
        let generated_code = program.block.code;

        debug_assert!(!generated_code.is_empty());
        debug_assert_eq!(generated_code, vec![8, 0, 15, 0, 16, 0]);
    }

    #[test]
    fn block_expression_should_not_fail() {
        // Arrange
        let source = "{ 5 }";
        let mut compiler = Compiler::new(source.to_owned());

        // Act
        compiler.block_expression();

        // Assert
        debug_assert!(!compiler.current().block.code.is_empty());
        debug_assert_eq!(compiler.scope_depth, 0);
        debug_assert_eq!(compiler.current().block.code, vec![19, 0]);

    }

    fn compile_source(source: &str) -> Program {
        let mut compiler = Compiler::new(source.to_owned());
        let program = compiler.compile();
        program
    }
}
