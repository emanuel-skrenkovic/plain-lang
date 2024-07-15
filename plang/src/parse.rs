use crate::{ast, scan, error};


// params, return_type, argument_type_names, body

pub struct Function
{
    left_paren: scan::Token,
    right_paren: scan::Token,
    params: Vec<scan::Token>, 
    param_types: Vec<scan::Token>,
    return_type: Option<scan::Token>, 
    body: Vec<Box<ast::Stmt>>,
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

type ParseFn = fn(&mut Parser) -> ast::Expr;

#[derive(Copy, Clone)]
struct ParseRule
{
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence
}

static RULES: [ParseRule; 42] =
[
    ParseRule { prefix: Some(Parser::function_expression), infix: Some(Parser::function_invocation), precedence: Precedence::Call }, // LeftParen
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // RightParen
    ParseRule { prefix: Some(Parser::block_expression),    infix: None,                              precedence: Precedence::None }, // LeftBracket
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // RightBracket
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Comparison }, // LeftAngle
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Comparison }, // RightAngle
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Questionmark
    ParseRule { prefix: Some(Parser::semicolon),           infix: None,                              precedence: Precedence::None }, // Semicolon
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Colon
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // ColonColon
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // ColonEquals
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Term }, // Plus
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Term }, // Minus
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Factor }, // Star
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Factor }, // Slash
    ParseRule { prefix: None,                              infix: Some(Parser::dot_operator),        precedence: Precedence::Call }, // Dot
    ParseRule { prefix: None,                              infix: Some(Parser::pipe),                precedence: Precedence::Call }, // Pipe
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Comma
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Bang
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Equality }, // BandEqual
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Equality }, // EqualEqual
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Comparison }, // GreaterEqual
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Comparison }, // LessEqual
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Equal
    ParseRule { prefix: Some(Parser::literal),             infix: None,                              precedence: Precedence::None }, // True
    ParseRule { prefix: Some(Parser::literal),             infix: None,                              precedence: Precedence::None }, // False
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // This
    ParseRule { prefix: Some(Parser::_if),                 infix: None,                              precedence: Precedence::None }, // If
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Else
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Break
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Continue
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Switch
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Case
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // For
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // While
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Func
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Struct
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Interface
    ParseRule { prefix: Some(Parser::literal),             infix: None,                              precedence: Precedence::None }, // Literal
    ParseRule { prefix: Some(Parser::variable),            infix: None,                              precedence: Precedence::None }, // Identifier
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Error
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }  // End
];

fn get_rule(token_kind: scan::TokenKind) -> ParseRule
{
    RULES[token_kind as usize]
}

pub struct TokenReader
{
    // Also keep the source in the parser for better error reporting.
    // Feel like this solution is bad, and that I should avoid cloning the source so much.
    // Got no other ideas, just feels that way currently.
    reporter: error::ErrorReporter,
    tokens: Vec<scan::Token>,
    scanned_tokens: Vec<scan::Token>,

    current_index: usize,

    current: scan::Token,
    previous: scan::Token,

    panic: bool,
}

impl TokenReader
{
    #[must_use]
    pub fn new(reporter: error::ErrorReporter, tokens: Vec<scan::Token>) -> Self
    {
        Self {
            reporter,
            tokens,
            scanned_tokens: Vec::with_capacity(1024 * 8),
            current_index: 0,
            current: scan::Token::default(),
            previous: scan::Token::default(),
            panic: false,
        }
    }

    fn advance(&mut self) -> Result<(), error::CompilerError>
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

    fn match_token(&mut self, token_kind: scan::TokenKind) -> Result<bool, error::CompilerError>
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

    fn consume(&mut self, token_kind: scan::TokenKind, error_message: &str) -> Result<(), error::CompilerError>
    {
        match self.match_token(token_kind) {
            Ok(true)  => Ok(()),
            Ok(false) => Err(self.error_at(error_message, &self.current.clone())),
            Err(err)  => Err(err),
        }
    }

    fn error_at(&mut self, message: &str, token: &scan::Token) -> error::CompilerError
    {
        self.reporter.error_at(message, error::CompilerErrorKind::ParseError, token)
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

pub struct Parser
{
    reader: TokenReader,

    scope_depth: usize,
    stack: Vec<ast::Expr>,
}

impl Parser
{
    #[must_use]
    pub fn new(reporter: error::ErrorReporter, tokens: Vec<scan::Token>) -> Self
    {
        Self {
            reader: TokenReader::new(reporter, tokens),
            scope_depth: 0,
            stack: Vec::with_capacity(1024),
        }
    }

    pub fn compile(mut self) -> Result<Vec<ast::Node>, Vec<error::CompilerError>>
    {
        let _ = self.reader.advance();

        // TODO: should probably enclose program itself.

        let mut nodes: Vec<ast::Node> = Vec::with_capacity(1024 * 8);

        loop {
            match self.reader.current.kind {
                scan::TokenKind::End => break,
                _ => {
                    let decl = self.declaration();
                    if let ast::Stmt::Expr { ref expr } = decl {
                        if let ast::ExprInfo { value: ast::Expr::Bad { token }, .. } = expr.as_ref() {
                            panic!("{:?}", token);
                        }
                    }
                    nodes.push(ast::Node::Stmt(decl));
                }
            }
        }

        if self.reader.panic {
            return Err(self.reader.reporter.errors)
        }

        Ok(nodes)
    }

    fn is_at_end(&self) -> bool
    {
        self.reader.current.kind.discriminant() == scan::TokenKind::End.discriminant()
    }

    fn parse_precedence(&mut self, prec: Precedence)
    {
        let _ = self.reader.advance();

        match get_rule(self.reader.previous.kind).prefix {
            Some(prefix) => {
                let prefix_expr = prefix(self);
                self.stack.push(prefix_expr);
            },
            _ => {
                if self.is_at_end() { return }
                panic!("{}", self.reader.error_at("Expect expression.", &self.reader.current.clone()))
            }
        }

        while prec.discriminator() <= get_rule(self.reader.current.kind).precedence.discriminator() {
            let _ = self.reader.advance();

            let Some(infix_rule) = get_rule(self.reader.previous.kind).infix else {
                self.error_at("Failed to get infix rule.", &self.reader.previous.clone());
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
        let (left_bracket, right_bracket, statements, value) = self.block();

        let value = ast::ExprInfo::new(value);
        let value = Box::new(value);
        
        ast::Expr::Block { 
            left_bracket, 
            right_bracket, 
            statements, 
            value,
        }
    }

    fn binary(&mut self) -> ast::Expr
    {
        let operator   = self.reader.previous.clone();
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

    fn dot_operator(&mut self) -> ast::Expr
    {
        let instance_name = self.reader.peek(-2).unwrap().clone();

        self.consume(scan::TokenKind::Identifier, "Expect identifier.");
        let member_name = self.reader.previous.clone();

        ast::Expr::MemberAccess { instance_name, member_name }
    }

    fn literal(&mut self) -> ast::Expr
    {
        let expr = ast::Expr::Literal { value: self.reader.previous.clone() };

        // Eat the semicolon only if present;
        self.match_token(scan::TokenKind::Semicolon);
        expr
    }

    fn declaration(&mut self) -> ast::Stmt
    {
        match self.reader.current.kind {
            scan::TokenKind::While => {
                let _ = self.reader.advance();
                self._while()
            },
            scan::TokenKind::For => {
                let _ = self.reader.advance();
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
        let next = self.reader.peek(1)?;

        let next_kind        = next.kind.discriminant();
        let second_next_kind = self.reader.peek(2)?.kind.discriminant();

        if second_next_kind == scan::TokenKind::Struct.discriminant() {
            return Some(self._struct());
        }

        let type_definition = next_kind == scan::TokenKind::Colon.discriminant();
        let immutable       = next_kind == scan::TokenKind::ColonColon.discriminant();
        let mutable         = next_kind == scan::TokenKind::ColonEquals.discriminant();

        if !(type_definition || immutable || mutable) {
            return None
        }

        let name = self.reader.current.clone();
        let _    = self.reader.advance();

        let mut type_name: Option<scan::Token> = None;
        if type_definition {
            let _ = self.match_token(scan::TokenKind::Colon);
            self.consume(scan::TokenKind::Identifier, "Expected identifier");

            type_name = Some(self.reader.previous.clone());

            if !self.match_token(scan::TokenKind::Equal) && !self.match_token(scan::TokenKind::Colon) {
                let error = self.error_at
                (
                    "Expected token '=' or token ':' after type definition.",
                    &self.reader.current.clone()
                );
                let error = ast::ExprInfo::new(error);
                let error = Box::new(error);

                return Some(ast::Stmt::Expr { expr: error })
            }
        } else if mutable || immutable {
            let mutable = if mutable { scan::TokenKind::ColonEquals } else { scan::TokenKind::ColonColon };
            self.consume(mutable, "Expected ':=' or '::' after identifier.");
        } else {
            return None
        }

        // Function declaration.
        // TODO: this needs to happen only in global scope, in other scopes, the function
        // value is the result of an expression instead of it being a statement.
        if self.reader.current.kind.discriminant() == scan::TokenKind::LeftParen.discriminant() && self.scope_depth == 0 {
            // Cannot use match here because this is only for global scope - we could
            // unintentionally advance.
            let _ = self.reader.advance();

            // Declare self first to allow recursion.
            let function = self.function();

            let stmt = ast::Stmt::Function {
                name,
                params: function.params,
                return_type: function.return_type,
                param_types: function.param_types,
                body: function.body,
            };
            return Some(stmt)
        }

        let initializer = self.expression();
        let initializer = ast::ExprInfo::new(initializer);
        let initializer = Box::new(initializer);

        self.match_token(scan::TokenKind::Semicolon);

        let stmt = if mutable { ast::Stmt::Var   { name, type_name, initializer } } 
                   else       { ast::Stmt::Const { name, type_name, initializer } };

        Some(stmt)
    }

    fn variable_statement(&mut self) -> Option<ast::Stmt>
    {
        let next      = self.reader.peek(1)?;
        let next_kind = next.kind.discriminant();

        let type_definition = next_kind == scan::TokenKind::Colon.discriminant();
        let immutable       = next_kind == scan::TokenKind::ColonColon.discriminant();
        let mutable         = next_kind == scan::TokenKind::ColonEquals.discriminant();

        if !(type_definition || immutable || mutable) {
            return None
        }

        let name = self.reader.current.clone();
        let _    = self.reader.advance();

        let mut type_name: Option<scan::Token> = None;
        match (type_definition, immutable, mutable) {
            (true, _, _) => {
                let _ = self.match_token(scan::TokenKind::Colon);
                self.consume(scan::TokenKind::Identifier, "Expect identifier");

                type_name = Some(self.reader.previous.clone());

                if !self.match_token(scan::TokenKind::Equal) && !self.match_token(scan::TokenKind::Colon) {
                    let error = self.error_at("Expected token '=' or ':' after type identifier.", &self.reader.current.clone());
                    let error = ast::ExprInfo::new(error);
                    return Some(ast::Stmt::Expr { expr: Box::new(error) })
                }
            }

            (false, true, _) | (false, _, true) => {
                self.match_token(
                    if mutable { scan::TokenKind::ColonEquals } else { scan::TokenKind::ColonColon }
                );
            }

            _ => { return None }
        }

        let initializer = self.expression();
        let initializer = ast::ExprInfo::new(initializer);
        let initializer = Box::new(initializer);

        self.match_token(scan::TokenKind::Semicolon);
        
        let stmt = if mutable { ast::Stmt::Var   { name, type_name, initializer } } 
                   else       { ast::Stmt::Const { name, type_name, initializer } };

        Some(stmt)
    }

    // If the variable type is specified, then assign the variable type here.
    // Otherwise, infer the type (if possible) from the value assigned to the variable
    // during type checking.
    // I can already see problems forming with this inference system. sadface
    fn variable(&mut self) -> ast::Expr
    {
        let name = self.reader.previous.clone();

        // If the next token is equal, handle assignment expression.
        if self.match_token(scan::TokenKind::Equal) {
            let value_expr = self.expression();
            let value_expr = ast::ExprInfo::new(value_expr);

            return ast::Expr::Assignment {
                name,
                value: Box::new(value_expr),
            }
        } else if self.match_token(scan::TokenKind::LeftBracket) {
            return self.struct_expression()
        } else if self.match_token(scan::TokenKind::Dot) {
            self.consume(scan::TokenKind::Identifier, "Expect identifier.");

            let member_name = self.reader.previous.clone();

            if self.match_token(scan::TokenKind::Equal) {
                let value = self.expression();
                let value = ast::ExprInfo::new(value);
                let value = Box::new(value);

                self.match_token(scan::TokenKind::Semicolon);

                return ast::Expr::MemberAssignment { 
                    instance_name: name,
                    member_name,
                    value,
                }
            } else {
                return ast::Expr::MemberAccess { instance_name: name, member_name };
            }
        }

        // Handles variable expression here.
        self.match_token(scan::TokenKind::Semicolon);
        ast::Expr::Variable { name }
    }

    fn function_expression(&mut self) -> ast::Expr
    {
        // let (params, return_type, param_types, body) = self.function();
        let function = self.function();
        ast::Expr::Function { 
            left_paren: function.left_paren,
            right_paren: function.right_paren,
            params: function.params, 
            return_type: function.return_type, 
            param_types: function.param_types, 
            body: function.body
        }
    }

    fn function(&mut self) -> Function
    {
        self.begin_scope();

        // a :: ()
        // ^ __
        // 3 21
        // let function_token = self.reader.peek(-3).unwrap().clone();
        // let function_name = function_token.value.clone();

        let left_paren = self.reader.previous.clone();

        let mut param_types = Vec::with_capacity(512);
        let mut params      = Vec::with_capacity(512);

        if !self.reader.check_token(scan::TokenKind::RightParen) {
            loop {
                self.consume(scan::TokenKind::Identifier, "Expect parameter identifier after '('.");

                let parameter_name_token = self.reader.previous.clone();
                params.push(parameter_name_token.clone());

                self.consume(scan::TokenKind::Colon, "Expect type definition");
                self.consume(scan::TokenKind::Identifier, "Expected type identifier");

                let type_name = &self.reader.previous;
                param_types.push(type_name.clone());

                if !self.match_token(scan::TokenKind::Comma) { break }
            }
        }

        self.consume(scan::TokenKind::RightParen, "Expect ')' after end of lambda parameters.");
        let right_paren = self.reader.previous.clone();

        self.consume(scan::TokenKind::Colon, "Expect ':' after function parameters.");

        let return_type = if self.match_token(scan::TokenKind::Identifier) {
            Some(self.reader.previous.clone())
        } else {
            None
        };

        self.consume(scan::TokenKind::LeftBracket, "Expect token '{' after function definition.");

        let mut body = Vec::with_capacity(512);

        while !self.reader.check_token(scan::TokenKind::RightBracket) && !self.reader.check_token(scan::TokenKind::End) {
            let statement = self.declaration();
            let statement = Box::new(statement);
            body.push(statement);
        }

        self.consume(scan::TokenKind::RightBracket, "Expect '}' at the end of a block expression.");

        self.end_scope();

        Function {
            left_paren,
            right_paren,
            params, 
            param_types,
            return_type, 
            body,
        }
    }

    // fn block(&mut self) -> (Vec<Box<ast::Stmt>>, ast::Expr)
    fn block(&mut self) -> (scan::Token, scan::Token, Vec<Box<ast::Stmt>>, ast::Expr)
    {
        let left_bracket = self.reader.previous.clone();

        self.begin_scope();

        let mut statements = Vec::with_capacity(1024);

        // Compile code until the end of the block or the end of the program is reached.
        while !self.reader.check_token(scan::TokenKind::RightBracket) && !self.reader.check_token(scan::TokenKind::End) {
            let statement = self.declaration();
            let statement = Box::new(statement);
            statements.push(statement);
        }

        // Blocks are expressions - this captures if the block contains a value,
        // or returns 'Unit'.
        // If the final statement in the block is a semicolon, then treat it
        // like a value-less block, else, return the last value in the block.
        let has_value = !matches!(self.reader.previous.kind, scan::TokenKind::Semicolon)
                        && !statements.is_empty();

        self.consume(scan::TokenKind::RightBracket, "Expect '}' at the end of a block expression.");
        let right_bracket = self.reader.previous.clone();

        self.match_token(scan::TokenKind::Semicolon);

        // TODO: need to take tokens into account here:
        // if the last expression in the block ends with a semicolon,
        // it is of Unit value;
        let expr = if has_value {
            let binding                  = statements.pop().unwrap();
            let ast::Stmt::Expr { expr } = binding.as_ref() else {
                // TODO: I don't like this panic here.
                panic!();
            };

            expr.to_owned().value
        } else {
            ast::Expr::Literal { value: scan::Token::default() }
        };

        self.end_scope();
        (left_bracket, right_bracket, statements, expr)
    }

    fn _if(&mut self) -> ast::Expr
    {
        let token = self.reader.previous.clone();

        let condition = self.expression();

        self.consume(scan::TokenKind::LeftBracket, "Expect '{");

        let (_, _, then_branch, then_value) = self.block();

        // This looks ugly. :(
        let (else_branch, else_value) = if self.match_token(scan::TokenKind::Else) {
            self.consume(scan::TokenKind::LeftBracket, "Expect '{");

            let (_, _, branch, value) = self.block();
            (branch, value)
        } else {
            (vec![], ast::Expr::Literal { value: scan::Token::default() })
        };

        let condition  = ast::ExprInfo::new(condition);
        let condition  = Box::new(condition);
        let then_value = Box::new(ast::ExprInfo::new(then_value));
        let else_value = Box::new(ast::ExprInfo::new(else_value));

        ast::Expr::If {
            token,
            condition,
            then_branch,
            then_value,
            else_branch,
            else_value,
        }
    }

    fn _while(&mut self) -> ast::Stmt
    {
        let while_token = self.reader.previous.clone();
        let condition   = self.expression();

        // Body
        self.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        let mut body = Vec::with_capacity(1024);

        while !self.reader.check_token(scan::TokenKind::RightBracket) && !self.reader.check_token(scan::TokenKind::End) {
            let stmt = self.declaration();
            let stmt = Box::new(stmt);
            body.push(stmt);
        }

        self.match_token(scan::TokenKind::RightBracket);

        let condition = ast::ExprInfo::new(condition);
        let condition = Box::new(condition);

        ast::Stmt::While {
            token: while_token,
            condition,
            body,
        }
    }

    // In this implementation, all the parts of a for
    // loop declaration are required. While and iterators (when I get to that)
    // will make up for everything.
    fn _for(&mut self) -> ast::Stmt
    {
        let for_token = self.reader.previous.clone();
         
        // TODO: no unwrap
        let variable = self.variable_statement().unwrap();
        let variable = Box::new(variable);

        let condition = self.expression();
        let condition = ast::ExprInfo::new(condition);
        let condition = Box::new(condition);

        // TODO: needs to be just unary statement.
        let advancement = self.declaration();
        let advancement = Box::new(advancement);

        self.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        let mut body: Vec<Box<ast::Stmt>> = Vec::with_capacity(1024);

        // Compile code until the end of the block or the end of the program is reached.
        while !self.reader.check_token(scan::TokenKind::RightBracket) && !self.reader.check_token(scan::TokenKind::End) {
            let declaration = self.declaration();
            let declaration = Box::new(declaration);
            body.push(declaration);
        }

        self.consume(scan::TokenKind::RightBracket, "Expect '}' after the 'for' block.");

        // end body

        ast::Stmt::For {
            token: for_token,
            initializer: variable,
            condition,
            advancement,
            body,
        }
    }

    fn function_invocation(&mut self) -> ast::Expr
    {
        let Some(token) = self.reader.peek(-2) else {
            return self
                .error_at("Failed to parse function - no function name found.", &self.reader.previous.clone());
        };

        let function_name_token = token.clone();

        let mut arguments: Vec<Box<ast::ExprInfo>> = Vec::with_capacity(256);

        if !self.reader.check_token(scan::TokenKind::RightParen) {
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

        self.match_token(scan::TokenKind::Semicolon);

        ast::Expr::Call { name: function_name_token, arguments }
    }

    fn struct_expression(&mut self) -> ast::Expr
    {
        let Some(token) = self.reader.peek(-2) else {
            return self
                .error_at("Failed to parse struct expression - no struct name found.", &self.reader.previous.clone());
        };

        let name = token.clone();

        let mut members = Vec::with_capacity(1024);
        let mut values  = Vec::with_capacity(1024);

        if !self.reader.check_token(scan::TokenKind::RightParen) {
            loop {
                self.consume(scan::TokenKind::Identifier, "Expect member ame.");

                let member_name = self.reader.previous.clone();
                members.push(member_name);

                self.consume(scan::TokenKind::Colon, "Expect ':' after member initializer name.");

                let expr = self.expression();
                let expr = ast::ExprInfo::new(expr);
                let expr = Box::new(expr);
                values.push(expr);

                self.match_token(scan::TokenKind::Comma);

                if self.match_token(scan::TokenKind::RightBracket) { break }
            }
        }

        ast::Expr::Struct { name, members, values }
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

    fn _struct(&mut self) -> ast::Stmt
    {
        self.consume(scan::TokenKind::Identifier, "Expect struct name identifier.");
        let struct_name = self.reader.previous.clone();

        self.consume(scan::TokenKind::ColonColon, "Expect '::' after struct identifier.");
        self.consume(scan::TokenKind::Struct, "Expect 'struct' token.");

        self.consume(scan::TokenKind::LeftBracket, "Expect '{' on struct definition.");

        let mut members      = Vec::with_capacity(1024);
        let mut member_types = Vec::with_capacity(1024);

        while !self.reader.check_token(scan::TokenKind::RightBracket) && !self.reader.check_token(scan::TokenKind::End) {
            self.consume(scan::TokenKind::Identifier, "Expect identifier.");
            let name_token = self.reader.previous.clone();

            if self.match_token(scan::TokenKind::Colon) {
                self.consume(scan::TokenKind::Identifier, "Expect type identifier after member name.");
                let type_name = self.reader.previous.clone();

                members.push(name_token);
                member_types.push(type_name);

                self.match_token(scan::TokenKind::Semicolon);
            } else if self.match_token(scan::TokenKind::ColonColon) {
                // TODO: method
            } else {
                panic!("Invalid token.");
            }
        }

        self.consume(scan::TokenKind::RightBracket, "Expect '}' after struct definition.");
        ast::Stmt::Struct {
            name: struct_name,
            members,
            member_types,
        }
    }

    fn semicolon(&mut self) -> ast::Expr
    {
        self.match_token(scan::TokenKind::Semicolon);
        ast::Expr::Literal { value: scan::Token::default() }
    }

    fn begin_scope(&mut self)
    {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self)
    {
        self.scope_depth -= 1;
    }

    fn match_token(&mut self, token_kind: scan::TokenKind) -> bool
    {
        if self.reader.current.kind.discriminant() != token_kind.discriminant() {
            return false
        }

        let _ = self.reader.advance();
        true
    }

    fn consume(&mut self, token_kind: scan::TokenKind, error_message: &str)
    {
        let _ = self.reader.consume(token_kind, error_message);
    }

    fn error_at(&mut self, msg: &str, token: &scan::Token) -> ast::Expr
    {
        self.reader.error_at(msg, token);
        ast::Expr::Bad { token: token.clone() }
    }
}
