use crate::{ast, context, scan, error};


// params, return_type, argument_type_names, body

pub struct Function
{
    left_paren: scan::TokenId,
    right_paren: scan::TokenId,
    params: Vec<scan::TokenId>, 
    param_types: Vec<scan::TokenId>,
    return_type: Option<scan::TokenId>, 
    body: Vec<ast::Stmt>,
}


#[repr(u8)]
#[derive(Copy, Clone, PartialOrd, PartialEq)]
enum Precedence
{
    None,
    Return,
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

type ParseFn = fn(&mut Parser, &ParsingContext) -> ast::Expr;

#[derive(Copy, Clone)]
struct ParseRule
{
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence
}

const RULES: [ParseRule; 51] =
[
    ParseRule { prefix: Some(Parser::function_expression), infix: Some(Parser::function_invocation), precedence: Precedence::Call }, // LeftParen
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // RightParen
    ParseRule { prefix: Some(Parser::block_expression),    infix: None,                              precedence: Precedence::None }, // LeftBracket
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // RightBracket
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Comparison }, // LeftAngle
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Comparison }, // RightAngle
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Questionmark
    ParseRule { prefix: Some(Parser::semicolon),           infix: Some(Parser::semicolon),           precedence: Precedence::Call }, // Semicolon
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Colon
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // ColonColon
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // ColonEquals
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Term }, // Plus
    ParseRule { prefix: Some(Parser::unary),               infix: Some(Parser::binary),              precedence: Precedence::Term }, // Minus
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Factor }, // Star
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Factor }, // Slash
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Factor }, // Caret
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::And }, // Ampersand
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Or }, // Pipe,
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::And }, // AmpersandAmpersand
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Or }, // PipePipe
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Factor }, // PlusEqual
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Factor }, // MinusEqual
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Factor }, // RightAngleRightAngle
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Factor }, // LeftAngleLeftAngle
    ParseRule { prefix: None,                              infix: Some(Parser::dot_operator),        precedence: Precedence::Call }, // Dot
    ParseRule { prefix: None,                              infix: Some(Parser::pipe),                precedence: Precedence::Call }, // PipeRightAngle
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Comma
    ParseRule { prefix: Some(Parser::unary),               infix: None,                              precedence: Precedence::None }, // Bang
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Equality }, // BandEqual
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Equality }, // EqualEqual
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Comparison }, // GreaterEqual
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Comparison }, // LessEqual
    ParseRule { prefix: None,                              infix: Some(Parser::binary),              precedence: Precedence::Assignment }, // Equal
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
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Struct
    ParseRule { prefix: None,                              infix: None,                              precedence: Precedence::None }, // Interface
    ParseRule { prefix: Some(Parser::literal),             infix: None,                              precedence: Precedence::None }, // Literal
    ParseRule { prefix: Some(Parser::_return),             infix: None,                              precedence: Precedence::Return }, // Return
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
    pub ctx: context::Context,
    pub scanned_tokens: Vec<scan::TokenId>,

    pub current: usize,
    pub previous: usize,

    pub panic: bool,
}

impl TokenReader
{
    #[must_use]
    pub fn new(ctx: context::Context) -> Self
    {
        Self {
            ctx,
            scanned_tokens: Vec::with_capacity(1024 * 8),
            current: 0,
            previous: 0,
            panic: false,
        }
    }

    fn advance(&mut self) -> Result<(), error::Error>
    {
        if matches!(self.ctx.token_kind(self.current), scan::TokenKind::End) {
            return Ok(())
        }

        if self.current >= self.ctx.tokens.kinds.len() {
            return Ok(())
        }

        self.previous = self.current;
        self.current += 1;

        if matches!(self.ctx.token_kind(self.current), scan::TokenKind::End) {
            return Ok(())
        }

        self.scanned_tokens.push(self.current);

        if !matches!(self.ctx.token_kind(self.current), scan::TokenKind::Error) {
            return Ok(())
        }

        Err(self.error_at("Scanner error.", self.current))
    }

    fn match_token(&mut self, token_kind: scan::TokenKind) -> Result<bool, error::Error>
    {
        if self.ctx.token_kind(self.current) != token_kind {
            return Ok(false)
        }

        self.advance()?;
        Ok(true)
    }

    fn check_token(&self, token_kind: scan::TokenKind) -> bool
    {
        self.ctx.token_kind(self.current) == token_kind
    }

    fn consume(&mut self, token_kind: scan::TokenKind, error_message: &str) -> Result<(), error::Error>
    {
        match self.match_token(token_kind) {
            Ok(true)  => Ok(()),
            Ok(false) => Err(self.error_at(error_message, self.current)),
            Err(err)  => Err(err),
        }
    }

    fn error_at(&mut self, message: &str, token: scan::TokenId) -> error::Error
    {
        self.ctx.error_at(message, error::Kind::ParseError, token)
    }

    // TODO: horrible and I should be publicly shamed, doesn't even work oh my gosh how embarrassing oh my
    fn peek(&self, diff: i32) -> Option<scan::TokenId>
    {
        let index = i32::try_from(self.current).unwrap();
        let index = index + diff;

        if index > self.ctx.tokens.len().try_into().unwrap() {
            return None
        }

        let index: usize = index.try_into().unwrap();

        Some(index)
    }
}

#[derive(PartialEq, Eq)]
pub enum ParsingContext
{
    Regular,
    If,
}

pub struct Parser
{
    pub reader: TokenReader,

    pub scope_depth: usize,
    pub stack: Vec<ast::Expr>,
}

impl Parser
{
    #[must_use]
    pub fn new(ctx: context::Context) -> Self
    {
        Self {
            reader: TokenReader::new(ctx),
            scope_depth: 0,
            stack: Vec::with_capacity(8),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<ast::Node>, Vec<error::Error>>
    {
        let mut nodes: Vec<ast::Node> = Vec::with_capacity(512 * 8);

        loop {
            if let scan::TokenKind::End = self.reader.ctx.token_kind(self.reader.current) {
                break
            }

            let decl = self.declaration();
            if let ast::Stmt::Expr { ref expr } = decl {
                if let ast::ExprInfo { value: ast::Expr::Bad { token }, .. } = expr.as_ref() {
                    panic!("{token:?}");
                }
            }
            nodes.push(ast::Node::Stmt(decl));
        }

        if self.reader.ctx.reporter.error {
            return Err(self.reader.ctx.reporter.errors.clone())
        }

        Ok(nodes)
    }

    fn is_at_end(&self) -> bool
    {
        self.reader.ctx.token_kind(self.reader.current) == scan::TokenKind::End
    }

    fn parse_precedence(&mut self, prec: Precedence, context: &ParsingContext)
    {
        let _ = self.reader.advance();

        let kind = self.reader.ctx.token_kind(self.reader.previous);

        if let Some(prefix) = get_rule(kind).prefix {
            let prefix_expr = prefix(self, context);

            match prefix_expr {
                ast::Expr::Literal { 
                    value // : scan::Token { kind: scan::TokenKind::Semicolon, .. } 
                } =>  {
                    if self.reader.ctx.token_kind(value) != scan::TokenKind::Semicolon {
                        self.stack.push(prefix_expr);
                    }
                },
                _ => self.stack.push(prefix_expr)
            }
        } else {
            if self.is_at_end() { return }
            panic!("{}", self.reader.error_at("Expect expression.", self.reader.current))
        }

        while prec.discriminator() <= get_rule(self.reader.ctx.token_kind(self.reader.current)).precedence.discriminator() {
            let _ = self.reader.advance();

            let Some(infix_rule) = get_rule(self.reader.ctx.token_kind(self.reader.previous)).infix else {
                self.error_at("Failed to get infix rule.", self.reader.previous);
                return
            };

            let infix_rule = infix_rule(self, context);
            if let ast::Expr::Literal { value } = infix_rule {
                if self.reader.ctx.token_kind(value) == scan::TokenKind::Semicolon {
                    continue
                }
            } 

            self.stack.push(infix_rule);
        }
    }

    fn expression_statement(&mut self) -> ast::Stmt
    {
        let expr = self.expression(&ParsingContext::Regular);
        let expr = ast::ExprInfo::new(expr);
        let expr = Box::new(expr);

        ast::Stmt::Expr { expr }
    }

    fn expression(&mut self, context: &ParsingContext) -> ast::Expr
    {
        self.parse_precedence(Precedence::Assignment, context);
        self.stack.pop().expect("Expect values in stack.")
    }

    fn block_expression(&mut self, _: &ParsingContext) -> ast::Expr
    {
        let (left_bracket, right_bracket, statements, value) = self.block();

        let statements = statements.into_iter().map(Box::new).collect();

        let value = if let Some(value)= value {
            let value = ast::ExprInfo::new(value);
            let value = Box::new(value);
            Some(value)
        } else {
            None
        };
        
        ast::Expr::Block { 
            left_bracket, 
            right_bracket, 
            statements, 
            value,
        }
    }

    fn binary(&mut self, context: &ParsingContext) -> ast::Expr
    {
        let operator   = self.reader.previous;
        let parse_rule = get_rule(self.reader.ctx.token_kind(operator));

        let left = self.stack.pop().expect("Expect value in stack.");

        let prec = Precedence
            ::try_from(parse_rule.precedence.discriminator() + 1)
            .unwrap();
        self.parse_precedence(prec, context);

        let right = self.stack.pop().expect("Expect value in stack.");

        let left  = ast::ExprInfo::new(left);
        let right = ast::ExprInfo::new(right);

        // Assignment is a special case
        if self.reader.ctx.token_kind(operator) == scan::TokenKind::Equal {
            return ast::Expr::Assignment { 
                left: Box::new(left),
                right: Box::new(right),
            }
        }

        let expr = ast::Expr::Binary {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        };

        self.match_token(scan::TokenKind::Semicolon);
        expr
    }

    fn unary(&mut self, _: &ParsingContext) -> ast::Expr
    {
        let operator = self.reader.previous;

        let expr = self.expression(&ParsingContext::Regular);
        let expr = ast::ExprInfo::new(expr);
        let expr = Box::new(expr);

        ast::Expr::Unary {
            operator,
            expr,
        }
    }

    fn dot_operator(&mut self, _: &ParsingContext) -> ast::Expr
    {
        let instance_name = self.reader.peek(-2).unwrap();

        self.consume(scan::TokenKind::Identifier, "Expect identifier.");
        let member_name = self.reader.previous;

        ast::Expr::MemberAccess { instance_name, member_name }
    }

    fn literal(&mut self, _: &ParsingContext) -> ast::Expr
    {
        ast::Expr::Literal { 
            value: self.reader.previous 
        }
    }

    fn declaration(&mut self) -> ast::Stmt
    {
        match self.reader.ctx.token_kind(self.reader.current) {
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

        let next_kind        = self.reader.ctx.token_kind(next);
        let second_next_kind = self.reader.ctx.token_kind(self.reader.peek(2)?);

        if second_next_kind == scan::TokenKind::Struct {
            return Some(self._struct());
        }

        let type_definition = next_kind == scan::TokenKind::Colon;
        let immutable       = next_kind == scan::TokenKind::ColonColon;
        let mutable         = next_kind == scan::TokenKind::ColonEquals;

        if !(type_definition || immutable || mutable) {
            return None
        }

        let name = self.reader.current;
        let _    = self.reader.advance();

        let mut type_name: Option<scan::TokenId> = None;
        if type_definition {
            let _ = self.match_token(scan::TokenKind::Colon);
            self.consume(scan::TokenKind::Identifier, "Expected identifier");

            type_name = Some(self.reader.previous);

            if !self.match_token(scan::TokenKind::Equal) && !self.match_token(scan::TokenKind::Colon) {
                let error = self.error_at
                (
                    "Expected token '=' or token ':' after type definition.",
                    self.reader.current,
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

        // Only in global scope.
        if self.scope_depth == 0 {
            if self.match_token(scan::TokenKind::Identifier) {
                let function_name = self.reader.previous;

                self.consume(scan::TokenKind::ColonColon, "Expect '::.");
                self.consume(scan::TokenKind::LeftParen, "Expect '('.");

                // Declare self first to allow recursion.

                let function = self.receiver_function(name);
                let body     = function.body.into_iter().map(Box::new).collect();

                let stmt = ast::Stmt::ReceiverFunction {
                    receiver_type_name: name,
                    name: function_name,
                    params: function.params,
                    return_type: function.return_type,
                    param_types: function.param_types,
                    body,
                };
                return Some(stmt)
            } else if self.match_token(scan::TokenKind::LeftParen) {
                // Function declaration.
                // TODO: this needs to happen only in global scope, in other scopes, the function
                // value is the result of an expression instead of it being a statement.

                // The below is such a shitty comment. I have not clue what this means.

                let function = self.function();
                let body     = function.body.into_iter().map(Box::new).collect();

                let stmt = ast::Stmt::Function {
                    name,
                    params: function.params,
                    return_type: function.return_type,
                    param_types: function.param_types,
                    body,
                };
                return Some(stmt)
            }
        }
        
        let initializer = self.expression(&ParsingContext::Regular);
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

        let next_kind = self.reader.ctx.token_kind(next);

        let type_definition = next_kind == scan::TokenKind::Colon;
        let immutable       = next_kind == scan::TokenKind::ColonColon;
        let mutable         = next_kind == scan::TokenKind::ColonEquals;

        if !(type_definition || immutable || mutable) {
            return None
        }

        let name = self.reader.current;
        let _    = self.reader.advance();

        let mut type_name: Option<scan::TokenId> = None;
        match (type_definition, immutable, mutable) {
            (true, _, _) => {
                let _ = self.match_token(scan::TokenKind::Colon);
                self.consume(scan::TokenKind::Identifier, "Expect identifier");

                type_name = Some(self.reader.previous);

                if !self.match_token(scan::TokenKind::Equal) && !self.match_token(scan::TokenKind::Colon) {
                    let error = self.error_at("Expected token '=' or ':' after type identifier.", self.reader.current);
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

        let initializer = self.expression(&ParsingContext::Regular);
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
    fn variable(&mut self, context: &ParsingContext) -> ast::Expr
    {
        let name = self.reader.previous;

        if self.match_token(scan::TokenKind::LeftBracket) && context != &ParsingContext::If {
            return self.struct_expression()
        } else if self.match_token(scan::TokenKind::Dot) {
            self.consume(scan::TokenKind::Identifier, "Expect identifier.");

            let member_name = self.reader.previous;

            if self.match_token(scan::TokenKind::LeftParen) {
                let mut arguments: Vec<Box<ast::ExprInfo>> = Vec::with_capacity(256);

                if !self.reader.check_token(scan::TokenKind::RightParen) {
                    loop {
                        let expr = self.expression(&ParsingContext::Regular);
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

                return ast::Expr::ReceiverCall { receiver_name: name, name: member_name, arguments }
            }

            return ast::Expr::MemberAccess { instance_name: name, member_name };
        }

        // Handles variable expression here.
        self.match_token(scan::TokenKind::Semicolon);
        ast::Expr::Variable { name }
    }

    fn _return(&mut self, _: &ParsingContext) -> ast::Expr
    {
        let token = self.reader.previous;

        let value = self.expression(&ParsingContext::Regular);
        let value = ast::ExprInfo::new(value);
        let value = Box::new(value);

        ast::Expr::Return { token, value }
    }

    fn function_expression(&mut self, _: &ParsingContext) -> ast::Expr
    {
        let function = self.function();
        let body     = function.body.into_iter().map(Box::new).collect();

        ast::Expr::Function { 
            left_paren: function.left_paren,
            right_paren: function.right_paren,
            params: function.params, 
            return_type: function.return_type, 
            param_types: function.param_types, 
            body,
        }
    }

    fn receiver_function(&mut self, receiver_type_name: scan::TokenId) -> Function
    {
        self.begin_scope();

        let left_paren = self.reader.previous;

        let mut param_types = Vec::with_capacity(64);
        let mut params      = Vec::with_capacity(64);

        self.consume(scan::TokenKind::Identifier, "Expect 'self' as first parameter.");
        let self_token = self.reader.previous;

        // TODO
        /*
        if self.reader.reporter.source.token_value(&self_token) != "self" {
            // TODO
        }
        */

        params.push(self_token);
        param_types.push(receiver_type_name);

        self.match_token(scan::TokenKind::Comma);

        if !self.reader.check_token(scan::TokenKind::RightParen) {
            loop {
                self.consume(scan::TokenKind::Identifier, "Expect parameter identifier after '('.");

                let parameter_name_token = self.reader.previous;
                params.push(parameter_name_token);

                self.consume(scan::TokenKind::Colon, "Expect type definition");
                self.consume(scan::TokenKind::Identifier, "Expected type identifier");

                let type_name = &self.reader.previous;
                param_types.push(*type_name);

                if !self.match_token(scan::TokenKind::Comma) { break }
            }
        }

        self.consume(scan::TokenKind::RightParen, "Expect ')' after end of lambda parameters.");
        let right_paren = self.reader.previous;

        let return_type = if self.match_token(scan::TokenKind::Colon) 
                          && self.match_token(scan::TokenKind::Identifier) {
            Some(self.reader.previous)
        } else {
            None
        };

        self.consume(scan::TokenKind::LeftBracket, "Expect token '{' after function definition.");

        let mut body = Vec::with_capacity(256);

        while !self.reader.check_token(scan::TokenKind::RightBracket) && !self.reader.check_token(scan::TokenKind::End) {
            let statement = self.declaration();
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


    fn function(&mut self) -> Function
    {
        self.begin_scope();

        let left_paren = self.reader.previous;

        let mut param_types = Vec::with_capacity(64);
        let mut params      = Vec::with_capacity(64);

        if !self.reader.check_token(scan::TokenKind::RightParen) {
            loop {
                self.consume(scan::TokenKind::Identifier, "Expect parameter identifier after '('.");

                let parameter_name_token = self.reader.previous;
                params.push(parameter_name_token);

                self.consume(scan::TokenKind::Colon, "Expect type definition");
                self.consume(scan::TokenKind::Identifier, "Expected type identifier");

                let type_name = &self.reader.previous;
                param_types.push(*type_name);

                if !self.match_token(scan::TokenKind::Comma) { break }
            }
        }

        self.consume(scan::TokenKind::RightParen, "Expect ')' after end of lambda parameters.");
        let right_paren = self.reader.previous;

        let return_type = if self.match_token(scan::TokenKind::Colon) 
                          && self.match_token(scan::TokenKind::Identifier) {
            Some(self.reader.previous)
        } else {
            None
        };

        self.consume(scan::TokenKind::LeftBracket, "Expect token '{' after function definition.");

        let mut body = Vec::with_capacity(256);

        while !self.reader.check_token(scan::TokenKind::RightBracket) && !self.reader.check_token(scan::TokenKind::End) {
            let statement = self.declaration();
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

    fn block(&mut self) -> (scan::TokenId, scan::TokenId, Vec<ast::Stmt>, Option<ast::Expr>)
    {
        let left_bracket = self.reader.previous;

        self.begin_scope();

        let mut statements = Vec::with_capacity(512);

        // Compile code until the end of the block or the end of the program is reached.
        while !self.reader.check_token(scan::TokenKind::RightBracket) && !self.reader.check_token(scan::TokenKind::End) {
            let statement = self.declaration();
            statements.push(statement);
        }

        let has_value = if let Some(ast::Stmt::Expr { expr: last, .. }) = statements.last() {
            // #horribleways
            // If the last token in the block is a semicolon, then the block is of Unit type.
            // This is a horrible way to check for this and I should be shamed. Preferably publicly.
            let has_value = self.reader.ctx.token_kind(self.reader.previous) != scan::TokenKind::Semicolon;

            has_value || matches!(&last.value, ast::Expr::Return { .. })
        } else {
            false
        };

        let expr = if has_value { 
            let ast::Stmt::Expr { expr } = statements.pop().unwrap() else {
                panic!("Expect expression statement.")
            };
            
            Some(expr.value) 
        } else { 
            None 
        };

        self.consume(scan::TokenKind::RightBracket, "Expect '}' at the end of a block expression.");
        let right_bracket = self.reader.previous;

        self.match_token(scan::TokenKind::Semicolon);

        self.end_scope();
        (left_bracket, right_bracket, statements, expr)
    }

    fn _if(&mut self, _: &ParsingContext) -> ast::Expr
    {
        let token = self.reader.previous;

        // TODO: this is a terrible way to implement this.
        // Would prefer a parameter.
        let condition = self.expression(&ParsingContext::If);

        self.consume(scan::TokenKind::LeftBracket, "Expect '{");

        let branch = self.block_expression(&ParsingContext::Regular);

        let mut conditions: Vec<ast::ExprInfo> = Vec::with_capacity(32);
        let mut branches: Vec<ast::ExprInfo>   = Vec::with_capacity(32);

        conditions.push(ast::ExprInfo::new(condition));
        branches.push(ast::ExprInfo::new(branch));

        loop {
            if !self.match_token(scan::TokenKind::Else) {
                break
            }

            if self.match_token(scan::TokenKind::If) {
                let condition = self.expression(&ParsingContext::If);
                let condition = ast::ExprInfo::new(condition);
                conditions.push(condition);
            }

            self.consume(scan::TokenKind::LeftBracket, "Expect '{.");
            
            let branch = self.block_expression(&ParsingContext::Regular);
            let branch = ast::ExprInfo::new(branch);
            branches.push(branch);
        }

        let conditions = conditions.into_iter().map(Box::new).collect();
        let branches   = branches.into_iter().map(Box::new).collect();

        ast::Expr::If {
            token,
            conditions,
            branches,
        }
    }

    fn _while(&mut self) -> ast::Stmt
    {
        let while_token = self.reader.previous;
        let condition   = self.expression(&ParsingContext::If);

        // Body
        self.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        let mut body = Vec::with_capacity(512);

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
        let for_token = self.reader.previous;
         
        // TODO: no unwrap
        let variable = self.variable_statement().unwrap();
        let variable = Box::new(variable);

        let condition = self.expression(&ParsingContext::If);
        let condition = ast::ExprInfo::new(condition);
        let condition = Box::new(condition);

        // TODO: needs to be just unary statement.
        let advancement = self.declaration();
        let advancement = Box::new(advancement);

        self.consume(scan::TokenKind::LeftBracket, "Expect '{' at the start of the 'for' block.");

        let mut body: Vec<Box<ast::Stmt>> = Vec::with_capacity(512);

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

    fn function_invocation(&mut self, _: &ParsingContext) -> ast::Expr
    {
        let Some(token) = self.reader.peek(-2) else {
            return self
                .error_at("Failed to parse function - no function name found.", self.reader.previous);
        };

        let function_name_token = token;

        let mut arguments: Vec<Box<ast::ExprInfo>> = Vec::with_capacity(256);

        if !self.reader.check_token(scan::TokenKind::RightParen) {
            loop {
                let expr = self.expression(&ParsingContext::Regular);
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
                .error_at("Failed to parse struct expression - no struct name found.", self.reader.previous);
        };

        let name = token;

        let mut members = Vec::with_capacity(64);
        let mut values  = Vec::with_capacity(64);

        if !self.reader.check_token(scan::TokenKind::RightParen) {
            loop {
                self.consume(scan::TokenKind::Identifier, "Expect member ame.");

                let member_name = self.reader.previous;
                members.push(member_name);

                self.consume(scan::TokenKind::Colon, "Expect ':' after member initializer name.");

                let expr = self.expression(&ParsingContext::Regular);
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
    fn pipe(&mut self, _: &ParsingContext) -> ast::Expr
    {
        todo!()
    }

    fn _struct(&mut self) -> ast::Stmt
    {
        self.consume(scan::TokenKind::Identifier, "Expect struct name identifier.");
        let struct_name = self.reader.previous;

        self.consume(scan::TokenKind::ColonColon, "Expect '::' after struct identifier.");
        self.consume(scan::TokenKind::Struct, "Expect 'struct' token.");

        self.consume(scan::TokenKind::LeftBracket, "Expect '{' on struct definition.");

        let mut members      = Vec::with_capacity(64);
        let mut member_types = Vec::with_capacity(64);

        while !self.reader.check_token(scan::TokenKind::RightBracket) && !self.reader.check_token(scan::TokenKind::End) {
            self.consume(scan::TokenKind::Identifier, "Expect identifier.");
            let name_token = self.reader.previous;

            if self.match_token(scan::TokenKind::Colon) {
                self.consume(scan::TokenKind::Identifier, "Expect type identifier after member name.");
                let type_name = self.reader.previous;

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

    fn semicolon(&mut self, _: &ParsingContext) -> ast::Expr
    {
        self.match_token(scan::TokenKind::Semicolon);
        ast::Expr::Literal { value: self.reader.previous }
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
        if self.reader.ctx.token_kind(self.reader.current) != token_kind {
            return false
        }

        let _ = self.reader.advance();
        true
    }

    fn consume(&mut self, token_kind: scan::TokenKind, error_message: &str)
    {
        let _ = self.reader.consume(token_kind, error_message);
    }

    fn error_at(&mut self, msg: &str, token: scan::TokenId) -> ast::Expr
    {
        self.reader.error_at(msg, token);
        ast::Expr::Bad { token }
    }
}
