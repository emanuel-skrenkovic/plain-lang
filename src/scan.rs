#[derive(Debug)]
pub enum TokenKind {
    LeftParen, RightParen,
    LeftBracket, RightBracket,
    LeftAngle, RightAngle,
    Semicolon, Colon,
    Bang, BangEqual, EqualEqual,
    True, False,
    Let, Var,
    This, If, Else, Break, Continue,
    Switch, Case, For, While,
    Func, Struct, Interface,
    Identifier,
    Error, End
}

pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub line: usize,
}

pub struct Scanner {
    source: String,

    current: usize,
    start: usize,

    line: usize
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            source,

            current: 0,
            start: 0,

            line: 1
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();

        self.start = self.current;

        if self.source_end() {
            return self.emit(TokenKind::End)
        }

        let c = self.advance();

        if c.is_alphabetic() {
            return self.identifier()
        }

        match c {
            '('  => { self.emit(TokenKind::LeftParen) }
            ')'  => { self.emit(TokenKind::RightParen) }
            '{'  => { self.emit(TokenKind::LeftBracket) }
            '}'  => { self.emit(TokenKind::RightBracket) }
            ':'  => { self.emit(TokenKind::Colon) }
            ';'  => { self.emit(TokenKind::Semicolon) }
            '\0' => { self.emit(TokenKind::End) }
            _    => { self.identifier() }
        }
    }

    fn identifier(&mut self) -> Token {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        self.emit(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenKind {
        let identifier_first_char = self.source.chars().nth(self.start).unwrap();

        match identifier_first_char {
            'f' => { return self.check_keyword(1, 3, "unc", TokenKind::Func) }
            'l' => { return self.check_keyword(1, 2, "et", TokenKind::Let) }
            _   => { return TokenKind::Identifier }
        }
    }

    fn check_keyword
    (
        &self,
        start: usize,
        length: usize,
        rest: &str,
        token_kind: TokenKind
    ) -> TokenKind {
        let keyword = &self.source[self.start + start..self.start + start + length];

        if keyword == rest {
            return token_kind
        }

        TokenKind::Identifier
    }

    fn advance(&mut self) -> char {
        self.current += 1;

        self.source
            .chars()
            .nth(self.current - 1)
            .unwrap()
    }

    fn peek(&self) -> char {
        self.source_char_at(self.current)
    }

    fn peek_next(&self) -> char {
        self.source_char_at(self.current + 1)
    }

    fn source_char_at(&self, i: usize) -> char {
        self.source.chars().nth(i).unwrap_or('\0')
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();

            match c {
                ' '  => { self.advance(); }
                '\r' => { self.advance(); }
                '\t' => { self.advance(); }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                _ => { break; }
            }
        }
    }

    fn source_end(&self) -> bool {
        self.peek() == '\0'
    }

    fn emit(&self, kind: TokenKind) -> Token {
        Token {
            kind,
            value: self.source[self.start..self.current].to_owned(),
            line: self.line
        }
    }
}
