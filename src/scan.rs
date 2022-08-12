use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
    LeftParen, RightParen, LeftBracket, RightBracket, LeftAngle, RightAngle,
    Questionmark, Semicolon, Colon, Plus, Minus, Star, Slash,
    Comma,
    Bang, BangEqual, EqualEqual, GreaterEqual, LessEqual, Equal,
    True, False,
    Let, Var,
    This, If, Else, Break, Continue,
    Switch, Case, For, While,
    Func, Struct, Interface, Literal,
    Identifier,
    Error, End
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub line: usize,
}

impl Clone for Token {
    fn clone(&self) -> Self {
        Token {
            kind: self.kind,
            value: self.value.clone(),
            line: self.line
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Kind {:?}\nValue {}\nLine {}\n", self.kind,
                                                    self.value,
                                                    self.line)
    }
}

impl std::default::Default for Token {
    fn default() -> Self {
        Token { kind: TokenKind::Error, value: "".to_owned(), line: 0 }
    }
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

        if c.is_numeric() {
            return self.literal()
        }

        if c.is_alphabetic() {
            return self.identifier()
        }

        match c {
            '(' => self.emit(TokenKind::LeftParen),
            ')' => self.emit(TokenKind::RightParen),
            '{' => self.emit(TokenKind::LeftBracket),
            '}' => self.emit(TokenKind::RightBracket),
            '<' => {
                if self.match_char('=') {
                    return self.emit(TokenKind::LessEqual)
                }

                self.emit(TokenKind::LeftAngle)
            },
            '>' => {
                if self.match_char('=') {
                    return self.emit(TokenKind::GreaterEqual)
                }

                self.emit(TokenKind::RightAngle)
            },
            '?' => self.emit(TokenKind::Questionmark),
            ':' => self.emit(TokenKind::Colon),
            ';' => self.emit(TokenKind::Semicolon),
            '+' => self.emit(TokenKind::Plus),
            '-' => self.emit(TokenKind::Minus),
            '*' => self.emit(TokenKind::Star),
            '/' => self.emit(TokenKind::Slash), // TODO: comments
            ',' => self.emit(TokenKind::Comma),
            '!' => {
                if self.match_char('=') {
                    return self.emit(TokenKind::BangEqual)
                }

                self.emit(TokenKind::Bang)
            }
            '='  => {
                if self.match_char('=') {
                    return self.emit(TokenKind::EqualEqual)
                }

                self.emit(TokenKind::Equal)
            },
            '\0' => self.emit(TokenKind::End),
            _    => self.identifier()
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
            'b' => self.check_keyword(1, 4, "reak", TokenKind::Break),
            'c' => {
                if self.source.chars().nth(self.start + 1).unwrap() == 'a' { // TODO: fix
                    return self.check_keyword(2, 2, "se", TokenKind::Case)
                }

                self.check_keyword(2, 6, "ntinue", TokenKind::Continue)
            },
            'e' => self.check_keyword(1, 3, "lse", TokenKind::Else),
            'f' => {
                if self.source.chars().nth(self.start + 1).unwrap() == 'o' { // TODO: fix
                    return self.check_keyword(2, 1, "r", TokenKind::For)
                }

                self.check_keyword(2, 2, "nc", TokenKind::Func)
            },
            'i' => {
                if self.source.chars().nth(self.start + 1).unwrap() == 'f' { // TODO: fix
                    return TokenKind::If
                }

                self.check_keyword(1, 8, "nterface", TokenKind::If)
            },
            'l' => self.check_keyword(1, 2, "et", TokenKind::Let),
            's' => {
                if self.source.chars().nth(self.start + 1).unwrap() == 't' { // TODO: fix
                    return self.check_keyword(2, 4, "ruct", TokenKind::Struct)
                }

                self.check_keyword(2, 4, "itch", TokenKind::Switch)
            },
            't' => self.check_keyword(1, 3, "his", TokenKind::This),
            'v' => self.check_keyword(1, 2, "ar", TokenKind::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenKind::While),
            _   => TokenKind::Identifier,
        }
    }

    fn literal(&mut self) -> Token {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        self.emit(TokenKind::Literal)
    }

    fn check_keyword
    (
        &self,
        start: usize,
        length: usize,
        rest: &str,
        token_kind: TokenKind
    ) -> TokenKind {
        let from = self.start + start;
        let to   = from + length;

        if from > self.source.len() || to > self.source.len() {
            return TokenKind::Identifier;
        }

        let keyword = &self.source[from..to];

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

    fn match_char(&mut self, c: char) -> bool {
        if self.peek() == c {
            self.advance();
            return true
        }

        false
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
