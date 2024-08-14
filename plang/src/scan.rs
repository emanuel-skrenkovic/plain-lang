#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TokenKind
{
    LeftParen, RightParen, LeftBracket, RightBracket, LeftAngle, RightAngle, LeftSquare, RightSquare,
    Questionmark, Semicolon, Colon, ColonColon, ColonEquals, Plus, Minus, Star, Slash,
    Caret,
    Ampersand, Pipe,
    AmpersandAmpersand, PipePipe,
    RightAngleRightAngle, LeftAngleLeftAngle,
    PlusEqual, MinusEqual,
    Dot,
    PipeRightAngle,
    Comma,
    Bang, BangEqual, EqualEqual, GreaterEqual, LessEqual, Equal,
    True, False,
    This, If, Else, Break, Continue,
    Switch, Case, For, While,
    Struct, Interface, Literal,
    Return,
    Identifier,
    Error, End
}

#[derive(Clone, Debug)]
pub struct Token
{
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

impl Default for Token
{
    fn default() -> Self
    {
        Token { 
            kind: TokenKind::Error, 
            line: 0, 
            column: 0,
        }
    }
}

pub type TokenId = usize;

// Use this, then instead of cloning the tokens,
// pass around the token id, which is actually just the index.
#[derive(Debug, Clone)]
pub struct Tokens
{
    pub kinds: Vec<TokenKind>,
    pub lines: Vec<usize>,
    pub columns: Vec<usize>,
}

impl Tokens 
{
    pub fn with_capacity(capacity: usize) -> Self
    {
        Tokens {
            kinds: Vec::with_capacity(capacity),
            lines: Vec::with_capacity(capacity),
            columns: Vec::with_capacity(capacity),
        }
    }

    pub fn kind(&self, id: TokenId) -> TokenKind
    {
        self.kinds[id]
    }

    pub fn token(&self, id: TokenId) -> Token
    {
        let kind   = self.kinds[id];
        let line   = self.lines[id];
        let column = self.columns[id];

        Token { kind, line, column }
    }

    pub fn len(&self) -> usize
    {
        self.kinds.len()
    }

    pub fn is_empty(&self) -> bool
    {
        self.kinds.is_empty()
    }
}

pub struct Scanner
{
    source: String,
    source_len: usize,

    current: usize,
    start: usize,

    line: usize,
    char_index: usize,
}

impl Scanner
{
    #[must_use]
    pub fn new(source: String) -> Scanner
    {
        Scanner {
            source_len: source.len(),
            source,

            current: 0,
            start: 0,

            line: 1,
            char_index: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> Tokens
    {
        let mut tokens: Tokens = Tokens::with_capacity(1024);

        loop {
            let current = self.scan_token();
            let end     = current.kind == TokenKind::End;

            tokens.kinds.push(current.kind);
            tokens.lines.push(current.line);
            tokens.columns.push(current.column);

            if end { break }
        }

        tokens
    }

    pub fn scan_token(&mut self) -> Token
    {
        self.skip_whitespace();
        self.start = self.current;

        if self.source_end() { return self.emit(TokenKind::End) }

        let c = self.advance();

        if c.is_numeric() { return self.literal() }

        // TODO: true | false?

        if c.is_alphabetic() { return self.identifier() }

        match c {
            '(' => self.emit(TokenKind::LeftParen),
            ')' => self.emit(TokenKind::RightParen),
            '{' => self.emit(TokenKind::LeftBracket),
            '}' => self.emit(TokenKind::RightBracket),
            '[' => self.emit(TokenKind::LeftSquare),
            ']' => self.emit(TokenKind::RightSquare),
            '<' => {
                if self.match_char('=') {
                    return self.emit(TokenKind::LessEqual)
                }

                if self.match_char('<') {
                    return self.emit(TokenKind::LeftAngleLeftAngle)
                }

                self.emit(TokenKind::LeftAngle)
            },
            '>' => {
                if self.match_char('=') {
                    return self.emit(TokenKind::GreaterEqual)
                }

                if self.match_char('>') {
                    return self.emit(TokenKind::RightAngleRightAngle)
                }

                self.emit(TokenKind::RightAngle)
            },
            '?' => self.emit(TokenKind::Questionmark),
            ':' => {
                if self.match_char(':') {
                    return self.emit(TokenKind::ColonColon)
                } else if self.match_char('=') {
                    return self.emit(TokenKind::ColonEquals)
                }

                self.emit(TokenKind::Colon)
            },
            ';' => self.emit(TokenKind::Semicolon),
            '+' => {
                if self.match_char('=') {
                    return self.emit(TokenKind::PlusEqual)
                }

                self.emit(TokenKind::Plus)
            }
            '-' => {
                if self.match_char('=') {
                    return self.emit(TokenKind::MinusEqual)
                }

                self.emit(TokenKind::Minus)
            }
            '*' => self.emit(TokenKind::Star),
            '/' => self.emit(TokenKind::Slash), // TODO: comments
            '.' => self.emit(TokenKind::Dot),
            '|' => {
                if self.match_char('>') {
                    return self.emit(TokenKind::PipeRightAngle)
                }

                if self.match_char('|') {
                    return self.emit(TokenKind::PipePipe)
                }

                self.emit(TokenKind::Pipe)
            },
            ',' => self.emit(TokenKind::Comma),
            '!' => {
                if self.match_char('=') {
                    return self.emit(TokenKind::BangEqual)
                }

                self.emit(TokenKind::Bang)
            }
            '"' => {
                while self.peek() != '\"' && !self.source_end() {
                    self.advance();
                }
                // Get the ending quotes as well.
                self.advance();

                self.emit(TokenKind::Literal)
            }
            '='  => {
                if self.match_char('=') {
                    return self.emit(TokenKind::EqualEqual)
                }

                self.emit(TokenKind::Equal)
            },
            '&' => {
                if self.match_char('&') {
                    return self.emit(TokenKind::AmpersandAmpersand)
                }

                self.emit(TokenKind::Ampersand)
            }
            '^' => self.emit(TokenKind::Caret),
            '\0' => self.emit(TokenKind::End),
            _    => self.identifier()
        }
    }

    fn identifier(&mut self) -> Token
    {
        let mut c = self.peek();
        while c.is_alphanumeric() || c == '_' {
            self.advance();
            c = self.peek();
        }

        self.emit(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenKind
    {
        let identifier_first_char = self.source_char_at(self.start);

        match identifier_first_char {
            'b' => self.check_keyword(1, 4, "reak", TokenKind::Break),
            'c' => {
                if self.source_char_at(self.start + 1) == 'a' {
                    return self.check_keyword(2, 2, "se", TokenKind::Case)
                }

                self.check_keyword(2, 6, "ntinue", TokenKind::Continue)
            },
            'e' => self.check_keyword(1, 3, "lse", TokenKind::Else),
            'f' => {
                if self.source_char_at(self.start + 1) == 'o' { // TODO: fix
                    return self.check_keyword(2, 1, "r", TokenKind::For)
                }

                self.check_keyword(2, 3, "lse", TokenKind::False)
            },
            'i' => {
                if self.source_char_at(self.start + 1) == 'f' { // TODO: fix
                    return TokenKind::If
                }

                self.check_keyword(1, 8, "nterface", TokenKind::If)
            },
            'r' => self.check_keyword(1, 5, "eturn", TokenKind::Return),
            's' => {
                if self.source_char_at(self.start + 1) == 't' { // TODO: fix
                    return self.check_keyword(2, 4, "ruct", TokenKind::Struct)
                }

                self.check_keyword(2, 4, "itch", TokenKind::Switch)
            },
            't' => {
                if self.source_char_at(self.start + 1) == 'r' {
                    return self.check_keyword(2, 2, "ue", TokenKind::True)
                }

                self.check_keyword(1, 3, "his", TokenKind::This)
            },
            'w' => self.check_keyword(1, 4, "hile", TokenKind::While),
            _   => TokenKind::Identifier,
        }
    }

    fn literal(&mut self) -> Token
    {
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
    ) -> TokenKind
    {
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

    fn advance(&mut self) -> char
    {
        self.current += 1;

        let c = self.source_char_at(self.current - 1);

        match c {
            '\n' => self.char_index = 0,
            _    => self.char_index += 1,
        }

        c
    }

    fn peek(&self) -> char
    {
        self.source_char_at(self.current)
    }

    fn match_char(&mut self, c: char) -> bool
    {
        if self.peek() == c {
            self.advance();
            return true
        }

        false
    }

    fn source_char_at(&self, i: usize) -> char
    {
        if i >= self.source_len { return '\0' }
        self.source.as_bytes()[i] as char
    }

    fn skip_whitespace(&mut self)
    {
        loop {
            let c = self.peek();

            match c {
                ' ' | '\r' | '\t' => { self.advance(); }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                _ => { break; }
            }
        }
    }

    fn source_end(&self) -> bool
    {
        self.peek() == '\0'
    }

    fn emit(&self, kind: TokenKind) -> Token
    {
        Token {
            kind,
            line: self.line,
            column: self.start,
        }
    }
}
