use crate::scan;
use crate::error;
use crate::source;


#[derive(Clone)]
pub struct Context
{
    pub source: source::Source,
    pub tokens: scan::Tokens,

    pub lines: Vec<String>,
    pub errors: Vec<error::Error>,
    pub error: bool,

}

impl Context
{
    pub fn new(source: source::Source, tokens: scan::Tokens) -> Self
    {
        let lines: Vec<String> = source.source
            .lines()
            .map(String::from)
            .collect();

        Self {
            source,        
            tokens,
            lines,
            errors: Vec::with_capacity(64),
            error: false,
        }
    }

    pub fn error_at
    (
        &mut self, 
        message: &str, 
        kind: error::Kind, 
        token: scan::TokenId
    ) -> error::Error
    {
        let token = self.tokens.token(token);

        self.error = true;

        // Clamp to the number of lines of source.
        // Probably indicative of crappy code elsewhere.
        let line_number = std::cmp::min(
            token.line - 1, 
            self.lines.len() - 1
        );

        let error = error::Error {
            msg: message.to_owned(),
            line: token.line,
            column: token.column,
            source_line: self.lines[line_number].clone(),
            token: self.source.token_value(token.column, token.kind).to_owned(),
            kind,
        };
        self.errors.push(error.clone());

        error

    }

    pub fn token_value(&self, token_id: scan::TokenId) -> &str
    {
        let kind = self.tokens.kinds[token_id];
        let column = self.tokens.columns[token_id];
        self.source.token_value(column, kind)
    }

    pub fn token_kind(&self, token_id: scan::TokenId) -> scan::TokenKind
    {
        self.tokens.kinds[token_id]
    }
}
