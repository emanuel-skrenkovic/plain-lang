use crate::scan;
use crate::error;
use crate::source;


#[derive(Clone)]
pub struct Context
{
    pub source: source::Source,
    pub reporter: error::Reporter,
    pub tokens: scan::Tokens,
}

impl Context
{
    pub fn error_at
    (
        &mut self, 
        message: &str, 
        kind: error::Kind, 
        token: scan::TokenId
    ) -> error::Error
    {
        let token = self.tokens.token(token);
        self.reporter.error_at(message, kind, &token)
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
