use std::fmt;
use crate::{scan, source};

#[derive(Clone, Debug)]
pub enum Kind
{
    ParseError,
    TypeError,
}

#[derive(Clone, Debug)]
pub struct Error
{
    pub line: usize,
    pub column: usize,
    pub source_line: String,
    pub token: String,
    pub msg: String,
    pub kind: Kind
}

impl fmt::Display for Error
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let token_len             = self.token.len();
        let token_underline_range = self.column..=(self.column + token_len);
        let mut underline         = " ".repeat(self.source_line.len() + token_len);

        underline.replace_range(token_underline_range, &"^".repeat(token_len));

        let line1 = format!
        (
            "{line:<width$} {line_text}", line=format!("{}:", self.line), 
            line_text=self.source_line, 
            width=6,
        );
        let line2 = format!
        (
            "{line:<width$} {line_text}", line="", 
            line_text=underline, 
            width=6,
        );

        write!(
            f,
            "Error: {}\n\n{}\n{}",
            self.msg,
            line1,
            line2,
        )
    }
}

#[derive(Clone)]
pub struct Reporter
{
    pub lines: Vec<String>,
    pub errors: Vec<Error>,
    pub error: bool,
    pub source: source::Source,
}

impl Reporter
{
    #[must_use]
    pub fn new(code: &str) -> Self
    {
        let lines: Vec<String> = code
            .lines()
            .map(String::from)
            .collect();

        let source = source::Source {
            source: code.to_owned()
        };

        Self {
            source,        
            lines,
            errors: Vec::with_capacity(64),
            error: false,
        }
    }

    pub fn error_at(&mut self, message: &str, kind: Kind, token: &scan::Token) -> Error
    {
        self.error = true;

        let error = Error {
            msg: message.to_owned(),
            line: token.line,
            column: token.column,
            source_line: self.lines[token.line - 1].clone(),
            token: self.source.token_value(token).to_owned(),
            kind,
        };
        self.errors.push(error.clone());

        error
    }
}

