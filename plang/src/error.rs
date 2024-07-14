use std::fmt;
use crate::scan;

#[derive(Clone, Debug)]
pub enum CompilerErrorKind
{
    ParseError,
    TypeError,
}

#[derive(Clone, Debug)]
pub struct CompilerError
{
    pub line: usize,
    pub token_index: usize,
    pub source_line: String,
    pub token: String,
    pub msg: String,
    pub kind: CompilerErrorKind
}

impl fmt::Display for CompilerError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        let token_len             = self.token.len();
        let token_underline_range = self.token_index..self.token_index + token_len + 1;
        let mut underline         = " ".repeat(self.source_line.len() + token_len);

        underline.replace_range(token_underline_range, &"^".repeat(token_len));

        let line1 = format!("{line:<width$} {line_text}", line=format!("{}:", self.line), line_text=self.source_line, width=6);
        let line2 = format!("{line:<width$} {line_text}", line="", line_text=underline, width=6);

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
pub struct ErrorReporter
{
    pub source: String,
    pub errors: Vec<CompilerError>,
    pub error: bool,
}

impl ErrorReporter
{
    pub fn error_at(&mut self, message: &str, kind: CompilerErrorKind, token: &scan::Token) -> CompilerError
    {
        self.error = true;

        let lines: Vec<String> = self
            .source
            .lines()
            .map(String::from)
            .collect();

        let error = CompilerError {
            msg: message.to_owned(),
            line: token.line,
            token_index: token.token_index,
            source_line: lines[token.line - 1].clone(),
            token: token.value.clone(),
            kind,
        };
        self.errors.push(error.clone());

        error
    }
}

