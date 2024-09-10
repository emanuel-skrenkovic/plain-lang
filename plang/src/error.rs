use std::fmt;

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
        // TODO: Fix this.
        // let token_len             = self.token.len();
        // let token_underline_range = self.column..self.column + token_len;
        // let mut underline         = " ".repeat(self.source_line.len() + token_len);

        // underline.replace_range(token_underline_range, &"^".repeat(token_len));

        let line1 = format!
        (
            "{line:<width$} {line_text}", line=format!("{}:", self.line), 
            line_text=self.source_line, 
            width=6,
        );
        /*
        let line2 = format!
        (
            "{line:<width$} {line_text}", line="", 
            line_text=underline, 
            width=6,
        );
        */

        write!(
            f,
            "Error: {}\n\n{}\n",
            self.msg,
            line1,
            // line2,
        )
    }
}


