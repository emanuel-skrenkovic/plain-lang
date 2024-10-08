use crate::scan;


#[derive(Debug, Clone)]
pub struct Source
{
    pub source: String
}

impl Source
{
    pub fn token_value(&self, starts_at: usize, kind: scan::TokenKind) -> &str
    {
        let length = self.token_length(starts_at, kind);
        &self.source[starts_at..starts_at+length]
    }

    pub fn token_length(&self, starts_at: usize, kind: scan::TokenKind) -> usize
    {
        match kind { 
            scan::TokenKind::End => 0,

            scan::TokenKind::Error
            | scan::TokenKind::LeftParen 
            | scan::TokenKind::RightParen 
            | scan::TokenKind::LeftBracket 
            | scan::TokenKind::RightBracket 
            | scan::TokenKind::LeftAngle 
            | scan::TokenKind::RightAngle
            | scan::TokenKind::LeftSquare
            | scan::TokenKind::RightSquare
            | scan::TokenKind::Questionmark 
            | scan::TokenKind::Semicolon 
            | scan::TokenKind::Colon 
            | scan::TokenKind::Plus 
            | scan::TokenKind::Minus 
            | scan::TokenKind::Star 
            | scan::TokenKind::Slash
            | scan::TokenKind::Dot
            | scan::TokenKind::Comma
            | scan::TokenKind::Bang 
            | scan::TokenKind::Equal
            | scan::TokenKind::Ampersand
            | scan::TokenKind::Pipe
            | scan::TokenKind::Caret => 1, 

            scan::TokenKind::ColonColon 
            | scan::TokenKind::ColonEquals 
            | scan::TokenKind::BangEqual 
            | scan::TokenKind::EqualEqual 
            | scan::TokenKind::GreaterEqual 
            | scan::TokenKind::LessEqual 
            | scan::TokenKind::If 
            | scan::TokenKind::AmpersandAmpersand
            | scan::TokenKind::PipePipe 
            | scan::TokenKind::PipeRightAngle
            | scan::TokenKind::RightAngleRightAngle
            | scan::TokenKind::LeftAngleLeftAngle 
            | scan::TokenKind::PlusEqual 
            | scan::TokenKind::MinusEqual => 2,

            scan::TokenKind::True 
            | scan::TokenKind::This 
            | scan::TokenKind::Else 
            | scan::TokenKind::Case => 4,  

            scan::TokenKind::False 
            | scan::TokenKind::Break 
            | scan::TokenKind::While => 5,

            scan::TokenKind::Switch 
            | scan::TokenKind::Struct 
            | scan::TokenKind::Return => 6, 

            scan::TokenKind::Continue => 8,

            scan::TokenKind::For => 3, 

            scan::TokenKind::Interface => 9, 

            scan::TokenKind::Literal => { 
                let mut index = starts_at;

                let mut length = 0;
                let mut c = self.source.as_bytes()[index] as char;

                if c == '"' {
                    length += 1;
                    index += 1;
                    c = self.source.as_bytes()[index] as char;

                    while c != '\"' && c != '\0' {
                        length += 1;
                        index += 1;
                        c = self.source.as_bytes()[index] as char;
                    }

                    length += 1;
                } else {
                    while (c.is_alphanumeric() || c == '_') && c != '\0' {
                        length += 1;
                        index += 1;
                        c = self.source.as_bytes()[index] as char;
                    }
                }
                
                length
            }

            scan::TokenKind::Identifier => { 
                let mut index = starts_at;

                let mut length = 0;
                let mut c = self.source.as_bytes()[index] as char;

                while (c.is_alphanumeric() || c == '_') && c != '\0' {
                    length += 1;
                    index += 1;
                    c = self.source.as_bytes()[index] as char;
                }
                
                length
            }
        }
    }
}
