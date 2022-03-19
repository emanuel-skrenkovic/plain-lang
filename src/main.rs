pub mod scan;

use crate::scan::{Scanner, TokenKind};

fn main() {
    let mut scanner = Scanner::new("func test()\n { let a = 5; }".to_owned());

    let mut token;
    loop {
        token = scanner.scan_token();

        match token.kind {
            TokenKind::Error | TokenKind::End => { break; }
            _ => {
                println!("Kind: {:?}", token.kind);
                println!("Value: {:?}", token.value);
                println!("Line: {:?}", token.line);
                println!("===============");
            }

        }
    }

    println!("Token {:?}", token.kind);
}
