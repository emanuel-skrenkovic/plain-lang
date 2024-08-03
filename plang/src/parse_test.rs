use crate::ast;
use crate::scan;
use crate::error;
use crate::parse::*;

#[allow(dead_code)]
fn parse_source(source: &str) -> Result<Vec<ast::Node>, Vec<error::Error>>
{
    let mut scanner = scan::Scanner::new(source.to_string());
    let reporter    = error::Reporter::new(&source);
    let tokens      = scanner.scan_tokens();

    let parser = Parser::new(reporter, tokens);

    // Act
    parser.parse()
}

#[cfg(test)]
mod unary_expression
{
    use super::*;
    use crate::ast;
    use crate::scan;

    #[test]
    fn test_minus()
    {
        let source = "-2";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Unary { operator, expr } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::Minus.discriminant());

        let ast::Expr::Literal { value } = &expr.value else {
            panic!("Expect literal, found {:?}", &expr.value);
        };

        assert_eq!(value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

    }

    #[test]
    fn test_not()
    {
        let source = "!false";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Unary { operator, expr } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::Bang.discriminant());

        let ast::Expr::Literal { value } = &expr.value else {
            panic!("Expect literal, found {:?}", &expr.value);
        };

        assert_eq!(value.kind.discriminant(), scan::TokenKind::False.discriminant());

    }

}

#[cfg(test)]
mod binary_expression 
{
    use super::*;
    use crate::ast;
    use crate::scan;

    #[test]
    fn test_equals()
    {
        // Arrange
        let source = "2 == 3";

        // Act
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::EqualEqual.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_plus()
    {
        // Arrange
        let source = "2 + 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::Plus.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_minus()
    {
        // Arrange
        let source = "2 - 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::Minus.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_multiply()
    {
        // Arrange
        let source = "2 * 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::Star.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

        #[test]
    fn test_divide()
    {
        // Arrange
        let source = "2 / 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::Slash.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_less_than()
    {
        // Arrange
        let source = "2 < 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::LeftAngle.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_less_than_or_equal()
    {
        // Arrange
        let source = "2 <= 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::LessEqual.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_greater_than()
    {
        // Arrange
        let source = "2 > 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::RightAngle.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_greater_than_or_equal()
    {
        // Arrange
        let source = "2 >= 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::GreaterEqual.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_not_equal()
    {
        // Arrange
        let source = "2 != 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::BangEqual.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_arithmetic_and()
    {
        // Arrange
        let source = "true && false";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::AmpersandAmpersand.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::True.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::False.discriminant());
    }

    #[test]
    fn test_arithmetic_or()
    {
        // Arrange
        let source = "true || false";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::PipePipe.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::True.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::False.discriminant());
    }

    #[test]
    fn test_bitwise_and()
    {
        // Arrange
        let source = "2 & 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::Ampersand.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_bitwise_or()
    {
        // Arrange
        let source = "2 | 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::Pipe.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_bitwise_left_shift()
    {
        // Arrange
        let source = "2 << 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::LeftAngleLeftAngle.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_bitwise_right_shift()
    {
        // Arrange
        let source = "2 >> 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::RightAngleRightAngle.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

    #[test]
    fn test_bitwise_xor()
    {
        // Arrange
        let source = "2 ^ 3";
        let tree = parse_source(source);

        // Assert
        assert!(tree.is_ok());
        let tree = tree.unwrap();

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(operator.kind.discriminant(), scan::TokenKind::Caret.discriminant());

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(left_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(right_value.kind.discriminant(), scan::TokenKind::Literal.discriminant());
    }

}


