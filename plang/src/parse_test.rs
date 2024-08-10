use crate::ast;
use crate::scan;
use crate::error;
use crate::source;
use crate::context;
use crate::parse::Parser;

#[allow(dead_code)]
fn parse_source(code: &str) -> Result<(Vec<ast::Node>, context::Context), Vec<error::Error>>
{

    let source = source::Source { source: code.to_string() };
    let tokens   = scan::Scanner::new(code.to_string()).scan_tokens();

    let context = context::Context::new(source, tokens);

    let (nodes, _) = Parser::new(context.clone()).parse()?;
    Ok((nodes, context))
}

#[cfg(test)]
mod dot_operator
{
    use super::*;
    use crate::ast;
    use crate::scan;

    #[test]
    fn member_access()
    {
        let source = "
            Test :: struct {
                a: i32;
            }

            main :: (): i32
            {
                test := Test { a: 15 };
                result :: test.a;
            }
        ";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Function { body, .. }) = &tree[1] else {
            panic!("Expect function statement.")
        };

        let ast::Stmt::Const { initializer, .. } = &body[1].as_ref() else {
            panic!("Expect const statement.");
        };

        let ast::Expr::MemberAccess { left, right } = &initializer.value else {
            panic!("Expect member access expression.")
        };

        assert_eq!(context.token_kind(*right), scan::TokenKind::Identifier);

        let ast::Expr::Variable { name } = &left.value else {
            panic!("Expect variable expression.")
        };

        assert_eq!(context.token_kind(*name), scan::TokenKind::Identifier);
    }

    #[test]
    fn nested_member_access()
    {
        let source = "
            Inner :: struct {
                x: i32;
            }

            Test :: struct {
                nested: Inner;
            }

            main :: (): i32
            {
                test := Test { 
                    nested: Inner { x: 15 },
                };
                result :: test.nested.x;
            }
        ";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Function { body, .. }) = &tree[2] else {
            panic!("Expect function statement.")
        };

        let ast::Stmt::Const { initializer, .. } = &body[1].as_ref() else {
            panic!("Expect const statement.");
        };

        let ast::Expr::MemberAccess { left, right } = &initializer.value else {
            panic!("Expect member access expression.")
        };

        assert_eq!(context.token_kind(*right), scan::TokenKind::Identifier);

        let ast::Expr::MemberAccess { left, right } = &left.value else {
            panic!("Expect variable expression.")
        };

        assert_eq!(context.token_kind(*right), scan::TokenKind::Identifier);

        let ast::Expr::Variable { name } = &left.value else {
            panic!("Expect variable expression.")
        };

        assert_eq!(context.token_kind(*name), scan::TokenKind::Identifier);
    }

}

#[cfg(test)]
mod function_expression
{
    use super::*;
    use crate::ast;
    use crate::scan;

    #[test]
    fn function_expression_no_return_type_no_parameters()
    {
        let source = "
            main :: (): i32
            {
                a :: () { };
            }
        ";
        let tree = parse_source(source);

        assert!(tree.is_ok());
        let (tree, _) = tree.unwrap();

        let ast::Node::Stmt(ast::Stmt::Function { body, .. }) = &tree[0] else {
            panic!("Expect function statement.")
        };

        let ast::Stmt::Const { initializer, .. } = &body[0].as_ref() else {
            panic!("Expect const statement.");
        };

        let ast::Expr::Function { .. } = &initializer.value else {
            panic!("Expect function expression.")
        };
    }

    #[test]
    fn function_expression_with_return_type()
    {
        let source = "
            main :: (): i32
            {
                a :: (): i32 { 0 };
            }
        ";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Function { body, .. }) = &tree[0] else {
            panic!("Expect function statement.")
        };

        let ast::Stmt::Const { initializer, .. } = &body[0].as_ref() else {
            panic!("Expect const statement.");
        };

        let ast::Expr::Function { return_type, .. } = &initializer.value else {
            panic!("Expect function expression.")
        };

        assert!(return_type.is_some());
        let return_type = return_type.clone().unwrap();
        assert_eq!(context.token_kind(return_type), scan::TokenKind::Identifier);
    }
}

#[cfg(test)]
mod function 
{
    use super::*;
    use crate::ast;
    use crate::scan;

    #[test]
    fn test_function_no_return_type_no_parameters()
    {
        let source = "
            a :: () { }
        ";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Function { name, return_type, .. }) = &tree[0] else {
            panic!("Expect function statement.");
        };

        assert_eq!(context.token_kind(*name), scan::TokenKind::Identifier);
        assert!(return_type.is_none());
    }

    #[test]
    fn test_function_missing_body()
    {
        let source = "
            a :: (): i32
        ";
        let tree = parse_source(source);

        assert!(tree.is_err());
    }

    #[test]
    fn test_function_with_return_type()
    {
        let source = "
            a :: (): i32 { 0 }
        ";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Function { name, return_type, .. }) = &tree[0] else {
            panic!("Expect function statement.");
        };

        assert_eq!(context.token_kind(*name), scan::TokenKind::Identifier);

        assert!(return_type.is_some());
        let return_type = return_type.clone().unwrap();
        assert_eq!(context.token_kind(return_type), scan::TokenKind::Identifier);
    }

    #[test]
    fn test_function_with_parameters_and_return_type()
    {
        let source = "
            add :: (a: i32, b: i32): i32 
            { 
                a + b
            }
        ";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Function { name, params, param_types, return_type, body }) = &tree[0] else {
            panic!("Expect function statement.");
        };

        assert_eq!(context.token_kind(*name), scan::TokenKind::Identifier);

        assert!(return_type.is_some());
        let return_type = return_type.clone().unwrap();
        assert_eq!(context.token_kind(return_type), scan::TokenKind::Identifier);

        for param in params {
            assert_eq!(context.token_kind(*param), scan::TokenKind::Identifier);
        }

        for param_type in param_types {
            assert_eq!(context.token_kind(*param_type), scan::TokenKind::Identifier);
        }

        assert!(!body.is_empty());
    }
}

#[cfg(test)]
mod if_expression
{
    use super::*;
    use crate::ast;
    use crate::scan;

    #[test]
    fn test_if()
    {
        let source = "if 2 < 3 { 2; }";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!("Expect expression statement.")
        };

        let ast::Expr::If { token, conditions, branches } = &expr.value else {
            panic!("Expect if expression.")
        };

        assert_eq!(context.token_kind(*token), scan::TokenKind::If);
        assert_eq!(conditions.len(), 1);
        assert_eq!(branches.len(), 1);

        for branch in branches {
            if !matches!(&branch.value, ast::Expr::Block { .. }) {
                panic!("Expect block expression")
            }
        }
    }

    #[test]
    fn test_if_else()
    {
        let source = "if 2 < 3 { 2; } else { 5; }";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!("Expect expression statement.")
        };

        let ast::Expr::If { token, conditions, branches } = &expr.value else {
            panic!("Expect if expression.")
        };

        assert_eq!(context.token_kind(*token), scan::TokenKind::If);
        assert_eq!(conditions.len(), 1);
        assert_eq!(branches.len(), 2);

        for branch in branches {
            if !matches!(&branch.value, ast::Expr::Block { .. }) {
                panic!("Expect block expression")
            }
        }
    }

    #[test]
    fn test_if_else_if()
    {
        let source = "
            if 2 < 3 { 
                2; 
            } else if 2 > 3 { 
                44; 
            }";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!("Expect expression statement.")
        };

        let ast::Expr::If { token, conditions, branches } = &expr.value else {
            panic!("Expect if expression.")
        };

        assert_eq!(context.token_kind(*token), scan::TokenKind::If);
        assert_eq!(conditions.len(), 2);
        assert_eq!(branches.len(), 2);

        for branch in branches {
            if !matches!(&branch.value, ast::Expr::Block { .. }) {
                panic!("Expect block expression")
            }
        }
    }

    #[test]
    fn test_if_else_if_else()
    {
        let source = "
            if 2 < 3 { 
                2; 
            } else if 2 > 3 { 
                44; 
            } else { 
                5; 
            }";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!("Expect expression statement.")
        };

        let ast::Expr::If { token, conditions, branches } = &expr.value else {
            panic!("Expect if expression.")
        };

        assert_eq!(context.token_kind(*token), scan::TokenKind::If);
        assert_eq!(conditions.len(), 2);
        assert_eq!(branches.len(), 3);

        for branch in branches {
            if !matches!(&branch.value, ast::Expr::Block { .. }) {
                panic!("Expect block expression")
            }
        }
    }

    #[test]
    fn test_if_block_condition()
    {
        let source = "if { 2 < 3 } { 2; }";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!("Expect expression statement.")
        };

        let ast::Expr::If { token, conditions, branches } = &expr.value else {
            panic!("Expect if expression.")
        };

        assert_eq!(context.token_kind(*token), scan::TokenKind::If);
        assert_eq!(conditions.len(), 1);
        assert_eq!(branches.len(), 1);

        for branch in branches {
            if !matches!(&branch.value, ast::Expr::Block { .. }) {
                panic!("Expect block expression")
            }
        }
    }

    #[test]
    fn test_if_block_condition_with_struct()
    {
        let source = "
        Test :: struct {
            a: i32;
        }

        if { s := Test { a: 2 }; s.a < 3 } { 
            2; 
        }
        ";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[1] else {
            panic!("Expect expression statement.")
        };

        let ast::Expr::If { token, conditions, branches } = &expr.value else {
            panic!("Expect if expression.")
        };

        assert_eq!(context.token_kind(*token), scan::TokenKind::If);
        assert_eq!(conditions.len(), 1);
        assert_eq!(branches.len(), 1);

        for branch in branches {
            if !matches!(&branch.value, ast::Expr::Block { .. }) {
                panic!("Expect block expression")
            }
        }
    }

    #[test]
    fn test_if_value_assignment()
    {
        let source = "
            c :: if 2 < 3 { 
                2; 
            } else if 2 > 3 { 
                44; 
            } else { 
                5; 
            }";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        let ast::Node::Stmt(ast::Stmt::Const { initializer, .. }) = &tree[0] else {
            panic!("Expect const statement.")
        };

        let ast::Expr::If { token, conditions, branches } = &initializer.value else {
            panic!("Expect if expression.")
        };

        assert_eq!(context.token_kind(*token), scan::TokenKind::If);
        assert_eq!(conditions.len(), 2);
        assert_eq!(branches.len(), 3);

        for branch in branches {
            if !matches!(&branch.value, ast::Expr::Block { .. }) {
                panic!("Expect block expression")
            }
        }
    }
}

#[cfg(test)]
mod block_expression
{
    use super::*;
    use crate::ast;
    use crate::scan;

    #[test]
    fn test_block() 
    {
        let source = "{ }";
        let Ok((tree, _)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);

        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!("Expect expression statement.")
        };

        let ast::Expr::Block { .. } = &expr.value else {
            panic!("Expect block expression.");
        };
    }

    #[test]
    fn test_block_with_value()
    {
        let source = "{ 2 }";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);

        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!("Expect expression statement.")
        };

        let ast::Expr::Block { value: block_value, .. } = &expr.value else {
            panic!("Expect block expression.");
        };

        assert!(block_value.is_some());
        let block_value = block_value.clone().unwrap();

        let ast::Expr::Literal { value } = &block_value.value else {
            panic!("Expect literal expression.")
        };

        assert_eq!(context.token_kind(*value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_block_with_statements()
    {
        let source = "
        { 
            result := 3 + 3;
            printf(\"%d\", result);
        }";
        let Ok((tree, _)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);

        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!("Expect expression statement.")
        };

        let ast::Expr::Block { statements, value: block_value, .. } = &expr.value else {
            panic!("Expect block expression.");
        };

        assert!(block_value.is_none());
        assert!(!statements.is_empty());
    }

    #[test]
    fn test_block_with_statements_and_value()
    {
        let source = "
        { 
            result := 3 + 3;
            printf(\"%d\", result);
            result
        }";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);

        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!("Expect expression statement.")
        };

        let ast::Expr::Block { statements, value: block_value, .. } = &expr.value else {
            panic!("Expect block expression.");
        };

        assert!(block_value.is_some());
        assert!(!statements.is_empty());

        let block_value = block_value.clone().unwrap();

        let ast::Expr::Variable { name } = &block_value.value else {
            panic!("Expect literal expression.")
        };

        assert_eq!(context.token_kind(*name), scan::TokenKind::Identifier);
    }
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
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Unary { operator, expr } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::Minus);

        let ast::Expr::Literal { value } = &expr.value else {
            panic!("Expect literal, found {:?}", &expr.value);
        };

        assert_eq!(context.token_kind(*value), scan::TokenKind::Literal);

    }

    #[test]
    fn test_not()
    {
        let source = "!false";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Unary { operator, expr } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::Bang);

        let ast::Expr::Literal { value } = &expr.value else {
            panic!("Expect literal, found {:?}", &expr.value);
        };

        assert_eq!(context.token_kind(*value), scan::TokenKind::False);

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
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::EqualEqual);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_plus()
    {
        // Arrange
        let source = "2 + 3";

        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::Plus);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_minus()
    {
        // Arrange
        let source = "2 - 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::Minus);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_multiply()
    {
        // Arrange
        let source = "2 * 3";

        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::Star);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

        #[test]
    fn test_divide()
    {
        // Arrange
        let source = "2 / 3";

        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::Slash);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_less_than()
    {
        // Arrange
        let source = "2 < 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::LeftAngle);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_less_than_or_equal()
    {
        // Arrange
        let source = "2 <= 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::LessEqual);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_greater_than()
    {
        // Arrange
        let source = "2 > 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::RightAngle);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal)
    }

    #[test]
    fn test_greater_than_or_equal()
    {
        // Arrange
        let source = "2 >= 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::GreaterEqual);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_not_equal()
    {
        // Arrange
        let source = "2 != 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::BangEqual);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_arithmetic_and()
    {
        // Arrange
        let source = "true && false";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::AmpersandAmpersand);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::True);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::False);
    }

    #[test]
    fn test_arithmetic_or()
    {
        // Arrange
        let source = "true || false";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::PipePipe);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::True);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::False);
    }

    #[test]
    fn test_bitwise_and()
    {
        // Arrange
        let source = "2 & 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::Ampersand);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_bitwise_or()
    {
        // Arrange
        let source = "2 | 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::Pipe);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_bitwise_left_shift()
    {
        // Arrange
        let source = "2 << 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::LeftAngleLeftAngle);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_bitwise_right_shift()
    {
        // Arrange
        let source = "2 >> 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::RightAngleRightAngle);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

    #[test]
    fn test_bitwise_xor()
    {
        // Arrange
        let source = "2 ^ 3";
        let Ok((tree, context)) = parse_source(source) else {
            panic!("Expect parse.")
        };

        assert_eq!(tree.len(), 1);
        let ast::Node::Stmt(ast::Stmt::Expr { expr }) = &tree[0] else {
            panic!()
        };

        let ast::Expr::Binary { left, right, operator } = &expr.value else {
            panic!("Expect binary expression, found {:?}.", &expr.value)
        };

        assert_eq!(context.token_kind(*operator), scan::TokenKind::Caret);

        let ast::Expr::Literal { value: left_value } = &left.value else {
            panic!("Expect literal, found {:?}", &left.value);
        };

        assert_eq!(context.token_kind(*left_value), scan::TokenKind::Literal);

        let ast::Expr::Literal { value: right_value } = &right.value else {
            panic!("Expect literal, found {:?}", &right.value);
        };

        assert_eq!(context.token_kind(*right_value), scan::TokenKind::Literal);
    }

}


