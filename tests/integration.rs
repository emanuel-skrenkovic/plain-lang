use sage::block::Value;
use sage::compiler::Compiler;
use sage::vm::VM;

#[cfg(test)]
mod equals_operator {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn equals_not_equal() {
        let source = "5 == 6";

        let mut scanner = Scanner::new(source.to_owned());

        let mut compiler = Compiler::new();
        let program = compiler.compile(scanner.scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Bool { val } => debug_assert_eq!(val, false),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    fn equals_equal() {
        let source = "5 == 5";

        let mut scanner = Scanner::new(source.to_owned());

        let mut compiler = Compiler::new();
        let program = compiler.compile(scanner.scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    // TODO: currently a run-time error. Should be compile-time.
    #[test]
    #[should_panic]
    fn equals_different_types() {
        let source = "5 == true";

        let mut scanner = Scanner::new(source.to_owned());

        let mut compiler = Compiler::new();
        let program = compiler.compile(scanner.scan_tokens());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }
}

#[cfg(test)]
mod not_equals_operator {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn not_equals_not_equal() {
        let source = "5 != 6";

        let mut scanner = Scanner::new(source.to_owned());

        let mut compiler = Compiler::new();
        let program = compiler.compile(scanner.scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    fn not_equals_equal() {
        let source = "5 != 5";

        let mut scanner = Scanner::new(source.to_owned());

        let mut compiler = Compiler::new();
        let program = compiler.compile(scanner.scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Bool { val } => debug_assert_eq!(val, false),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    #[should_panic]
    fn not_equals_different_types() {
        let source = "5 != true";

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source.to_owned()).scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();
    }
}

#[cfg(test)]
mod plus_operator {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn add() {
        let source = "5 + 6";

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source.to_owned()).scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Number { val } => debug_assert_eq!(val, 11),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    fn add_multiple_values() {
        let source = "5 + 6 + 7 + 8";

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source.to_owned()).scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Number { val } => debug_assert_eq!(val, 26),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    #[should_panic]
    fn add_different_types() {
        let source = "5 + true";

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source.to_owned()).scan_tokens());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();
    }
}

#[cfg(test)]
mod minus_operator {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn subtract() {
        let source = "5 - 6".to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source.to_owned()).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Number { val } => debug_assert_eq!(val, -1),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    #[should_panic]
    fn subtract_different_types() {
        let source = "5 - true";

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source.to_owned()).scan_tokens());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();
    }
}

#[cfg(test)]
mod greater_than_operator {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn greater_than_lesser() {
        let source = "5 > 6".to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Bool { val } => debug_assert_eq!(val, false),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn greater_than_greater() {
        let source = "5 > 4".to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    #[should_panic]
    fn greater_than_different_types() {
        let source = "5 > true".to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }
}

#[cfg(test)]
mod less_than_operator {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn less_than_lesser() {
        let source = "5 < 6".to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn less_than_greater() {
        let source = "5 < 4".to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Bool { val } => debug_assert_eq!(val, false),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    #[should_panic]
    fn less_than_different_types() {
        let source = "5 > true".to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }
}

#[cfg(test)]
mod block_expression {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn block_assignment() {
        let source = "
            a :: { b :: 3; b + 3 };
            a + 4;
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Number { val } => debug_assert_eq!(val, 10),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    #[should_panic]
    fn block_scope_out_of_scope_access() {
        let source = "
            { a :: 5; }
            b :: a + 3;
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();
    }

    #[test]
    fn block_encloses_outer_scope() {
        // Arrange
        let source = "
            a :: 5;
            {
                b :: a + 2;
                b;
            }
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Number { val } => debug_assert_eq!(val, 7),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }
}

#[cfg(test)]
mod function {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn t() {
        let source = "
            a :: () { 5 };
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Function {
                name: _,
                arity: _,
                closure: _,
            } => {}
            _ => debug_assert!(false, "Value on stack is not a function."),
        }
    }

    #[test]
    fn function_declaration_puts_function_on_stack() {
        let source = "
            func test() {
                5
            }
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Function {
                name: _,
                arity: _,
                closure: _,
            } => {}
            _ => debug_assert!(false, "Value on stack is not a function."),
        }
    }

    #[test]
    fn function_declaration_has_correct_arity() {
        let source = "
            func test(a, b) {
                a + b
            }
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Function {
                name: _,
                arity,
                closure: _,
            } => debug_assert_eq!(arity, 2),
            _ => debug_assert!(false, "Value on stack is not a function."),
        }
    }

    #[test]
    fn function_declaration_has_correct_name() {
        let source = "
            func test(a, b) {
                a + b
            }
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Function {
                name,
                arity: _,
                closure: _,
            } => debug_assert_eq!(name, "test"),
            _ => debug_assert!(false, "Value on stack is not a function."),
        }
    }

    #[test]
    fn function_call_puts_function_result_onto_stack() {
        let source = "
            func test(a, b) {
                a + b
            }

            test(1, 2);
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        match value {
            Value::Number { val } => debug_assert_eq!(val, 3),
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }
    }

    #[test]
    fn function_call_encloses_outer_scope() {
        // Arrange
        let source = "
            a :: 5;
            func test() {
                a + 3
            }

            test() == 8;
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();

        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }
    }

    #[test]
    fn function_call_returns_value_if_function_returns_value() {
        // Arrange
        let source = "
            func test() {
                5
            }

            test();
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 5),
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }
    }

    #[test]
    fn function_pushes_unit_to_stack_if_function_has_no_return_value() {
        // Arrange
        let source = "
            func test() {
                5;
            }

            test();
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Unit => {}
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }
    }

    #[test]
    fn function_called_twice_without_return_value() {
        // Arrange
        let source = "
            func test() {
                5;
            }

            test();
            test();
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Unit => {}
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }
    }

    #[test]
    fn function_called_twice_without_return_value_with_arguments() {
        // Arrange
        let source = "
            func test(a, b) {
                a + b + 5;
            }

            test(4, 2);
            test(4, 2);
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Unit => {}
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }
    }

    #[test]
    fn function_called_twice_with_return_value() {
        // Arrange
        let source = "
            func test() {
                5
            }

            test();
            test();
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 5),
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 5),
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }
    }

    #[test]
    fn function_called_twice_with_return_value_with_arguments() {
        // Arrange
        let source = "
            func test(a, b) {
                a + b + 5
            }

            test(4, 2);
            test(4, 2);
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 11),
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 11),
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }
    }

    #[test]
    fn function_stays_on_stack_if_function_has_no_return_value() {
        // Arrange
        let source = "
            func test() {
                5;
            }

            test();
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Unit => {}
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }

        let value = vm.pop();
        match value {
            Value::Function {
                name,
                arity: _,
                closure: _,
            } => {
                debug_assert_eq!(name, "test")
            }
            _ => debug_assert!(false, "Value of incorrect type pushed onto the stack."),
        }
    }

    #[test]
    fn function_call_compile_time_error_on_call_nonexistent_function() {
        // Arrange
        let source = "
            test();
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(!program.is_ok());
    }

    #[test]
    fn function_parameters_parsed() {
        // Arrange
        let source = "
            func test(a, b) {
                a + b
            }

            test(1, 2);
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();

        match value {
            Value::Number { val } => debug_assert_eq!(val, 3),
            _ => debug_assert!(false, "Value on stack is not a function."),
        }
    }

    #[test]
    fn function_parameters_parsed_three() {
        // Arrange
        let source = "
            func test(a, b, c) {
                a + b + c
            }

            test(1, 2, 3);
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();

        match value {
            Value::Number { val } => debug_assert_eq!(val, 6),
            _ => debug_assert!(false, "Value on stack is not a function."),
        }
    }

    // TODO: should be a compile time error
    #[test]
    #[should_panic]
    fn function_runtime_error_on_incorrect_number_of_arguments_passed_to_function() {
        // Arrange
        let source = "
            func test(a, b) {
                a + b
            }

            test(1);
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();
    }

    #[test]
    fn function_assigned_to_variable() {
        // Arrange
        let source = "
            func test(a, b) {
                a + b
            }
            holder :: test;
            holder(1, 2);
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 3),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_called_by_name_after_being_assigned_to_variable() {
        // Arrange
        let source = "
            func test(a, b) {
                a + b
            }
            holder :: test;
            test(1, 2);
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 3),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_result_called_by_name_and_from_variable_equal() {
        // Arrange
        let source = "
            func test(a, b) {
                a + b
            }
            holder :: test;
            test(1, 2) == holder(1, 2);
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();
        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_invoked_in_nested_scope() {
        // Arrange
        let source = "
            func add(a, b) {
                a + b
            }

            result := 0;

            {
                result = add(4, 2);
            }

            result;
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 6),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_invoked_in_loop() {
        // Arrange
        let source = "
            func test(a, b) {
                a + b
            }

            result := 0;
            for i := 0; i < 10; i = i + 1; {
                result = result + test(i, i);
            }

            result;
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 90),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_invoked_from_variable_in_loop() {
        // Arrange
        let source = "
            func test(a, b) {
                a + b
            }

            proxy :: test;

            result := 0;
            for i := 0; i < 10; i = i + 1; {
                result = result + proxy(i, i);
            }

            result;
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 90),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_invoked_from_variable_in_nested_scope() {
        // Arrange
        let source = "
            func add(a, b) {
                a + b
            }

            proxy :: add;

            result := 0;

            {
                result = proxy(4, 2);
            }

            result;
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        // Act
        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 6),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_lambda_parsed_interpreted() {
        // Arrange
        let source = "
            square :: (x) { x * x };
            square(2);
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }

    #[test]
    fn function_lambda_places_correct_value_on_stack() {
        // Arrange
        let source = "
            square :: (x) { x * x };
            square(2);
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 4),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_lambda_results_equal_when_invoked_multiple_times_with_same_arguments() {
        let source = "
            square :: (x) { x * x };
            square(2) == square(2);
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_lambda_called_in_loop() {
        let source = "
            square :: (x) { x * x };

            result := 0;
            for i := 0; i < 10; i = i + 1; {
                result = square(i);
            }
            result;
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 81),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_lambda_two_arguments() {
        // Arrange
        let source = "
            add :: (a, b) { a + b };
            add(4, 2);
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 6),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn function_lambda_three_arguments() {
        // Arrange
        let source = "
            add :: (a, b, c) { a + b * c };
            add(1, 2, 3);
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 7),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }
}

#[cfg(test)]
mod closure {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn closure_should_access_variable_from_outside_scope() {
        let source = "
            a :: 5;
            b :: { a + 2 };
            b == 7;
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());

        let mut vm = VM::new(program.unwrap());
        vm.interpret();

        let value = vm.pop();

        // TODO: macro?
        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    fn closure_should_not_capture_sibling_values() {
        let source = "
            { a :: 5; }
            { b :: a + 2; }
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(!program.is_ok());
    }
}

#[cfg(test)]
mod var_variables {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn var_defined_with_number_value() {
        // Arrange
        let source = "a := 5;".to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }

    #[test]
    fn var_returns_number_value_when_referenced() {
        // Arrange
        let source = "
            a := 5;
            a;
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 5),
            _ => debug_assert!(false, "Value is of incorrect type,"),
        }
    }

    #[test]
    fn var_defined_with_boolean_value() {
        // Arrange
        let source = "a := true;".to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }

    #[test]
    fn var_returns_boolean_value_when_referenced() {
        // Arrange
        let source = "
            a := true;
            a;
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type,"),
        }
    }

    // TODO
    // #[test]
    // fn var_declared_not_defined() {
    //     // Arrange
    //     let source = "var a;".to_owned();
    //
    //     // Act
    //     let mut compiler = Compiler::new();
    //     let program = compiler.compile(Scanner::new(source).scan_tokens());
    //     debug_assert!(program.is_ok());
    //     let program = program.unwrap();
    //
    //     let mut vm = VM::new(program);
    //     vm.interpret();
    // }

    #[test]
    fn var_redefined() {
        // Arrange
        let source = "
            a := 5;
            a = 2;
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }

    #[test]
    fn var_redefined_returns_new_value() {
        // Arrange
        let source = "
            a := 5;
            a = 2;
            a;
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 2),
            _ => debug_assert!(false, "Value is of incorrect type,"),
        }
    }

    // TODO
    // #[test]
    // fn var_declared_then_defined() {
    //     // Arrange
    //     let source = "
    //         var a;
    //         a = 2;
    //         a;
    //     "
    //     .to_owned();
    //
    //     // Act
    //     let mut compiler = Compiler::new();
    //     let program = compiler.compile(Scanner::new(source).scan_tokens());
    //     debug_assert!(program.is_ok());
    //     let program = program.unwrap();
    //
    //     let mut vm = VM::new(program);
    //     vm.interpret();
    //
    //     // Assert
    //     let value = vm.pop();
    //     match value {
    //         Value::Number { val } => debug_assert_eq!(val, 2),
    //         _ => debug_assert!(false, "Value is of incorrect type,"),
    //     }
    // }
}

#[cfg(test)]
mod let_variables {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn let_defined_with_number_value() {
        // Arrange
        let source = "a :: 5;".to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }

    #[test]
    fn let_returns_number_value_when_referenced() {
        // Arrange
        let source = "
            a :: 5;
            a;
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 5),
            _ => debug_assert!(false, "Value is of incorrect type,"),
        }
    }

    #[test]
    fn let_defined_with_boolean_value() {
        // Arrange
        let source = "a :: true;".to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }

    #[test]
    fn let_returns_boolean_value_when_referenced() {
        // Arrange
        let source = "
            a :: true;
            a;
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type,"),
        }
    }

    // TODO
    // #[test]
    // fn let_error_if_value_not_defined() {
    //     // Arrange
    //     let source = "let a;".to_owned();
    //
    //     // Act
    //     let mut compiler = Compiler::new();
    //     let program = compiler.compile(Scanner::new(source).scan_tokens());
    //     debug_assert!(!program.is_ok());
    // }

    #[test]
    fn let_error_if_redefined() {
        // Arrange
        let source = "
            a :: 5;
            a = 2;
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(!program.is_ok());
    }
}

#[cfg(test)]
mod colon_equals {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn declaration() {
        let source = "
            a := 5;
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
    }

    #[test]
    fn _for() {
        let source = "
            for i := 0; i < 5; i = i + 1; {
                i;
            }
        "
        .to_owned();

        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
    }
}

#[cfg(test)]
mod control_flow {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn if_expression_true_branch() {
        // Arrange
        let source = "
            if 2 == 2 {
                true
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    #[should_panic]
    fn if_expression_false() {
        // Arrange
        let source = "
            if 2 == 3 {
                true
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        // should panic on pop as nothing was pushed
        let _ = vm.pop();
    }

    #[test]
    fn if_else_expression_true_branch() {
        let source = "
            if 2 == 2 {
                true
            } else {
                false
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Bool { val } => debug_assert_eq!(val, true),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    fn if_else_expression_false_branch() {
        let source = "
            if 2 == 3 {
                true
            } else {
                false
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Bool { val } => debug_assert_eq!(val, false),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    fn if_else_if_if_branch() {
        let source = "
            if 5 == 5 {
                1
            } else if 5 == 3 {
                2
            } else {
                3
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 1),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    fn if_else_if_else_if_branch() {
        let source = "
            if 3 == 5 {
                1
            } else if 5 == 5 {
                2
            } else {
                3
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 2),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    fn if_else_if_else_branch() {
        let source = "
            if 3 == 5 {
                1
            } else if 4 == 5 {
                2
            } else {
                3
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 3),
            _ => debug_assert!(false, "Value is of incorrect type"),
        }
    }

    #[test]
    #[should_panic]
    fn if_else_if_no_matches() {
        let source = "
            if 3 == 5 {
                1
            } else if 4 == 5 {
                2
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        // Assert
        // Panics as no value to pop.
        let _ = vm.pop();
    }
}

#[cfg(test)]
mod for_loop {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn for_parsed_interpreted() {
        let source = "
            for i := 0; i < 10; i = i + 1; {
                i;
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }

    #[test]
    #[should_panic(expected = "Cannot pop empty stack.")]
    fn for_does_not_leave_dangling_values() {
        let source = "
            for i := 0; i < 10; i = i + 1; {
                test := 5;
                test = test + i;
                test;
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        let _ = vm.pop();
    }

    #[test]
    fn for_captures_enclosing_variable() {
        let source = "
            test := 0;
            for i := 0; i < 10; i = i + 1 {
                test = test + i;
            }
            test;
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 45),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }
}

#[cfg(test)]
mod while_loop {
    use super::*;
    use sage::scan::Scanner;

    #[test]
    fn while_parsed_and_interpreted() {
        let source = "
            i := 0;
            while i < 5 {
                i = i + 1;
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();
    }

    #[test]
    fn while_does_not_leave_dangling_values() {
        let source = "
            i := 0;
            while i < 10 {
                test := 5;
                test = test + i;
                test;
                i = i + 1;
            }
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 10),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }

    #[test]
    fn while_captures_enclosing_variable() {
        let source = "
            test := 0;
            i := 0;
            while i < 10 {
                test = test + i;
                i = i + 1;
            }
            test;
        "
        .to_owned();

        // Act
        let mut compiler = Compiler::new();
        let program = compiler.compile(Scanner::new(source).scan_tokens());
        debug_assert!(program.is_ok());
        let program = program.unwrap();

        let mut vm = VM::new(program);
        vm.interpret();

        let value = vm.pop();
        match value {
            Value::Number { val } => debug_assert_eq!(val, 45),
            _ => debug_assert!(false, "Value is of incorrect type."),
        }
    }
}
