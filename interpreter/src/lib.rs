pub mod token;
pub mod lexer;
pub mod repl;
pub mod ast;
pub mod parser;
pub mod object;
pub mod evaluator;

#[cfg(test)]
mod tests {
    use super::object::{Object, ObjectVariant, Integer, INTEGER_OBJ, Boolean, BOOLEAN_OBJ};
    use super::evaluator;

    fn test_eval(input: &str) -> Option<Object> {
        let lexer = super::lexer::Lexer::new(input);
        let mut parser = super::parser::Parser::new(lexer);
         
         if let Ok(program) = parser.parse_program() {
             println!("{:#?}", program);
             return evaluator::eval(program);
         } else {
             panic!("failed to parse program");
         }
    }

    fn test_integer_object(obj: Integer, expected: u64) -> bool {
        obj.kind() == INTEGER_OBJ && obj.value == expected
    }

    fn test_boolean_object(obj: Boolean, expected: bool) -> bool {
        obj.kind() == BOOLEAN_OBJ && obj.value == expected
    }


    #[test]
    fn test_eval_integer_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: u64,
        }

        let tests = vec![
            Test { input: "32", expected: 32 },
            Test { input: "10", expected: 10 },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();


            if let Object::Integer(int) = evaluated {
                assert!(test_integer_object(int, test.expected));
            }
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: bool,
        }

        let tests = vec![
            Test { input: "true", expected: true },
            Test { input: "false", expected: false },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();


            if let Object::Boolean(boolean) = evaluated {
                assert!(test_boolean_object(boolean, test.expected));
            }
        }
    }
}
