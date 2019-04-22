use std::rc::Rc;
use std::cell::RefCell;

use crate::ast::{boolean, if_expression, int};
use crate::ast::{
    expression::Expression,
    identifier::Identifier,
    statement::{ReturnStatement, Statement},
    Node, NodeKind,
};
use crate::environment::Environment;
use crate::object::{Object, BOOLEAN_OBJ};

#[derive(Default)]
pub struct Evaluator {
    environment: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    fn eval_integer(&self, integer_literal: &int::IntegerLiteral) -> Object {
        Object::Integer(integer_literal.value)
    }

    fn eval_boolean(&self, boolean: &boolean::Boolean) -> Object {
        if boolean.value {
            Object::Boolean(true)
        } else {
            Object::Boolean(false)
        }
    }

    fn eval_statements(
        &mut self,
        statements: &[Statement],
    ) -> Option<Object> {
        let mut result = None;

        for statement in statements {
            result = self.eval(statement);

            if let Some(Object::ReturnValue(_)) = result {
                break;
            }

            if let Some(Object::Error(_)) = result {
                break;
            }
        }

        result
    }

    fn eval_prefix_expression(&self, operator: &str, right: Object) -> Object {
        match operator {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_prexif_operator_expression(right),
            _ => Object::Error(format!("unknown operator: {}{}", operator, right)),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Boolean(boolean) => {
                if boolean {
                    Object::Boolean(false)
                } else {
                    Object::Boolean(true)
                }
            }
            Object::Null => Object::Boolean(true),
            _ => Object::Boolean(false),
        }
    }

    fn native_boolean_to_boolean_object(&self, value: bool) -> Object {
        if value {
            Object::Boolean(true)
        } else {
            Object::Boolean(false)
        }
    }

    fn eval_minus_prexif_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Integer(integer) => Object::Integer(-integer),
            _ => Object::Error(format!("unknown operator: -{}", right.kind())),
        }
    }

    fn eval_infix_expression(&self, operator: &str, left: &Object, right: &Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.eval_integer_infix_expression(operator, *l, *r)
            }
            (Object::Boolean(l), Object::Boolean(r)) => {
                self.eval_boolean_infix_expression(operator, *l, *r)
            }
            (Object::Integer(_), Object::Boolean(_)) => Object::Error(format!(
                "type mismatch: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )),
            (Object::Boolean(_), Object::Integer(_)) => Object::Error(format!(
                "type mismatch: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )),
            (_, _) => Object::Error(format!(
                "unknown operator: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )),
        }
    }

    fn eval_boolean_infix_expression(&self, operator: &str, left: bool, right: bool) -> Object {
        match operator {
            "==" => self.native_boolean_to_boolean_object(left == right),
            "!=" => self.native_boolean_to_boolean_object(left != right),
            _ => Object::Error(format!("unknown operator: {} {} {}", BOOLEAN_OBJ, operator, BOOLEAN_OBJ,)),
        }
    }

    fn eval_integer_infix_expression(&self, operator: &str, left: i64, right: i64) -> Object {
        match operator {
            "+" => Object::Integer(left + right),
            "-" => Object::Integer(left - right),
            "*" => Object::Integer(left * right),
            "/" => Object::Integer(left / right),
            "<" => self.native_boolean_to_boolean_object(left < right),
            ">" => self.native_boolean_to_boolean_object(left > right),
            "==" => self.native_boolean_to_boolean_object(left == right),
            "!=" => self.native_boolean_to_boolean_object(left != right),
            _ => Object::Error(format!("unknown operator: {} {} {}", left, operator, right)),
        }
    }

    fn eval_if_expression(
        &mut self,
        if_expression: &if_expression::If
    ) -> Option<Object> {
        let condition = if_expression
            .condition
            .as_ref()
            .map(std::convert::AsRef::as_ref)
            .unwrap();
        let consequence = if_expression.consequence.as_ref().unwrap();
        let alternative = if_expression.alternative.as_ref();
        let eval_condition = self.eval(condition);

        if eval_condition.as_ref().map_or(false, Object::is_error) {
            return eval_condition;
        }

        if eval_condition
            .as_ref()
            .map_or(false, |v| self.is_thruthy(v))
        {
            return self.eval(consequence);
        } else if if_expression.alternative.is_some() {
            return self.eval(alternative.unwrap());
        }

        Some(Object::Null)
    }

    fn is_thruthy(&self, object: &Object) -> bool {
        match object {
            Object::Null => false,
            Object::Boolean(value) => *value,
            _ => true,
        }
    }

    fn eval_return_statement(
        &mut self,
        return_statement: &ReturnStatement,
    ) -> Option<Object> {
        if let Some(return_value) = &return_statement.return_value {
            let value = self.eval(return_value).unwrap();

            if let Object::Error(error) = value {
                return Some(Object::Error(error));
            }

            return Some(Object::ReturnValue(Box::new(value)));
        }

        None
    }

    fn eval_identifier(&self, identifier: &Identifier) -> Option<Object> {
        let value = self.environment.borrow().get(&identifier.value);

        if value.is_none() {
            return Some(Object::Error(format!(
                "identifier not found: {}",
                identifier.value
            )));
        }

        value
    }

    pub fn eval(&mut self, node: impl Node) -> Option<Object> {
        match node.kind() {
            NodeKind::Program(program) => self.eval_statements(&program.statements),
            NodeKind::Statement(statement) => match &statement {
                Statement::Expression(expression_statement) => {
                    if let Some(expression) = &expression_statement.expression {
                        return self.eval(expression);
                    }

                    None
                }
                Statement::Return(return_statement) => {
                    self.eval_return_statement(return_statement)
                }
                Statement::Block(block_statement) => {
                    self.eval_statements(&block_statement.statements)
                }
                Statement::Let(let_statement) => {
                    if let Some(expression) = &let_statement.value {
                        let value = self.eval(expression);

                        return match &value {
                            Some(Object::Error(_)) => value,
                            None => None,
                            Some(v) => {
                                if let Some(identifier) = &let_statement.name {
                                    self.environment.borrow_mut().set(&identifier.value, &v);
                                }

                                value
                            }
                        };
                    }

                    None
                }
            },
            NodeKind::Block(block) => self.eval_statements(&block.statements),
            NodeKind::Expression(expression) => match expression {
                Expression::Int(integer_expression) => self.eval(integer_expression),
                Expression::Bool(boolean_expression) => self.eval(boolean_expression),
                Expression::Prefix(prefix_expression) => {
                    if let Some(right) = &*prefix_expression.right {
                        let eval_right = self.eval(right);

                        if let Some(Object::Error(error)) = eval_right {
                            return Some(Object::Error(error));
                        }

                        return Some(self.eval_prefix_expression(
                            prefix_expression.operator.as_ref(),
                            eval_right.unwrap(),
                        ));
                    }

                    None
                }
                Expression::Infix(infix_expression) => {
                    if let Some(left) = &*infix_expression.left {
                        if let Some(right) = &*infix_expression.right {
                            let eval_left = self.eval(left).unwrap();

                            if let Object::Error(error) = eval_left {
                                return Some(Object::Error(error));
                            }

                            let eval_right = self.eval(right).unwrap();

                            if let Object::Error(error) = eval_right {
                                return Some(Object::Error(error));
                            }

                            return Some(self.eval_infix_expression(
                                infix_expression.operator.as_ref(),
                                &eval_left,
                                &eval_right,
                            ));
                        }
                    }

                    None
                }
                Expression::If(if_expression) => self.eval_if_expression(if_expression),
                Expression::Ident(identifier) => self.eval_identifier(identifier),
                Expression::Function(function_literal) => Some(Object::Function(
                    function_literal.parameters.clone(),
                    function_literal.body.clone().unwrap(),
                    Rc::clone(&self.environment),
                )),
                Expression::Call(call_expression) => {
                    // let function_object = self.eval(*call_expression.function.clone()).unwrap();

                    // return match function_object {
                    //     Object::Function(_, _, _) => {
                    //         let args = self.eval_expressions(call_expression.arguments.clone());
                    //         println!("{:#?}", args);
                    //         self.apply_function(&function_object, args)

                    //     },
                    //     Object::Error(_) => Some(function_object),
                    //     _ => None,
                    // }
                    // if let Some(call_function) = call_expression.clone().function {
                    //     let function = self.eval(*call_function, env.clone());

                    //     if function.as_ref().map_or(false, Object::is_error) {
                    //         return function;
                    //     }

                    //     let args = self.eval_expressions(
                    //         call_expression.clone().arguments,
                    //         env,
                    //     );

                    //     if let Some(Object::Error(_)) = args[0] {
                    //         return args[0].clone();
                    //     }

                    //     return self.apply_function(function.as_ref().unwrap(), args);
                    // }

                    None
                }
            },
            NodeKind::Integer(integer_literal) => Some(self.eval_integer(integer_literal)),
            NodeKind::Boolean(boolean) => Some(self.eval_boolean(boolean)),
            NodeKind::Identifier(identifier) => self.eval_identifier(identifier),
            _ => None,
        }
    }

    // fn apply_function(&mut self, function: &Object, args: Vec<Option<Object>>) -> Option<Object> {
    //     if let Object::Function(params, body, env) = function {
    //         let extended_env = self.extend_function_env(env, params, args);
    //         let evaluated = self.eval(body);

    //         return evaluated;
    //     }

    //     Some(Object::Error(format!(
    //         "not a function: {}",
    //         function.kind()
    //     )))
    // }

    // fn extend_function_env(
    //     &mut self,
    //     function_env: &Environment,
    //     function_params: &[Identifier],
    //     args: Vec<Option<Object>>,
    // ) -> Environment {
    //     let mut env = Environment::new_enclosed(*function_env);

    //     for (index, param) in function_params.iter().enumerate() {
    //         args[index]
    //             .as_ref()
    //             .and_then(|arg| env.set(&param.value, arg));
    //     }

    //     env
    // }

    fn eval_expressions(
        &mut self,
        expressions: Vec<Box<Expression>>,
    ) -> Vec<Option<Object>> {
        let mut result: Vec<Option<Object>> = vec![];

        for expression in expressions {
            let evaluated = self.eval(expression.as_ref());

            if let Some(Object::Error(_)) = evaluated {
                result = vec![evaluated];

                break;
            }

            result.push(evaluated);
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator::Evaluator;
    use crate::lexer::Lexer;
    use crate::object::{Object, INTEGER_OBJ, RETURN_VALUE_OBJ};
    use crate::parser::Parser;

    fn compare(input: &str, expected: Object) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut evaluator = Evaluator::new();

        if let Ok(program) = parser.parse_program() {
            let evaluated = evaluator.eval(program).unwrap();

            assert_eq!(evaluated, expected);
        } else {
            panic!("failed to parse program");
        }
    }

    #[test]
    fn primitives() {
        compare("3", Object::Integer(3));
    }

    fn test_eval(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut evaluator = Evaluator::new();

        if let Ok(program) = parser.parse_program() {
            return evaluator.eval(program);
        } else {
            panic!("failed to parse program");
        }
    }

    fn test_integer_object(obj: Object, expected: Option<i64>) {
        match expected {
            Some(value) => {
                println!("{}", obj);
                    assert_eq!(obj.kind(), INTEGER_OBJ);
                    
                    if let Object::Integer(integer) = obj {
                        assert_eq!(integer, value);
                    };
            }
            None => assert!(obj.is_null()),
        };
    }

    fn test_return_value_object(obj: Object, expected: Option<Object>) {
        match expected {
            Some(value) => {
                assert_eq!(obj.kind(), RETURN_VALUE_OBJ);

                    if let Object::ReturnValue(rv) = obj {
                        assert_eq!(rv, Box::new(value));
                    }
            }
            None => assert!(obj.is_null()),
        }
    }

    fn test_boolean_object(obj: Object, expected: bool) -> bool {
        obj.is_boolean()
            && match obj {
                Object::Boolean(boolean) => boolean == expected,
                _ => false,
            }
    }

    fn test_error_object(obj: Object, expected: &str) {
        assert!(obj.is_error(), format!("{} is not an error", obj.kind()));

        match obj {
            Object::Error(message) => assert_eq!(message, expected),
            _ => panic!("{} is not an error", obj.kind()),
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: "32",
                expected: 32,
            },
            Test {
                input: "10",
                expected: 10,
            },
            Test {
                input: "-32",
                expected: -32,
            },
            Test {
                input: "-10",
                expected: -10,
            },
            Test {
                input: "5 + 5 + 5 + 5 - 10",
                expected: 10,
            },
            Test {
                input: "2 * 2 * 2 * 2 * 2",
                expected: 32,
            },
            Test {
                input: "-50 + 100 + -50",
                expected: 0,
            },
            Test {
                input: "5 * 2 + 10",
                expected: 20,
            },
            Test {
                input: "5 + 2 * 10",
                expected: 25,
            },
            Test {
                input: "20 + 2 * -10",
                expected: 0,
            },
            Test {
                input: "50 / 2 * 2 + 10",
                expected: 60,
            },
            Test {
                input: "2 * (5 + 10)",
                expected: 30,
            },
            Test {
                input: "3 * (3 * 3) + 10",
                expected: 37,
            },
            Test {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                expected: 50,
            },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            test_integer_object(evaluated, Some(test.expected));
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        struct Test<'a> {
            input: &'a str,
            expected: bool,
        }

        let tests = vec![
            Test {
                input: "true",
                expected: true,
            },
            Test {
                input: "false",
                expected: false,
            },
            Test {
                input: "1 < 2",
                expected: true,
            },
            Test {
                input: "1 > 2",
                expected: false,
            },
            Test {
                input: "1 < 1",
                expected: false,
            },
            Test {
                input: "1 > 1",
                expected: false,
            },
            Test {
                input: "1 == 1",
                expected: true,
            },
            Test {
                input: "1 != 1",
                expected: false,
            },
            Test {
                input: "1 == 2",
                expected: false,
            },
            Test {
                input: "1 != 2",
                expected: true,
            },
            Test {
                input: "true == true",
                expected: true,
            },
            Test {
                input: "false == false",
                expected: true,
            },
            Test {
                input: "true == false",
                expected: false,
            },
            Test {
                input: "true != false",
                expected: true,
            },
            Test {
                input: "false != true",
                expected: true,
            },
            Test {
                input: "(1 < 2) == true",
                expected: true,
            },
            Test {
                input: "(1 < 2) == false",
                expected: false,
            },
            Test {
                input: "(1 > 2) == true",
                expected: false,
            },
            Test {
                input: "(1 > 2) == false",
                expected: true,
            },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            assert!(test_boolean_object(evaluated, test.expected));
        }
    }

    #[test]
    fn test_bang_operator() {
        struct Test<'a> {
            input: &'a str,
            expected: bool,
        }

        let tests = vec![
            Test {
                input: "!true",
                expected: false,
            },
            Test {
                input: "!false",
                expected: true,
            },
            Test {
                input: "!5",
                expected: false,
            },
            Test {
                input: "!!true",
                expected: true,
            },
            Test {
                input: "!!false",
                expected: false,
            },
            Test {
                input: "!!5",
                expected: true,
            },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            assert!(test_boolean_object(evaluated, test.expected));
        }
    }

    #[test]
    fn test_if_else_expressions() {
        struct Test<'a> {
            input: &'a str,
            expected: Option<i64>,
        }

        let tests = vec![
            Test {
                input: "if (true) { 10 }",
                expected: Some(10),
            },
            Test {
                input: "if (false) { 10 }",
                expected: None,
            },
            Test {
                input: "if (1) { 10 }",
                expected: Some(10),
            },
            Test {
                input: "if (1 < 2) { 10 }",
                expected: Some(10),
            },
            Test {
                input: "if (1 > 2) { 10 }",
                expected: None,
            },
            Test {
                input: "if (1 > 2) { 10 } else { 20 }",
                expected: Some(20),
            },
            Test {
                input: "if (1 < 2) { 10 } else { 20 }",
                expected: Some(10),
            },
            Test {
                input: "if (1 > 2) { 10 } else { 20 }",
                expected: Some(20),
            },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            test_integer_object(evaluated, test.expected);
        }
    }

    #[test]
    fn test_eval_return_statements() {
        struct Test<'a> {
            input: &'a str,
            expected: Option<Object>,
        }

        let tests = vec![
            Test {
                input: "return 10",
                expected: Some(Object::Integer(10)),
            },
            Test {
                input: "return 10; 9;",
                expected: Some(Object::Integer(10)),
            },
            Test {
                input: "return 2 * 5; 9;",
                expected: Some(Object::Integer(10)),
            },
            Test {
                input: "9; return 2 * 5; 9;",
                expected: Some(Object::Integer(10)),
            },
            Test {
                input: "
if (true) {
    if (5 == 5) {
        return 10;
    }

    return 1;
}
",
                expected: Some(Object::Integer(10)),
            },
        ];

        for test in tests.into_iter() {
            let evaluated = test_eval(test.input).unwrap();

            test_return_value_object(evaluated, test.expected);
        }
    }

    #[test]
    fn test_error_handling() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let tests = vec![
            Test {
                input: "5 + true",
                expected: "type mismatch: INTEGER + BOOLEAN",
            },
            Test {
                input: "5 + true; 5;",
                expected: "type mismatch: INTEGER + BOOLEAN",
            },
            Test {
                input: "-true",
                expected: "unknown operator: -BOOLEAN",
            },
            Test {
                input: "true + false;",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "5; true + false; 5",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "if (10 > 1) { true + false }",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "
if (10 > 1) {
    if (10 > 1) {
       return true + false;
    }

    return 1;
}
",
                expected: "unknown operator: BOOLEAN + BOOLEAN",
            },
            Test {
                input: "foobar",
                expected: "identifier not found: foobar",
            },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            test_error_object(evaluated, test.expected);
        }
    }

    #[test]
    fn test_eval_let_statements() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: "let a = 5; a;",
                expected: 5,
            },
            Test {
                input: "let a = 5 * 5; a;",
                expected: 25,
            },
            Test {
                input: "let a = 5; let b = a; b;",
                expected: 5,
            },
            Test {
                input: "let a = 5; let b = a; let c = a + b + 5; c;",
                expected: 15,
            },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            test_integer_object(evaluated, Some(test.expected));
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input).expect("failed to evaluate");

        if let Object::Function(parameters, body, env) = evaluated {
            assert_eq!(parameters.len(), 1);
            assert_eq!(parameters[0].to_string(), "x");

            let expected_body = "(x + 2)";

            assert_eq!(body.to_string(), expected_body);
        } else {
            panic!("not a function object: {}", evaluated.kind());
        }
    }

    #[test]
    fn test_eval_function() {
        struct Test<'a> {
            input: &'a str,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: "let identity = fn(x) { x; }; identity(5)",
                expected: 5,
            },
            // Test {
            //     input: "let identity = fn(x) { return x; }; identity(5)",
            //     expected: 5,
            // },
            // Test {
            //     input: "let double = fn(x) { x * 2 }; double(5)",
            //     expected: 10,
            // },
            // Test {
            //     input: "let add = fn(x, y) { x + y }; add(5, 5);",
            //     expected: 10,
            // },
            // Test {
            //     input: "let add = fn(x, y) { x + y }; add(5, 5);",
            //     expected: 10,
            // },
            // Test {
            //     input: "let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5));",
            //     expected: 20,
            // },
            // Test {
            //     input: "fn(x) { x; }(5)",
            //     expected: 5,
            // },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            test_integer_object(evaluated, Some(test.expected));
        }
    }
}
