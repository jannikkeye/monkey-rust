use crate::ast::{boolean, if_expression, int};
use crate::ast::{
    expression::Expression,
    identifier::Identifier,
    statement::{ReturnStatement, Statement},
    Node, NodeKind,
};
use crate::environment::Environment;
use crate::object::{Error, Integer, Object, ObjectVariant, ReturnValue, FALSE, NULL, TRUE};

#[derive(Default)]
pub struct Evaluator {
    environment: Environment,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            environment: Environment::new(),
        }
    }

    fn eval_integer(&self, integer_literal: &int::IntegerLiteral) -> Object {
        Object::Integer(Integer {
            value: integer_literal.value,
        })
    }

    fn eval_boolean(&self, boolean: &boolean::Boolean) -> Object {
        if boolean.value {
            Object::Boolean(TRUE)
        } else {
            Object::Boolean(FALSE)
        }
    }

    fn eval_statements(&mut self, statements: &[Statement]) -> Option<Object> {
        let mut result = None;

        for statement in statements {
            result = self.eval(statement);

            match result {
                Some(Object::ReturnValue(return_value)) => return Some(return_value.value),
                Some(Object::Error(error)) => return Some(Object::Error(error)),
                _ => {}
            }
        }

        result
    }

    fn eval_prefix_expression(&self, operator: &str, right: Object) -> Object {
        match operator {
            "!" => self.eval_bang_operator_expression(right),
            "-" => self.eval_minus_prexif_operator_expression(right),
            _ => Object::Error(Error::new(format!(
                "unkown operator: {}{}",
                operator,
                right.inspect()
            ))),
        }
    }

    fn eval_bang_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Boolean(boolean) => match boolean {
                TRUE => Object::Boolean(FALSE),
                FALSE => Object::Boolean(TRUE),
            },
            Object::Null(_) => Object::Boolean(TRUE),
            _ => Object::Boolean(FALSE),
        }
    }

    fn native_boolean_to_boolean_object(&self, value: bool) -> Object {
        if value {
            Object::Boolean(TRUE)
        } else {
            Object::Boolean(FALSE)
        }
    }

    fn eval_minus_prexif_operator_expression(&self, right: Object) -> Object {
        match right {
            Object::Integer(integer_object) => Object::Integer(Integer {
                value: -integer_object.value,
            }),
            _ => Object::Error(Error::new(format!("unknown operator: -{}", right.kind()))),
        }
    }

    fn eval_infix_expression(&self, operator: &str, left: Object, right: Object) -> Object {
        match (&left, &right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.eval_integer_infix_expression(operator, l, r)
            }
            (Object::Integer(l), Object::Boolean(r)) => Object::Error(Error::new(format!(
                "type mismatch: {} {} {}",
                l.kind(),
                operator,
                r.kind()
            ))),
            (Object::Boolean(l), Object::Integer(r)) => Object::Error(Error::new(format!(
                "type mismatch: {} {} {}",
                l.kind(),
                operator,
                r.kind()
            ))),
            (_, _) => Object::Error(Error::new(format!(
                "unknown operator: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            ))),
        }
    }

    fn eval_integer_infix_expression(
        &self,
        operator: &str,
        left: &Integer,
        right: &Integer,
    ) -> Object {
        let left_val = left.value;
        let right_val = right.value;

        match operator {
            "+" => Object::Integer(Integer {
                value: left_val + right_val,
            }),
            "-" => Object::Integer(Integer {
                value: left_val - right_val,
            }),
            "*" => Object::Integer(Integer {
                value: left_val * right_val,
            }),
            "/" => Object::Integer(Integer {
                value: left_val / right_val,
            }),
            "<" => self.native_boolean_to_boolean_object(left_val < right_val),
            ">" => self.native_boolean_to_boolean_object(left_val > right_val),
            "==" => self.native_boolean_to_boolean_object(left_val == right_val),
            "!=" => self.native_boolean_to_boolean_object(left_val != right_val),
            _ => Object::Error(Error::new(format!(
                "unkown operator: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            ))),
        }
    }

    fn eval_if_expression(&mut self, if_expression: &if_expression::If) -> Option<Object> {
        let condition = if_expression
            .condition
            .as_ref()
            .map(|c| c.as_ref())
            .unwrap();
        let consequence = if_expression.consequence.as_ref().unwrap();
        let alternative = if_expression.alternative.as_ref();
        let eval_condition = self.eval(condition).unwrap();

        if let Object::Error(error) = eval_condition {
            return Some(Object::Error(error));
        }

        if self.is_thruthy(eval_condition) {
            return self.eval(consequence);
        } else if if_expression.alternative.is_some() {
            return self.eval(alternative.unwrap());
        }

        Some(Object::Null(NULL))
    }

    fn is_thruthy(&self, object: Object) -> bool {
        match object {
            Object::Null(_) => false,
            Object::Boolean(value) => value.value,
            _ => true,
        }
    }

    fn eval_return_statement(&mut self, return_statement: &ReturnStatement) -> Option<Object> {
        if let Some(return_value) = &return_statement.return_value {
            let value = self.eval(return_value).unwrap();

            if let Object::Error(error) = value {
                return Some(Object::Error(error));
            }

            return Some(Object::ReturnValue(Box::new(ReturnValue { value })));
        }

        None
    }

    fn eval_identifier(&self, identifier: &Identifier) -> Option<Object> {
        let value = self.environment.get(&identifier.value);

        if value.is_none() {
            return Some(Object::Error(Error::new(format!(
                "identifier not found: {}",
                identifier.value
            ))));
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
                Statement::Return(return_statement) => self.eval_return_statement(return_statement),
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
                                    self.environment.set(&identifier.value, &v);
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
                                eval_left,
                                eval_right,
                            ));
                        }
                    }

                    None
                }
                Expression::If(if_expression) => self.eval_if_expression(if_expression),
                Expression::Ident(identifier) => self.eval_identifier(identifier),
                _ => None,
            },
            NodeKind::Integer(integer_literal) => Some(self.eval_integer(integer_literal)),
            NodeKind::Boolean(boolean) => Some(self.eval_boolean(boolean)),
            NodeKind::Identifier(identifier) => self.eval_identifier(identifier),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator::Evaluator;
    use crate::lexer::Lexer;
    use crate::object::{
        Boolean, Integer, Object, ObjectVariant, BOOLEAN_OBJ, INTEGER_OBJ, NULL_OBJ,
    };
    use crate::parser::Parser;

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

    fn test_integer_object(obj: Integer, expected: Option<i64>) -> bool {
        match expected {
            Some(value) => obj.kind() == INTEGER_OBJ && obj.value == value,
            None => obj.kind() == NULL_OBJ,
        }
    }

    fn test_boolean_object(obj: Boolean, expected: bool) -> bool {
        obj.kind() == BOOLEAN_OBJ && obj.value == expected
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

            if let Object::Integer(int) = evaluated {
                assert!(test_integer_object(int, Some(test.expected)));
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

            if let Object::Boolean(boolean) = evaluated {
                assert!(test_boolean_object(boolean, test.expected));
            }
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

            if let Object::Boolean(boolean) = evaluated {
                assert!(test_boolean_object(boolean, test.expected));
            }
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
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            if let Object::Integer(integer) = evaluated {
                assert!(test_integer_object(integer, test.expected));
            }
        }
    }

    #[test]
    fn test_eval_return_statements() {
        struct Test<'a> {
            input: &'a str,
            expected: Option<i64>,
        }

        let tests = vec![
            Test {
                input: "return 10",
                expected: Some(10),
            },
            Test {
                input: "return 10; 9;",
                expected: Some(10),
            },
            Test {
                input: "return 2 * 5; 9;",
                expected: Some(10),
            },
            Test {
                input: "9; return 2 * 5; 9;",
                expected: Some(10),
            },
            Test {
                input: "
if (true) {
    if (5 == 5) {
        if (true) {
            return 10;
        }
       return 10;
    }

    return 1;
}

return 11;
",
                expected: Some(10),
            },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            if let Object::Integer(integer) = evaluated {
                assert!(test_integer_object(integer, test.expected));
            } else {
                panic!("not an integer object: {}", evaluated.kind());
            }
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

return 11;
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

            if let Object::Error(error) = evaluated {
                assert_eq!(error.message, test.expected);
            } else {
                panic!("not an error message: {:?}", evaluated.kind());
            }
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

            if let Object::Integer(int) = evaluated {
                assert_eq!(int.value, test.expected);
            } else {
                panic!("not an integer object: {}", evaluated.kind());
            }
        }
    }
}
