use crate::ast::{
    Node,
    NodeKind,
    statement::{Statement, ReturnStatement},
    expression::Expression,

};
use crate::ast::{int, boolean, if_expression};
use crate::object::{Object, Integer, ReturnValue, TRUE, FALSE, NULL};

fn eval_integer(integer_literal: &int::IntegerLiteral) -> Object {
    Object::Integer(Integer { value: integer_literal.value })
}

fn eval_boolean(boolean: &boolean::Boolean) -> Object {
    if boolean.value {
        return Object::Boolean(TRUE);
    } else {
        return Object::Boolean(FALSE);
    }
}

fn eval_statements(statements: &[Statement]) -> Option<Object> {
    let mut result = None;

    println!("{:?}", statements);

    for statement in statements {
        println!("{:?}", statement);
        result = eval(statement);

        if let Some(Object::ReturnValue(return_value)) = result {
            return Some(return_value.value);
        }
    };

    result
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prexif_operator_expression(right),
        _ => Object::Null(NULL),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(boolean) => match boolean {
            TRUE => Object::Boolean(FALSE),
            FALSE => Object::Boolean(TRUE),
        },
        Object::Null(_) => Object::Boolean(TRUE),
        _ => Object::Boolean(FALSE),
    }
}

fn native_boolean_to_boolean_object(value: bool) -> Object {
    if value {
        return Object::Boolean(TRUE);
    } else {
        return Object::Boolean(FALSE);
    }
}

fn eval_minus_prexif_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(integer_object) => Object::Integer(Integer { value: -integer_object.value }),
        _ => Object::Null(NULL),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    if let Object::Integer(l) = left {
        if let Object::Integer(r) = right {
            return eval_integer_infix_expression(operator, l, r);
        }
    }

    Object::Null(NULL)
}

fn eval_integer_infix_expression(operator: &str, left: Integer, right: Integer) -> Object {
    let left_val = left.value;
    let right_val = right.value;

    match operator {
        "+" => Object::Integer(Integer { value: left_val + right_val }),
        "-" => Object::Integer(Integer { value: left_val - right_val }),
        "*" => Object::Integer(Integer { value: left_val * right_val }),
        "/" => Object::Integer(Integer { value: left_val / right_val }),
        "<" => native_boolean_to_boolean_object(left_val < right_val),
        ">" => native_boolean_to_boolean_object(left_val > right_val),
        "==" => native_boolean_to_boolean_object(left_val == right_val),
        "!=" => native_boolean_to_boolean_object(left_val != right_val),
        _ => Object::Null(NULL),
    }
}

fn eval_if_expression(if_expression: &if_expression::If) -> Option<Object> {
    let condition = if_expression.condition.as_ref().map(|c| c.as_ref()).unwrap();
    let consequence = if_expression.consequence.as_ref().unwrap();
    let alternative = if_expression.alternative.as_ref();
    let eval_condition = eval(condition).unwrap();

    if is_thruthy(eval_condition) {
        return  eval(consequence);
    } else if if_expression.alternative.is_some() {
        return eval(alternative.unwrap());
    }

    Some(Object::Null(NULL))
}

fn is_thruthy(object: Object) -> bool {
    match object {
        Object::Null(_) => false,
        Object::Boolean(value) => value.value,
        _ => true,
    }
}

fn eval_return_statement(return_statement: &ReturnStatement) -> Option<Object> {
    if let Some(return_value) = &return_statement.return_value {
        let value = eval(return_value).unwrap();

        return Some(Object::ReturnValue(Box::new(ReturnValue { value })))
    }

    None
}

pub fn eval(node: impl Node) -> Option<Object> {
    match node.kind() {
        NodeKind::Program(program) => eval_statements(&program.statements),
        NodeKind::Statement(statement) => {
            match &statement {
                Statement::Expression(expression_statement) => {
                    if let Some(expression) = &expression_statement.expression {
                        return eval(expression);
                    }

                    None
                },
                Statement::Return(return_statement) => eval_return_statement(return_statement),
                Statement::Block(block_statement) => eval_statements(&block_statement.statements),
                _ => None,
            }
        },
        NodeKind::Block(block) => eval_statements(&block.statements),
        NodeKind::Expression(expression) => match expression {
            Expression::Int(integer_expression) => eval(integer_expression),
            Expression::Bool(boolean_expression) => eval(boolean_expression),
            Expression::Prefix(prefix_expression) => {
                if let Some(right) = &*prefix_expression.right {
                    let eval_right = eval(right);

                    return Some(eval_prefix_expression(prefix_expression.operator.as_ref(), eval_right.unwrap()));
                }

                None

            },
            Expression::Infix(infix_expression) => {
                if let Some(left) = &*infix_expression.left {
                    if let Some(right) = &*infix_expression.right {
                        let eval_left = eval(left).unwrap();
                        let eval_right = eval(right).unwrap();

                        return Some(eval_infix_expression(infix_expression.operator.as_ref(), eval_left, eval_right));
                    }
                }

                None
            },
            Expression::If(if_expression) => eval_if_expression(if_expression),
            _ => None,
        },
        NodeKind::Integer(integer_literal) => Some(eval_integer(integer_literal)),
        NodeKind::Boolean(boolean) => Some(eval_boolean(boolean)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::object::{Object, ObjectVariant, Integer, INTEGER_OBJ, Boolean, BOOLEAN_OBJ, NULL_OBJ};
    use crate::evaluator;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn test_eval(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

         if let Ok(program) = parser.parse_program() {
             return evaluator::eval(program);
         } else {
             panic!("failed to parse program");
         }
    }

    fn test_integer_object(obj: Integer, expected: Option<i64>) -> bool {
        match expected {
            Some(value) => obj.kind() == INTEGER_OBJ && obj.value == value,
            None => obj.kind() == NULL_OBJ
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
            Test { input: "32", expected: 32 },
            Test { input: "10", expected: 10 },
            Test { input: "-32", expected: -32 },
            Test { input: "-10", expected: -10 },
            Test { input: "5 + 5 + 5 + 5 - 10", expected: 10 },
            Test { input: "2 * 2 * 2 * 2 * 2", expected: 32 },
            Test { input: "-50 + 100 + -50", expected: 0 },
            Test { input: "5 * 2 + 10", expected: 20 },
            Test { input: "5 + 2 * 10", expected: 25 },
            Test { input: "20 + 2 * -10", expected: 0 },
            Test { input: "50 / 2 * 2 + 10", expected: 60 },
            Test { input: "2 * (5 + 10)", expected: 30 },
            Test { input: "3 * (3 * 3) + 10", expected: 37 },
            Test { input: "(5 + 10 * 2 + 15 / 3) * 2 + -10", expected: 50 },
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
            Test { input: "true", expected: true },
            Test { input: "false", expected: false },
            Test { input: "1 < 2", expected: true },
            Test { input: "1 > 2", expected: false },
            Test { input: "1 < 1", expected: false },
            Test { input: "1 > 1", expected: false },
            Test { input: "1 == 1", expected: true },
            Test { input: "1 != 1", expected: false },
            Test { input: "1 == 2", expected: false },
            Test { input: "1 != 2", expected: true },
            Test { input: "true == true", expected: true },
            Test { input: "false == false", expected: true },
            Test { input: "true == false", expected: false },
            Test { input: "true != false", expected: true },
            Test { input: "false != true", expected: true },
            Test { input: "(1 < 2) == true", expected: true },
            Test { input: "(1 < 2) == false", expected: false },
            Test { input: "(1 > 2) == true", expected: false },
            Test { input: "(1 > 2) == false", expected: true },
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
            Test { input: "!true", expected: false },
            Test { input: "!false", expected: true },
            Test { input: "!5", expected: false },
            Test { input: "!!true", expected: true },
            Test { input: "!!false", expected: false },
            Test { input: "!!5", expected: true },
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
            Test { input: "if (true) { 10 }", expected: Some(10) },
            Test { input: "if (false) { 10 }", expected: None },
            Test { input: "if (1) { 10 }", expected: Some(10) },
            Test { input: "if (1 < 2) { 10 }", expected: Some(10) },
            Test { input: "if (1 > 2) { 10 }", expected: None },
            Test { input: "if (1 > 2) { 10 } else { 20 }", expected: Some(20) },
            Test { input: "if (1 < 2) { 10 } else { 20 }", expected: Some(10) },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            if let Object::Integer(integer) = evaluated {
                assert!(test_integer_object(integer, test.expected));
            }
        }
    }

    #[test]
    fn test_return_statements() {
        struct Test<'a> {
            input: &'a str,
            expected: Option<i64>,
        }

        let tests = vec![
            Test { input: "return 10", expected: Some(10) },
            Test { input: "return 10; 9;", expected: Some(10) },
            Test { input: "return 2 * 5; 9;", expected: Some(10) },
            Test { input: "9; return 2 * 5; 9;", expected: Some(10) },
        ];

        for test in tests.iter() {
            let evaluated = test_eval(test.input).unwrap();

            if let Object::Integer(integer) = evaluated {
                assert!(test_integer_object(integer, test.expected));
            }
        }
    }
}
