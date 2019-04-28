use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{boolean, if_expression, int, string, array};
use crate::ast::{
    expression::Expression,
    identifier::Identifier,
    statement::{ReturnStatement, Statement},
    Node, NodeKind,
};
use crate::environment::Environment;
use crate::object::{Object, BOOLEAN_OBJ, STRING_OBJ};
use crate::builtins::BUILTINS;

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

    fn eval_statements(&mut self, statements: &[Statement]) -> Option<Object> {
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
            (Object::Str(l), Object::Str(r)) => self.eval_string_infix_expression(operator, l.clone(), r.clone()),
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
            (Object::Str(_), Object::Boolean(_)) => Object::Error(format!(
                "type mismatch: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )),
            (Object::Str(_), Object::Integer(_)) => Object::Error(format!(
                "type mismatch: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )),
            (Object::Boolean(_), Object::Str(_)) => Object::Error(format!(
                "type mismatch: {} {} {}",
                left.kind(),
                operator,
                right.kind()
            )),
            (Object::Integer(_), Object::Str(_)) => Object::Error(format!(
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
            _ => Object::Error(format!(
                "unknown operator: {} {} {}",
                BOOLEAN_OBJ, operator, BOOLEAN_OBJ,
            )),
        }
    }

    fn eval_string_infix_expression(&self, operator: &str, left: String, right: String) -> Object {
        match operator {
            "==" => self.native_boolean_to_boolean_object(left == right),
            "!=" => self.native_boolean_to_boolean_object(left != right),
            "+" => {
                let mut new_string = String::new();

                new_string.push_str(&left);
                new_string.push_str(&right);

                Object::Str(new_string)
            }
            _ => Object::Error(format!(
                "unknown operator: {} {} {}",
                STRING_OBJ, operator, STRING_OBJ,
            )),
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

    fn eval_if_expression(&mut self, if_expression: &if_expression::If) -> Option<Object> {
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

    fn eval_return_statement(&mut self, return_statement: &ReturnStatement) -> Option<Object> {
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
        if let Some(value) = self.environment.borrow().get(&identifier.value) {
            return Some(value);
        }


        if let Some((_, built_in)) = BUILTINS.iter().find(|(key, _)| key.as_ref() == identifier.value) {
            return Some(built_in.clone());
        }

        return Some(Object::Error(format!(
            "identifier not found: {}",
            identifier.value
        )));
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

                        return match value.clone() {
                            Some(Object::Error(_)) => value,
                            None => None,
                            Some(v) => {
                                if let Some(identifier) = &let_statement.name {
                                    self.environment.borrow_mut().set(&identifier.value, v);
                                }

                                value.clone()
                            }
                        };
                    }

                    None
                }
            },
            NodeKind::Block(block) => self.eval_statements(&block.statements),
            NodeKind::Expression(expression) => match expression {
                Expression::Array(array) => self.eval(array),
                Expression::Str(string_literal) => self.eval(string_literal),
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
                    let function_object = self.eval(*call_expression.function.clone()).unwrap();

                    return match function_object {
                        Object::Function(_, _, _) | Object::BuiltInFunction(_) => {
                            let args = self.eval_expressions(call_expression.arguments.clone());
                            self.apply_function(&function_object, args)
                        },
                        Object::Error(_) => Some(function_object),
                        _ => None,
                    };
                }
            },
            NodeKind::Integer(integer_literal) => Some(self.eval_integer(integer_literal)),
            NodeKind::Boolean(boolean) => Some(self.eval_boolean(boolean)),
            NodeKind::StringLiteral(string_literal) => Some(self.eval_string(string_literal)),
            NodeKind::Identifier(identifier) => self.eval_identifier(identifier),
            NodeKind::Array(array) => Some(self.eval_array(array)),
            _ => None,
        }
    }

    fn eval_array(&mut self, array_object: &array::Array) -> Object {
        let elements = self.eval_expressions(array_object.clone().elements.into_iter().map(|v| Box::new(v)).collect());

        if let Some(Object::Error(_)) = elements[0] {
            return elements[0].clone().unwrap();
        }

        Object::Array(elements.into_iter().map(|v| v.unwrap()).collect())
    }

    fn eval_string(&self, string_literal: &string::StringLiteral) -> Object {
        Object::Str(string_literal.value.clone())
    }

    fn apply_function(&mut self, function: &Object, args: Vec<Option<Object>>) -> Option<Object> {
        if let Object::Function(params, body, function_env) = function {
            if params.len() != args.len() {
                return Some(Object::Error(format!(
                    "Wrong number of arguments supplied ({})/({})",
                    args.len(),
                    params.len()
                )));
            }
            let old_env = Rc::clone(&self.environment);
            let mut new_env = Environment::new_enclosed(Rc::clone(&function_env));
            let zipped = params.into_iter().zip(args.into_iter());

            for (param, arg) in zipped {
                new_env.set(&param.value, arg.unwrap());
            }

            // use new environment for evaluating function body
            self.environment = Rc::new(RefCell::new(new_env));

            let evaluated = self.eval(body);

            // switch back to old enviroment afterwards
            self.environment = old_env;

            return evaluated;
        }

        if let Object::BuiltInFunction(built_in) = function {
            return Some((built_in.func)(&args));
        }

        Some(Object::Error(format!(
            "not a function: {}",
            function.kind()
        )))
    }

    fn eval_expressions(&mut self, expressions: Vec<Box<Expression>>) -> Vec<Option<Object>> {
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
    use crate::object::{Object};
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

    #[test]
    fn test_eval_integer_expression() {
        compare("32", Object::Integer(32));
        compare("10", Object::Integer(10));
        compare("-32", Object::Integer(-32));
        compare("-10", Object::Integer(-10));
        compare("5 + 5 + 5 + 5 - 10", Object::Integer(10));
        compare("2 * 2 * 2 * 2 * 2", Object::Integer(32));
        compare("-50 + 100 + -50", Object::Integer(0));
        compare("5 * 2 + 10", Object::Integer(20));
        compare("5 + 2 * 10", Object::Integer(25));
        compare("20 + 2 * -10", Object::Integer(0));
        compare("50 / 2 * 2 + 10", Object::Integer(60));
        compare("2 * (5 + 10)", Object::Integer(30));
        compare("3 * (3 * 3) + 10", Object::Integer(37));
        compare("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50));
    }

    #[test]
    fn test_eval_boolean_expression() {
        compare("true", Object::Boolean(true));
        compare("false", Object::Boolean(false));
        compare("1 < 2", Object::Boolean(true));
        compare("1 > 2", Object::Boolean(false));
        compare("1 < 1", Object::Boolean(false));
        compare("1 > 1", Object::Boolean(false));
        compare("1 == 1", Object::Boolean(true));
        compare("1 != 1", Object::Boolean(false));
        compare("1 == 2", Object::Boolean(false));
        compare("1 != 2", Object::Boolean(true));
        compare("true == true", Object::Boolean(true));
        compare("false == false", Object::Boolean(true));
        compare("true == false", Object::Boolean(false));
        compare("true != false", Object::Boolean(true));
        compare("false != true", Object::Boolean(true));
        compare("(1 < 2) == true", Object::Boolean(true));
        compare("(1 < 2) == false", Object::Boolean(false));
        compare("(1 > 2) == true", Object::Boolean(false));
        compare("(1 > 2) == false", Object::Boolean(true));
    }

    #[test]
    fn test_bang_operator() {
        compare("!true", Object::Boolean(false));
        compare("!false", Object::Boolean(true));
        compare("!5", Object::Boolean(false));
        compare("!!true", Object::Boolean(true));
        compare("!!false", Object::Boolean(false));
        compare("!!5", Object::Boolean(true));
    }

    #[test]
    fn test_if_else_expressions() {
        compare("if (true) { 10 }", Object::Integer(10));
        compare("if (false) { 10 }", Object::Null);
        compare("if (1) { 10 }", Object::Integer(10));
        compare("if (1 < 2) { 10 }", Object::Integer(10));
        compare("if (1 > 2) { 10 }", Object::Null);
        compare("if (1 > 2) { 10 } else { 20 }", Object::Integer(20));
        compare("if (1 < 2) { 10 } else { 20 }", Object::Integer(10));
        compare("if (1 > 2) { 10 } else { 20 }", Object::Integer(20));
    }

    #[test]
    fn test_eval_return_statements() {
        compare("return 10", Object::ReturnValue(Box::new(Object::Integer(10))));
        compare("return 10; 9;", Object::ReturnValue(Box::new(Object::Integer(10))));
        compare("return 2 * 5; 9;", Object::ReturnValue(Box::new(Object::Integer(10))));
        compare("9; return 2 * 5; 9;", Object::ReturnValue(Box::new(Object::Integer(10))));
        compare("
if (true) {
    if (5 == 5) {
        return 10;
    }

    return 1;
}
", Object::ReturnValue(Box::new(Object::Integer(10))));
    }

    #[test]
    fn test_error_handling() {
        compare("5 + true", Object::Error(format!("{}", "type mismatch: INTEGER + BOOLEAN")));
        compare("5 + true; 5;", Object::Error(format!("{}", "type mismatch: INTEGER + BOOLEAN")));
        compare("-true", Object::Error(format!("{}", "unknown operator: -BOOLEAN")));
        compare("true + false;", Object::Error(format!("{}", "unknown operator: BOOLEAN + BOOLEAN")));
        compare("5; true + false; 5", Object::Error(format!("{}", "unknown operator: BOOLEAN + BOOLEAN")));
        compare("if (10 > 1) { true + false }", Object::Error(format!("{}", "unknown operator: BOOLEAN + BOOLEAN")));
        compare("
if (10 > 1) {
    if (10 > 1) {
        return true + false;
    }

    return 1;
}
", Object::Error(format!("{}", "unknown operator: BOOLEAN + BOOLEAN")));
        compare("foobar", Object::Error(format!("{}", "identifier not found: foobar")));
        compare(r#"3 + "hello there""#, Object::Error(format!("{}", "type mismatch: INTEGER + STRING")));
        compare(r#""3" + true"#, Object::Error(format!("{}", "type mismatch: STRING + BOOLEAN")));
        compare(r#""b" - "b""#, Object::Error(format!("{}", "unknown operator: STRING - STRING")));
    }

    #[test]
    fn test_eval_let_statements() {
        compare("let a = 5; a;", Object::Integer(5));
        compare("let a = 5 * 5; a;", Object::Integer(25));
        compare("let a = 5; let b = a; b;", Object::Integer(5));
        compare("let a = 5; let b = a; let c = a + b + 5; c;", Object::Integer(15));
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input).expect("failed to evaluate");

        if let Object::Function(parameters, body, _) = evaluated {
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
        compare("let identity = fn(x) { x; }; identity(5)", Object::Integer(5));
        compare("let identity = fn(x) { x; }; identity(5)", Object::Integer(5));
        compare("let double = fn(x) { x * 2 }; double(5)", Object::Integer(10));
        compare("let add = fn(x, y) { x + y }; add(5, 5);", Object::Integer(10));
        compare("let add = fn(x, y) { x + y }; add(5, 5);", Object::Integer(10));
        compare("let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5));", Object::Integer(20));
        compare("fn(x) { x; }(5)", Object::Integer(5));
    }

    #[test]
    fn test_eval_string_expressions() {
        compare(r#""hello world""#, Object::Str(String::from("hello world")));
        compare(r#""hello \"world\"""#, Object::Str(String::from("hello \"world\"")));
    }

    #[test]
    fn test_eval_built_in_functions() {
        compare(r#"len("")"#, Object::Integer(0));
        compare(r#"len("four")"#, Object::Integer(4));
        compare(r#"len("hello world")"#, Object::Integer(11));
    }

    #[test]
    fn test_eval_array() {
        compare("[1, 2, 3, 4]", Object::Array(vec![Object::Integer(1), Object::Integer(2), Object::Integer(3), Object::Integer(4)]));
    }
}
