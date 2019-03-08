use crate::ast::{
    Node,
    NodeKind,
    statement::Statement,
    expression::Expression,

};
use crate::ast::{int, boolean};
use crate::object::{Object, Integer, Boolean, Null, TRUE, FALSE, NULL};

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

fn eval_statements(statements: &Vec<Statement>) -> Option<Object> {
    let mut result = None;

    statements.iter().for_each(|statement| {
        result = eval(statement)
    });

    result
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
                _ => None,
            }
        },
        NodeKind::Expression(expression) => match expression {
            Expression::Int(integer_expression) => eval(integer_expression),
            Expression::Bool(boolean_expression) => eval(boolean_expression),
            _ => None,
        },
        NodeKind::Integer(integer_literal) => Some(eval_integer(integer_literal)),
        NodeKind::Boolean(boolean) => Some(eval_boolean(boolean)),
        _ => None,
    }
}