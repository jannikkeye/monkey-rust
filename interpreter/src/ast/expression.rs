use super::boolean::Boolean;
use super::call::Call;
use super::function::FunctionLiteral;
use super::identifier::Identifier;
use super::if_expression::If;
use super::infix::Infix;
use super::int::IntegerLiteral;
use super::prefix::Prefix;
use super::string::StringLiteral;
use super::array::Array;
use super::index_expression::IndexExpression;
use super::{Node, NodeKind};
use std::fmt;

#[derive(Debug, Eq, Clone)]
pub enum Expression {
    Ident(Identifier),
    Int(IntegerLiteral),
    Bool(Boolean),
    Prefix(Prefix),
    Infix(Infix),
    If(If),
    Function(FunctionLiteral),
    Call(Call),
    Str(StringLiteral),
    Array(Array),
    Index(IndexExpression),
}

impl PartialEq for Expression {
    fn eq(&self, other: &Expression) -> bool {
        match self {
            Expression::Ident(ident) => match other {
                Expression::Ident(other_ident) => ident == other_ident,
                _ => false,
            },
            Expression::Int(int) => match other {
                Expression::Int(other_int) => int == other_int,
                _ => false,
            },
            Expression::Bool(boolean) => match other {
                Expression::Bool(other_boolean) => boolean == other_boolean,
                _ => false,
            },
            Expression::Prefix(prefix) => match other {
                Expression::Prefix(other_prefix) => prefix == other_prefix,
                _ => false,
            },
            Expression::Infix(infix) => match other {
                Expression::Infix(other_infix) => infix == other_infix,
                _ => false,
            },
            Expression::If(if_expression) => match other {
                Expression::If(other_if_expression) => if_expression == other_if_expression,
                _ => false,
            },
            Expression::Function(function) => match other {
                Expression::Function(other_function) => function == other_function,
                _ => false,
            },
            Expression::Call(call) => match other {
                Expression::Call(other_call) => call == other_call,
                _ => false,
            },
            Expression::Str(string_literal) => match other {
                Expression::Str(other_string_literal) => string_literal == other_string_literal,
                _ => false,
            },
            Expression::Array(array) => match other {
                Expression::Array(other_array) => array == other_array,
                _ => false,
            },
            Expression::Index(index_expression) => match other {
                Expression::Index(other_index_expression) => index_expression == other_index_expression,
                _ => false,
            }
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Ident(ident) => ident.token_literal(),
            Expression::Int(int) => &int.token_literal(),
            Expression::Bool(boolean) => &boolean.token_literal(),
            Expression::Prefix(prefix) => &prefix.token_literal(),
            Expression::Infix(infix) => &infix.token_literal(),
            Expression::If(if_expression) => &if_expression.token_literal(),
            Expression::Function(function) => &function.token_literal(),
            Expression::Call(call) => &call.token_literal(),
            Expression::Str(string_literal) => &string_literal.token_literal(),
            Expression::Array(array) => &array.token_literal(),
            Expression::Index(index_expression) => &index_expression.token_literal(),
        }
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Expression(self)
    }
}

impl Node for &Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Ident(ident) => ident.token_literal(),
            Expression::Int(int) => &int.token_literal(),
            Expression::Bool(boolean) => &boolean.token_literal(),
            Expression::Prefix(prefix) => &prefix.token_literal(),
            Expression::Infix(infix) => &infix.token_literal(),
            Expression::If(if_expression) => &if_expression.token_literal(),
            Expression::Function(function) => &function.token_literal(),
            Expression::Call(call) => &call.token_literal(),
            Expression::Str(string_literal) => &string_literal.token_literal(),
            Expression::Array(array) => &array.token_literal(),
            Expression::Index(index_expression) => &index_expression.token_literal(),
        }
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Expression(self)
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident.value),
            Expression::Int(int) => write!(f, "{}", int.value),
            Expression::Bool(boolean) => write!(f, "{}", boolean.value),
            Expression::Prefix(prefix) => write!(
                f,
                "({}{})",
                prefix.operator,
                match &*prefix.right {
                    Some(expr) => expr.to_string(),
                    None => "".to_owned(),
                },
            ),
            Expression::Infix(infix) => write!(
                f,
                "({} {} {})",
                match &*infix.left {
                    Some(expr) => expr.to_string(),
                    None => "".to_owned(),
                },
                infix.operator,
                match &*infix.right {
                    Some(expr) => expr.to_string(),
                    None => "".to_owned(),
                },
            ),
            Expression::If(if_expression) => write!(f, "{}", if_expression.to_string()),
            Expression::Function(function) => write!(f, "{}", function.to_string()),
            Expression::Call(call) => write!(f, "{}", call.to_string()),
            Expression::Str(string_literal) => write!(f, "{}", string_literal.value),
            Expression::Array(array) => write!(f, "{}", array),
            Expression::Index(index_expression) => write!(f, "{}", index_expression.to_string()),
        }
    }
}
