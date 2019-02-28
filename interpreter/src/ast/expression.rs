use std::fmt;
use super::Node;
use super::identifier::Identifier;
use super::int::IntegerLiteral;
use super::prefix::Prefix;
use super::infix::Infix;
use super::boolean::Boolean;

#[derive(Debug, Eq)]
pub enum Expression {
    Ident(Identifier),
    Int(IntegerLiteral),
    Bool(Boolean),
    Prefix(Prefix),
    Infix(Infix),
}

impl PartialEq for Expression {
    fn eq(&self, other: &Expression) -> bool {
        match self {
            Expression::Ident(ident) => {
                match other {
                    Expression::Ident(other_ident) => ident.token == other_ident.token && ident.value == other_ident.value,
                    _ => false,
                }
            },
            Expression::Int(int) => {
                match other {
                    Expression::Int(other_int) => int.token == other_int.token && int.value == other_int.value,
                    _ => false
                }
            },
            Expression::Bool(boolean) => {
                match other {
                    Expression::Bool(other_boolean) => boolean.value == other_boolean.value,
                    _ => false,
                }
            },
            Expression::Prefix(prefix) => {
                match other {
                    Expression::Prefix(other_prefix) => prefix.token == other_prefix.token && prefix.operator == other_prefix.operator && prefix.right == other_prefix.right,
                    _ => false
                }
            },
            Expression::Infix(infix) => {
                match other {
                    Expression::Infix(other_infix) => infix.token == other_infix.token && infix.operator == other_infix.operator && infix.left == other_infix.left && infix.right == other_infix.right,
                    _ => false
                }
            }
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Ident(ident) => ident.token_literal(),
            Expression::Int(int) => &int.token_literal(),
            Expression::Bool(boolean) =>&boolean.token_literal(),
            Expression::Prefix(prefix) => &prefix.token_literal(),
            Expression::Infix(infix) => &infix.token_literal(),
        }
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
            )
        }
    }
}