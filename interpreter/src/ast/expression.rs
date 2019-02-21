use std::fmt;
use super::Node;
use super::identifier::Identifier;
use super::int::IntegerLiteral;
use super::prefix::Prefix;

#[derive(Debug)]
pub enum Expression {
    Ident(Identifier),
    Int(IntegerLiteral),
    Prefix(Prefix),
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
            Expression::Prefix(prefix) => {
                match other {
                    Expression::Prefix(other_prefix) => prefix.token == other_prefix.token && prefix.operator == other_prefix.operator && prefix.right == other_prefix.right,
                    _ => false
                }
            }
        }
    }
}

impl Eq for Expression {}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Ident(ident) => ident.token_literal(),
            Expression::Int(int) => &int.token_literal(),
            Expression::Prefix(prefix) => &prefix.token_literal(),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident.value),
            Expression::Int(int) => write!(f, "{}", int.value),
            Expression::Prefix(prefix) => write!(
                f, 
                "({}{})",
                prefix.operator,
                match &*prefix.right {
                    Some(prefix) => prefix.to_string(),
                    None => "".to_owned(),
                },
            )
        }
    }
}