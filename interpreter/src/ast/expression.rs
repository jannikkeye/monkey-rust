use std::fmt;
use super::Node;
use super::identifier::Identifier;

#[derive(Debug)]
pub enum Expression {
    Ident(Identifier),
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
            _ => false,
        }
    }
}

impl Eq for Expression {}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Ident(ident) => &ident.token.literal,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident.value),
        }
    }
}