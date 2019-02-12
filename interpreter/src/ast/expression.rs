use std::fmt;
use super::Node;
use super::identifier::Identifier;
use super::int::IntegerLiteral;

#[derive(Debug)]
pub enum Expression {
    Ident(Identifier),
    Int(IntegerLiteral),
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
            }
        }
    }
}

impl Eq for Expression {}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Ident(ident) => ident.token_literal(),
            Expression::Int(int) => &int.token_literal()
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Ident(ident) => write!(f, "{}", ident.value),
            Expression::Int(int) => write!(f, "{}", int.value),
        }
    }
}