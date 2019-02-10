use crate::token::Token;
use super::Node;

#[derive(Debug)]
pub struct Expression {
    pub token: Token,
    pub value: String,
}

impl PartialEq for Expression {
    fn eq(&self, other: &Expression) -> bool {
        self.token == other.token && self.value == other.value
    }
}

impl Eq for Expression {}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}