use crate::token::Token;
use super::Node;

#[derive(Debug, Eq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: &Token, value: &str) -> Self {
        let parsed_value = value.parse::<i64>().expect("failed to parse integer literal");

        IntegerLiteral {
            token: token.clone(),
            value: parsed_value,
        }
        
    }
}

impl PartialEq for IntegerLiteral {
    fn eq(&self, other: &IntegerLiteral) -> bool {
        self.token == other.token && self.value == other.value
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}