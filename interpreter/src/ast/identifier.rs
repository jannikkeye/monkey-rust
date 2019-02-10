use crate::token::Token;
use super::Node;

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: &Token, value: &str) -> Self {
        Identifier {
            token: token.clone(),
            value: value.to_owned(),
        }
    }
}


impl PartialEq for Identifier {
    fn eq(&self, other: &Identifier) -> bool {
        self.token == other.token && self.value == other.value
    }
}

impl Eq for Identifier {}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}