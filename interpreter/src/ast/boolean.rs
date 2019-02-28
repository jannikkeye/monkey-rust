use std::fmt;

use super::Node;
use crate::token::Token;


#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Boolean {
    pub fn new(token: &Token, value: bool) -> Self {
        Boolean {
            token: token.clone(),
            value,
        }
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}