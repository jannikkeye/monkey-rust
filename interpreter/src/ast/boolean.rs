use std::fmt;

use super::{Node, NodeKind};
use crate::token::Token;

#[derive(Debug, Eq, Clone)]
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

impl PartialEq for Boolean {
    fn eq(&self, other: &Boolean) -> bool {
        self.token == other.token && self.value == other.value
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Boolean(self)
    }
}

impl Node for &Boolean {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Boolean(self)
    }
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}
