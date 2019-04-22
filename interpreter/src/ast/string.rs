use super::{Node, NodeKind};
use crate::token::Token;

#[derive(Debug, Eq, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl StringLiteral {
    pub fn new(token: &Token, value: &str) -> Self {
        StringLiteral {
            token: token.clone(),
            value: value.to_owned().clone(),
        }
    }
}

impl PartialEq for StringLiteral {
    fn eq(&self, other: &StringLiteral) -> bool {
        self.token == other.token && self.value == other.value
    }
}

impl Node for StringLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::StringLiteral(self)
    }
}

impl Node for &StringLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::StringLiteral(self)
    }
}
