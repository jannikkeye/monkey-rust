use crate::token::Token;
use super::expression::Expression;
use super::{Node, NodeKind};

#[derive(Debug, Eq)]
pub struct Prefix {
    pub token: Token,
    pub operator: String,
    pub right: std::boxed::Box<Option<Expression>>,
}

impl Prefix {
    pub fn new(token: &Token, operator: &str, right: Option<Expression>) -> Self {
        Prefix {
            token: token.clone(),
            operator: operator.to_owned(),
            right: Box::new(right),
        }
    }
}

impl PartialEq for Prefix {
    fn eq(&self, other: &Prefix) -> bool {
        self.token == other.token && self.operator == other.operator && self.right == other.right
    }
}

impl Node for Prefix {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Prefix(self)
    }
}