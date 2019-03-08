use crate::token::Token;
use super::expression::Expression;
use super::{Node, NodeKind};

#[derive(Debug)]
pub struct Infix {
    pub token: Token,
    pub left: std::boxed::Box<Option<Expression>>,
    pub operator: String,
    pub right: std::boxed::Box<Option<Expression>>,
}

impl Infix {
    pub fn new(token: &Token, left: Option<Expression>, operator: &str, right: Option<Expression>) -> Self {
        Infix {
            token: token.clone(),
            left: Box::new(left),
            operator: operator.to_owned(),
            right: Box::new(right),
        }
    }
}

impl PartialEq for Infix {
    fn eq(&self, other: &Infix) -> bool {
        self.token == other.token && self.operator == other.operator && self.right == other.right && self.left == other.left
    }
}

impl Eq for Infix {}

impl Node for Infix {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Infix(self)
    }
}