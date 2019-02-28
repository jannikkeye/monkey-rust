use crate::token::Token;
use super::expression::Expression;
use super::Node;

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

impl Node for Infix {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}