use super::expression::Expression;
use super::statement::BlockStatement;
use super::{Node, NodeKind};
use crate::token::Token;
use std::boxed::Box;
use std::fmt;

#[derive(Debug, Eq)]
pub struct If {
    pub token: Token,
    pub condition: Option<Box<Expression>>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
}

impl If {
    pub fn new(token: &Token) -> Self {
        If {
            token: token.clone(),
            condition: None,
            consequence: None,
            alternative: None,
        }
    }
}

impl PartialEq for If {
    fn eq(&self, other: &If) -> bool {
        self.token == other.token
            && self.condition == other.condition
            && self.consequence == other.consequence
            && self.alternative == other.alternative
    }
}

impl Node for If {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::If(self)
    }
}

impl fmt::Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();

        string.push_str("if");

        if let Some(condition) = self.condition.as_ref().map(|c| c.as_ref()) {
            string.push_str(&condition.to_string());
        }

        string.push_str(" ");

        if let Some(consequence) = &self.consequence {
            string.push_str(&consequence.to_string());
        }

        if let Some(alternative) = &self.alternative {
            string.push_str("else ");
            string.push_str(&alternative.to_string());
        }

        write!(f, "{}", string)
    }
}
