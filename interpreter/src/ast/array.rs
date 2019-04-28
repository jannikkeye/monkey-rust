use std::fmt;

use super::expression::Expression;
use super::{Node, NodeKind};
use crate::token::Token;

#[derive(Debug, Clone, Eq)]
pub struct Array {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl PartialEq for Array {
    fn eq(&self, other: &Array) -> bool {
        if self.elements.len() != other.elements.len() {
            return false;
        }

        let mut el_zip = self.elements.iter().zip(other.elements.iter());

        self.token == other.token && el_zip.all(|(a, b)| a == b)
    }
}

impl Node for Array {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Array(self)
    }
}

impl Node for &Array {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Array(self)
    }
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();

        string.push_str("[");

        let element_strings: Vec<String> = self.elements.iter()
            .map(|e| e.to_string())
            .collect();

        string.push_str(&element_strings.join(", "));

        string.push_str("]");

        write!(f, "{}", string)
    }
}