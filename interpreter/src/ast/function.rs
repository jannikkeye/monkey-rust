use super::identifier::Identifier;
use super::statement::BlockStatement;
use super::{Node, NodeKind};
use crate::token::Token;
use std::fmt;

#[derive(Debug, Eq, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Option<BlockStatement>,
}

impl FunctionLiteral {
    pub fn new(token: &Token) -> Self {
        FunctionLiteral {
            token: token.clone(),
            parameters: vec![],
            body: None,
        }
    }
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Function(self)
    }
}

impl PartialEq for FunctionLiteral {
    fn eq(&self, other: &FunctionLiteral) -> bool {
        self.token == other.token && self.parameters == other.parameters && self.body == other.body
    }
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();
        let parameters = self
            .parameters
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<String>>();

        string.push_str("(");
        string.push_str(&parameters.join(", "));
        string.push_str(")");

        if let Some(body) = &self.body {
            string.push_str(&body.to_string());
        }

        write!(f, "{}", &string)
    }
}
