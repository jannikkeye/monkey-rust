use crate::token::Token;
use super::identifier::Identifier;
use super::statement::BlockStatement;
use super::Node;
use std::fmt;

#[derive(Debug, Eq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub paramters: Vec<Identifier>,
    pub body: Option<BlockStatement>
}

impl FunctionLiteral {
    pub fn new(token: &Token) -> Self {
        FunctionLiteral {
            token: token.clone(),
            paramters: vec![],
            body: None,
        }
    }
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }
}

impl PartialEq for FunctionLiteral {
    fn eq(&self, other: &FunctionLiteral) -> bool {
        self.token == other.token && self.paramters == other.paramters && self.body == other.body
    }
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();
        let parameters = self.paramters.iter().map(|v| v.to_string()).collect::<Vec<String>>();

        string.push_str("(");
        string.push_str(&parameters.join(", "));
        string.push_str(")");

        if let Some(body) = &self.body {
            string.push_str(&body.to_string());
        }


        write!(f, "{}", &string)
    }
}
