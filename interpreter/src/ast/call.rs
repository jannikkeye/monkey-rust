use crate::token::Token;
use super::expression::Expression;
use super::{Node, NodeKind};
use std::fmt;

#[derive(Debug, Eq)]
pub struct Call {
    pub token: Token,
    pub function: Option<Box<Expression>>,
    pub arguments: Vec<Option<Box<Expression>>>,
}

impl Call {
    pub fn new(token: &Token, function: Option<Box<Expression>>) -> Self {
        Call {
            token: token.clone(),
            function,
            arguments: vec![],
        }
    }
}

impl PartialEq for Call {
    fn eq(&self, other: &Call) -> bool {
        self.token == other.token && self.function == other.function && self.arguments == other.arguments
    }
}

impl Node for Call {
    fn token_literal(&self) -> &str {
        &self.token.literal
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Call(self)
    }
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();
        let mut arguments: Vec<String> = vec![];

        for argument in self.arguments.iter() {
            if let Some(arg) = argument {
                let str_rep = arg.to_string();

                arguments.push(str_rep);
            }
        }

        if let Some(func) = &self.function {
            string.push_str(&func.to_string());
        }

        string.push_str("(");
        string.push_str(&arguments.join(", "));
        string.push_str(")");

        write!(f, "{}", &string)
    }
}
