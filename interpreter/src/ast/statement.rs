use std::fmt;
use crate::token::{Token, TokenKind};
use super::{
    Node,
    identifier::Identifier,
    expression::Expression,
};

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>,
    pub value: Option<Expression>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
}

impl ExpressionStatement {
    pub fn new(token: &Token) -> ExpressionStatement {
        ExpressionStatement {
            token: token.clone(),
            expression: None,
        }
    }
}

impl ReturnStatement {
    pub fn new(token: &Token) -> ReturnStatement {
        ReturnStatement {
            token: token.clone(),
            return_value: None,
        }
    }
}

impl LetStatement {
    pub fn new(token: &Token) -> LetStatement {
        LetStatement {
            token: token.clone(),
            name: None,
            value: None,
        }
    }
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(l) => &l.token.literal,
            Statement::Return(l) => &l.token.literal,
            Statement::Expression(l) => &l.token.literal,
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(s) => write!(
                f,
                "{} {} = {};",
                s.token.literal,
                match &s.name {
                    Some(name) => name.to_string(),
                    None => String::new(),
                },
                match &s.value {
                    Some(value) => value.to_string(),
                    None => String::new(),
                },
            ),
            Statement::Return(s) => write!(
                f,
                "{} {};",
                s.token.literal,
                match &s.return_value {
                    Some(return_value) => return_value.to_string(),
                    None => String::new(),
                }
            ),
            _ => write!(f, "")
        }
    }
}