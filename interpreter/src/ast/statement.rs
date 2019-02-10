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
        }
    }
}