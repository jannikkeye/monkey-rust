use std::fmt;
use crate::token::{Token, TokenKind};
use super::{
    Node,
    NodeKind,
    identifier::Identifier,
    expression::Expression,
};

#[derive(Debug, Eq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

#[derive(Debug, Eq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>,
    pub value: Option<Expression>,
}

#[derive(Debug, Eq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

#[derive(Debug, Eq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
}

#[derive(Debug, Eq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl ExpressionStatement {
    pub fn new(token: &Token) -> ExpressionStatement {
        ExpressionStatement {
            token: token.clone(),
            expression: None,
        }
    }
}

impl BlockStatement {
    pub fn new(token: &Token) -> BlockStatement {
        BlockStatement {
            token: token.clone(),
            statements: vec![],
        }
    }

    pub fn push_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
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
            Statement::Block(l) => &l.token.literal,
        }
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Statement(self)
    }
}

impl Node for &Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(l) => &l.token.literal,
            Statement::Return(l) => &l.token.literal,
            Statement::Expression(l) => &l.token.literal,
            Statement::Block(l) => &l.token.literal,
        }
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Statement(self)
    }
}

impl PartialEq for LetStatement {
    fn eq(&self, other: &LetStatement) -> bool {
        self.name == other.name && self.token == other.token && self.value == other.value
    }
}

impl PartialEq for ReturnStatement {
    fn eq(&self, other: &ReturnStatement) -> bool {
        self.token == other.token && self.return_value == other.return_value
    }
}

impl PartialEq for ExpressionStatement {
    fn eq(&self, other: &ExpressionStatement) -> bool {
        self.token == other.token && self.expression == other.expression
    }
}

impl PartialEq for BlockStatement {
    fn eq(&self, other: &BlockStatement) -> bool {
        self.token == other.token && self.statements == other.statements
    }
}

impl PartialEq for Statement {
    fn eq(&self, other: &Statement) -> bool {
        match self {
            Statement::Let(let_statement) => {
                match other {
                    Statement::Let(other_let_statement) => let_statement == other_let_statement,
                    _ => false,
                }
            },
            Statement::Return(return_statement) => {
                match other {
                    Statement::Return(other_return_statement) => return_statement == other_return_statement,
                    _ => false,
                }
            },
            Statement::Expression(expression_statement) => {
                match other {
                    Statement::Expression(other_expression_statement) => expression_statement == other_expression_statement,
                    _ => false,
                }
            },
            Statement::Block(block_statement) => {
                match other {
                    Statement::Block(other_block_statement) => block_statement == other_block_statement,
                    _ => false,
                }
            }
        }
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token.literal,
            match &self.name {
                Some(name) => name.to_string(),
                None => String::new(),
            },
            match &self.value {
                Some(value) => value.to_string(),
                None => String::new(),
            },
        )
    }
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {};",
            self.token.literal,
            match &self.return_value {
                Some(return_value) => return_value.to_string(),
                None => String::new(),
            }
        )
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();

        for s in &self.statements {
            string.push_str(&s.to_string());
        }

        write!(f, "{}", string)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(s) => write!(
                f,
                "{}",
                s.to_string(),
            ),
            Statement::Return(s) => write!(
                f,
                "{}",
                s.to_string(),
            ),
            Statement::Expression(s) => write!(
                f,
                "{}",
                match &s.expression {
                    Some(e) => e.to_string(),
                    None => String::new(),
                }
            ),
            Statement::Block(b) => write!(
                f,
                "{}",
                b.to_string(),
            ),
            _ => write!(f, "")
        }
    }
}