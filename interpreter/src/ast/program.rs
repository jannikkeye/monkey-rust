use super::{
    Node,
    statement::Statement,
};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: vec![],
        }
    }
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        match self.statements.len() > 0 {
            true => self.statements[0].token_literal(),
            false => ""
        }
    }
}