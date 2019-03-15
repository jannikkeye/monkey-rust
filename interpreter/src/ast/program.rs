use super::{statement::Statement, Node, NodeKind};
use std::fmt;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program { statements: vec![] }
    }
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        match self.statements.len() > 0 {
            true => self.statements[0].token_literal(),
            false => "",
        }
    }

    fn kind(&self) -> NodeKind {
        NodeKind::Program(self)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();

        for s in &self.statements {
            string.push_str(&s.to_string());
        }

        write!(f, "{}", string)
    }
}
