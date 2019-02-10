pub mod expression;
pub mod identifier;
pub mod program;
pub mod statement;

pub trait Node {
    fn token_literal(&self) -> &str;
}