pub mod expression;
pub mod identifier;
pub mod program;
pub mod int;
pub mod statement;
pub mod prefix;

pub trait Node {
    fn token_literal(&self) -> &str;
}