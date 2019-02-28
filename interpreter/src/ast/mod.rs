pub mod expression;
pub mod identifier;
pub mod program;
pub mod int;
pub mod statement;
pub mod prefix;
pub mod infix;
pub mod boolean;
pub mod if_expression;
pub mod function;

pub trait Node {
    fn token_literal(&self) -> &str;
}