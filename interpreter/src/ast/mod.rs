pub mod boolean;
pub mod call;
pub mod expression;
pub mod function;
pub mod identifier;
pub mod if_expression;
pub mod infix;
pub mod int;
pub mod prefix;
pub mod program;
pub mod statement;

#[derive(Debug)]
pub enum NodeKind<'a> {
    Expression(&'a expression::Expression),
    Identifier(&'a identifier::Identifier),
    Program(&'a program::Program),
    Integer(&'a int::IntegerLiteral),
    Statement(&'a statement::Statement),
    Prefix(&'a prefix::Prefix),
    Infix(&'a infix::Infix),
    Boolean(&'a boolean::Boolean),
    If(&'a if_expression::If),
    Function(&'a function::FunctionLiteral),
    Call(&'a call::Call),
    Block(&'a statement::BlockStatement),
}

pub trait Node {
    fn token_literal(&self) -> &str;
    fn kind(&self) -> NodeKind;
}
