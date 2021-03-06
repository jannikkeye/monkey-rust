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
pub mod string;
pub mod array;
pub mod index_expression;

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
    StringLiteral(&'a string::StringLiteral),
    Array(&'a array::Array),
    IndexExpression(&'a index_expression::IndexExpression),
}

pub trait Node {
    fn token_literal(&self) -> &str;
    fn kind(&self) -> NodeKind;
}
