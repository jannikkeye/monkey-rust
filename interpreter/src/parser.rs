use crate::ast::{
    program::Program,
    statement::{Statement, LetStatement, ReturnStatement, ExpressionStatement},
    expression::Expression,
    identifier::Identifier
};
use std::collections::HashMap;
use crate::lexer::Lexer;
use crate:: token::{Token, TokenKind};
use std::{error::Error, fmt};

type Prefix_parse_fn = fn () -> Option<Expression>;
type Infix_parse_fn = fn (expr: Expression) -> Option<Expression>;

#[repr(C)]
enum Precedence {
    LOWEST = 1,
    EQUALS = 2,
    LESSGREATER = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6,
    CALL = 7,
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<String>,
}

#[derive(Debug)]
struct ProgramParsingError;
impl Error for ProgramParsingError {}
impl fmt::Display for ProgramParsingError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Failed to parse program.")
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
            errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        println!("{:?} {:?}", parser.current_token, parser.peek_token);

        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        match &self.current_token {
            Some(token) => {
                match token.kind {
                    TokenKind::IDENT => self.parse_identifier(),
                    _ => None,
                }
            },
            None => None,
        }
    }

    fn parse_program(&mut self) -> Result<Program, ProgramParsingError> {
        let mut program = Program::new();

        
        while let Some(token) = &self.current_token {
            if token.kind != TokenKind::EOF {
                let statement = self.parse_statement();

                match statement {
                    Some(statement) => program.statements.push(statement),
                    None => self.next_token(),
                };
            } else {
                self.current_token = None;
            }
        }

        Ok(program)
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        match &self.current_token {
            Some(token) => {
                Some(Expression::Ident(Identifier::new(&token, &token.literal)))
            },
            None => None,
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match &self.current_token {
            Some(token) => match token.kind {
                TokenKind::LET => self.parse_let_statement(),
                TokenKind::RETURN => self.parse_return_statement(),
                _ => self.parse_expression_statement(),
            },
            None => {
                None
            },
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        match &mut self.current_token {
            Some(token) => {
                let mut statement = ExpressionStatement::new(token);

                statement.expression = self.parse_expression(Precedence::LOWEST);

                if self.peek_token_is(TokenKind::SEMICOLON) {
                    self.next_token();
                }
                
                if self.peek_token_is(TokenKind::EOF) {
                    self.next_token();
                }

                Some(Statement::Expression(statement))
            },
            None => None,
        }
    }

    fn parse_expression(&mut self, precendence: Precedence) -> Option<Expression> {
        match self.parse_prefix() {
            Some(expression) => Some(expression),
            None => None,
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        match &mut self.current_token {
            Some(token) => {
                let return_statement = ReturnStatement::new(&token);

                self.next_token();

                Some(Statement::Return(return_statement))
            },
            None => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        match &mut self.current_token {
            Some(token) => {
                let mut let_statement = LetStatement::new(&token);

                if !self.expect_peek(TokenKind::IDENT) {
                    return None;
                }

                let identifier = self.current_token.clone().unwrap();

                let_statement.name = Some(Identifier::new(&identifier, &identifier.literal));


                if !self.expect_peek(TokenKind::ASSIGN) {
                    return None;
                }

                while !self.current_token_is(TokenKind::SEMICOLON) {
                    self.next_token();
                }

                Some(Statement::Let(let_statement))
            },
            None => None,
        }
    }

    fn current_token_is(&self, kind: TokenKind) -> bool {
        match &self.current_token {
            Some(token) => token.kind == kind,
            None => false,
        }
    }

    fn peek_token_is(&self, kind: TokenKind) -> bool {
        match &self.peek_token {
            Some(token) => token.kind == kind,
            None => false,
        }
    }

    fn expect_peek(&mut self, kind: TokenKind) -> bool {
        match &self.peek_token {
            Some(token) => {
                if token.kind == kind {
                    self.next_token();

                    return true;
                }

                self.peek_error(kind);

                false
            },
            None => false
        }
    }

    fn peek_error(&mut self, kind: TokenKind) {
        let peek_token = self.peek_token.clone();

        self.errors.push(
            format!(
                "expected next token to be {:?}, got {:?} instead",
                kind,
                peek_token.unwrap().kind,
            )
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        use crate::ast::{statement::Statement, Node};
        let input = "
let x =  5;
let y = 10;
let foobar = 838383;
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        for e in parser.errors.iter() {
            println!("parse error: {}", e);
        }

        match program {
            Err(error) => panic!(format!("{}", error)),
            Ok(program) => {
                println!("{:#?}", program);
                assert_eq!(program.statements.len(), 3);
                let tests: [(&str, &str); 3] = [
                    ("x", "5"),
                    ("y", "10"),
                    ("foobar", "838383"),
                ];

                for (i, (name, value)) in tests.iter().enumerate() {
                    let statement: &Statement = &program.statements[i];

                    assert_eq!(statement.token_literal(), "let");
                    // assert_eq!(statement.name, Some(Identifier::new()));
                    // assert_eq!(&statement.value.unwrap().value, value);
                }
            }
        }

    }

    #[test]
    fn test_return_statements() {
        use crate::ast::{statement::Statement, Node};
        let input = "
return 5;
return 10;
return 993322;
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        for e in parser.errors.iter() {
            println!("parse error: {}", e);
        }

        match program {
            Err(error) => panic!(format!("{}", error)),
            Ok(program) => {
                println!("{:#?}", program);
                assert_eq!(program.statements.len(), 3);
                let tests: [&str; 3] = [
                    "5",
                    "10",
                    "838383",
                ];

                for (i, return_value) in tests.iter().enumerate() {
                    let statement: &Statement = &program.statements[i];

                    match statement {
                        Statement::Return(r) => {
                            match &r.return_value {
                                Some(value) => assert_eq!(&value.token_literal(), return_value),
                                None => {}
                            }
                        },
                        _ => {}
                    };
                }
            }
        }
    }

    #[test]
    fn test_string() {
        use crate::token::{Token, TokenKind};
        
        let input = "let five = 5;
let ten = 10;
let nine = 9;
        ";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        match program {
            Ok(p) => assert_eq!(p.to_string(), "let five = 5;let ten = 10;let nine = 9;"),
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        for e in parser.errors.iter() {
            println!("parse error: {}", e);
        }

        match program {
            Ok(p) => {
                assert_eq!(p.statements.len(), 1);

                match &p.statements[0] {
                    Statement::Expression(statement) => {
                        println!("{:?}", statement);
                        assert_eq!(statement.token.kind, TokenKind::IDENT);
                        assert_eq!(statement.token.literal, "foobar");
                        assert!(statement.expression.is_some());
                    },
                    _ => panic!("statement not an expression"),
                }
            },
            Err(err) => panic!(err),
        }
    }
}