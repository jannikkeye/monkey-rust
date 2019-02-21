use crate::ast::{
    program::Program,
    statement::{Statement, LetStatement, ReturnStatement, ExpressionStatement},
    expression::Expression,
    identifier::Identifier,
    int::IntegerLiteral,
    prefix::Prefix,
};
use std::collections::HashMap;
use crate::lexer::Lexer;
use crate:: token::{Token, TokenKind};
use std::{error::Error, fmt};

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

        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            let mut prefix = Prefix::new(token, &token.literal, None);

            self.next_token();

            prefix.right = Box::new(self.parse_expression(Precedence::PREFIX));

            return Some(Expression::Prefix(prefix));
        }

        None
    }

    fn parse_program(&mut self) -> Result<Program, ProgramParsingError> {
        let mut program = Program::new();

        
        while let Some(token) = &self.current_token {
            println!("{:?}", token);
            if token.kind != TokenKind::EOF {
                let statement = self.parse_statement();

                println!("{:?}", statement);

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
        if let Some(token) = &self.current_token {
            return match token.kind {
                TokenKind::IDENT => Some(Expression::Ident(Identifier::new(&token, &token.literal))),
                TokenKind::INT => Some(Expression::Int(IntegerLiteral::new(&token, &token.literal))),
                _ => None,
            };
        }

        None
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        if let Some(token) = &self.current_token {
            return match token.kind {
                TokenKind::LET => self.parse_let_statement(),
                TokenKind::RETURN => self.parse_return_statement(),
                _ => self.parse_expression_statement(),
            };
        }

        None
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        if let Some(token) = &mut self.current_token {
            let mut statement = ExpressionStatement::new(token);

            statement.expression = self.parse_expression(Precedence::LOWEST);

            if self.peek_token_is(TokenKind::SEMICOLON) {
                self.next_token();
            }
            
            if self.peek_token_is(TokenKind::EOF) {
                self.next_token();
            }

            return Some(Statement::Expression(statement));
        }

        None
    }

    fn is_prefix_expression(&self) -> bool {
        if let Some(token) = &self.current_token {
            return match token.kind {
                TokenKind::BANG => true,
                TokenKind::MINUS => true,
                _ => false
            };
        } else {
            return false;
        }
    }

    fn parse_expression(&mut self, precendence: Precedence) -> Option<Expression> {
        if self.is_prefix_expression() {
            return self.parse_prefix();
        }

        self.parse_identifier()
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
                        assert_eq!(statement.token.kind, TokenKind::IDENT);
                        assert_eq!(statement.token.literal, "foobar");
                        assert!(statement.expression.is_some());

                        match &statement.expression {
                            Some(Expression::Ident(identifier)) => {
                                assert_eq!(identifier.value, "foobar");
                                assert_eq!(identifier.token.kind, TokenKind::IDENT);
                                assert_eq!(identifier.token.literal, "foobar");
                            },
                            Some(_) => {},
                            None => {},
                        }
                    },
                    _ => panic!("statement not an expression"),
                }
            },
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_integer_expression() {
        let input = "5;";
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program();

        println!("{:#?}", program);

        for e in parser.errors.iter() {
            println!("parse error: {}", e);
        }

        match program {
            Ok(p) => {
                assert_eq!(p.statements.len(), 1);

                match &p.statements[0] {
                    Statement::Expression(statement) => {
                        assert_eq!(statement.token.kind, TokenKind::INT);
                        assert_eq!(statement.token.literal, "5");
                        assert!(statement.expression.is_some());
                        
                        match &statement.expression {
                            Some(Expression::Int(int)) => {
                                assert_eq!(int.value, 5);
                                assert_eq!(int.token.kind, TokenKind::INT);
                                assert_eq!(int.token.literal, "5");
                            },
                            Some(_) => {},
                            None => {},
                        }
                    },
                    _ => panic!("statement not an expression"),
                }
            },
            Err(err) => panic!(err),
        }
    }

    #[test]
    fn test_prefix_expression() {
        let inputs = vec!["!5;", "-15;"];
        let operators = vec!["!", "-"];
        let integer_values = vec![5, 15];
        let token_kinds = vec![TokenKind::BANG, TokenKind::MINUS];
        let token_literals = vec!["!", "-"];
        let integer_literals = vec!["5", "15"];

        for (index, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            println!("{:#?}", program);

            for e in parser.errors.iter() {
                println!("parse error: {}", e);
            }

            match program {
                Ok(program) => {
                    assert_eq!(program.statements.len(), 1);

                match &program.statements[0] {
                    Statement::Expression(statement) => {
                        assert_eq!(statement.token.kind, token_kinds[index]);
                        assert_eq!(statement.token.literal, token_literals[index]);
                        assert!(statement.expression.is_some());
                        
                        match &statement.expression {
                            Some(Expression::Int(int)) => {
                                assert_eq!(int.value, integer_values[index]);
                                assert_eq!(int.token.kind, TokenKind::INT);
                                assert_eq!(int.token.literal, integer_literals[index]);
                            },
                            Some(_) => {},
                            None => {},
                        }
                    },
                    _ => panic!("statement not an expression"),
                }
                },
                Err(err) => panic!(err),
            }
        }
    }
}