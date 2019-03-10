use crate::ast::{
    program::Program,
    statement::{Statement, LetStatement, ReturnStatement, ExpressionStatement, BlockStatement},
    expression::Expression,
    identifier::Identifier,
    int::IntegerLiteral,
    prefix::Prefix,
    infix::Infix,
    boolean::Boolean,
    if_expression::If,
    call::Call,
    function::FunctionLiteral,
};
use std::collections::HashMap;
use crate::lexer::Lexer;
use crate:: token::{Token, TokenKind};
use std::{error::Error, fmt};

#[repr(C)]
#[derive(PartialEq, PartialOrd, Copy, Clone, Debug)]
enum Precedence {
    LOWEST = 1,
    EQUALS = 2,
    LESSGREATER = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 6,
    CALL = 7,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    pub errors: Vec<String>,
    precedences: HashMap<TokenKind, Precedence>,
}

#[derive(Debug)]
pub struct ProgramParsingError;
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
            precedences: HashMap::with_capacity(8)
        };

        let precedences = vec![
            (TokenKind::EQ, Precedence::EQUALS),
            (TokenKind::NEQ, Precedence::EQUALS),
            (TokenKind::LT, Precedence::LESSGREATER),
            (TokenKind::GT, Precedence::LESSGREATER),
            (TokenKind::PLUS, Precedence::SUM),
            (TokenKind::MINUS, Precedence::SUM),
            (TokenKind::SLASH, Precedence::PRODUCT),
            (TokenKind::ASTERIKS, Precedence::PRODUCT),
            (TokenKind::LPAREN, Precedence::CALL),
        ];

        for p in precedences {
            parser.precedences.insert(p.0, p.1);
        }

        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();

        let expression = self.parse_expression(&Precedence::LOWEST);


        if !self.expect_peek(TokenKind::RPAREN) {
            return None;
        }

        expression
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            match token.kind {
                TokenKind::LPAREN => return self.parse_grouped_expression(),
                TokenKind::IF => return self.parse_if_expression(),
                TokenKind::FUNCTION => return self.parse_function_literal(),
                _ => {
                    let mut prefix = Prefix::new(token, &token.literal, None);

                    self.next_token();

                    prefix.right = Box::new(self.parse_expression(&Precedence::PREFIX));

                    return Some(Expression::Prefix(prefix));
                }
            }
        }

        None
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            let mut function_literal = FunctionLiteral::new(token);

            if !self.expect_peek(TokenKind::LPAREN) {
                return None;
            }

            function_literal.paramters = self.parse_function_parameters();

            if !self.expect_peek(TokenKind::LBRACE) {
                return None;
            }

            function_literal.body = self.parse_block_statement();

            return Some(Expression::Function(function_literal));
        }

        None
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut identifiers = vec![];

        if self.peek_token_is(TokenKind::RPAREN) {
            self.next_token();

            return identifiers;
        }

        self.next_token();

        if let Some(next_token) = &self.current_token {
            let mut identifier = Identifier::new(next_token, &next_token.literal);

            identifiers.push(identifier);

            while self.peek_token_is(TokenKind::COMMA) {
                self.next_token();
                self.next_token();

                if let Some(new_next_token) = &self.current_token {
                    identifier = Identifier::new(new_next_token, &new_next_token.literal);
                    identifiers.push(identifier);
                }
            }

            if !self.expect_peek(TokenKind::RPAREN) {
                return vec![];
            }
        }

        identifiers
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            let mut if_expression = If::new(&token);

            if !self.expect_peek(TokenKind::LPAREN) {
                return None;
            }


            self.next_token();

            if_expression.condition = Some(Box::new(self.parse_expression(&Precedence::LOWEST).unwrap()));

            if !self.expect_peek(TokenKind::RPAREN) {
                return None;
            }

            if !self.expect_peek(TokenKind::LBRACE) {
                return None;
            }

            if_expression.consequence = self.parse_block_statement();

            if self.peek_token_is(TokenKind::ELSE) {
                self.next_token();

                if !self.expect_peek(TokenKind::LBRACE) {
                    return None;
                }

                if_expression.alternative = self.parse_block_statement();
            }

            return Some(Expression::If(if_expression));
        }

        None
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        if let Some(token) = &self.current_token {
            let mut block = BlockStatement::new(&token);


            self.next_token();

            while self.current_token.is_some() && !self.current_token_is(TokenKind::RBRACE) && !self.current_token_is(TokenKind::EOF) {
                let statement = self.parse_statement();

                if let Some(s) = statement {
                    block.statements.push(s);
                }

                self.next_token();
            }

            return Some(block);
        }

        None
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        if let Some(token) = &self.current_token {
            let mut expression = Call::new(token, Some(Box::new(function)));

            if let Some(arguments) = self.parse_call_arguments() {
                expression.arguments = arguments;
            }

            return Some(Expression::Call(expression));
        }

        None
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Option<Box<Expression>>>> {
        let mut args = vec![];

        if self.peek_token_is(TokenKind::RPAREN) {
            self.next_token();

            return Some(args);
        }

        self.next_token();

        if let Some(expression) = self.parse_expression(&Precedence::LOWEST) {
            args.push(Some(Box::new(expression)));
        }

        while self.peek_token_is(TokenKind::COMMA) {
            self.next_token();
            self.next_token();

            if let Some(expression) = self.parse_expression(&Precedence::LOWEST) {
                args.push(Some(Box::new(expression)));
            }
        }

        if !self.expect_peek(TokenKind::RPAREN) {
            return None;
        }

        Some(args)
    }

    fn parse_infix(&mut self, left: Option<Expression>) -> Option<Expression> {
        if let Some(token) = self.current_token.clone() {
            if token.kind == TokenKind::LPAREN {
                if let Some(l) = left {
                    return self.parse_call_expression(l);
                }
            }

            let mut infix = Infix::new(&token, left, &token.literal, None);
            let precedence = self.current_precedence();
            
            self.next_token();

            infix.right = Box::new(self.parse_expression(&precedence));

            return Some(Expression::Infix(infix));
        }

        None
    }

    pub fn parse_program(&mut self) -> Result<Program, ProgramParsingError> {
        let mut program = Program::new();

        
        while let Some(token) = &self.current_token {
            if token.kind != TokenKind::EOF {
                if token.kind == TokenKind::SEMICOLON {
                    self.next_token();
                }

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
        if let Some(token) = &self.current_token {
            return match token.kind {
                TokenKind::IDENT => Some(Expression::Ident(Identifier::new(&token, &token.literal))),
                TokenKind::INT => Some(Expression::Int(IntegerLiteral::new(&token, &token.literal))),
                TokenKind::TRUE => Some(Expression::Bool(Boolean::new(&token, true))),
                TokenKind::FALSE => Some(Expression::Bool(Boolean::new(&token, false))),
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

            statement.expression = self.parse_expression(&Precedence::LOWEST);

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
                TokenKind::IF => true,
                TokenKind::FUNCTION => true,
                TokenKind::LPAREN => true,
                _ => false
            };
        } else {
            return false;
        }
    }

    fn is_infix_expression(&self) -> bool {
        if let Some(token) = &self.current_token {
            if let Some(peek_token) = &self.peek_token {
                if token.kind == TokenKind::IDENT || token.kind == TokenKind::INT || token.kind == TokenKind::TRUE || token.kind == TokenKind::FALSE || token.kind == TokenKind::RPAREN || token.kind == TokenKind::RBRACE {
                    return match peek_token.kind {
                        TokenKind::LPAREN => true,
                        TokenKind::PLUS => true,
                        TokenKind::MINUS => true,
                        TokenKind::SLASH => true,
                        TokenKind::ASTERIKS => true,
                        TokenKind::EQ => true,
                        TokenKind::NEQ => true,
                        TokenKind::LT => true,
                        TokenKind::GT => true,
                        _ => false,
                    }
                }
            }

            return false;
        } else {
            return false;
        }
    }

    fn parse_expression(&mut self, precendence: &Precedence) -> Option<Expression> {
        let mut left_expr;

        if self.is_prefix_expression() {
            left_expr = self.parse_prefix();
        } else {
            left_expr = self.parse_identifier();
        };

        while !self.peek_token_is(TokenKind::SEMICOLON) && precendence < self.peek_precedence() {
            if !self.is_infix_expression() {

                return left_expr;
            }

            self.next_token();

            left_expr = self.parse_infix(left_expr);
        }

        left_expr
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        match &mut self.current_token {
            Some(token) => {
                let mut return_statement = ReturnStatement::new(&token);

                self.next_token();

                return_statement.return_value = self.parse_expression(&Precedence::LOWEST);

                self.next_token();

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

                self.next_token();

                while !self.current_token_is(TokenKind::SEMICOLON) {
                    let_statement.value = self.parse_expression(&Precedence::LOWEST);

                    self.next_token();
                }

                self.next_token();

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

    fn current_precedence(&self) -> Precedence {
        match &self.current_token {
            Some(token) => {
                match self.precedences.get(&token.kind) {
                    Some(precedence) => precedence.clone(),
                    None => Precedence::LOWEST,
                }
            },
            None => Precedence::LOWEST,
        }
    }

    fn peek_precedence(&self) -> &Precedence {
        match &self.peek_token {
            Some(token) => {
                match self.precedences.get(&token.kind) {
                    Some(precedence) => precedence,
                    None => &Precedence::LOWEST,
                }
            },
            None => &Precedence::LOWEST,
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

        let error = format!(
            "expected next token to be {:?}, got {:?} instead",
            kind,
            peek_token.unwrap().kind,
        );

        println!("{}", error);

        self.errors.push(error)
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
                assert_eq!(program.statements.len(), 3);
                let tests: [&str; 3] = [
                    "5",
                    "10",
                    "993322",
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
    fn test_prefix_expressions() {
        struct Test<'a> {
            input: &'a str,
            operator: &'a str,
            right_value: Expression,
        }

        impl<'a> Test<'a> {
            pub fn new(input: &'a str, operator: &'a str, right_value: Expression) -> Self {
                Test {
                    input, operator, right_value,
                }
            }
        }

        let tests = vec![
            Test::new("!5", "!", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5"))),
            Test::new("!true", "!", Expression::Bool(Boolean::new(&Token::new(TokenKind::TRUE, "true"), true))),
            Test::new("-variable", "-", Expression::Ident(Identifier::new(&Token::from_literal("variable"), "variable")))
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("failed to parse program");

            assert_eq!(program.statements.len(), 1);

            match &program.statements[0] {
                Statement::Expression(expression_statement) => {
                    match &expression_statement.expression {
                        Some(Expression::Prefix(prefix)) => {
                            assert_eq!(prefix.operator, test.operator);

                            match &*prefix.right {
                                Some(expression) => assert_eq!(expression, &test.right_value),
                                None => panic!("no right expression found"),
                            };
                        },
                        Some(_) | None => panic!("no prefix expression found"),
                    }
                },
                _ => panic!("no statement found"),
            };
        }
    }


    #[test]
    fn test_infix_expressions() {
        struct Test<'a> {
            input: &'a str,
            left_value: Expression,
            operator: &'a str,
            right_value: Expression,
        }

        impl<'a> Test<'a> {
            pub fn new(input: &'a str, left_value: Expression, operator: &'a str, right_value: Expression) -> Self {
                Test {
                    input, left_value, operator, right_value,
                }
            }
        }

        let tests: Vec<Test> = vec![
            Test::new("5 + 5", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5")), "+", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5"))),
            Test::new("5 - 5", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5")), "-", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5"))),
            Test::new("5 * 5", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5")), "*", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5"))),
            Test::new("5 / 5", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5")), "/", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5"))),
            Test::new("5 > 5", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5")), ">", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5"))),
            Test::new("5 < 5", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5")), "<", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5"))),
            Test::new("5 == 5", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5")), "==", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5"))),
            Test::new("5 != 5", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5")), "!=", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "5"), "5"))),
            Test::new("true == true", Expression::Bool(Boolean::new(&Token::new(TokenKind::TRUE, "true"), true)), "==", Expression::Bool(Boolean::new(&Token::new(TokenKind::TRUE, "true"), true))),
            Test::new("true != 100", Expression::Bool(Boolean::new(&Token::new(TokenKind::TRUE, "true"), true)), "!=", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "100"), "100"))),
            Test::new("variable != 100", Expression::Ident(Identifier::new(&Token::from_literal("variable"), "variable")), "!=", Expression::Int(IntegerLiteral::new(&Token::new(TokenKind::INT, "100"), "100"))),
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("failed to parse program");

            assert_eq!(program.statements.len(), 1);

            match &program.statements[0] {
                Statement::Expression(expression_statement) => {
                    match &expression_statement.expression {
                        Some(Expression::Infix(infix)) => {
                            match &*infix.left {
                                Some(expression) => assert_eq!(expression, &test.left_value),
                                None => panic!("no left expression found"),
                            };

                            assert_eq!(infix.operator, test.operator);

                            match &*infix.right {
                                Some(expression) => assert_eq!(expression, &test.right_value),
                                None => panic!("no left expression found"),
                            };
                        },
                        Some(_) | None => panic!("expected infix expression")
                    }
                },
                _ => panic!("expected infix expression"),
            }
        }
    }

    #[test]
    fn test_boolean_infix_expression() {
        struct Test {

        }

        let inputs = vec!["true == true;"];
        let operators = vec!["=="];
        let token_kinds = vec![TokenKind::EQ];

        for (index, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();


            for e in parser.errors.iter() {
                println!("parse error: {}", e);
            }


            match program {
                Ok(p) => {
                    assert_eq!(p.statements.len(), 1);

                match &p.statements[0] {
                    Statement::Expression(statement) => {
                        assert_eq!(statement.token.kind, TokenKind::TRUE);
                        assert_eq!(statement.token.literal, "true");
                        assert!(statement.expression.is_some());

                        match &statement.expression {
                            Some(Expression::Infix(infix)) => {
                                assert_eq!(infix.token.kind, token_kinds[index]);
                                assert_eq!(infix.operator, operators[index]);
                                assert!(infix.right.is_some());
                                
                                match &*infix.left {
                                    Some(Expression::Bool(boolean)) => {
                                        assert_eq!(boolean.value, true);
                                        assert_eq!(boolean.token.kind, TokenKind::TRUE);
                                    },
                                    Some(_) => panic!("Expected integer expression. Got None."),
                                    None => panic!("Expected integer expression. Got None.")
                                }

                                match &*infix.right {
                                    Some(Expression::Bool(boolean)) => {
                                        assert_eq!(boolean.value, true);
                                        assert_eq!(boolean.token.kind, TokenKind::TRUE);
                                    },
                                    Some(_) => panic!("Expected integer expression. Got None."),
                                    None => panic!("Expected integer expression. Got None.")
                                }
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

    #[test]
    fn test_operator_precedence_parsing() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }

        let tests = vec![
            Test { input: "true", expected: "true" },
            Test { input: "false", expected: "false" },
            Test { input: "-1 * 2 + 3", expected: "(((-1) * 2) + 3)" },
            Test { input: "3 > 5 == false", expected: "((3 > 5) == false)" },
            Test { input: "3 < 5 == true", expected: "((3 < 5) == true)" },
            Test { input: "-3 * 5 != true / -100", expected: "(((-3) * 5) != (true / (-100)))"},
            Test { input: "1 + (2 + 3) + 4", expected: "((1 + (2 + 3)) + 4)" },
            Test { input: "(2 + 3) + 1", expected: "((2 + 3) + 1)" },
            Test { input: "a + add(b * c) + d", expected: "((a + add((b * c))) + d)" },
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program().expect("parsing failed");
            for e in parser.errors.iter() {
                println!("parse error: {}", e);
            }

            assert_eq!(test.expected, program.to_string());
        }
    }
    
    #[test]
    fn test_boolean_expression() {
        let input = "let boolean = true;";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Let(let_statement) => {
                assert_eq!(let_statement.token.kind, TokenKind::LET);

                match &let_statement.name {
                    Some(name) => {
                        assert_eq!(name.token.kind, TokenKind::IDENT);
                        assert_eq!(name.value, "boolean");
                    },
                    None => panic!("boolean expression failed")
                }

                match &let_statement.value {
                    Some(Expression::Bool(boolean)) => {
                        assert_eq!(boolean.token.kind, TokenKind::TRUE);
                        assert_eq!(boolean.value, true);
                    },
                    Some(_) | None => panic!("boolean expression test failed"),
                }
            },
            _ => panic!("boolean expression test failed"),
        }

        assert_eq!(input, program.to_string());
    }

    #[test]
    fn test_if_expressions() {
        let input = "if (x < y) { x }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("failed to parse program");

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expression(statement) => {
                assert_eq!(statement.token.kind, TokenKind::IF);

                match &statement.expression {
                    Some(Expression::If(if_expression)) => {
                        assert_eq!(if_expression.token.kind, TokenKind::IF);

                        if let Some(consequence) = &if_expression.consequence {
                            assert_eq!(consequence.token.kind, TokenKind::LBRACE);

                            match &consequence.statements[0] {
                                Statement::Expression(statement) => {
                                    match &statement.expression {
                                        Some(Expression::Ident(ident)) => {
                                            assert_eq!(ident.token.kind, TokenKind::IDENT);
                                            assert_eq!(ident.value, "x");
                                        },
                                        Some(_) | None => panic!("not an identifier expression"),
                                    }
                                },
                                _ => panic!("not an expression statement")
                            }
                        }

                        assert!(if_expression.alternative.is_none());
                    },
                    _ => panic!("not an if expression"),
                }
            },
            _ => panic!("not an expression statement"),
        }    
    }

    #[test]
    fn test_if_else_expressions() {
        let input = "if (x < y) { x } else { y }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("failed to parse program");

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expression(statement) => {
                assert_eq!(statement.token.kind, TokenKind::IF);

                match &statement.expression {
                    Some(Expression::If(if_expression)) => {
                        assert_eq!(if_expression.token.kind, TokenKind::IF);

                        if let Some(consequence) = &if_expression.consequence {
                            assert_eq!(consequence.token.kind, TokenKind::LBRACE);

                            match &consequence.statements[0] {
                                Statement::Expression(statement) => {
                                    match &statement.expression {
                                        Some(Expression::Ident(ident)) => {
                                            assert_eq!(ident.token.kind, TokenKind::IDENT);
                                            assert_eq!(ident.value, "x");
                                        },
                                        Some(_) | None => panic!("not an identifier expression"),
                                    }
                                },
                                _ => panic!("not an expression statement")
                            }
                        }

                        if let Some(alternative) = &if_expression.alternative {
                            assert_eq!(alternative.token.kind, TokenKind::LBRACE);

                            match &alternative.statements[0] {
                                Statement::Expression(statement) => {
                                    match &statement.expression {
                                        Some(Expression::Ident(ident)) => {
                                            assert_eq!(ident.token.kind, TokenKind::IDENT);
                                            assert_eq!(ident.value, "y");
                                        },
                                        Some(_) | None => panic!("not an identifier expression"),
                                    }
                                },
                                _ => panic!("not an expression statement"),
                            }
                        }
                    },
                    _ => panic!("not an if expression"),
                }
            },
            _ => panic!("not an expression statement"),
        }
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("failed to parse program");

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expression(statement) => {
                assert_eq!(statement.token.kind, TokenKind::FUNCTION);

                match &statement.expression {
                    Some(Expression::Function(function_expression)) => {
                        assert_eq!(function_expression.token.kind, TokenKind::FUNCTION);

                        assert_eq!(function_expression.paramters[0].token, Token::from_literal("x"));
                        assert_eq!(function_expression.paramters[1].token, Token::from_literal("y"));

                        if let Some(body) = &function_expression.body {
                            assert_eq!(body.token.kind, TokenKind::LBRACE);

                            match &body.statements[0] {
                                Statement::Expression(statement) => {
                                    match &statement.expression {
                                        Some(Expression::Infix(infix)) => {
                                            assert_eq!(infix.token.kind, TokenKind::PLUS);
                                            assert_eq!(infix.operator, "+");

                                            match &*infix.left {
                                                Some(Expression::Ident(ident)) => {
                                                    assert_eq!(ident.value, "x");
                                                    assert_eq!(ident.token.kind, TokenKind::IDENT);
                                                },
                                                Some(_) => panic!("Expected identifier expression. Got None."),
                                                None => panic!("Expected integer expression. Got None.")
                                            }

                                            match &*infix.right {
                                                Some(Expression::Ident(ident)) => {
                                                    assert_eq!(ident.value, "y");
                                                    assert_eq!(ident.token.kind, TokenKind::IDENT);
                                                },
                                                Some(_) => panic!("Expected identifier expression. Got None."),
                                                None => panic!("Expected integer expression. Got None.")
                                            }
                                        },
                                        Some(_) | None => panic!("not an identifier expression"),
                                    }
                                },
                                _ => panic!("not an expression statement")
                            }
                        }
                    },
                    _ => panic!("not an if expression"),
                }
            },
            _ => panic!("not an expression statement"),
        }
    }

    #[test]
    fn test_call_expression() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().expect("parsing failed");

        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::Expression(statement) => {
                assert_eq!(statement.token.kind, TokenKind::IDENT);

                match &statement.expression {
                    Some(Expression::Call(call_expression)) => {
                        assert_eq!(call_expression.token.kind, TokenKind::LPAREN);

                        if let Some(Expression::Function(func)) = call_expression.function.as_ref().map(|b| b.as_ref()) {
                            assert_eq!(func.token.kind, TokenKind::LPAREN);
                            assert_eq!(func.token.literal, "add");
                        }

                        for arg in call_expression.arguments.iter() {
                            if let Some(expression) = arg.as_ref().map(|b| b.as_ref()) {
                                println!("{:?}", expression);
                            }
                        }

                        assert_eq!(call_expression.arguments.len(), 3);
                    },
                    _ => panic!("not a call expression"),
                }
            },
            _ => panic!("not an expression statement"),
        }
    }
}