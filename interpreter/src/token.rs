#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum TokenKind {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    FUNCTION,
    LET,
    BANG,
    MINUS,
    SLASH,
    ASTERIKS,
    LT,
    GT,
    IF,
    RETURN,
    TRUE,
    ELSE,
    FALSE,
    EQ,
    NEQ,
    STRING,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
}

impl Token {
    pub fn new(kind: TokenKind, literal: &str) -> Self {
        Token {
            kind,
            literal: literal.to_owned(),
        }
    }

    pub fn from_bytes(bytes: Vec<u8>) -> Self {
        let literal = String::from_utf8(bytes).unwrap();

        if literal.chars().all(|v| v.is_digit(10)) {
            return Token::new(TokenKind::INT, &literal);
        }

        match literal.as_ref() {
            "let" => Token::new(TokenKind::LET, &literal),
            "fn" => Token::new(TokenKind::FUNCTION, &literal),
            "=" => Token::new(TokenKind::ASSIGN, &literal),
            ";" => Token::new(TokenKind::SEMICOLON, &literal),
            "(" => Token::new(TokenKind::LPAREN, &literal),
            ")" => Token::new(TokenKind::RPAREN, &literal),
            "," => Token::new(TokenKind::COMMA, &literal),
            "+" => Token::new(TokenKind::PLUS, &literal),
            "{" => Token::new(TokenKind::LBRACE, &literal),
            "}" => Token::new(TokenKind::RBRACE, &literal),
            "[" => Token::new(TokenKind::LBRACKET, &literal),
            "]" => Token::new(TokenKind::RBRACKET, &literal),
            "!" => Token::new(TokenKind::BANG, &literal),
            "-" => Token::new(TokenKind::MINUS, &literal),
            "/" => Token::new(TokenKind::SLASH, &literal),
            "*" => Token::new(TokenKind::ASTERIKS, &literal),
            "<" => Token::new(TokenKind::LT, &literal),
            ">" => Token::new(TokenKind::GT, &literal),
            "if" => Token::new(TokenKind::IF, &literal),
            "return" => Token::new(TokenKind::RETURN, &literal),
            "true" => Token::new(TokenKind::TRUE, &literal),
            "else" => Token::new(TokenKind::ELSE, &literal),
            "false" => Token::new(TokenKind::FALSE, &literal),
            "==" => Token::new(TokenKind::EQ, &literal),
            "!=" => Token::new(TokenKind::NEQ, &literal),
            "" => Token::new(TokenKind::EOF, &literal),
            _ => Token::new(TokenKind::IDENT, &literal),
        }
    }

    pub fn from_literal(literal: &str) -> Self {
        if let Ok(_) = literal.parse::<i64>() {
            return Token::new(TokenKind::INT, &literal);
        }

        match literal {
            "let" => Token::new(TokenKind::LET, &literal),
            "fn" => Token::new(TokenKind::FUNCTION, &literal),
            "=" => Token::new(TokenKind::ASSIGN, &literal),
            ";" => Token::new(TokenKind::SEMICOLON, &literal),
            "(" => Token::new(TokenKind::LPAREN, &literal),
            ")" => Token::new(TokenKind::RPAREN, &literal),
            "," => Token::new(TokenKind::COMMA, &literal),
            "+" => Token::new(TokenKind::PLUS, &literal),
            "{" => Token::new(TokenKind::LBRACE, &literal),
            "}" => Token::new(TokenKind::RBRACE, &literal),
            "[" => Token::new(TokenKind::LBRACKET, &literal),
            "]" => Token::new(TokenKind::RBRACKET, &literal),
            "!" => Token::new(TokenKind::BANG, &literal),
            "-" => Token::new(TokenKind::MINUS, &literal),
            "/" => Token::new(TokenKind::SLASH, &literal),
            "*" => Token::new(TokenKind::ASTERIKS, &literal),
            "<" => Token::new(TokenKind::LT, &literal),
            ">" => Token::new(TokenKind::GT, &literal),
            "if" => Token::new(TokenKind::IF, &literal),
            "return" => Token::new(TokenKind::RETURN, &literal),
            "true" => Token::new(TokenKind::TRUE, &literal),
            "else" => Token::new(TokenKind::ELSE, &literal),
            "false" => Token::new(TokenKind::FALSE, &literal),
            "==" => Token::new(TokenKind::EQ, &literal),
            "!=" => Token::new(TokenKind::NEQ, &literal),
            "" => Token::new(TokenKind::EOF, &literal),
            _ => Token::new(TokenKind::IDENT, &literal),
        }
    }
}
