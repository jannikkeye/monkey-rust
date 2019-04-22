use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    ch: u8,
    eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        let byte_input = input.as_bytes();

        Lexer {
            input: byte_input,
            position: 0,
            read_position: 0,
            ch: 0,
            eof: false,
        }
    }

    fn is_letter(&self, byte: u8) -> bool {
        b'a' <= byte && byte <= b'z' || b'A' <= byte && byte <= b'Z' || byte == b'_'
    }

    fn is_number(&self, byte: u8) -> bool {
        b'0' <= byte && byte <= b'9'
    }

    fn is_whitespace(&self, byte: u8) -> bool {
        byte == b' ' || byte == b'\t' || byte == b'\n' || byte == b'\r'
    }

    fn is_double_quote(&self, byte: u8) -> bool {
        byte == b'"'
    }

    fn is_escape_char(&self, byte: u8) -> bool {
        byte == b'\\'
    }

    fn read_char(&mut self) -> u8 {
        self.position = self.read_position;
        self.read_position = self.position + 1;
        self.ch = if self.position >= self.input.len() {
            0
        } else {
            self.input[self.position]
        };

        self.ch
    }

    fn peek_char(&self) -> u8 {
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input[self.read_position]
        }
    }

    fn read_letters(&mut self) -> Vec<u8> {
        let mut bytes: Vec<u8> = vec![];

        while self.is_letter(self.peek_char()) {
            bytes.push(self.read_char());
        }

        bytes
    }

    fn read_numbers(&mut self) -> Vec<u8> {
        let mut bytes: Vec<u8> = vec![];

        while self.is_number(self.peek_char()) {
            bytes.push(self.read_char());
        }

        bytes
    }

    fn read_whitespace(&mut self) {
        while self.is_whitespace(self.peek_char()) {
            self.read_char();
        }
    }

    fn read_string(&mut self) -> Vec<u8> {
        let mut bytes: Vec<u8> = vec![];

        self.read_char();

        while !self.is_double_quote(self.peek_char()) && self.peek_char() != 0 {
            if self.is_escape_char(self.peek_char()) {
                self.read_char();
            }

            bytes.push(self.read_char());
        }

        if self.is_double_quote(self.peek_char()) {
            self.read_char();
        }

        bytes
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.read_whitespace();

        let peaked_char = self.peek_char();

        if peaked_char == 0 && self.eof {
            return None;
        } else if peaked_char == 0 && !self.eof {
            self.eof = true;
            return Some(Token::from_literal(""));
        }

        let is_letter = self.is_letter(peaked_char);
        let is_number = self.is_number(peaked_char);
        let is_double_quote = self.is_double_quote(peaked_char);
        let bytes = if is_double_quote {
            self.read_string()
        } else if is_letter {
            self.read_letters()
        } else if is_number {
            self.read_numbers()
        } else {
            self.read_char();

            match self.ch {
                b'=' | b'!' => {
                    let mut bytes = vec![];

                    bytes.push(self.ch);

                    if self.peek_char() == b'=' {
                        bytes.push(self.read_char());
                    }

                    bytes
                }
                _ => vec![self.ch],
            }
        };

        if is_number {
            return Some(Token::new(
                TokenKind::INT,
                String::from_utf8(bytes).unwrap().as_ref(),
            ));
        }

        if is_double_quote {
            return Some(Token::new(
                TokenKind::STRING,
                String::from_utf8(bytes).unwrap().as_ref(),
            ));
        }

        Some(Token::from_bytes(bytes))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_letter() {
        let lexer = Lexer::new("abcdefg");

        assert_eq!(lexer.is_letter(b'l'), true);
        assert_eq!(lexer.is_letter(b'e'), true);
        assert_eq!(lexer.is_letter(b't'), true);
        assert_eq!(lexer.is_letter(b'a'), true);
        assert_eq!(lexer.is_letter(b'z'), true);
        assert_eq!(lexer.is_letter(b'A'), true);
        assert_eq!(lexer.is_letter(b'Z'), true);

        assert_eq!(lexer.is_letter(b';'), false);
        assert_eq!(lexer.is_letter(b','), false);
        assert_eq!(lexer.is_letter(b'}'), false);
    }

    #[test]
    fn next_token() {
        use crate::token::{Token, TokenKind};

        let input = r#"let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
       return true;
   } else {
       return false;
}

10 == 10;
10 != 9;
"foobar"
"foo bar"
"\"foo\" bar
   "#;
        let matches: Vec<Token> = vec![
            Token::new(TokenKind::LET, "let"),
            Token::new(TokenKind::IDENT, "five"),
            Token::new(TokenKind::ASSIGN, "="),
            Token::new(TokenKind::INT, "5"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::LET, "let"),
            Token::new(TokenKind::IDENT, "ten"),
            Token::new(TokenKind::ASSIGN, "="),
            Token::new(TokenKind::INT, "10"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::LET, "let"),
            Token::new(TokenKind::IDENT, "add"),
            Token::new(TokenKind::ASSIGN, "="),
            Token::new(TokenKind::FUNCTION, "fn"),
            Token::new(TokenKind::LPAREN, "("),
            Token::new(TokenKind::IDENT, "x"),
            Token::new(TokenKind::COMMA, ","),
            Token::new(TokenKind::IDENT, "y"),
            Token::new(TokenKind::RPAREN, ")"),
            Token::new(TokenKind::LBRACE, "{"),
            Token::new(TokenKind::IDENT, "x"),
            Token::new(TokenKind::PLUS, "+"),
            Token::new(TokenKind::IDENT, "y"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::RBRACE, "}"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::LET, "let"),
            Token::new(TokenKind::IDENT, "result"),
            Token::new(TokenKind::ASSIGN, "="),
            Token::new(TokenKind::IDENT, "add"),
            Token::new(TokenKind::LPAREN, "("),
            Token::new(TokenKind::IDENT, "five"),
            Token::new(TokenKind::COMMA, ","),
            Token::new(TokenKind::IDENT, "ten"),
            Token::new(TokenKind::RPAREN, ")"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::BANG, "!"),
            Token::new(TokenKind::MINUS, "-"),
            Token::new(TokenKind::SLASH, "/"),
            Token::new(TokenKind::ASTERIKS, "*"),
            Token::new(TokenKind::INT, "5"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::INT, "5"),
            Token::new(TokenKind::LT, "<"),
            Token::new(TokenKind::INT, "10"),
            Token::new(TokenKind::GT, ">"),
            Token::new(TokenKind::INT, "5"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::IF, "if"),
            Token::new(TokenKind::LPAREN, "("),
            Token::new(TokenKind::INT, "5"),
            Token::new(TokenKind::LT, "<"),
            Token::new(TokenKind::INT, "10"),
            Token::new(TokenKind::RPAREN, ")"),
            Token::new(TokenKind::LBRACE, "{"),
            Token::new(TokenKind::RETURN, "return"),
            Token::new(TokenKind::TRUE, "true"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::RBRACE, "}"),
            Token::new(TokenKind::ELSE, "else"),
            Token::new(TokenKind::LBRACE, "{"),
            Token::new(TokenKind::RETURN, "return"),
            Token::new(TokenKind::FALSE, "false"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::RBRACE, "}"),
            Token::new(TokenKind::INT, "10"),
            Token::new(TokenKind::EQ, "=="),
            Token::new(TokenKind::INT, "10"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::INT, "10"),
            Token::new(TokenKind::NEQ, "!="),
            Token::new(TokenKind::INT, "9"),
            Token::new(TokenKind::SEMICOLON, ";"),
            Token::new(TokenKind::STRING, "foobar"),
            Token::new(TokenKind::STRING, "foo bar"),
            Token::new(TokenKind::STRING, "hello \"world\""),
            Token::new(TokenKind::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.input, input.as_bytes());

        for m in matches.iter() {
            let token: Token = lexer.next().unwrap();
            assert_eq!(m.kind, token.kind);
        }
    }
}
