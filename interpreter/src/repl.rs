use crate::lexer;
use crate::parser;
use std::io::{Write, stdin, stdout};

const PROMPT: &str = ">>";

pub fn start() {
    println!("

    ");
    loop {
        let mut input = String::new();
        print!("{}", PROMPT);

        stdout().flush().expect("Error flushing stdout");
        stdin().read_line(&mut input).expect("Error reading from STDIN");

        let lexer = lexer::Lexer::new(&input);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program();

        println!("{:#?}", program);
    }
}