use crate::lexer;
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

        for token in lexer {
            println!("{:?}", token);
        }
    }
}