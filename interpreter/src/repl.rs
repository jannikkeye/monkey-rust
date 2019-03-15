use crate::evaluator;
use crate::lexer;
use crate::object::{Object, ObjectVariant};
use crate::parser;
use std::io::{stdin, stdout, Write};

const PROMPT: &str = ">> ";

pub fn start() {
    println!(
        "

    "
    );
    let mut evaluator = evaluator::Evaluator::new();

    loop {
        let mut input = String::new();
        print!("{}", PROMPT);

        stdout().flush().expect("Error flushing stdout");
        stdin()
            .read_line(&mut input)
            .expect("Error reading from STDIN");

        let lexer = lexer::Lexer::new(&input);
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse_program().expect("parsing failed");

        if parser.errors.len() > 0 {
            println!("Whoops! We ran into some monkey business here!");
            println!("parse errors:");
            for e in parser.errors.iter() {
                println!("\t{}", e);
            }

            continue;
        }

        let evaluated = evaluator.eval(program);

        if let Some(eval) = evaluated {
            if let Object::Error(_) = &eval {
                println!("Whoops! We ran into some monkey business here!");
                println!("evaluation error:");
            }

            println!("{}", eval.inspect());
            println!("\n");
        }
    }
}
