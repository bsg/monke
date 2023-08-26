use std::io;

use crate::parser::Parser;

pub struct Repl {}

impl Repl {
    pub fn start() {
        let mut buffer = String::new();
        let stdin = io::stdin();
        loop {
            match stdin.read_line(&mut buffer) {
                Ok(_) => match Parser::new(buffer.as_str()).parse_expression(0).as_ref() {
                    Some(ast) => println!("{:?}", ast),
                    None => (),
                },
                Err(_) => break,
            }
            buffer.clear();
        }
    }
}
