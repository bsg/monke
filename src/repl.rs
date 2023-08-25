use std::io;

use crate::parser::Parser;

pub struct Repl {}

impl Repl {
    pub fn start() {
        let mut buffer = String::new();
        let stdin = io::stdin();
        loop {
            match stdin.read_line(&mut buffer) {
                Ok(_) => {
                    println!("{:?}", Parser::new(buffer.as_str()).parse_expression(None, 0).unwrap())
                }
                Err(_) => break,
            }
            buffer.clear();
        }
    }
}
