use std::io;

use crate::lexer::Lexer;

pub struct Repl {}

impl Repl {
    pub fn start() {
        let mut buffer = String::new();
        let stdin = io::stdin();
        loop {
            match stdin.read_line(&mut buffer) {
                Ok(_) => {
                    Lexer::new(buffer.as_str())
                        .tokens()
                        .for_each(|token| println!("    {:?}", token));
                }
                Err(_) => break,
            }
            buffer.clear();
        }
    }
}
