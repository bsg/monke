use std::io::{self, Write};

use crate::eval::Eval;

pub struct Repl {}

impl Repl {
    pub fn start() {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        loop {
            print!("> ");
            stdout.flush().unwrap();
            match stdin.read_line(&mut buffer) {
                Ok(_) => {
                    println!("{}", Eval::eval_statement(&buffer));
                }
                Err(_) => break,
            }
            buffer.clear();
        }
    }
}
