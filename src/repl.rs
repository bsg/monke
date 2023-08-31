use std::io::{self, Write};

use crate::eval::Eval;

pub struct Repl {}

impl Repl {
    pub fn start() {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut stdout = io::stdout();

        let eval = Eval::new();
        loop {
            print!("> ");
            stdout.flush().unwrap();
            match stdin.read_line(&mut buffer) {
                Ok(_) => {
                    println!("{}", eval.eval(buffer.clone().into()));
                }
                Err(_) => break,
            }
            buffer.clear();
        }
    }
}
