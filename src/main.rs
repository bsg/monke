use repl::Repl;

mod lexer;
mod repl;
mod ast;
mod parser;

fn main() {
    println!("monke go");
    Repl::start();
}
