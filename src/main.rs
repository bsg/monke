use repl::Repl;

mod parser;
mod lexer;
mod repl;
mod ast;

fn main() {
    println!("monke go");
    Repl::start();
}
