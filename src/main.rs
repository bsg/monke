use repl::Repl;

mod parser;
mod lexer;
mod repl;
mod ast;
mod eval;

fn main() {
    println!("monke go");
    Repl::start();
}
