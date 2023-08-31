use repl::Repl;

mod ast;
mod eval;
mod lexer;
mod parser;
mod repl;

fn main() {
    println!("monke go");
    Repl::start();
}
