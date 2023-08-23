use lexer::Lexer;

mod lexer;

fn main() {
    let lexer = Lexer::new("let res = 123 + 100 / fn(x)");
    lexer.tokens().for_each(|token| println!("{}", token));
}
