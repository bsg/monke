use std::str;
use std::iter::Peekable;
use std::rc::Rc;

use crate::ast::*;
use crate::lexer::{Lexer, Tokens};

struct Parser<'a> {
    tokens: Peekable<Tokens<'a>>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Parser<'a> {
        Parser {
            tokens: Lexer::new(input).tokens().peekable(),
        }
    }

    fn parse_statement(&mut self) -> Statement<'a> {
        use crate::lexer::Token;
        match self.tokens.next() {
            Some(token) => match token {
                Token::Let => Statement {
                    kind: StatementKind::Let,
                    root: todo!(),
                },
                Token::Return => Statement {
                    kind: StatementKind::Return,
                    root: todo!(),
                },
                _ => Statement {
                    kind: StatementKind::Expr,
                    root: todo!(),
                },
            },
            None => todo!(),
        }
    }

    fn parse_expression(&mut self) -> Node {
        use crate::lexer::Token;
        match self.tokens.next() {
            Some(token) => match token {
                Token::Int(s) => {
                    let i = match str::from_utf8(s) {
                        Ok(s) => match s.parse() {
                            Ok(i) => i,
                            Err(_) => panic!(),
                        },
                        Err(_) => panic!(),
                    };

                    Node {
                        kind: NodeKind::Int(i),
                        left: None,
                        right: None,
                    }
                }
                Token::Ident(token) => {
                    let name = match str::from_utf8(token) {
                        Ok(s) => s,
                        Err(_) => panic!(),
                    };
                    
                    Node {
                        kind: NodeKind::Ident(&name),
                        left: None,
                        right: None,
                    }
                }
                _ => todo!(),
            },
            None => todo!(),
        }
    }
}

mod tests {
    use super::*;

    #[test]
    fn int_literal() {
        let input = "453;";
        let expected = Node {
            kind: NodeKind::Int(453),
            left: None,
            right: None,
        };
        let mut parser = Parser::new(input);
        assert_eq!(parser.parse_expression(), expected);
    }

    #[test]
    fn ident() {
        let input = "__var_12;";
        let expected = Node {
            kind: NodeKind::Ident("__var_12"),
            left: None,
            right: None,
        };
        let mut parser = Parser::new(input);
        assert_eq!(parser.parse_expression(), expected);
    }
}
