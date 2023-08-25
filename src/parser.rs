use std::iter::Peekable;
use std::rc::Rc;
use std::str;

use crate::ast::*;
use crate::lexer::{Lexer, Token, Tokens};

pub struct Parser<'a> {
    tokens: Peekable<Tokens<'a>>,
    curr_token: Option<Token<'a>>,
    peek_token: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Parser<'a> {
        Parser {
            tokens: Lexer::new(input).tokens().peekable(),
            curr_token: None,
            peek_token: None,
        }
    }

    fn next_token(&mut self) {
        self.curr_token = self.tokens.next();
        self.peek_token = match self.tokens.peek() {
            Some(token) => Some(*token),
            None => None,
        };
    }

    // fn parse_statement(&mut self) -> Statement<'a> {
    //     match self.tokens.next() {
    //         Some(token) => match token {
    //             Token::Let => Statement {
    //                 kind: StatementKind::Let,
    //                 root: todo!(),
    //             },
    //             Token::Return => Statement {
    //                 kind: StatementKind::Return,
    //                 root: todo!(),
    //             },
    //             _ => Statement {
    //                 kind: StatementKind::Expr,
    //                 root: todo!(),
    //             },
    //         },
    //         None => todo!(),
    //     }
    // }

    // TODO remember to make this non pub
    pub fn parse_expression(
        &mut self,
        left: Option<Rc<Node<'a>>>,
        precedence: i32,
    ) -> Option<Rc<Node<'a>>> {
        let mut node: Option<Rc<Node>> = None;

        self.next_token();

        node = match self.curr_token {
            Some(token) => match token {
                Token::Int(s) => {
                    let i = match str::from_utf8(s) {
                        Ok(s) => match s.parse() {
                            Ok(i) => i,
                            Err(_) => panic!(),
                        },
                        Err(_) => panic!(),
                    };

                    Some(Rc::from(Node {
                        kind: NodeKind::Int(i),
                        left: None,
                        right: None,
                    }))
                }
                Token::Ident(name) => {
                    let name = match str::from_utf8(name) {
                        Ok(s) => s,
                        Err(_) => panic!(),
                    };

                    Some(Rc::from(Node {
                        kind: NodeKind::Ident(&name),
                        left: None,
                        right: None,
                    }))
                }
                Token::Minus => match node {
                    Some(_) => Some(Rc::from(Node {
                        kind: NodeKind::Op(Op::Sub),
                        left: Some(Rc::from(node.unwrap())),
                        right: self.parse_expression(None, precedence),
                    })),
                    None => Some(Rc::from(Node {
                        kind: NodeKind::Op(Op::Neg),
                        left: None,
                        right: self.parse_expression(None, precedence),
                    })),
                },
                Token::Bang => Some(Rc::from(Node {
                    kind: NodeKind::Op(Op::Not),
                    left: None,
                    right: self.parse_expression(None, Token::Bang.precedence()),
                })),
                Token::Plus => Some(Rc::from(Node {
                    kind: NodeKind::Op(Op::Add),
                    left: Some(Rc::from(left.unwrap())),
                    right: self.parse_expression(None, Token::Plus.precedence()),
                })),
                Token::Asterisk => Some(Rc::from(Node {
                    kind: NodeKind::Op(Op::Mul),
                    left: Some(Rc::from(left.unwrap())),
                    right: self.parse_expression(None, Token::Asterisk.precedence()),
                })),
                _ => None,
            },
            None => todo!(),
        };

        while precedence < self.peek_token.unwrap_or(Token::EOF).precedence() {
            match self.peek_token {
                Some(Token::Semicolon) => break,
                None => break,
                _ => {
                    node = self.parse_expression(node, self.curr_token.unwrap().precedence());
                }
            }
        }

        match node {
            Some(node) => Some(Rc::from(node)),
            None => None,
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
        assert_eq!(*parser.parse_expression(None, 0).unwrap(), expected);
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
        assert_eq!(*parser.parse_expression(None, 0).unwrap(), expected);
    }

    #[test]
    fn op_neg() {
        let input = "-12;";
        let expected = Node {
            kind: NodeKind::Op(Op::Neg),
            left: None,
            right: Some(Rc::from(Node {
                kind: NodeKind::Int(12),
                left: None,
                right: None,
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(None, 0).unwrap(), expected);
    }

    #[test]
    fn op_not() {
        let input = "!1;";
        let expected = Node {
            kind: NodeKind::Op(Op::Not),
            left: None,
            right: Some(Rc::from(Node {
                kind: NodeKind::Int(1),
                left: None,
                right: None,
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(None, 0).unwrap(), expected);
    }

    #[test]
    fn op_add() {
        let input = "6 + 2";
        let expected = Node {
            kind: NodeKind::Op(Op::Add),
            left: Some(Rc::from(Node {
                kind: NodeKind::Int(6),
                left: None,
                right: None,
            })),
            right: Some(Rc::from(Node {
                kind: NodeKind::Int(2),
                left: None,
                right: None,
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(None, 0).unwrap(), expected);
    }

    #[test]
    fn op_sub() {
        let input = "6 - 2";
        let expected = Node {
            kind: NodeKind::Op(Op::Sub),
            left: Some(Rc::from(Node {
                kind: NodeKind::Int(6),
                left: None,
                right: None,
            })),
            right: Some(Rc::from(Node {
                kind: NodeKind::Int(2),
                left: None,
                right: None,
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(None, 0).unwrap(), expected);
    }
}
