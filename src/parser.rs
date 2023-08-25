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
    pub fn parse_expression(&mut self, precedence: i32) -> Rc<Option<Node<'a>>> {
        self.next_token();

        let mut lhs = match self.curr_token {
            // these are prefix...
            Some(Token::Int(s)) => {
                let i = match str::from_utf8(s) {
                    Ok(s) => match s.parse() {
                        Ok(i) => i,
                        Err(_) => panic!(),
                    },
                    Err(_) => panic!(),
                };

                Rc::from(Some(Node {
                    kind: NodeKind::Int(i),
                    left: None.into(),
                    right: None.into(),
                }))
            }
            Some(Token::LParen) => self.parse_expression(0),
            _ => None.into(),
        };

        loop {
            // ... and these are infix / postfix
            match self.peek_token {
                Some(Token::RParen) => {
                    self.next_token();
                    break;
                }
                _ => {
                    let op = match self.peek_token {
                        Some(Token::Eof) => break,
                        None => break,
                        Some(Token::Plus) => Some(Op::Add),
                        Some(Token::Asterisk) => Some(Op::Mul),
                        _ => None,
                    };

                    match op {
                        Some(op) => {
                            if op.precedence() < precedence {
                                break;
                            }
                            self.next_token();

                            let rhs = self.parse_expression(op.precedence());

                            lhs = Rc::from(Some(Node {
                                kind: NodeKind::Op(op),
                                left: lhs,
                                right: rhs,
                            }));
                        }
                        None => break,
                    }
                }
            }
        }

        Rc::from(lhs)
    }
}

mod tests {
    use super::*;
    // todo make these easier to write
    #[test]
    fn int_literal() {
        let input = "453;";
        let expected = Node {
            kind: NodeKind::Int(453),
            left: None.into(),
            right: None.into(),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(0), Some(expected));
    }

    #[test]
    fn ident() {
        let input = "__var_12;";
        let expected = Node {
            kind: NodeKind::Ident("__var_12"),
            left: None.into(),
            right: None.into(),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(0), Some(expected));
    }

    #[test]
    fn bool() {
        let input = "true";
        let expected = Node {
            kind: NodeKind::Bool(true),
            left: None.into(),
            right: None.into(),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(0), Some(expected));
    }

    #[test]
    fn op_neg() {
        let input = "-12;";
        let expected = Node {
            kind: NodeKind::Op(Op::Neg),
            left: None.into(),
            right: Rc::from(Some(Node {
                kind: NodeKind::Int(12),
                left: None.into(),
                right: None.into(),
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(0), Some(expected));
    }

    #[test]
    fn op_not() {
        let input = "!1;";
        let expected = Node {
            kind: NodeKind::Op(Op::Not),
            left: None.into(),
            right: Rc::from(Some(Node {
                kind: NodeKind::Int(1),
                left: None.into(),
                right: None.into(),
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(0), Some(expected));
    }

    #[test]
    fn op_add() {
        let input = "6 + 2";
        let expected = Node {
            kind: NodeKind::Op(Op::Add),
            left: Rc::from(Some(Node {
                kind: NodeKind::Int(6),
                left: None.into(),
                right: None.into(),
            })),
            right: Rc::from(Some(Node {
                kind: NodeKind::Int(2),
                left: None.into(),
                right: None.into(),
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(0), Some(expected));
    }

    #[test]
    fn op_sub() {
        let input = "6 - 2";
        let expected = Node {
            kind: NodeKind::Op(Op::Sub),
            left: Rc::from(Some(Node {
                kind: NodeKind::Int(6),
                left: None.into(),
                right: None.into(),
            })),
            right: Rc::from(Some(Node {
                kind: NodeKind::Int(2),
                left: None.into(),
                right: None.into(),
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(0), Some(expected));
    }

    #[test]
    fn op_mul() {
        let input = "6 * 2";
        let expected = Node {
            kind: NodeKind::Op(Op::Mul),
            left: Rc::from(Some(Node {
                kind: NodeKind::Int(6),
                left: None.into(),
                right: None.into(),
            })),
            right: Rc::from(Some(Node {
                kind: NodeKind::Int(2),
                left: None.into(),
                right: None.into(),
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(0), Some(expected));
    }

    #[test]
    fn op_div() {
        let input = "6 / 2";
        let expected = Node {
            kind: NodeKind::Op(Op::Div),
            left: Rc::from(Some(Node {
                kind: NodeKind::Int(6),
                left: None.into(),
                right: None.into(),
            })),
            right: Rc::from(Some(Node {
                kind: NodeKind::Int(2),
                left: None.into(),
                right: None.into(),
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(0), Some(expected));
    }

    #[test]
    fn op_precedence() {
        let input = "1 + 2 * 3 - 4 / 5";
        let expected = Node {
            kind: NodeKind::Op(Op::Add),
            left: Rc::from(Some(Node {
                kind: NodeKind::Int(1),
                left: None.into(),
                right: None.into(),
            })),
            right: Rc::from(Some(Node {
                kind: NodeKind::Op(Op::Sub),
                left: Rc::from(Some(Node {
                    kind: NodeKind::Op(Op::Mul),
                    left: Rc::from(Some(Node {
                        kind: NodeKind::Int(2),
                        left: None.into(),
                        right: None.into(),
                    })),
                    right: Rc::from(Some(Node {
                        kind: NodeKind::Int(3),
                        left: None.into(),
                        right: None.into(),
                    })),
                })),
                right: Rc::from(Some(Node {
                    kind: NodeKind::Op(Op::Div),
                    left: Rc::from(Some(Node {
                        kind: NodeKind::Int(4),
                        left: None.into(),
                        right: None.into(),
                    })),
                    right: Rc::from(Some(Node {
                        kind: NodeKind::Int(5),
                        left: None.into(),
                        right: None.into(),
                    })),
                })),
            })),
        };
        let mut parser = Parser::new(input);
        assert_eq!(*parser.parse_expression(0), Some(expected));
    }
}
