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

    fn parse_block(&mut self) -> Rc<Option<Node<'a>>> {
        let mut statements: Vec<Rc<Node<'a>>> = Vec::new();
        match self.tokens.peek() {
            Some(Token::LBrace) => {
                self.next_token();
                loop {
                    // TODO fix unwrap
                    match Rc::into_inner(self.parse_statement()).unwrap() {
                        Some(stmt) => statements.push(Rc::from(stmt)),
                        None => break,
                    }
                    self.next_token();
                }
                assert_eq!(self.curr_token, Some(Token::RBrace));
                Rc::from(Some(Node {
                    kind: NodeKind::Block(Rc::from(BlockStatement { statements })),
                    left: None.into(),
                    right: None.into(),
                }))
            }
            Some(_) => todo!(),
            None => None.into(),
        }
    }

    fn parse_if(&mut self) -> Rc<Option<Node<'a>>> {
        self.next_token();
        match self.tokens.peek() {
            Some(Token::LParen) => {
                self.next_token();
                match Rc::into_inner(self.parse_expression(0)).unwrap() {
                    Some(cond) => {
                        let lhs = self.parse_block();
                        let rhs = self.parse_block();
                        match *lhs {
                            Some(_) => Rc::from(Some(Node {
                                kind: NodeKind::If(IfExpression {
                                    condition: cond.into(),
                                }),
                                left: lhs,
                                right: rhs,
                            })),
                            None => todo!(),
                        }
                    }
                    None => todo!(),
                }
            }
            _ => None.into(),
        }
    }

    // TODO remember to make this non pub
    pub fn parse_fn(&mut self) -> Rc<Option<Node<'a>>> {
        let mut args: Vec<&'a str> = Vec::new();

        self.next_token();
        match self.tokens.peek() {
            Some(Token::LParen) => {
                self.next_token();
                self.next_token();
                loop {
                    // TODO fix unwrap
                    match self.curr_token {
                        Some(Token::Ident(name)) => args.push(str::from_utf8(name).unwrap()),
                        Some(Token::Comma) => (),
                        _ => break,
                    }
                    self.next_token();
                }
                let body = self.parse_block();

                Rc::from(Some(Node {
                    kind: NodeKind::Fn(Rc::from(FnExpression { args })),
                    left: None.into(),
                    right: body,
                }))
            }
            Some(_) => todo!(),
            None => None.into(),
        }
    }

    fn parse_statement(&mut self) -> Rc<Option<Node<'a>>> {
        match self.tokens.peek() {
            Some(Token::Let) => {
                self.next_token();
                self.next_token();
                match self.curr_token {
                    Some(Token::Ident(_)) => {
                        let lhs = self.parse_ident();
                        self.next_token();
                        match self.curr_token {
                            Some(Token::Assign) => Rc::from(Some(Node {
                                kind: NodeKind::Let,
                                left: lhs,
                                right: self.parse_expression(0),
                            })),
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
            Some(Token::Return) => {
                self.next_token();
                Rc::from(Some(Node {
                    kind: NodeKind::Return,
                    left: None.into(),
                    right: self.parse_expression(0),
                }))
            }
            Some(_) => self.parse_expression(0),
            None => None.into(),
        }
    }

    fn parse_ident(&self) -> Rc<Option<Node<'a>>> {
        match self.curr_token {
            Some(Token::Ident(name)) => match str::from_utf8(name) {
                Ok(name) => Rc::from(Some(Node {
                    kind: NodeKind::Ident(name),
                    left: None.into(),
                    right: None.into(),
                })),
                Err(_) => todo!(),
            },
            _ => todo!(),
        }
    }

    fn parse_expression(&mut self, precedence: i32) -> Rc<Option<Node<'a>>> {
        self.next_token();
        let mut lhs = match self.curr_token {
            // these are prefix...
            // INT
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
            // IDENT
            Some(Token::Ident(_)) => self.parse_ident(),
            // TRUE
            Some(Token::True) => Rc::from(Some(Node {
                kind: NodeKind::Bool(true),
                left: None.into(),
                right: None.into(),
            })),
            // FALSE
            Some(Token::False) => Rc::from(Some(Node {
                kind: NodeKind::Bool(false),
                left: None.into(),
                right: None.into(),
            })),
            // NEG
            Some(Token::Minus) => Rc::from(Some(Node {
                kind: NodeKind::Op(Op::Neg),
                left: None.into(),
                right: self.parse_expression(0),
            })),
            // NOT
            Some(Token::Bang) => Rc::from(Some(Node {
                kind: NodeKind::Op(Op::Not),
                left: None.into(),
                right: self.parse_expression(0),
            })),
            // LPAREN
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
                        Some(Token::Assign) => Some(Op::Assign),
                        Some(Token::Plus) => Some(Op::Add),
                        Some(Token::Minus) => Some(Op::Sub),
                        Some(Token::Asterisk) => Some(Op::Mul),
                        Some(Token::Slash) => Some(Op::Div),
                        Some(Token::Eq) => Some(Op::Eq),
                        Some(Token::NotEq) => Some(Op::NotEq),
                        Some(Token::Lt) => Some(Op::Lt),
                        Some(Token::Gt) => Some(Op::Gt),
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
    // TODO clusterfuck ahead
    use super::*;

    #[test]
    fn int_literal() {
        let input = "453;";
        let expected = "Some(\
            Int(453)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn ident() {
        let input = "__var_12;";
        let expected = "Some(\
            Ident(\"__var_12\")\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn bool() {
        let input = "true";
        let expected = "Some(\
            Bool(true)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_neg() {
        let input = "-12;";
        let expected = "Some(\
            Op(Neg)\n\
            -Int(12)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_not() {
        let input = "!false;";
        let expected = "Some(\
            Op(Not)\n\
            -Bool(false)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_assign() {
        let input = "x = 2";
        let expected = "Some(\
            Op(Assign)\n\
            -Ident(\"x\")\n\
            -Int(2)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_add() {
        let input = "6 + 2";
        let expected = "Some(\
            Op(Add)\n\
            -Int(6)\n\
            -Int(2)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_sub() {
        let input = "6 - 2";
        let expected = "Some(\
            Op(Sub)\n\
            -Int(6)\n\
            -Int(2)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_mul() {
        let input = "6 * 2";
        let expected = "Some(\
            Op(Mul)\n\
            -Int(6)\n\
            -Int(2)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_div() {
        let input = "6 / 2";
        let expected = "Some(\
            Op(Div)\n\
            -Int(6)\n\
            -Int(2)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_eq() {
        let input = "5 == 5";
        let expected = "Some(\
            Op(Eq)\n\
            -Int(5)\n\
            -Int(5)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_not_eq() {
        let input = "5 != 5";
        let expected = "Some(\
            Op(NotEq)\n\
            -Int(5)\n\
            -Int(5)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }
    #[test]
    fn op_lt() {
        let input = "5 < 5";
        let expected = "Some(\
            Op(Lt)\n\
            -Int(5)\n\
            -Int(5)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_gt() {
        let input = "5 > 5";
        let expected = "Some(\
            Op(Gt)\n\
            -Int(5)\n\
            -Int(5)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn op_precedence() {
        let input = "1 + 2 * (3 - 4) / 5";
        let expected = "Some(\
            Op(Add)\n\
            -Int(1)\n\
            -Op(Mul)\n\
            --Int(2)\n\
            --Op(Div)\n\
            ---Op(Sub)\n\
            ----Int(3)\n\
            ----Int(4)\n\
            ---Int(5)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", *parser.parse_expression(0)), expected);
    }

    #[test]
    fn let_statement() {
        let input = "let x = 1 + 2";
        let expected = "Some(\
            Let\n\
            -Ident(\"x\")\n\
            -Op(Add)\n\
            --Int(1)\n\
            --Int(2)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", parser.parse_statement()), expected);
    }

    #[test]
    fn return_statement() {
        let input = "return 1 + 2";
        let expected = "Some(\
            Return\n\
            -Op(Add)\n\
            --Int(1)\n\
            --Int(2)\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", parser.parse_statement()), expected);
    }

    #[test]
    fn block_statement() {
        let input = "{1 + 2;3 + 4}";
        let expected = "Some(\
            Block(\
            Op(Add)\n\
            -Int(1)\n\
            -Int(2)\n\
            Op(Add)\n\
            -Int(3)\n\
            -Int(4)\n\
            )\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", parser.parse_block()), expected);
    }

    #[test]
    fn if_statement() {
        let input = "if (x < 0) {return 0}";
        let expected = "Some(\
            If\n\
            Op(Lt)\n\
            -Ident(\"x\")\n\
            -Int(0)\n\
            )\n\
            -Block(Return\n\
            -Int(0)\n\
            )\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", parser.parse_if()), expected);
    }

    #[test]
    fn fn_expression() {
        let input = "fn(a, b, c){return a * b - c}";
        let expected = "Some(\
            Fn([\"a\", \"b\", \"c\"]))\n\
            -Block(Return\n\
            -Op(Sub)\n\
            --Op(Mul)\n\
            ---Ident(\"a\")\n\
            ---Ident(\"b\")\n\
            --Ident(\"c\")\n\
            )\n\
        )";
        let mut parser = Parser::new(input);
        assert_eq!(format!("{:?}", parser.parse_fn()), expected);
    }
}
