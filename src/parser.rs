use std::iter::Peekable;
use std::rc::Rc;

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
        loop {
            self.curr_token = self.tokens.next();
            self.peek_token = match self.tokens.peek() {
                Some(token) => Some(*token),
                None => None,
            };
            if let Some(Token::Semicolon) = self.curr_token {
            } else {
                break;
            }
        }
    }

    /// Caller must ensure current token is LBrace
    fn parse_block(&mut self) -> Rc<Option<Node<'a>>> {
        let mut statements: Vec<Rc<Node<'a>>> = Vec::new();

        match self.curr_token {
            Some(Token::LBrace) => {
                loop {
                    match Rc::into_inner(self.parse_statement()).unwrap() {
                        Some(stmt) => statements.push(Rc::from(stmt)),
                        None => break,
                    }
                }
                assert_eq!(self.curr_token, Some(Token::RBrace));
                self.next_token(); // Consume RBrace
                Rc::from(Some(Node {
                    kind: NodeKind::Block(BlockExpression { statements }),
                    left: None.into(),
                    right: None.into(),
                }))
            }
            _ => None.into(),
        }
    }

    fn parse_if(&mut self) -> Rc<Option<Node<'a>>> {
        assert_eq!(self.curr_token, Some(Token::If));
        match Rc::into_inner(self.parse_expression(0)).unwrap() {
            Some(cond) => {
                assert_eq!(self.curr_token, Some(Token::RParen));
                self.next_token();
                let lhs = self.parse_block();

                // eat 'else' if there is one
                loop {
                    match self.curr_token {
                        Some(Token::Else) => self.next_token(),
                        _ => break,
                    }
                }

                assert_ne!(self.curr_token, Some(Token::RBrace));
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

    fn parse_fn(&mut self) -> Rc<Option<Node<'a>>> {
        let mut args: Vec<&'a str> = Vec::new();

        match (self.curr_token, self.peek_token) {
            (Some(Token::Function), Some(Token::LParen)) => {
                self.next_token();
                self.next_token();
                loop {
                    match self.curr_token {
                        Some(Token::Ident(name)) => args.push(name),
                        Some(Token::Comma) => (),
                        _ => break,
                    }
                    self.next_token();
                }

                self.next_token();
                assert_eq!(self.curr_token, Some(Token::LBrace));
                let body = self.parse_block();

                Rc::from(Some(Node {
                    kind: NodeKind::Fn(FnExpression { args }),
                    left: None.into(),
                    right: body,
                }))
            }
            (Some(_), Some(_)) => todo!(),
            (_, _) => None.into(),
        }
    }

    fn parse_call(&mut self, rhs: Rc<Option<Node<'a>>>) -> Rc<Option<Node<'a>>> {
        let mut args: Vec<Rc<Node<'a>>> = Vec::new();

        self.next_token();
        loop {
            match Rc::into_inner(self.parse_expression(0)).unwrap() {
                Some(node) => {
                    args.push(node.into());
                }
                None => break,
            }
            self.next_token();
        }
        self.next_token();

        Rc::from(Some(Node {
            kind: NodeKind::Call(CallExpression { args }),
            left: None.into(),
            right: rhs,
        }))
    }

    pub fn parse_statement(&mut self) -> Rc<Option<Node<'a>>> {
        let node = match self.tokens.peek() {
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
        };

        loop {
            match self.peek_token {
                Some(Token::Semicolon) => self.next_token(),
                _ => break,
            }
        }
        node
    }

    fn parse_ident(&self) -> Rc<Option<Node<'a>>> {
        match self.curr_token {
            Some(Token::Ident(name)) => Rc::from(Some(Node {
                kind: NodeKind::Ident(name),
                left: None.into(),
                right: None.into(),
            })),
            _ => todo!(),
        }
    }

    pub fn parse_expression(&mut self, precedence: i32) -> Rc<Option<Node<'a>>> {
        self.next_token();

        let mut lhs = match self.curr_token {
            // these are prefix...
            // INT
            Some(Token::Int(s)) => {
                let i = match s.parse() {
                    Ok(i) => i,
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
            // LET
            Some(Token::Let) => self.parse_statement(),
            // LPAREN
            Some(Token::LParen) => self.parse_expression(0),
            // BLOCK
            Some(Token::LBrace) => self.parse_block(),
            // FUNCTION
            Some(Token::Function) => self.parse_fn(),
            // IF
            Some(Token::If) => self.parse_if(),
            _ => None.into(),
        };

        loop {
            // ... and these are infix
            match self.peek_token {
                Some(Token::RParen) => {
                    self.next_token();
                    break;
                }
                _ => {
                    let op = match self.peek_token {
                        Some(Token::Assign) => Some(Op::Assign),
                        Some(Token::Plus) => Some(Op::Add),
                        Some(Token::Minus) => Some(Op::Sub),
                        Some(Token::Asterisk) => Some(Op::Mul),
                        Some(Token::Slash) => Some(Op::Div),
                        Some(Token::Eq) => Some(Op::Eq),
                        Some(Token::NotEq) => Some(Op::NotEq),
                        Some(Token::Lt) => Some(Op::Lt),
                        Some(Token::Gt) => Some(Op::Gt),
                        Some(Token::LParen) => Some(Op::Call),
                        _ => break,
                    };

                    match op {
                        Some(Op::Call) => {
                            let tmp = self.parse_call(lhs);
                            lhs = tmp;
                        }
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
        lhs
    }
}

mod tests {
    use super::*;

    macro_rules! assert_parse {
        ($input:expr, $expected:expr) => {
            let mut parser = Parser::new($input);
            match Rc::into_inner(parser.parse_statement()).unwrap() {
                Some(ast) => assert_eq!(
                    format!("{}", ast)
                        .chars()
                        .filter(|c| !c.is_ascii_control())
                        .collect::<String>(),
                    $expected
                ),
                None => panic!(),
            }
        };
    }

    #[test]
    fn int_literal() {
        assert_parse!("453;", "Int(453)");
    }

    #[test]
    fn ident() {
        assert_parse!("__var_12;", "Ident(__var_12)");
    }

    #[test]
    fn bool() {
        assert_parse!("true", "Bool(true)");
    }

    #[test]
    fn op_neg() {
        assert_parse!(
            "-12;",
            "Neg\
            -Int(12)"
        );
    }

    #[test]
    fn op_not() {
        assert_parse!(
            "!false;",
            "Not\
            -Bool(false)"
        );
    }

    #[test]
    fn op_assign() {
        assert_parse!(
            "x = 2",
            "Assign\
            -Ident(x)\
            -Int(2)"
        );
    }

    #[test]
    fn op_add() {
        assert_parse!(
            "6 + 2",
            "Add\
            -Int(6)\
            -Int(2)"
        );
    }

    #[test]
    fn op_sub() {
        assert_parse!(
            "6 - 2",
            "Sub\
            -Int(6)\
            -Int(2)"
        );
    }

    #[test]
    fn op_mul() {
        assert_parse!(
            "6 * 2",
            "Mul\
            -Int(6)\
            -Int(2)"
        );
    }

    #[test]
    fn op_div() {
        assert_parse!(
            "6 / 2",
            "Div\
            -Int(6)\
            -Int(2)"
        );
    }

    #[test]
    fn op_eq() {
        assert_parse!(
            "5 == 5",
            "Eq\
            -Int(5)\
            -Int(5)"
        );
    }

    #[test]
    fn op_not_eq() {
        assert_parse!(
            "5 != 5",
            "NotEq\
            -Int(5)\
            -Int(5)"
        );
    }
    #[test]
    fn op_lt() {
        assert_parse!(
            "5 < 5",
            "Lt\
            -Int(5)\
            -Int(5)"
        );
    }

    #[test]
    fn op_gt() {
        assert_parse!(
            "5 > 5",
            "Gt\
            -Int(5)\
            -Int(5)"
        );
    }

    #[test]
    fn if_expression() {
        assert_parse!(
            "if (x < 0) {return 0}",
            "If\
            -Lt\
            --Ident(x)\
            --Int(0)\
            Then\
            -Block\
            --Return\
            ---Int(0)"
        );
    }

    #[test]
    fn if_with_alternate() {
        assert_parse!(
            "if (a < b) {return a} {return b}",
            "If\
            -Lt\
            --Ident(a)\
            --Ident(b)\
            Then\
            -Block\
            --Return\
            ---Ident(a)\
            Else\
            -Block\
            --Return\
            ---Ident(b)"
        );
    }

    #[test]
    fn if_else() {
        assert_parse!(
            "if (a < b) {return a} else {return b}",
            "If\
            -Lt\
            --Ident(a)\
            --Ident(b)\
            Then\
            -Block\
            --Return\
            ---Ident(a)\
            Else\
            -Block\
            --Return\
            ---Ident(b)"
        );
    }

    #[test]
    fn op_precedence() {
        assert_parse!(
            "1 + 2 * (3 - 4) / 5",
            "Add\
            -Int(1)\
            -Mul\
            --Int(2)\
            --Div\
            ---Sub\
            ----Int(3)\
            ----Int(4)\
            ---Int(5)"
        );
    }

    #[test]
    fn let_statement() {
        assert_parse!(
            "let x = 1 + 2",
            "Let\
            -Ident(x)\
            -Add\
            --Int(1)\
            --Int(2)"
        );
    }

    #[test]
    fn return_statement() {
        assert_parse!(
            "return 1 + 2",
            "Return\
            -Add\
            --Int(1)\
            --Int(2)"
        );
    }

    #[test]
    fn block_with_semicolons() {
        assert_parse!(
            "{let x = 1;let y = 2;return x + y}",
            "Block\
            -Let\
            --Ident(x)\
            --Int(1)\
            -Let\
            --Ident(y)\
            --Int(2)\
            -Return\
            --Add\
            ---Ident(x)\
            ---Ident(y)"
        );
    }

    #[test]
    fn block_without_semicolons() {
        assert_parse!(
            "{1 + 2 3 + 4}",
            "Block\
            -Add\
            --Int(1)\
            --Int(2)\
            -Add\
            --Int(3)\
            --Int(4)"
        );
    }

    #[test]
    fn nested_blocks() {
        assert_parse!(
            "{{1+2}}",
            "Block\
            -Block\
            --Add\
            ---Int(1)\
            ---Int(2)"
        );
    }

    #[test]
    fn fn_expression() {
        assert_parse!(
            "fn(a, b, c){return a * b - c;}",
            "Fn(a, b, c)\
            -Block\
            --Return\
            ---Sub\
            ----Mul\
            -----Ident(a)\
            -----Ident(b)\
            ----Ident(c)"
        );
    }

    #[test]
    fn fn_call() {
        assert_parse!(
            "f(2, a+1)",
            "Call f\
                -Int(2)\
                -Add\
                --Ident(a)\
                --Int(1)"
        );
    }
}
