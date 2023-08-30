use std::iter::Peekable;
use std::rc::Rc;

use crate::ast::*;
use crate::lexer::{Lexer, Token, Tokens};

pub struct Parser<'a> {
    tokens: Peekable<Tokens<'a>>,
    curr_token: Option<Token<'a>>,
    peek_token: Option<Token<'a>>,
}

macro_rules! node {
    ($kind:expr, $left:expr, $right:expr) => {
        Some(Rc::from(Node {
            kind: $kind,
            left: $left,
            right: $right,
        }))
    };
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Parser {
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

    fn parse_block(&mut self) -> NodeRef<'a> {
        let mut statements: Vec<NodeRef> = Vec::new();

        match self.curr_token {
            Some(Token::LBrace) => {
                loop {
                    match self.parse_statement() {
                        Some(stmt) => statements.push(Some(stmt)),
                        None => break,
                    }
                }
                assert_eq!(self.curr_token, Some(Token::RBrace));
                self.next_token(); // Consume RBrace
                node!(NodeKind::Block(BlockExpression { statements }), None, None)
            }
            _ => None,
        }
    }

    /// caller must ensure current token is If
    fn parse_if(&mut self) -> NodeRef<'a> {
        assert_eq!(self.curr_token, Some(Token::If));
        match self.parse_expression(0) {
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

                match lhs {
                    Some(_) => node!(
                        NodeKind::If(IfExpression {
                            condition: Some(cond),
                        }),
                        lhs,
                        rhs
                    ),
                    None => todo!(),
                }
            }
            None => todo!(),
        }
    }

    // caller must ensure current token is Fn
    fn parse_fn(&mut self) -> NodeRef<'a> {
        let mut args: Vec<&'a str> = Vec::new();

        match (self.curr_token, self.peek_token) {
            (Some(Token::Fn), Some(Token::LParen)) => {
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

                node!(NodeKind::Fn(FnExpression { args }), None, body)
            }
            (Some(_), Some(_)) => todo!(),
            (_, _) => None,
        }
    }

    fn parse_call(&mut self, rhs: NodeRef<'a>) -> NodeRef<'a> {
        let mut args: Vec<NodeRef<'a>> = Vec::new();
        let ident;

        match self.parse_ident() {
            Some(node) => match node.kind {
                NodeKind::Ident(id) => ident = id,
                _ => todo!(),
            },
            None => todo!(),
        }

        loop {
            match self.parse_expression(0) {
                Some(node) => {
                    args.push(node.into());
                    match self.peek_token {
                        Some(Token::Comma) => self.next_token(),
                        _ => break,
                    }
                }
                None => break,
            }
        }

        assert_eq!(self.curr_token, Some(Token::RParen));

        node!(NodeKind::Call(CallExpression { ident, args }), None, rhs)
    }

    pub fn parse_statement(&mut self) -> NodeRef<'a> {
        let node = match self.tokens.peek() {
            Some(Token::Let) => {
                self.next_token();
                self.next_token();
                match self.curr_token {
                    Some(Token::Ident(_)) => {
                        let lhs = self.parse_ident();
                        self.next_token();
                        match self.curr_token {
                            Some(Token::Assign) => {
                                node!(NodeKind::Let, lhs, self.parse_expression(0))
                            }
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
            Some(Token::Return) => {
                self.next_token();
                node!(NodeKind::Return, None, self.parse_expression(0))
            }
            Some(_) => self.parse_expression(0),
            None => None,
        };

        loop {
            match self.peek_token {
                Some(Token::Semicolon) => self.next_token(),
                _ => break,
            }
        }
        node
    }

    fn parse_ident(&self) -> NodeRef<'a> {
        match self.curr_token {
            Some(Token::Ident(name)) => node!(NodeKind::Ident(name), None, None),
            _ => todo!(),
        }
    }

    pub fn parse_expression(&mut self, precedence: i32) -> NodeRef<'a> {
        self.next_token();
        let mut lhs = match self.curr_token {
            // these are prefix...
            // INT
            Some(Token::Int(s)) => {
                let i = match s.parse() {
                    Ok(i) => i,
                    Err(_) => panic!(),
                };

                node!(NodeKind::Int(i), None, None)
            }
            // IDENT
            Some(Token::Ident(_)) => self.parse_ident(),
            // TRUE
            Some(Token::True) => node!(NodeKind::Bool(true), None, None),
            // FALSE
            Some(Token::False) => node!(NodeKind::Bool(false), None, None),
            // NEG
            Some(Token::Minus) => node!(NodeKind::PrefixOp(Op::Neg), None, self.parse_expression(0)),
            // NOT
            Some(Token::Bang) => node!(NodeKind::PrefixOp(Op::Not), None, self.parse_expression(0)),
            // LET
            Some(Token::Let) => self.parse_statement(),
            // LPAREN
            Some(Token::LParen) => self.parse_expression(0),
            // BLOCK
            Some(Token::LBrace) => self.parse_block(),
            // FUNCTION
            Some(Token::Fn) => self.parse_fn(),
            // IF
            Some(Token::If) => self.parse_if(),
            _ => None,
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
                            lhs = node!(NodeKind::InfixOp(op), lhs, rhs);
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
            match parser.parse_statement() {
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
    fn fn_call_noarg() {
        assert_parse!(
            "f()",
            "Call f"
        );
    }

    #[test]
    fn fn_call_with_args() {
        assert_parse!(
            "f(2, a+1)",
            "Call f\
                -Int(2)\
                -Add\
                --Ident(a)\
                --Int(1)"
        );
    }

    #[test]
    fn fn_call_with_if_arg() {
        assert_parse!(
            "f(2, if(x){1}{2})",
            "Call f\
                -Int(2)\
                -If\
                --Then\
                ---Int(1)\
                --Else\
                ---Int(2)"
        );
    }
}
