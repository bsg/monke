// TODO we need a ParseResult

use std::iter::Peekable;
use std::rc::Rc;

use crate::ast::*;
use crate::lexer::{Lexer, Token, Tokens};

pub struct Parser {
    tokens: Peekable<Tokens>,
    curr_token: Option<Token>,
    peek_token: Option<Token>,
}

macro_rules! node {
    ($kind:expr, $left:expr, $right:expr) => {
        Rc::new(Node {
            kind: $kind,
            left: $left,
            right: $right,
        })
    };
}

impl Parser {
    pub fn new(input: &str) -> Parser {
        Parser {
            tokens: Lexer::new(input).tokens().peekable(),
            curr_token: None,
            peek_token: None,
        }
    }

    fn next_token(&mut self) {
        self.curr_token = self.tokens.next();
        self.peek_token = self.tokens.peek().cloned();
    }

    fn parse_block(&mut self) -> Option<NodeRef> {
        assert_eq!(self.curr_token, Some(Token::LBrace));

        let mut statements: Vec<NodeRef> = Vec::new();

        match self.curr_token {
            Some(Token::LBrace) => {
                while let Some(stmt) = self.parse_statement().as_ref() {
                    statements.push(stmt.clone());
                }

                Some(node!(
                    NodeKind::Block(BlockExpression {
                        statements: Rc::from(statements.as_slice())
                    }),
                    None,
                    None
                ))
            }
            _ => todo!(),
        }
    }

    /// caller must ensure current token is If
    fn parse_if(&mut self) -> Option<NodeRef> {
        assert_eq!(self.curr_token, Some(Token::If));
        assert_eq!(self.peek_token, Some(Token::LParen));
        self.next_token();

        match self.parse_expression(0) {
            Some(cond) => {
                assert_eq!(self.peek_token, Some(Token::RParen));
                self.next_token();

                assert_eq!(self.peek_token, Some(Token::LBrace));
                self.next_token();
                let lhs = self.parse_block();

                // eat 'else' if there is one
                if let Some(Token::Else) = self.peek_token {
                    self.next_token();
                    assert_eq!(self.peek_token, Some(Token::LBrace));
                }

                let rhs = if self.peek_token == Some(Token::LBrace) {
                    self.next_token();
                    self.parse_block()
                } else {
                    None
                };

                match lhs {
                    Some(_) => Some(node!(
                        NodeKind::If(IfExpression { condition: cond }),
                        lhs,
                        rhs
                    )),
                    None => todo!(),
                }
            }
            None => todo!(),
        }
    }

    // caller must ensure current token is Fn
    fn parse_fn(&mut self) -> Option<NodeRef> {
        let mut args: Vec<Rc<str>> = Vec::new();

        match (&self.curr_token, &self.peek_token) {
            (Some(Token::Fn), Some(Token::LParen)) => {
                self.next_token();
                self.next_token();
                loop {
                    match &self.curr_token {
                        Some(Token::Ident(name)) => args.push(name.clone()),
                        Some(Token::Comma) => (),
                        _ => break,
                    }
                    self.next_token();
                }

                self.next_token();
                assert_eq!(self.curr_token, Some(Token::LBrace));
                let body = self.parse_block().unwrap(); // FIXME

                Some(node!(
                    NodeKind::Fn(FnExpression {
                        args: Rc::from(args.as_slice())
                    }),
                    None,
                    Some(body)
                ))
            }
            (_, _) => todo!(),
        }
    }

    fn parse_call(&mut self, rhs: NodeRef) -> Option<NodeRef> {
        let mut args: Vec<NodeRef> = Vec::new();
        // TODO ident is already parsed in parse_expression, use it
        let ident;

        match self.parse_ident() {
            Some(node) => match &node.kind {
                NodeKind::Ident(id) => ident = id.clone(),
                _ => todo!(),
            },
            None => todo!(),
        }

        self.next_token();
        if self.peek_token != Some(Token::RParen) {
            while let Some(node) = self.parse_expression(0) {
                args.push(node.into());
                match self.peek_token {
                    Some(Token::Comma) => {
                        self.next_token();
                    }
                    _ => break,
                }
            }
        }

        assert_eq!(self.peek_token, Some(Token::RParen));
        self.next_token();

        Some(node!(
            NodeKind::Call(CallExpression {
                ident,
                args: Rc::from(args.as_slice())
            }),
            None,
            Some(rhs)
        ))
    }

    fn parse_array(&mut self) -> Option<NodeRef> {
        assert_eq!(self.curr_token, Some(Token::LBracket));

        let mut arr: Vec<Rc<Node>> = Vec::new();

        if self.peek_token != Some(Token::RBracket) {
            while let Some(node) = self.parse_expression(0) {
                arr.push(node.clone());
                match self.peek_token {
                    Some(Token::Comma) => self.next_token(),
                    _ => break,
                }
            }
        }
        assert_eq!(self.peek_token, Some(Token::RBracket));
        self.next_token();

        Some(node!(NodeKind::Array(arr), None, None))
    }

    fn parse_index(&mut self) -> Option<NodeRef> {
        // TODO ident is already parsed in parse_expression, use it
        let ident;

        assert_eq!(self.peek_token, Some(Token::LBracket));

        match self.parse_ident() {
            Some(node) => match &node.kind {
                NodeKind::Ident(id) => ident = id.clone(),
                _ => todo!(),
            },
            None => todo!(),
        }
        self.next_token();

        let index = self.parse_expression(0).unwrap(); // FIXME
        assert_eq!(self.peek_token, Some(Token::RBracket));
        self.next_token();

        Some(node!(
            NodeKind::Index(IndexExpression { ident, index }),
            None,
            None
        ))
    }

    fn parse_pair(&mut self, key: NodeRef) -> Option<NodeRef> {
        assert_eq!(self.peek_token, Some(Token::Colon));
        self.next_token();
        let value = self.parse_expression(0).unwrap(); // FIXME
        if let Some(Token::Comma) = self.peek_token {
            self.next_token();
        }

        Some(node!(
            NodeKind::Pair(PairExpression { key, value }),
            None,
            None
        ))
    }

    pub fn parse_statement(&mut self) -> Option<NodeRef> {
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
                                Some(node!(NodeKind::Let, lhs, self.parse_expression(0)))
                            }
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
            Some(Token::Return) => {
                self.next_token();
                Some(node!(NodeKind::Return, None, self.parse_expression(0)))
            }
            Some(_) => self.parse_expression(0),
            None => return None,
        };

        while let Some(Token::Semicolon) = self.peek_token {
            self.next_token();
        }

        node
    }

    fn parse_ident(&self) -> Option<NodeRef> {
        match &self.curr_token {
            Some(Token::Ident(name)) => Some(node!(NodeKind::Ident(name.clone()), None, None)),
            _ => todo!(),
        }
    }

    fn parse_range(&mut self, lower: NodeRef) -> Option<NodeRef> {
        assert_eq!(self.peek_token, Some(Token::Range));
        self.next_token();

        if let Some(Token::Assign) = self.peek_token {
            self.next_token();
            if let Some(upper) = self.parse_expression(0) {
                Some(node!(
                    NodeKind::RangeInclusive(RangeExpression { lower, upper }),
                    None,
                    None
                ))
            } else {
                todo!()
            }
        } else {
            if let Some(upper) = self.parse_expression(0) {
                Some(node!(
                    NodeKind::Range(RangeExpression { lower, upper }),
                    None,
                    None
                ))
            } else {
                todo!()
            }
        }
    }

    pub fn parse_expression(&mut self, precedence: i32) -> Option<NodeRef> {
        self.next_token();
        let mut lhs = match &self.curr_token {
            // these are prefix...
            // INT
            Some(Token::Int(s)) => {
                let i = match s.parse() {
                    Ok(i) => i,
                    Err(_) => panic!(),
                };

                node!(NodeKind::Int(i), None, None)
            }
            // STRING
            Some(Token::Str(s)) => node!(NodeKind::Str(s.clone()), None, None),
            // IDENT
            Some(Token::Ident(_)) => {
                if let Some(node) = self.parse_ident() {
                    node
                } else {
                    todo!()
                }
            }
            // TRUE
            Some(Token::True) => node!(NodeKind::Bool(true), None, None),
            // FALSE
            Some(Token::False) => node!(NodeKind::Bool(false), None, None),
            // NEG
            Some(Token::Minus) => {
                node!(NodeKind::PrefixOp(Op::Neg), None, self.parse_expression(0))
            }
            // NOT
            Some(Token::Bang) => node!(NodeKind::PrefixOp(Op::Not), None, self.parse_expression(0)),
            // LET
            Some(Token::Let) => {
                if let Some(node) = self.parse_statement() {
                    node
                } else {
                    todo!()
                }
            }
            // LPAREN
            Some(Token::LParen) => {
                if let Some(node) = self.parse_expression(0) {
                    assert_eq!(self.peek_token, Some(Token::RParen));
                    self.next_token();
                    node
                } else {
                    todo!()
                }
            }
            // ARRAY
            Some(Token::LBracket) => {
                if let Some(node) = self.parse_array() {
                    node
                } else {
                    todo!()
                }
            }
            // BLOCK
            Some(Token::LBrace) => {
                if let Some(node) = self.parse_block() {
                    node
                } else {
                    todo!()
                }
            }
            // FUNCTION
            Some(Token::Fn) => {
                if let Some(node) = self.parse_fn() {
                    node
                } else {
                    todo!()
                }
            }
            // IF
            Some(Token::If) => {
                if let Some(node) = self.parse_if() {
                    node
                } else {
                    todo!()
                }
            }

            _ => return None,
        };

        loop {
            // ... and these are infix
            let op = match self.peek_token {
                Some(Token::Assign) => Some(Op::Assign),
                Some(Token::Plus) => Some(Op::Add),
                Some(Token::Minus) => Some(Op::Sub),
                Some(Token::Asterisk) => Some(Op::Mul),
                Some(Token::Slash) => Some(Op::Div),
                Some(Token::Percent) => Some(Op::Mod),
                Some(Token::Eq) => Some(Op::Eq),
                Some(Token::NotEq) => Some(Op::NotEq),
                Some(Token::Lt) => Some(Op::Lt),
                Some(Token::Gt) => Some(Op::Gt),
                Some(Token::Le) => Some(Op::Le),
                Some(Token::Ge) => Some(Op::Ge),
                Some(Token::And) => Some(Op::And),
                Some(Token::Or) => Some(Op::Or),
                Some(Token::LParen) => Some(Op::Call),
                Some(Token::LBracket) => Some(Op::Index),
                Some(Token::Colon) => Some(Op::Colon),
                Some(Token::Range) => Some(Op::Range),
                _ => break,
            };

            match op {
                Some(Op::Call) => {
                    lhs = self.parse_call(lhs).unwrap(); // FIXME
                }
                Some(Op::Index) => {
                    lhs = self.parse_index().unwrap(); // FIXME
                }
                Some(Op::Colon) => {
                    lhs = self.parse_pair(lhs).unwrap(); // FIXME
                }
                Some(Op::Range) => {
                    lhs = self.parse_range(lhs).unwrap(); // FIXME
                }
                Some(op) => {
                    if op.precedence() < precedence {
                        break;
                    }
                    self.next_token();
                    if let Some(rhs) = self.parse_expression(op.precedence()) {
                        lhs = node!(NodeKind::InfixOp(op), Some(lhs), Some(rhs));
                    } else {
                        todo!()
                    }
                }
                None => break,
            }
        }
        Some(lhs)
    }
}

#[cfg(test)]
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
    fn sequential_blocks() {
        // NOTE second block should not be parsed since we're parsing a single statement
        assert_parse!(
            r#"{"1"}{"2"}"#,
            "Block\
            -Str(1)"
        );
    }

    #[test]
    fn nested_sequential_blocks() {
        assert_parse!(
            "{{1}{2}}",
            "Block\
            -Block\
            --Int(1)\
            -Block\
            --Int(2)"
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
        assert_parse!("f()", "Call f");
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
    fn fn_call_with_block_arg() {
        assert_parse!(
            "f(2, {a+1})",
            "Call f\
            -Int(2)\
            -Block\
            --Add\
            ---Ident(a)\
            ---Int(1)"
        );
    }

    #[test]
    fn fn_call_with_if_arg() {
        assert_parse!(
            "f(2, if(x){1}else{2})",
            "Call f\
                -Int(2)\
                -If\
                --Ident(x)\
                -Then\
                --Block\
                ---Int(1)\
                -Else\
                --Block\
                ---Int(2)"
        );
    }

    #[test]
    fn sequential_ifs() {
        assert_parse!(
            "{if(x){1}if(y){2}}",
            "Block\
            -If\
            --Ident(x)\
            -Then\
            --Block\
            ---Int(1)\
            -If\
            --Ident(y)\
            -Then\
            --Block\
            ---Int(2)"
        );
    }

    #[test]
    fn call_precedence() {
        assert_parse!(
            "f(1) + f(2)",
            "Add\
            -Call f\
            --Int(1)\
            -Call f\
            --Int(2)"
        );
    }

    #[test]
    fn if_precedence() {
        // TODO
        assert_parse!(
            "if(a){1}{2} + if(b){3}{4}",
            "Add\
            -If\
            --Ident(a)\
            -Then\
            --Block\
            ---Int(1)\
            -Else\
            --Block\
            ---Int(2)\
            -If\
            --Ident(b)\
            -Then\
            --Block\
            ---Int(3)\
            -Else\
            --Block\
            ---Int(4)"
        );
    }

    #[test]
    fn parse_array() {
        assert_parse!("[]", "Array[]");
        assert_parse!("[1, 2, x]", "Array[Int(1), Int(2), Ident(\"x\")]");
    }

    #[test]
    #[should_panic]
    fn parse_invalid_array() {
        assert_parse!("[1, 2, x", "");
    }

    #[test]
    fn parse_index() {
        assert_parse!("arr[2]", "arr[Int(2)]");
        assert_parse!("arr[x]", "arr[Ident(\"x\")]");
        assert_parse!("arr[x + 2]", "arr[Op(Add)-Ident(\"x\")-Int(2)]");
        assert_parse!(
            "arr[1] + arr[2]",
            "Add\
            -arr[Int(1)]\
            -arr[Int(2)]"
        );
    }

    #[test]
    #[should_panic]
    fn parse_invalid_index() {
        assert_parse!("arr[2", "");
    }

    #[test]
    fn parse_pairs() {
        assert_parse!(
            r#"{"a": 1, true: 2}"#,
            "Block\
            -Pair(Str(\"a\"), Int(1))\
            -Pair(Bool(true), Int(2))"
        );
    }

    #[test]
    fn parse_range() {
        assert_parse!(r#"1..10 ignore_this"#, "Range(Int(1), Int(10))");

        assert_parse!(r#"1..=10"#, "RangeInclusive(Int(1), Int(10))");
    }
}
