mod env;
mod error;
mod result;

use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{BlockExpression, NodeKind, NodeRef, Op},
    err,
    parser::Parser,
};

use self::{
    env::Env,
    error::Error,
    result::{EvalResult, Value},
};

pub struct Eval<'a> {
    env: Rc<RefCell<Env<'a>>>,
}

use EvalResult::*;
use Value::*;

impl<'a> Eval<'a> {
    pub fn new() -> Eval<'a> {
        Eval { env: Env::new() }
    }

    fn eval_infix(op: &Op, lhs: EvalResult, rhs: EvalResult) -> EvalResult {
        match (lhs, rhs) {
            (Val(lhs), Val(rhs))
            | (Val(lhs), Return(rhs))
            | (Return(lhs), Val(rhs))
            | (Return(lhs), Return(rhs)) => {
                match op {
                    Op::Assign => todo!(),
                    Op::Eq => Val(Bool(lhs == rhs)),
                    Op::NotEq => Val(Bool(lhs != rhs)),
                    Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Lt | Op::Gt => {
                        // FIXME ugly
                        match (lhs, rhs) {
                            (Int(a), Int(b)) => match op {
                                Op::Add => Val(Int(a + b)),
                                Op::Sub => Val(Int(a - b)),
                                Op::Mul => Val(Int(a * b)),
                                Op::Div => Val(Int(a / b)),
                                Op::Lt => Val(Bool(a < b)),
                                Op::Gt => Val(Bool(a > b)),
                                _ => unreachable!(),
                            },
                            p @ _ => err!(
                                "unknown operator {} {} {}",
                                p.0.type_str(),
                                op,
                                p.1.type_str()
                            ),
                        }
                    }
                    Op::Neg | Op::Not | Op::Call => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    fn eval_prefix(op: &Op, rhs: Value) -> EvalResult {
        match op {
            Op::Neg => match rhs {
                Int(i) => Val(Int(-i)),
                _ => err!("unknown operator {}{}", op, rhs.type_str()),
            },
            Op::Not => match rhs {
                Bool(b) => Val(Bool(!b)),
                _ => err!("unknown operator {}{}", op, rhs.type_str()),
            },
            _ => unreachable!(),
        }
    }

    fn eval_block(env: Rc<RefCell<Env>>, block: &BlockExpression) -> EvalResult {
        let mut rv = Val(Nil);
        let env = Env::from(env);
        for stmt in block.statements.to_owned() {
            match Self::eval_subtree(env.clone(), stmt) {
                val @ Val(_) | val @ Return(_) => {
                    rv = val;
                }
                err @ Err(_) => return err,
            }
        }
        rv
    }

    fn eval_subtree(env: Rc<RefCell<Env>>, node_ref: NodeRef) -> EvalResult {
        match node_ref {
            Some(node) => match &node.kind {
                NodeKind::Ident(name) => match env.borrow().get(&name.to_string()) {
                    Some(val) => Val(val),
                    None => todo!(),
                },
                NodeKind::Int(i) => Val(Int(*i)),
                NodeKind::Bool(b) => Val(Bool(*b)),
                NodeKind::PrefixOp(op) => match Self::eval_subtree(env, node.right.to_owned()) {
                    err @ Err(_) => err,
                    Val(val) | Return(val) => Self::eval_prefix(op, val),
                },
                NodeKind::InfixOp(op) => {
                    match (
                        Self::eval_subtree(env.clone(), node.left.to_owned()),
                        Self::eval_subtree(env.clone(), node.right.to_owned()),
                    ) {
                        p @ (Err(_), _) => p.0,
                        p @ (_, Err(_)) => p.1,
                        (lhs, rhs) => Self::eval_infix(op, lhs, rhs),
                    }
                }
                NodeKind::Let => match &node.left {
                    Some(lhs) => match &lhs.kind {
                        NodeKind::Ident(name) => {
                            env.borrow_mut().bind_local(name.to_string(), Nil);
                            match Self::eval_subtree(env.clone(), node.right.to_owned()) {
                                Val(val) | Return(val) => {
                                    if env.borrow_mut().bind(name.to_string(), val) {
                                        return Val(Nil);
                                    } else {
                                        todo!()
                                    }
                                }
                                Err(_) => todo!(),
                            }
                        }
                        _ => todo!(),
                    },
                    None => todo!(),
                },
                NodeKind::Return => match Self::eval_subtree(env, node.right.to_owned()) {
                    Val(val) | Return(val) => Return(val),
                    err @ Err(_) => err,
                },
                NodeKind::If(if_expr) => {
                    match Self::eval_subtree(env.clone(), if_expr.condition.to_owned()) {
                        Val(val) | Return(val) => match val {
                            Bool(cond) => {
                                if cond {
                                    Self::eval_subtree(env, node.left.to_owned())
                                } else {
                                    Self::eval_subtree(env, node.right.to_owned())
                                }
                            }
                            Nil => err!("expected condition after if"),
                            _ => Self::eval_subtree(env, node.left.to_owned()),
                        },
                        err @ Err(_) => err,
                    }
                }
                NodeKind::Block(block) => Self::eval_block(env, block),
                NodeKind::Fn(_) => todo!(),
                NodeKind::Call(_) => todo!(),
            },
            None => Val(Nil),
        }
    }

    fn eval_statement(env: Rc<RefCell<Env>>, stmt: &str) -> EvalResult {
        let mut parser = Parser::new(stmt);
        Self::eval_subtree(env, parser.parse_statement())
    }

    pub fn eval(&self, stmt: &str) -> EvalResult {
        Self::eval_statement(self.env.clone(), stmt)
    }
}

#[cfg(test)]
mod tests {
    use super::{Eval, EvalResult::*, Value::*};

    macro_rules! assert_eval_stmt {
        ($input:expr, $expected:expr) => {
            let eval = Eval::new();
            assert_eq!(eval.eval($input), $expected);
        };
    }

    #[test]
    fn nil() {
        assert_eval_stmt!("", Val(Nil));
    }

    #[test]
    fn int_literal() {
        assert_eval_stmt!("5", Val(Int(5)));
    }

    #[test]
    fn bool_literal() {
        assert_eval_stmt!("false", Val(Bool(false)));
    }

    #[test]
    fn numerical() {
        assert_eval_stmt!("5 + -10 * 2", Val(Int(-15)));
    }

    #[test]
    fn neg() {
        assert_eval_stmt!("-5", Val(Int(-5)));
    }

    #[test]
    fn not() {
        assert_eval_stmt!("!false", Val(Bool(true)));
    }

    #[test]
    fn int_comparison() {
        assert_eval_stmt!("1 > 5", Val(Bool(false)));
    }

    #[test]
    fn equal_int_int() {
        assert_eval_stmt!("1 == 1", Val(Bool(true)));
        assert_eval_stmt!("1 == 2", Val(Bool(false)));
    }

    #[test]
    fn not_equal_int_int() {
        assert_eval_stmt!("1 != 2", Val(Bool(true)));
        assert_eval_stmt!("2 != 2", Val(Bool(false)));
    }

    #[test]
    fn equal_bool_bool() {
        assert_eval_stmt!("true == true", Val(Bool(true)));
        assert_eval_stmt!("true == false", Val(Bool(false)));
    }

    #[test]
    fn not_equal_bool_bool() {
        assert_eval_stmt!("true != false", Val(Bool(true)));
        assert_eval_stmt!("true != true", Val(Bool(false)));
    }

    #[test]
    fn block_single_statement() {
        assert_eval_stmt!("{1}", Val(Int(1)));
    }

    #[test]
    fn block_multi_statement() {
        assert_eval_stmt!("{1;2;3}", Val(Int(3)));
    }

    #[test]
    fn block_with_early_return() {
        assert_eval_stmt!("{1;return 2;3}", Val(Int(3)));
    }

    #[test]
    fn if_statement() {
        assert_eval_stmt!("if (1 < 2) {1} {2}", Val(Int(1)));
    }

    #[test]
    fn eval_block_with_let() {
        assert_eval_stmt!("{let x = 1; x}", Val(Int(1)));
    }
}
