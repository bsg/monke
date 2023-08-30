use crate::{
    ast::{NodeKind, NodeRef},
    parser::Parser,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Value {
    Nil,
    Int(i64),
    Bool(bool),
}

impl Value {
    fn type_str(&self) -> &str {
        match self {
            Value::Nil => "Nil",
            Value::Int(_) => "Int",
            Value::Bool(_) => "Bool",
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => todo!(),
            Value::Int(val) => f.write_fmt(format_args!("{}", val)),
            Value::Bool(val) => f.write_fmt(format_args!("{}", val)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    message: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

macro_rules! err {
    ($($arg:tt)*) => {
        EvalResult::Err(Error { message: format!($($arg)*) })
    };
}

#[derive(Debug, PartialEq, Eq)]
pub enum EvalResult {
    Val(Value),
    Return(Value),
    Err(Error),
}

impl std::fmt::Display for EvalResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Val(val) | Self::Return(val) => f.write_fmt(format_args!("{}", val)),
            Self::Err(err) => f.write_fmt(format_args!("error: {}", err)),
        }
    }
}

pub struct Eval {}

impl Eval {
    fn eval_subtree(node_ref: NodeRef) -> EvalResult {
        use crate::ast::Op;
        use EvalResult::*;
        use Value::*;

        match node_ref {
            Some(node) => match &node.kind {
                NodeKind::Ident(_) => todo!(),
                NodeKind::Int(i) => Val(Int(*i)),
                NodeKind::Bool(b) => Val(Bool(*b)),
                NodeKind::PrefixOp(op) => match Self::eval_subtree(node.right.to_owned()) {
                    Val(rhs) | Return(rhs) => match op {
                        Op::Neg => match rhs {
                            Int(i) => Val(Int(-i)),
                            _ => err!("unknown operator -{}", op),
                        },
                        Op::Not => match rhs {
                            Bool(b) => Val(Bool(!b)),
                            _ => todo!(),
                        },
                        _ => unreachable!(),
                    },
                    err @ Err(_) => err,
                },
                NodeKind::InfixOp(op) => {
                    match (
                        Self::eval_subtree(node.left.to_owned()),
                        Self::eval_subtree(node.right.to_owned()),
                    ) {
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
                        p @ (Err(_), _) => return p.0,
                        p @ (_, Err(_)) => return p.1,
                    }
                }
                NodeKind::Let => todo!(),
                NodeKind::Return => match Self::eval_subtree(node.right.to_owned()) {
                    Val(val) | Return(val) => Return(val),
                    err @ Err(_) => err,
                },
                NodeKind::If(if_expr) => match Self::eval_subtree(if_expr.condition.to_owned()) {
                    Val(val) | Return(val) => match val {
                        Nil => todo!(),
                        Int(_) => todo!(),
                        Bool(cond) => {
                            if cond {
                                Self::eval_subtree(node.left.to_owned())
                            } else {
                                Self::eval_subtree(node.right.to_owned())
                            }
                        }
                    },
                    Err(_) => todo!(),
                },
                NodeKind::Block(block) => {
                    let mut rv = Val(Nil);
                    for stmt in block.statements.to_owned() {
                        match Self::eval_subtree(stmt) {
                            val @ Val(_) | val @ Return(_) => {
                                rv = val;
                            }
                            err @ Err(_) => return err,
                        }
                    }
                    rv
                }
                NodeKind::Fn(_) => todo!(),
                NodeKind::Call(_) => todo!(),
            },
            None => Val(Nil),
        }
    }

    pub fn eval_statement(stmt: &str) -> EvalResult {
        let mut parser = Parser::new(stmt);
        Self::eval_subtree(parser.parse_statement())
    }
}

#[cfg(test)]
mod tests {
    use super::{Eval, EvalResult::*, Value::*};

    macro_rules! assert_eval_stmt {
        ($input:expr, $expected:expr) => {
            assert_eq!(Eval::eval_statement($input), $expected);
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
}
