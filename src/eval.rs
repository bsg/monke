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
pub enum EvalResult {
    NonReturn(Value),
    Return(Value),
}

impl EvalResult {
    fn unwrap(self) -> Value {
        match self {
            EvalResult::NonReturn(val) => val,
            EvalResult::Return(val) => val,
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
                NodeKind::Int(i) => NonReturn(Int(*i)),
                NodeKind::Bool(b) => NonReturn(Bool(*b)),
                NodeKind::PrefixOp(op) => {
                    let rhs = Self::eval_subtree(node.right.to_owned());

                    match op {
                        Op::Neg => match rhs.unwrap() {
                            Int(i) => NonReturn(Int(-i)),
                            _ => todo!(),
                        },
                        Op::Not => todo!(),
                        _ => unreachable!(),
                    }
                }
                NodeKind::InfixOp(op) => {
                    let lhs = Self::eval_subtree(node.left.to_owned()).unwrap();
                    let rhs = Self::eval_subtree(node.right.to_owned()).unwrap();

                    match op {
                        Op::Assign => todo!(),
                        Op::Eq => NonReturn(Bool(lhs == rhs)),
                        Op::NotEq => NonReturn(Bool(lhs != rhs)),
                        Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Lt | Op::Gt => {
                            // FIXME ugly
                            match (lhs, rhs) {
                                (Int(a), Int(b)) => match op {
                                    Op::Add => NonReturn(Int(a + b)),
                                    Op::Sub => NonReturn(Int(a - b)),
                                    Op::Mul => NonReturn(Int(a * b)),
                                    Op::Div => NonReturn(Int(a / b)),
                                    Op::Lt => NonReturn(Bool(a < b)),
                                    Op::Gt => NonReturn(Bool(a > b)),
                                    _ => unreachable!(),
                                },
                                _ => todo!(),
                            }
                        }
                        Op::Neg | Op::Not | Op::Call => unreachable!(),
                    }
                }
                NodeKind::Let => todo!(),
                NodeKind::Return => todo!(),
                NodeKind::If(ifExpr) => {
                    match Self::eval_subtree(ifExpr.condition.to_owned()).unwrap() {
                        Nil => todo!(),
                        Int(_) => todo!(),
                        Bool(cond) => {
                            if cond {
                                Self::eval_subtree(node.left.to_owned())
                            } else {
                                Self::eval_subtree(node.right.to_owned())
                            }
                        }
                    }
                }
                NodeKind::Block(block) => {
                    let mut rv = NonReturn(Nil);
                    for stmt in block.statements.to_owned() {
                        rv = Self::eval_subtree(stmt)
                    }
                    NonReturn(rv.unwrap())
                }
                NodeKind::Fn(_) => todo!(),
                NodeKind::Call(_) => todo!(),
            },
            None => NonReturn(Nil),
        }
    }

    pub fn eval_statement(stmt: &str) -> Value {
        let mut parser = Parser::new(stmt);
        Self::eval_subtree(parser.parse_statement()).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::{Eval, Value::*};

    macro_rules! assert_eval_stmt {
        ($input:expr, $expected:expr) => {
            assert_eq!(Eval::eval_statement($input), $expected);
        };
    }

    #[test]
    fn nil() {
        assert_eval_stmt!("", Nil);
    }

    #[test]
    fn int_literal() {
        assert_eval_stmt!("5", Int(5));
    }

    #[test]
    fn bool_literal() {
        assert_eval_stmt!("false", Bool(false));
    }

    #[test]
    fn numerical() {
        assert_eval_stmt!("5 + -10 * 2", Int(-15));
    }

    #[test]
    fn int_comparison() {
        assert_eval_stmt!("1 > 5", Bool(false));
    }

    #[test]
    fn equal_int_int() {
        assert_eval_stmt!("1 == 1", Bool(true));
        assert_eval_stmt!("1 == 2", Bool(false));
    }

    #[test]
    fn not_equal_int_int() {
        assert_eval_stmt!("1 != 2", Bool(true));
        assert_eval_stmt!("2 != 2", Bool(false));
    }

    #[test]
    fn equal_bool_bool() {
        assert_eval_stmt!("true == true", Bool(true));
        assert_eval_stmt!("true == false", Bool(false));
    }

    #[test]
    fn not_equal_bool_bool() {
        assert_eval_stmt!("true != false", Bool(true));
        assert_eval_stmt!("true != true", Bool(false));
    }

    #[test]
    fn block_single_statement() {
        assert_eval_stmt!("{1}", Int(1));
    }

    #[test]
    fn block_multi_statement() {
        assert_eval_stmt!("{1;2;3}", Int(3));
    }

    #[test]
    fn if_statement() {
        assert_eval_stmt!("if (1 < 2) {1} {2}", Int(1));
    }
}
