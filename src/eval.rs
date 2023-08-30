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

pub struct Eval {}

impl Eval {
    fn eval_subtree(node_ref: NodeRef) -> Value {
        use crate::ast::Op;
        match node_ref {
            Some(node) => match &node.kind {
                NodeKind::Ident(_) => todo!(),
                NodeKind::Int(i) => Value::Int(*i),
                NodeKind::Bool(b) => Value::Bool(*b),
                NodeKind::PrefixOp(op) => {
                    let rhs = Self::eval_subtree(node.right.to_owned());
                    match op {
                        Op::Neg => {
                            match rhs {
                                Value::Int(i) => Value::Int(-i),
                                _ => todo!()
                            }
                        },
                        Op::Not => todo!(),
                        _ => unreachable!(),
                    }
                },
                NodeKind::InfixOp(op) => {
                    let lhs = Self::eval_subtree(node.left.to_owned());
                    let rhs = Self::eval_subtree(node.right.to_owned());

                    match op {
                        Op::Assign => todo!(),
                        Op::Eq => Value::Bool(lhs == rhs),
                        Op::NotEq => Value::Bool(lhs != rhs),
                        Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Lt | Op::Gt => {
                            // FIXME ugly
                            match (lhs, rhs) {
                                (Value::Int(a), Value::Int(b)) => match op {
                                    Op::Add => Value::Int(a + b),
                                    Op::Sub => Value::Int(a - b),
                                    Op::Mul => Value::Int(a * b),
                                    Op::Div => Value::Int(a / b),
                                    Op::Lt => Value::Bool(a < b),
                                    Op::Gt => Value::Bool(a > b),
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
                NodeKind::If(_) => todo!(),
                NodeKind::Block(_) => todo!(),
                NodeKind::Fn(_) => todo!(),
                NodeKind::Call(_) => todo!(),
            },
            None => Value::Nil,
        }
    }

    pub fn eval_statement(statement: &str) -> Value {
        let mut parser = Parser::new(statement);
        Self::eval_subtree(parser.parse_statement())
    }
}

#[cfg(test)]
mod tests {
    use super::{Eval, Value::*};

    macro_rules! assert_eval {
        ($input:expr, $expected:expr) => {
            assert_eq!(Eval::eval_statement($input), $expected);
        };
    }

    #[test]
    fn nil() {
        assert_eval!("", Nil);
    }

    #[test]
    fn int_literal() {
        assert_eval!("5", Int(5));
    }

    #[test]
    fn bool_literal() {
        assert_eval!("false", Bool(false));
    }

    #[test]
    fn numerical() {
        assert_eval!("5 + -10 * 2", Int(-15));
    }

    #[test]
    fn int_comparison() {
        assert_eval!("1 > 5", Bool(false));
    }

    #[test]
    fn equal_int_int() {
        assert_eval!("1 == 1", Bool(true));
        assert_eval!("1 == 2", Bool(false));
    }

    #[test]
    fn not_equal_int_int() {
        assert_eval!("1 != 2", Bool(true));
        assert_eval!("2 != 2", Bool(false));
    }

    #[test]
    fn equal_bool_bool() {
        assert_eval!("true == true", Bool(true));
        assert_eval!("true == false", Bool(false));
    }

    #[test]
    fn not_equal_bool_bool() {
        assert_eval!("true != false", Bool(true));
        assert_eval!("true != true", Bool(false));
    }
}
