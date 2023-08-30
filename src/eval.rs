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
                        Op::Eq => todo!(),
                        Op::NotEq => todo!(),
                        Op::Lt => todo!(),
                        Op::Gt => todo!(),
                        Op::Add | Op::Sub | Op::Mul | Op::Div => {
                            // FIXME ugly
                            match (lhs, rhs) {
                                (Value::Int(a), Value::Int(b)) => match op {
                                    Op::Add => Value::Int(a + b),
                                    Op::Sub => Value::Int(a - b),
                                    Op::Mul => Value::Int(a * b),
                                    Op::Div => Value::Int(a / b),
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
    fn numerical() {
        assert_eval!("5 + -10 * 2", Int(-15));
    }
}
