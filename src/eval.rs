use crate::{
    ast::{NodeKind, NodeRef},
    parser::Parser,
};

#[derive(Debug)]
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
        match node_ref {
            Some(node) => match node.kind {
                NodeKind::Ident(_) => todo!(),
                NodeKind::Int(i) => Value::Int(i),
                NodeKind::Bool(b) => Value::Bool(b),
                NodeKind::InfixOp(_) => todo!(),
                NodeKind::PrefixOp(_) => todo!(),
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
