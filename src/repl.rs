use std::io;

use crate::{
    ast::{Node, NodeKind, Op},
    parser::Parser,
};

pub struct Repl {}

impl Repl {
    pub fn start() {
        let mut buffer = String::new();
        let stdin = io::stdin();
        loop {
            match stdin.read_line(&mut buffer) {
                Ok(_) => match Parser::new(buffer.as_str()).parse_statement().as_ref() {
                    Some(ast) => println!("{}\n{}", ast, Self::eval(ast)),
                    None => (),
                },
                Err(_) => break,
            }
            buffer.clear();
        }
    }

    fn eval(node: &Node) -> i64 {
        match &node.kind {
            NodeKind::Ident(_) => todo!(),
            NodeKind::Int(n) => *n,
            NodeKind::Bool(_) => todo!(),
            NodeKind::InfixOp(op) => {
                let lhs = Self::eval(node.left.as_ref().unwrap());
                let rhs = Self::eval(node.right.as_ref().unwrap());
                match op {
                    Op::Add => lhs + rhs,
                    Op::Sub => lhs - rhs,
                    Op::Mul => lhs * rhs,
                    Op::Div => lhs / rhs,
                    _ => todo!()
                }
            },
            NodeKind::PrefixOp(op) => todo!(),
            NodeKind::Let => todo!(),
            NodeKind::Return => todo!(),
            NodeKind::If(_) => todo!(),
            NodeKind::Block(_) => todo!(),
            NodeKind::Fn(_) => todo!(),
            NodeKind::Call(_) => todo!(),
        }
    }
}
