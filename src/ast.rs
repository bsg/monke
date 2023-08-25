use std::{fmt, rc::Rc};

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Assign,
    Eq,
    NotEq,
    Lt,
    Gt,
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Not,
    Call,
}

impl Op {
    pub fn precedence(&self) -> i32 {
        match self {
            Op::Eq | Op::NotEq => 1,
            Op::Lt | Op::Gt => 2,
            Op::Add | Op::Sub => 3,
            Op::Mul | Op::Div => 4,
            _ => 0,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NodeKind<'a> {
    Ident(&'a str),
    Int(i64),
    Bool(bool),
    Op(Op),
    Let,
    Return,
    If(Rc<Option<Node<'a>>>),
    Block(Rc<Vec<Rc<Option<Node<'a>>>>>)
}

#[derive(PartialEq, Eq)]
pub struct Node<'a> {
    pub kind: NodeKind<'a>,
    pub left: Rc<Option<Node<'a>>>,
    pub right: Rc<Option<Node<'a>>>,
}

impl fmt::Debug for Node<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_with_indent(node: &Node, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            (0..indent).for_each(|_| _ = f.write_str("-"));
            f.write_fmt(format_args!("{:?}\n", node.kind))?;
            match node.left.as_ref().clone() {
                Some(node) => {
                    fmt_with_indent(&node, f, indent + 1)?;
                }
                None => (),
            };
            match node.right.as_ref().clone() {
                Some(node) => {
                    fmt_with_indent(&node, f, indent + 1)?;
                }
                None => (),
            };
            Ok(())
        }
        fmt_with_indent(self, f, 0)?;
        Ok(())
    }
}