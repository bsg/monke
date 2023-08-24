use std::{fmt, rc::Rc};

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Lt,
    Gt,
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Call,
}

#[derive(Debug, PartialEq, Eq)]
pub enum NodeKind<'a> {
    Ident(&'a str),
    InfixOp(Op),
    PrefixOp(Op),
    Int(i64),
}

#[derive(PartialEq, Eq)]
pub struct Node<'a> {
    pub kind: NodeKind<'a>,
    pub left: Option<Rc<Node<'a>>>,
    pub right: Option<Rc<Node<'a>>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StatementKind {
    Let,
    Return,
    Expr,
}

#[derive(PartialEq, Eq)]
pub struct Statement<'a> {
    pub kind: StatementKind,
    pub root: Rc<Node<'a>>,
}

impl fmt::Debug for Node<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_with_indent(node: &Node, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            (0..indent).for_each(|_| _ = f.write_str("-"));
            f.write_fmt(format_args!("{:?}\n", node.kind))?;
            match node.left.clone() {
                Some(node) => {
                    fmt_with_indent(&node, f, indent + 1)?;
                }
                None => (),
            }
            match node.right.clone() {
                Some(node) => {
                    fmt_with_indent(&node, f, indent + 1)?;
                }
                None => (),
            }
            Ok(())
        }
        fmt_with_indent(self, f, 0)?;
        Ok(())
    }
}

impl fmt::Debug for Statement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("[{:?}]\n{:?}", self.kind, self.root))
    }
}
