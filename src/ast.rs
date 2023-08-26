use std::{fmt, rc::Rc};

#[derive(Debug)]
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

pub struct BlockExpression<'a> {
    pub statements: Vec<Rc<Node<'a>>>,
}

pub struct IfExpression<'a> {
    pub condition: Rc<Node<'a>>,
}

pub struct FnExpression<'a> {
    pub args: Vec<&'a str>,
}

pub struct CallExpression<'a> {
    pub args: Vec<Rc<Node<'a>>>,
}

pub enum NodeKind<'a> {
    Ident(&'a str),
    Int(i64),
    Bool(bool),
    Op(Op),
    Let,
    Return,
    If(IfExpression<'a>),
    Block(BlockExpression<'a>),
    Fn(FnExpression<'a>),
    Call(CallExpression<'a>),
    // TODO expression list?
}

pub struct Node<'a> {
    pub kind: NodeKind<'a>,
    pub left: Rc<Option<Node<'a>>>,
    pub right: Rc<Option<Node<'a>>>,
}

impl fmt::Debug for NodeKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(arg0) => f.debug_tuple("Ident").field(arg0).finish(),
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Op(arg0) => f.debug_tuple("Op").field(arg0).finish(),
            Self::Let => write!(f, "Let"),
            Self::Return => write!(f, "Return"),
            Self::If(arg0) => f.write_fmt(format_args!("If\n{:?}", arg0)),
            Self::Block(arg0) => f.debug_tuple("Block").field(arg0).finish(),
            Self::Fn(arg0) => write!(f, "Fn({:?})", arg0),
            Self::Call(arg0) => write!(f, "Call(\n{:?})", arg0),
        }
    }
}

impl fmt::Display for FnExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.args.join(", ").as_str())
    }
}

impl fmt::Display for Node<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_with_indent(node: &Node, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            (0..indent).for_each(|_| _ = f.write_str("-"));

            f.write_fmt(format_args!(
                "{}",
                match &node.kind {
                    NodeKind::Ident(s) => format!("Ident({})\n", s),
                    NodeKind::Int(i) => format!("Int({})\n", i),
                    NodeKind::Bool(b) => format!("Bool({})\n", b),
                    NodeKind::Op(op) => format!("{:?}\n", op),
                    NodeKind::Let => format!("Let\n"),
                    NodeKind::Return => format!("Return\n"),
                    NodeKind::If(_) => format!("If\n"),
                    NodeKind::Fn(args) => format!("Fn({})\n", args),
                    NodeKind::Block(_) => format!("Block\n"),
                    NodeKind::Call(_) => format!("Call "),
                }
            ))?;
            match &node.kind {
                NodeKind::Block(block) => {
                    for stmt in &block.statements {
                        fmt_with_indent(stmt, f, indent + 1)?;
                    }
                }
                NodeKind::If(c) => {
                    fmt_with_indent(&c.condition, f, indent + 1)?;
                    match node.left.as_ref().clone() {
                        Some(node) => {
                            f.write_str("Then\n")?;
                            fmt_with_indent(&node, f, indent + 1)?;
                        }
                        None => (),
                    };
                    match node.right.as_ref().clone() {
                        Some(node) => {
                            f.write_str("Else\n")?;
                            fmt_with_indent(&node, f, indent + 1)?;
                        }
                        None => (),
                    };
                    return Ok(());
                }

                NodeKind::Call(c) => match node.right.as_ref().clone() {
                    Some(rhs) => match rhs.kind {
                        NodeKind::Ident(ident) => {
                            f.write_fmt(format_args!("{}\n", ident))?;
                            for arg in c.args.iter() {
                                fmt_with_indent(arg, f, indent + 1)?;
                            }
                            return Ok(())
                        }
                        _ => (),
                    },
                    _ => (),
                },
                _ => (),
            }
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

impl fmt::Debug for BlockExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.statements {
            f.write_fmt(format_args!("{:?}", stmt))?;
        }
        Ok(())
    }
}

impl fmt::Debug for IfExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?})", self.condition))
    }
}

impl fmt::Debug for FnExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?})", self.args))
    }
}

impl fmt::Debug for CallExpression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?})", self.args))
    }
}
