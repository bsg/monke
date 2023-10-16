use std::{fmt, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Assign,
    Eq,
    NotEq,
    Lt,
    Gt,
    Le,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    Not,
    And,
    Or,
    Call,
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Assign => f.write_str("="),
            Op::Eq => f.write_str("=="),
            Op::NotEq => f.write_str("!="),
            Op::Lt => f.write_str("<"),
            Op::Gt => f.write_str(">"),
            Op::Le => f.write_str("<="),
            Op::Ge => f.write_str(">="),
            Op::Add => f.write_str("+"),
            Op::Sub => f.write_str("-"),
            Op::Mul => f.write_str("*"),
            Op::Div => f.write_str("/"),
            Op::Mod => f.write_str("%"),
            Op::Neg => f.write_str("-"),
            Op::Not => f.write_str("!"),
            Op::And => f.write_str("&&"),
            Op::Or => f.write_str("||"),
            Op::Call => f.write_str(""),
        }
    }
}

impl Op {
    pub fn precedence(&self) -> i32 {
        match self {
            Op::Eq | Op::NotEq => 1,
            Op::Lt | Op::Gt | Op::Le | Op::Ge => 2,
            Op::Add | Op::Sub => 3,
            Op::Mul | Op::Div | Op::Mod => 4,
            _ => 0,
        }
    }
}
pub type NodeRef = Option<Rc<Node>>;

#[derive(Clone, PartialEq)]
pub struct BlockExpression {
    pub statements: Rc<[NodeRef]>,
}

#[derive(Clone, PartialEq)]
pub struct IfExpression {
    pub condition: NodeRef,
}

#[derive(Clone)]
pub struct FnExpression {
    pub args: Rc<[Rc<str>]>,
}

impl core::cmp::PartialEq for FnExpression {
    fn eq(&self, _other: &Self) -> bool {
        false // TODO what do
    }
}

#[derive(Clone, PartialEq)]
pub struct CallExpression {
    pub ident: Rc<str>,
    pub args: Rc<[NodeRef]>,
}

#[derive(Clone, PartialEq)]
pub enum NodeKind {
    Ident(Rc<str>),
    Int(i64),
    Bool(bool),
    String(Rc<str>),
    InfixOp(Op),
    PrefixOp(Op),
    Let,
    Return,
    If(IfExpression),
    Block(BlockExpression),
    Fn(FnExpression),
    Call(CallExpression),
}

#[derive(Clone, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub left: NodeRef,
    pub right: NodeRef,
}

impl fmt::Debug for NodeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(arg0) => f.debug_tuple("Ident").field(arg0).finish(),
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::InfixOp(arg0) => f.debug_tuple("Op").field(arg0).finish(),
            Self::PrefixOp(arg0) => f.debug_tuple("Op").field(arg0).finish(),
            Self::Let => write!(f, "Let"),
            Self::Return => write!(f, "Return"),
            Self::If(arg0) => f.write_fmt(format_args!("If\n{:?}", arg0)),
            Self::Block(arg0) => f.debug_tuple("Block").field(arg0).finish(),
            Self::Fn(arg0) => write!(f, "Fn({:?})", arg0),
            Self::Call(arg0) => write!(f, "Call(\n{:?})", arg0),
        }
    }
}

impl fmt::Display for FnExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.args.join(", ").as_str())
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_with_indent(node: &Node, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            (0..indent).for_each(|_| _ = f.write_str("-"));

            f.write_fmt(format_args!(
                "{}",
                match &node.kind {
                    NodeKind::Ident(s) => format!("Ident({})\n", s),
                    NodeKind::Int(i) => format!("Int({})\n", i),
                    NodeKind::Bool(b) => format!("Bool({})\n", b),
                    NodeKind::String(s) => format!("String({})\n", s),
                    NodeKind::InfixOp(op) => format!("{:?}\n", op),
                    NodeKind::PrefixOp(op) => format!("{:?}\n", op),
                    NodeKind::Let => "Let\n".to_string(),
                    NodeKind::Return => "Return\n".to_string(),
                    NodeKind::If(_) => "If\n".to_string(),
                    NodeKind::Fn(args) => format!("Fn({})\n", args),
                    NodeKind::Block(_) => "Block\n".to_string(),
                    NodeKind::Call(_) => "Call ".to_string(),
                }
            ))?;
            match &node.kind {
                NodeKind::Block(block) => {
                    for stmt in block.statements.iter() {
                        stmt.as_ref()
                            .map(|node| fmt_with_indent(node, f, indent + 1));
                    }
                }
                NodeKind::If(c) => {
                    c.condition
                        .as_ref()
                        .map(|node| fmt_with_indent(node, f, indent + 1));

                    if let Some(node) = node.left.as_ref() {
                        f.write_fmt(format_args!("{}Then\n", "-".repeat(indent)))?;
                        fmt_with_indent(node, f, indent + 1)?;
                    }

                    if let Some(node) = node.right.as_ref() {
                        f.write_fmt(format_args!("{}Else\n", "-".repeat(indent)))?;
                        fmt_with_indent(node, f, indent + 1)?;
                    }

                    return Ok(());
                }

                NodeKind::Call(c) => {
                    if let Some(rhs) = node.right.as_ref() {
                        if let NodeKind::Ident(ident) = &rhs.kind {
                            f.write_fmt(format_args!("{}\n", ident))?;
                            for arg in c.args.iter() {
                                arg.as_ref()
                                    .map(|node| fmt_with_indent(node, f, indent + 1));
                            }
                            return Ok(());
                        }
                    }
                }
                _ => (),
            }

            if let Some(lhs) = node.left.as_ref() {
                fmt_with_indent(lhs, f, indent + 1)?;
            }

            if let Some(rhs) = node.right.as_ref() {
                fmt_with_indent(rhs, f, indent + 1)?;
            }

            Ok(())
        }
        fmt_with_indent(self, f, 0)?;
        Ok(())
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_with_indent(node: &Node, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            (0..indent).for_each(|_| _ = f.write_str("-"));
            f.write_fmt(format_args!("{:?}\n", node.kind))?;

            if let Some(lhs) = node.left.as_ref() {
                fmt_with_indent(lhs, f, indent + 1)?;
            }

            if let Some(rhs) = node.right.as_ref() {
                fmt_with_indent(rhs, f, indent + 1)?
            }

            Ok(())
        }
        fmt_with_indent(self, f, 0)?;
        Ok(())
    }
}

impl fmt::Debug for BlockExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in self.statements.iter() {
            f.write_fmt(format_args!("{:?}", stmt))?;
        }
        Ok(())
    }
}

impl fmt::Debug for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?})", self.condition))
    }
}

impl fmt::Debug for FnExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?})", self.args))
    }
}

impl fmt::Debug for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?})", self.args))
    }
}
