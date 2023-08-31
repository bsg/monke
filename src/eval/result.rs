use crate::ast::{FnExpression, NodeRef};

use super::error::Error;

#[derive(Debug, PartialEq, Clone)]
pub enum Value<'a> {
    Nil,
    Int(i64),
    Bool(bool),
    Fn(FnExpression<'a>, NodeRef<'a>),
}

impl Value<'_> {
    pub fn type_str(&self) -> &str {
        match self {
            Value::Nil => "Nil",
            Value::Int(_) => "Int",
            Value::Bool(_) => "Bool",
            Value::Fn(..) => "Fn",
        }
    }
}

impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => f.write_str("nil"),
            Value::Int(val) => f.write_fmt(format_args!("{}", val)),
            Value::Bool(val) => f.write_fmt(format_args!("{}", val)),
            Value::Fn(..) => f.write_str("fn"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum EvalResult<'a> {
    Val(Value<'a>),
    Return(Value<'a>),
    Err(Error),
}

impl std::fmt::Display for EvalResult<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Val(val) | Self::Return(val) => f.write_fmt(format_args!("{}", val)),
            Self::Err(err) => f.write_fmt(format_args!("error: {}", err)),
        }
    }
}
