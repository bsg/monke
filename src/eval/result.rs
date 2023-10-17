use std::rc::Rc;

use crate::ast::{FnExpression, NodeRef};

use super::{env::EnvRef, error::Error};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Nil,
    Int(i64),
    Bool(bool),
    String(Rc<str>),
    Fn(FnExpression, NodeRef, EnvRef),
    BuiltIn(fn(Vec<Value>) -> EvalResult),
    Array(Vec<Value>),
}

impl Value {
    pub fn type_str(&self) -> &str {
        match self {
            Value::Nil => "Nil",
            Value::Int(_) => "Int",
            Value::Bool(_) => "Bool",
            Value::String(_) => "String",
            Value::Fn(..) => "Fn",
            Value::BuiltIn(..) => "Fn",
            Value::Array(_) => "Array",
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => f.write_str("nil"),
            Value::Int(val) => f.write_fmt(format_args!("{}", val)),
            Value::Bool(val) => f.write_fmt(format_args!("{}", val)),
            Value::String(val) => f.write_fmt(format_args!("{}", val)),
            Value::Fn(..) => f.write_str("fn"),
            Value::BuiltIn(..) => f.write_str("builtin"),
            Value::Array(arr) => f.write_fmt(format_args!("Array{:?}", arr)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum EvalResult {
    Val(Value),
    Return(Value),
    Err(Error),
}

impl std::fmt::Display for EvalResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Val(val) | Self::Return(val) => f.write_fmt(format_args!("{}", val)),
            Self::Err(err) => f.write_fmt(format_args!("error: {}", err)),
        }
    }
}
