// TODO logical and/or should short circuit

mod env;
mod error;
mod result;

use std::rc::Rc;

use crate::{
    ast::{BlockExpression, NodeKind, NodeRef, Op},
    err,
    parser::Parser,
};

use self::{
    env::{Env, EnvRef},
    error::Error,
    result::{EvalResult, Value},
};

pub struct Eval {
    env: EnvRef,
}

use EvalResult::Err;
use EvalResult::Return;
use EvalResult::Val;

use Value::Array;
use Value::Bool;
use Value::BuiltIn;
use Value::Fn;
use Value::Int;
use Value::Nil;
use Value::String;

impl Eval {
    pub fn new() -> Eval {
        // TODO construct this somewhere else
        let builtins = Env::new();

        builtins.borrow_mut().bind_local(
            "len".into(),
            Value::BuiltIn(|args| {
                if args.len() == 1 {
                    if let String(s) = &args[0] {
                        EvalResult::Return(Int(s.len().try_into().unwrap()))
                    } else {
                        err!("len() expected a string argument")
                    }
                } else {
                    err!("len() expected one argument")
                }
            }),
        );

        builtins.borrow_mut().bind_local(
            "puts".into(),
            Value::BuiltIn(|args| {
                if args.len() == 1 {
                    if let String(s) = &args[0] {
                        print!("{}", s);
                    }
                }
                EvalResult::Return(Nil)
            }),
        );

        Eval {
            env: Env::from(builtins),
        }
    }

    fn eval_infix(op: &Op, lhs: EvalResult, rhs: EvalResult) -> EvalResult {
        match (lhs, rhs) {
            (Val(lhs), Val(rhs))
            | (Val(lhs), Return(rhs))
            | (Return(lhs), Val(rhs))
            | (Return(lhs), Return(rhs)) => match (op, lhs, rhs) {
                (Op::Assign, ..) => unreachable!(),
                (Op::Eq, lhs, rhs) => Val(Bool(lhs == rhs)),
                (Op::NotEq, lhs, rhs) => Val(Bool(lhs != rhs)),
                (Op::Add, Int(a), Int(b)) => Val(Int(a + b)),
                (Op::Sub, Int(a), Int(b)) => Val(Int(a - b)),
                (Op::Mul, Int(a), Int(b)) => Val(Int(a * b)),
                (Op::Div, Int(a), Int(b)) => Val(Int(a / b)),
                (Op::Mod, Int(a), Int(b)) => Val(Int(a % b)),
                (Op::Lt, Int(a), Int(b)) => Val(Bool(a < b)),
                (Op::Gt, Int(a), Int(b)) => Val(Bool(a > b)),
                (Op::Le, Int(a), Int(b)) => Val(Bool(a <= b)),
                (Op::Ge, Int(a), Int(b)) => Val(Bool(a >= b)),
                (Op::And, Bool(a), Bool(b)) => Val(Bool(a && b)),
                (Op::Or, Bool(a), Bool(b)) => Val(Bool(a || b)),
                (Op::Add, String(a), String(b)) => {
                    let mut s = a.to_string();
                    s.push_str(&b);
                    Val(String(Rc::from(s.as_str())))
                }
                (_, lhs, rhs) => err!(
                    "unknown operator {} {} {}",
                    lhs.type_str(),
                    op,
                    rhs.type_str()
                ),
            },
            _ => unreachable!(),
        }
    }

    fn eval_prefix(op: &Op, rhs: Value) -> EvalResult {
        match op {
            Op::Neg => match rhs {
                Int(i) => Val(Int(-i)),
                _ => err!("unknown operator {}{}", op, rhs.type_str()),
            },
            Op::Not => match rhs {
                Bool(b) => Val(Bool(!b)),
                _ => err!("unknown operator {}{}", op, rhs.type_str()),
            },
            _ => unreachable!(),
        }
    }

    fn eval_block(env: EnvRef, block: BlockExpression) -> EvalResult {
        let mut rv = Val(Nil);
        let new_env = Env::from(env);
        for stmt in block.statements.iter().cloned() {
            match Self::eval_ast(new_env.clone(), stmt) {
                val @ Val(_) => {
                    rv = val;
                }
                val @ Return(_) => return val,
                err @ Err(_) => return err,
            }
        }
        rv
    }

    fn eval_assign(env: EnvRef, lhs: NodeRef, rhs: NodeRef, is_let: bool) -> EvalResult {
        match lhs {
            Some(lhs) => match &lhs.kind {
                NodeKind::Ident(name) => {
                    match Self::eval_ast(env.clone(), rhs.clone()) {
                        Val(val) | Return(val) => {
                            if is_let {
                                env.borrow_mut().bind_local(name.clone(), val);
                            } else {
                                env.borrow_mut().bind(name.clone(), val);
                            }
                            Val(Nil) // TODO should EvalResult have something like Ok?
                        }
                        Err(_) => todo!(),
                    }
                }
                _ => todo!(),
            },
            None => todo!(),
        }
    }

    fn eval_ast(env: EnvRef, node_ref: NodeRef) -> EvalResult {
        match node_ref.clone() {
            Some(node) => match node.kind.clone() {
                NodeKind::Ident(name) => match env.borrow().get(name.clone()) {
                    Some(val) => Val(val),
                    None => err!("Unknown ident {}", name),
                },
                NodeKind::Int(i) => Val(Int(i)),
                NodeKind::Bool(b) => Val(Bool(b)),
                NodeKind::String(b) => Val(String(b)),
                NodeKind::PrefixOp(op) => match Self::eval_ast(env, node.right.to_owned()) {
                    err @ Err(_) => err,
                    Val(val) | Return(val) => Self::eval_prefix(&op, val),
                },
                NodeKind::InfixOp(op) => match op {
                    Op::Assign => {
                        Self::eval_assign(env, node.left.to_owned(), node.right.to_owned(), false)
                    }
                    _ => {
                        match (
                            Self::eval_ast(env.clone(), node.left.to_owned()),
                            Self::eval_ast(env.clone(), node.right.to_owned()),
                        ) {
                            p @ (Err(_), _) => p.0,
                            p @ (_, Err(_)) => p.1,
                            (lhs, rhs) => Self::eval_infix(&op, lhs, rhs),
                        }
                    }
                },
                NodeKind::Let => {
                    Self::eval_assign(env, node.left.to_owned(), node.right.to_owned(), true)
                }
                NodeKind::Return => match Self::eval_ast(env, node.right.to_owned()) {
                    Val(val) | Return(val) => Return(val),
                    err @ Err(_) => err,
                },
                NodeKind::If(if_expr) => {
                    match Self::eval_ast(env.clone(), if_expr.condition.to_owned()) {
                        Val(val) | Return(val) => match val {
                            Bool(cond) => {
                                if cond {
                                    Self::eval_ast(env, node.left.to_owned())
                                } else {
                                    Self::eval_ast(env, node.right.to_owned())
                                }
                            }
                            Nil => err!("expected condition after if"),
                            _ => Self::eval_ast(env, node.left.to_owned()),
                        },
                        err @ Err(_) => err,
                    }
                }
                NodeKind::Block(block) => Self::eval_block(env, block),
                NodeKind::Fn(func) => Val(Fn(func, node.right.clone(), Env::from(env))),
                NodeKind::Call(call) => match env.borrow().get(call.ident.clone()) {
                    Some(Fn(func, ast, fnenv)) => {
                        if func.args.len() != call.args.len() {
                            return err!(
                                "function {} expects {} arguments",
                                call.ident,
                                func.args.len()
                            );
                        }
                        for (name, arg) in func.args.iter().zip(call.args.iter()) {
                            match Self::eval_ast(env.clone(), arg.clone()) {
                                Val(val) | Return(val) => {
                                    fnenv.borrow_mut().bind_local(name.clone(), val);
                                }
                                err @ Err(_) => return err,
                            }
                        }
                        Self::eval_ast(fnenv, ast)
                    }
                    Some(BuiltIn(f)) => {
                        let mut args: Vec<Value> = Vec::new();
                        for arg in call.args.iter() {
                            match Self::eval_ast(env.clone(), arg.clone()) {
                                Val(val) | Return(val) => args.push(val),
                                err @ Err(_) => return err,
                            }
                        }
                        f(args)
                    }
                    _ => todo!(),
                },
                NodeKind::Array(nodes) => {
                    let mut arr: Vec<Value> = Vec::new();
                    nodes.iter().for_each(|node| {
                        match Self::eval_ast(env.clone(), Some(node.clone())) {
                            Val(value) | Return(value) => {
                                arr.push(value);
                            }
                            Err(_) => todo!(),
                        }
                    });
                    Val(Array(arr))
                }
            },
            None => Val(Nil),
        }
    }

    pub fn eval(&self, code: Rc<str>) -> EvalResult {
        let mut last_result = Val(Nil);
        let mut parser = Parser::new(&code);

        loop {
            let ast = parser.parse_statement();
            if ast.is_some() {
                last_result = Self::eval_ast(self.env.clone(), ast);
            } else {
                break;
            }
        }

        last_result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_eval_stmt {
        ($input:expr, $expected:expr) => {
            let eval = Eval::new();
            assert_eq!(eval.eval($input.into()), $expected);
        };
    }

    #[test]
    fn nil() {
        assert_eval_stmt!("", Val(Nil));
    }

    #[test]
    fn int_literal() {
        assert_eval_stmt!("5", Val(Int(5)));
    }

    #[test]
    fn bool_literal() {
        assert_eval_stmt!("false", Val(Bool(false)));
    }

    #[test]
    fn numerical() {
        assert_eval_stmt!("5 + -10 * 2", Val(Int(-15)));
    }

    #[test]
    fn neg() {
        assert_eval_stmt!("-5", Val(Int(-5)));
    }

    #[test]
    fn not() {
        assert_eval_stmt!("!false", Val(Bool(true)));
    }

    #[test]
    fn int_comparison() {
        assert_eval_stmt!("1 > 5", Val(Bool(false)));
    }

    #[test]
    fn equal_int_int() {
        assert_eval_stmt!("1 == 1", Val(Bool(true)));
        assert_eval_stmt!("1 == 2", Val(Bool(false)));
    }

    #[test]
    fn not_equal_int_int() {
        assert_eval_stmt!("1 != 2", Val(Bool(true)));
        assert_eval_stmt!("2 != 2", Val(Bool(false)));
    }

    #[test]
    fn equal_bool_bool() {
        assert_eval_stmt!("true == true", Val(Bool(true)));
        assert_eval_stmt!("true == false", Val(Bool(false)));
    }

    #[test]
    fn not_equal_bool_bool() {
        assert_eval_stmt!("true != false", Val(Bool(true)));
        assert_eval_stmt!("true != true", Val(Bool(false)));
    }

    #[test]
    fn block_single_statement() {
        assert_eval_stmt!("{1}", Val(Int(1)));
    }

    #[test]
    fn block_multi_statement() {
        assert_eval_stmt!("{1;2;3}", Val(Int(3)));
    }

    #[test]
    fn block_with_early_return() {
        assert_eval_stmt!("{1;return 2;3}", Return(Int(2)));
    }

    #[test]
    fn if_statement() {
        assert_eval_stmt!("if (1 < 2) {1} {2}", Val(Int(1)));
    }

    #[test]
    fn eval_block_with_let() {
        assert_eval_stmt!("{let x = 1; x}", Val(Int(1)));
    }

    #[test]
    fn fn_call() {
        let ctx = Eval::new();

        let code = "
            let f = fn(x){x * x};
            f(2);
        ";
        assert_eq!(ctx.eval(code.into()), Val(Int(4)));

        let code = "
            let g = fn(x, y){x * y}
            g(2, 10);
        ";
        assert_eq!(ctx.eval(code.into()), Val(Int(20)));
        assert_eq!(ctx.eval("f(g(2, 10));".into()), Val(Int(400)));
    }

    #[test]
    fn nested_scopes() {
        let code = "
            {
                let x = 1;
                {
                    let y = 2;
                    {
                        x + y;
                    }
                }
            }
        ";
        let ctx = Eval::new();
        assert_eq!(ctx.eval(code.into()), Val(Int(3)));
    }

    #[test]
    fn fn_scope() {
        let ctx = Eval::new();
        let code = "
            let x = 1;
            let f = fn(){x = 2};
        ";
        ctx.eval(code.into());
        assert_eq!(ctx.eval("x".into()), Val(Int(1)));
        ctx.eval("f()".into());
        assert_eq!(ctx.eval("x".into()), Val(Int(2)));
    }

    #[test]
    fn closure() {
        let code = "
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);
        ";
        assert_eq!(Eval::new().eval(code.into()), Val(Int(4)));
    }

    #[test]
    fn functions_as_arguments() {
        let code = "
            let add = fn(a, b) { a + b };
            let sub = fn(a, b) { a - b };
            let applyFunc = fn(a, b, func) { func(a, b) };
        ";
        let ctx = Eval::new();
        ctx.eval(code.into());
        assert_eq!(ctx.eval("applyFunc(2, 2, add);".into()), Val(Int(4)));
        assert_eq!(ctx.eval("applyFunc(4, 2, sub);".into()), Val(Int(2)));
    }

    #[test]
    fn function_literals() {
        let code = "
            let applyFunc = fn(a, b, func) { func(a, b) };
        ";
        let ctx = Eval::new();
        ctx.eval(code.into());
        assert_eq!(
            ctx.eval("applyFunc(2, 2, fn(a, b) { a + b });".into()),
            Val(Int(4))
        );
    }

    #[test]
    fn fibonacci() {
        let code = "
            let fibonacci = fn(n) {
                if(n <= 1) {
                    1;
                } else {
                    n + fibonacci(n - 1);
                }
            }
        ";
        let ctx = Eval::new();
        ctx.eval(code.into());
        assert_eq!(ctx.eval("fibonacci(0)".into()), Val(Int(1)));
        assert_eq!(ctx.eval("fibonacci(1)".into()), Val(Int(1)));
        assert_eq!(ctx.eval("fibonacci(2)".into()), Val(Int(3)));
        assert_eq!(ctx.eval("fibonacci(3)".into()), Val(Int(6)));
    }

    #[test]
    fn fizzbuzz1() {
        let code = r#"
            let fizzbuzz = fn(n) {
                if(n % 3 == 0 && n % 5 == 0) {
                    return "fizzbuzz";
                } else {
                    if(n % 3 == 0) {
                        return "fizz";
                    } else {
                        if(n % 5 == 0) {
                            return "buzz";
                        }   
                    }
                }
                
            }
        "#;
        let ctx = Eval::new();
        ctx.eval(code.into());
        assert_eq!(
            ctx.eval("fizzbuzz(15)".into()),
            Return(String("fizzbuzz".into()))
        );
        assert_eq!(
            ctx.eval("fizzbuzz(3)".into()),
            Return(String("fizz".into()))
        );
        assert_eq!(
            ctx.eval("fizzbuzz(5)".into()),
            Return(String("buzz".into()))
        );
    }

    #[test]
    fn fizzbuzz2() {
        let code = r#"
            let fizzbuzz = fn(n) {
                if(n % 3 == 0 && n % 5 == 0) {
                    return "fizzbuzz";
                }
                if(n % 3 == 0) {
                    return "fizz";
                }
                if(n % 5 == 0) {
                    return "buzz";
                } 
            }
        "#;
        let ctx = Eval::new();
        ctx.eval(code.into());
        assert_eq!(
            ctx.eval("fizzbuzz(15)".into()),
            Return(String("fizzbuzz".into()))
        );
        assert_eq!(
            ctx.eval("fizzbuzz(3)".into()),
            Return(String("fizz".into()))
        );
        assert_eq!(
            ctx.eval("fizzbuzz(5)".into()),
            Return(String("buzz".into()))
        );
    }

    #[test]
    fn fizzbuzz3() {
        let code = r#"
            let fizzbuzz = fn(n) {
                let s = "";
                if(n % 3 == 0) {
                    s = s + "fizz";
                }
                if(n % 5 == 0) {
                    s = s + "buzz";
                }
                return s;
            }
        "#;
        let ctx = Eval::new();
        ctx.eval(code.into());
        assert_eq!(
            ctx.eval("fizzbuzz(15)".into()),
            Return(String("fizzbuzz".into()))
        );
        assert_eq!(
            ctx.eval("fizzbuzz(3)".into()),
            Return(String("fizz".into()))
        );
        assert_eq!(
            ctx.eval("fizzbuzz(5)".into()),
            Return(String("buzz".into()))
        );
    }

    #[test]
    fn fizzbuzz4() {
        let code = r#"
            let fizzbuzz = fn(n) {
                if(n % 3 == 0){"fizz"}{""} + if(n % 5 == 0){"buzz"}{""}
            }
        "#;
        let ctx = Eval::new();
        ctx.eval(code.into());
        assert_eq!(
            ctx.eval("fizzbuzz(15)".into()),
            Val(String("fizzbuzz".into()))
        );
        assert_eq!(ctx.eval("fizzbuzz(3)".into()), Val(String("fizz".into())));
        assert_eq!(ctx.eval("fizzbuzz(5)".into()), Val(String("buzz".into())));
    }

    #[test]
    fn builtin_len() {
        assert_eq!(Eval::new().eval(r#"len("asdfg")"#.into()), Return(Int(5)));
    }

    #[test]
    fn eval_array() {
        let code = r#"
            [1, 2, false]
        "#;
        assert_eq!(
            Eval::new().eval(code.into()),
            Val(Array(vec!(Int(1), Int(2), Bool(false))))
        );
    }
}
