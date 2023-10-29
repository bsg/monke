use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use super::result::Value;

pub type EnvRef = Rc<RefCell<Env>>;

#[derive(PartialEq, Clone)]
pub struct Env {
    store: RefCell<BTreeMap<Rc<str>, Value>>,
    outer: Option<EnvRef>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env {
            store: RefCell::new(BTreeMap::new()),
            outer: None,
        }))
    }

    pub fn from(outer: EnvRef) -> EnvRef {
        let env = Env {
            store: RefCell::new(BTreeMap::new()),
            outer: Some(outer),
        };
        Rc::new(RefCell::new(env))
    }

    pub fn bind_local(&self, name: Rc<str>, val: Value) {
        self.store.borrow_mut().insert(name, val);
    }

    pub fn bind(&self, name: Rc<str>, val: Value) -> bool {
        if let Some(v) = self.store.borrow_mut().get_mut(&name) {
            *v = val;
            true
        } else {
            match &self.outer {
                Some(outer) => outer.borrow().bind(name, val),
                None => false,
            }
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        if let v @ Some(_) = self.store.borrow_mut().get(name) {
            v.cloned()
        } else {
            match &self.outer {
                Some(outer) => outer.borrow().get(name),
                None => None,
            }
        }
    }
}

impl std::fmt::Debug for Env {
    // TODO pretty print
    // NOTE circular reference due to Value::Fn having a reference to the env at declaration site
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Env")
            .field("store", &self.store)
            .field("outer", &self.outer)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::result::Value;
    use Value::Bool;
    use Value::Int;

    use super::*;

    #[test]
    fn env() {
        let env = Env::new();
        env.borrow_mut().bind_local("x".into(), Int(5));

        assert_eq!(env.borrow().get("x".into()), Some(Int(5)));
    }

    #[test]
    fn nested_envs() {
        let outer = Env::new();
        outer.borrow_mut().bind_local("x".into(), Int(5));
        outer.borrow_mut().bind_local("y".into(), Int(1));

        let inner = Env::from(outer);
        assert!(inner.borrow_mut().bind("y".into(), Bool(true)));
        assert_eq!(inner.borrow().get("x".into()), Some(Int(5)));
        assert_eq!(inner.borrow().get("y".into()), Some(Bool(true)));
    }
}
