use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use super::result::Value;

pub type EnvRef = Rc<RefCell<Env>>;

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
        let env = Self::new();
        env.borrow_mut().outer = Some(outer);
        env
    }

    pub fn bind_local(&self, name: Rc<str>, val: Value) {
        self.store.borrow_mut().insert(name, val);
    }

    pub fn bind(&self, name: Rc<str>, val: Value) -> bool {
        if self.store.borrow().contains_key(&name) {
            self.store.borrow_mut().insert(name, val);
            true
        } else {
            match &self.outer {
                Some(outer) => outer.borrow().bind(name, val),
                None => false,
            }
        }
    }

    pub fn get(&self, name: Rc<str>) -> Option<Value> {
        if self.store.borrow().contains_key(&name) {
            self.store.borrow_mut().get(&name).cloned()
        } else {
            match &self.outer {
                Some(outer) => outer.borrow().get(name),
                None => None,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::eval::result::Value;
    use Value::Bool as Bool;
    use Value::Int as Int;

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
