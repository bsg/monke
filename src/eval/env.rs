use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use super::result::Value;

pub type EnvRef<'a> = Rc<RefCell<Env<'a>>>;

pub struct Env<'a> {
    store: RefCell<BTreeMap<&'a str, Value<'a>>>,
    outer: Option<EnvRef<'a>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Rc<RefCell<Env<'a>>> {
        Rc::new(RefCell::new(Env {
            store: RefCell::new(BTreeMap::new()),
            outer: None,
        }))
    }

    pub fn from(outer: EnvRef<'a>) -> EnvRef<'a> {
        let env = Self::new();
        env.borrow_mut().outer = Some(outer);
        env
    }

    pub fn bind_local(&self, name: &'a str, val: Value<'a>) {
        self.store.borrow_mut().insert(name, val);
    }

    pub fn bind(&self, name: &'a str, val: Value<'a>) -> bool {
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

    pub fn get(&self, name: &'a str) -> Option<Value<'a>> {
        if self.store.borrow().contains_key(name) {
            self.store.borrow_mut().get(name).cloned()
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
    use crate::eval::result::Value::*;

    use super::*;

    #[test]
    fn env() {
        let env = Env::new();
        env.borrow_mut().bind_local("x", Int(5));

        assert_eq!(env.borrow().get(&"x"), Some(Int(5)));
    }

    #[test]
    fn nested_envs() {
        let outer = Env::new();
        outer.borrow_mut().bind_local("x", Int(5));
        outer.borrow_mut().bind_local("y", Int(1));

        let inner = Env::from(outer);
        assert!(inner.borrow_mut().bind("y", Bool(true)));
        assert_eq!(inner.borrow().get(&"x"), Some(Int(5)));
        assert_eq!(inner.borrow().get(&"y"), Some(Bool(true)));
    }
}
