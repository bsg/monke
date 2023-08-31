use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use super::result::Value;

pub struct Env<'a> {
    // TODO &str instead of String
    store: BTreeMap<String, Value>,
    outer: Option<Rc<RefCell<Env<'a>>>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Rc<RefCell<Env<'a>>> {
        Rc::new(RefCell::new(Env {
            store: BTreeMap::new(),
            outer: None,
        }))
    }

    pub fn from(outer: Rc<RefCell<Env<'a>>>) -> Rc<RefCell<Env<'a>>> {
        let env = Self::new();
        env.borrow_mut().outer = Some(outer);
        env
    }

    pub fn bind_local(&mut self, name: String, val: Value) {
        self.store.insert(name, val);
    }

    pub fn bind(&mut self, name: String, val: Value) -> bool {
        if self.store.contains_key(&name) {
            self.store.insert(name, val);
            return true;
        } else {
            match &mut self.outer {
                Some(outer) => outer.borrow_mut().bind(name, val),
                None => false,
            }
        }
    }

    pub fn get(&self, name: &String) -> Option<Value> {
        if self.store.contains_key(name) {
            self.store.get(name).copied()
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
        env.borrow_mut().bind_local("x".to_string(), Int(5));

        assert_eq!(env.borrow().get(&"x".to_string()), Some(Int(5)));
    }

    #[test]
    fn nested_envs() {
        let outer = Env::new();
        outer.borrow_mut().bind_local("x".to_string(), Int(5));
        outer.borrow_mut().bind_local("y".to_string(), Int(1));

        let inner = Env::from(outer);
        assert!(inner.borrow_mut().bind("y".to_string(), Bool(true)));
        assert_eq!(inner.borrow().get(&"x".to_string()), Some(Int(5)));
        assert_eq!(inner.borrow().get(&"y".to_string()), Some(Bool(true)));
    }
}
