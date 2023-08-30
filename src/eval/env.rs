use std::collections::BTreeMap;

use super::result::Value;

pub struct Env<'a> {
    store: BTreeMap<&'a str, Value>,
    parent: Option<&'a mut Env<'a>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Env<'a> {
        Env {
            store: BTreeMap::new(),
            parent: None,
        }
    }

    pub fn from(parent: &'a mut Env<'a>) -> Env<'a> {
        let mut env = Self::new();
        env.parent = Some(parent);
        env
    }

    pub fn bind_local(&mut self, name: &'a str, val: Value) {
        self.store.insert(name, val);
    }

    pub fn bind(&mut self, name: &'a str, val: Value) -> bool {
        if self.store.contains_key(name) {
            self.store.insert(name, val);
            return true;
        } else {
            match &mut self.parent {
                Some(parent) => parent.bind(name, val),
                None => false,
            }
        }
    }

    pub fn get(&'a self, name: &'a str) -> Option<&'a Value> {
        if self.store.contains_key(name) {
            self.store.get(name)
        } else {
            match &self.parent {
                Some(parent) => parent.get(name),
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
        let mut env = Env::new();
        env.bind_local("x", Int(5));

        assert_eq!(env.get("x"), Some(&Int(5)));
    }

    #[test]
    fn nested_envs() {
        let mut outer = Env::new();
        outer.bind_local("x", Int(5));
        outer.bind_local("y", Int(1));

        let mut inner = Env::from(&mut outer);
        inner.bind("y", Bool(true));
        assert_eq!(inner.get("x"), Some(&Int(5)));
        assert_eq!(inner.get("y"), Some(&Bool(true)));
    }
}
