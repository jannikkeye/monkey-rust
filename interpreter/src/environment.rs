use std::cell::RefCell;
use std::rc::Rc;

use crate::object::Object;
use std::collections::HashMap;

#[derive(Default, Debug, PartialEq, Eq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }

    fn make_key(&self, name: &str) -> String {
        let mut key = String::new();

        key.push_str(name);

        key
    }

    pub fn set(&mut self, name: &str, value: Object) -> Option<Object> {
        let key = self.make_key(name);

        self.store.insert(key, value)
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let value = self.store.get(name);

        match value {
            Some(v) => Some(v.clone()),
            None => match self.outer {
                Some(ref env) => env.borrow().get(name),
                None => None
            }
        }
    }

    pub fn remove(&mut self, name: &str) {
        self.store.remove(name);
    }
}
