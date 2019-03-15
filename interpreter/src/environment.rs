use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    fn make_key(&self, name: &str) -> String {
        let mut key = String::new();

        key.push_str(name);

        key
    }

    pub fn set(&mut self, name: &str, value: &Object) -> Option<Object> {
        let key = self.make_key(name);

        self.store.insert(key, value.clone())
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let value = self.store.get(name);

        if value.is_some() {
            return Some(value.unwrap().clone());
        }

        None
    }

    pub fn remove(&mut self, name: &str) {
        self.store.remove(name);
    }
}
