use super::Object;
use std::collections::HashMap;

pub struct Environment {
    store: HashMap<String, Object>,
}

pub fn new_environment() -> Box<Environment> {
    let store: HashMap<String, Object> = HashMap::new();
    Box::new(Environment { store })
}

impl Environment {
    pub fn get(&self, name: &String) -> Option<Object> {
        self.store.get(name).map(|v| v.clone())
    }

    pub fn set(&mut self, name: String, val: &Object) -> Option<Object> {
        self.store.insert(name, val.clone())
    }
}
