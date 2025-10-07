use super::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

pub fn new_environment() -> Box<Environment> {
    let store: HashMap<String, Object> = HashMap::new();
    Box::new(Environment { store, outer: None })
}

pub fn new_enclosed_environment(outer: Rc<RefCell<Environment>>) -> Box<Environment> {
    let mut env = new_environment();
    env.outer = Some(outer);
    env
}

impl Environment {
    pub fn get(&self, name: &String) -> Option<Object> {
        let store_result = self.store.get(name);
        match store_result {
            Some(v) => Some(v.clone()),
            None => {
                if let Some(store) = &self.outer {
                    store.borrow().get(name)
                } else {
                    None
                }
            }
        }
    }

    pub fn set(&mut self, name: String, val: &Object) -> Option<Object> {
        self.store.insert(name, val.clone())
    }
}
