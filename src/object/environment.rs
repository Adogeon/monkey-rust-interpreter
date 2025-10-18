use super::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Env>,
}

pub type Env = Rc<RefCell<Environment>>;

impl Environment {
    pub fn new() -> Env {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn extend(parent: &Env) -> Env {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: Some(parent.clone()),
        }))
    }

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
