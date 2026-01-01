use crate::object::Object;
use std::hash::{DefaultHasher, Hash, Hasher};

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum HashKey {
    INTEGER(i64),
    STRING(String),
    BOOLEAN(bool),
}

impl HashKey {
    pub fn new(val: Object) -> Result<HashKey, String> {
        match HashKey::try_from(val) {
            Ok(v) => Ok(v),
            Err(_) => Err("Can't generate hash key for object".to_string()),
        }
    }

    fn calculate_hash(val: HashKey) -> u64 {
        let mut hasher = DefaultHasher::new();
        val.hash(&mut hasher);
        hasher.finish()
    }
}

impl TryFrom<Object> for HashKey {
    type Error = ();
    fn try_from(value: Object) -> Result<Self, Self::Error> {
        match value {
            Object::INTEGER(val) => Ok(HashKey::INTEGER(val)),
            Object::STRING(val) => Ok(HashKey::STRING(val)),
            Object::BOOLEAN(val) => Ok(HashKey::BOOLEAN(val)),
            _ => Err(()),
        }
    }
}

pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

#[test]
fn test_hash_key() -> Result<(), String> {
    let hello1 = Object::STRING("Hello World".to_string());
    let hello2 = Object::STRING("Hello World".to_string());

    let diff1 = Object::STRING("I am Johnny Silverhand".to_string());
    let diff2 = Object::STRING("I am Johnny Silverhand".to_string());
    let dif3 = Object::STRING("true".to_string());
    let dif4 = Object::STRING("300".to_string());

    let bool1 = Object::BOOLEAN(true);
    let bool2 = Object::BOOLEAN(false);
    let bool3 = Object::BOOLEAN(true & false);

    let int1 = Object::INTEGER(54 as i64);
    let int2 = Object::INTEGER(300 as i64);
    let int3 = Object::INTEGER(300 as i64);

    assert_eq!(HashKey::new(hello1.clone()), HashKey::new(hello2.clone()));
    assert_ne!(HashKey::new(hello1.clone()), HashKey::new(diff1.clone()));
    assert_eq!(HashKey::new(diff1), HashKey::new(diff2.clone()));
    assert_ne!(HashKey::new(diff2), HashKey::new(hello2));
    assert_ne!(HashKey::new(dif3), HashKey::new(bool1));
    assert_eq!(HashKey::new(bool2), HashKey::new(bool3));
    assert_ne!(HashKey::new(int1), HashKey::new(int2.clone()));
    assert_eq!(HashKey::new(int2), HashKey::new(int3.clone()));
    assert_ne!(HashKey::new(int3), HashKey::new(dif4));

    Ok(())
}
