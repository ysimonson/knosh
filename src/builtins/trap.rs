use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::usize;

use ketos::function::Lambda;
use ketos::Error;

use crate::error::ketos_err;

#[derive(ForeignValue, FromValueRef)]
pub struct TrapMap {
    pub name: String,
    pub index: u8,
    next_key: AtomicUsize,
    traps: RefCell<HashMap<usize, Lambda>>,
}

impl TrapMap {
    pub fn new<S: Into<String>>(name: S, index: u8) -> Self {
        Self {
            name: name.into(),
            index,
            next_key: AtomicUsize::new(0),
            traps: RefCell::new(HashMap::new()),
        }
    }

    pub fn add(&self, callback: &Lambda) -> Result<usize, Error> {
        let key = self.next_key.fetch_add(1, Ordering::Relaxed);
        if key == usize::max_value() {
            return Err(ketos_err("out of trap keys"));
        }

        self.traps.borrow_mut().insert(key, callback.clone());
        Ok(key)
    }

    pub fn remove(&self, key: usize) -> Result<bool, Error> {
        Ok(self.traps.borrow_mut().remove(&key).is_some())
    }

    pub fn get(&self) -> Vec<Lambda> {
        // Note: interpreters consume lambdas when they're called. We need to
        // clone the lambda at some point, so might as well do it here so we
        // can avoid the borrow checker a bit more.
        self.traps.borrow().values().cloned().collect()
    }
}

impl fmt::Debug for TrapMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
