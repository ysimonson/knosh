use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::usize;

use ketos::function::Lambda;
use ketos::Error;

use crate::error::ketos_err;

pub struct TrapMap {
    next_key: AtomicUsize,
    traps: RefCell<HashMap<usize, Lambda>>,
}

impl Default for TrapMap {
    fn default() -> Self {
        Self {
            next_key: AtomicUsize::new(0),
            traps: RefCell::new(HashMap::new()),
        }
    }
}

impl TrapMap {
    pub fn add(&self, callback: &Lambda) -> Result<usize, Error> {
        let key = self.next_key.fetch_add(1, Ordering::Relaxed);
        if key == usize::max_value() {
            return Err(ketos_err("out of trap keys"));
        }

        self.traps.borrow_mut().insert(key, callback.clone());
        Ok(key)
    }

    pub fn remove(&self, key: usize) -> bool {
        self.traps.borrow_mut().remove(&key).is_some()
    }

    pub fn get(&self) -> Vec<Lambda> {
        // Note: interpreters consume lambdas when they're called. We need to
        // clone the lambda at some point, so might as well do it here so we
        // can avoid the borrow checker a bit more.
        self.traps.borrow().values().cloned().collect()
    }
}
