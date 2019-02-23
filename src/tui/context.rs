use std::cell::RefCell;

use ketos::Context;

thread_local! {
    // linefeed requires a Completer to impl Send + Sync.
    // Because a Context object contains Rc, it does not impl these traits.
    // Therefore, we must store the Context object in thread-local storage.
    // (We only use the linefeed Interface from a single thread, anyway.)
    static CONTEXT: RefCell<Option<Context>> = RefCell::new(None);
}

pub fn set_thread_context(context: Context) {
    CONTEXT.with(|key| {
        *key.borrow_mut() = Some(context);
    });
}

pub fn thread_context() -> Context {
    CONTEXT.with(|key| {
        key.borrow()
            .clone()
            .unwrap_or_else(|| panic!("no thread-local Context object set"))
    })
}
