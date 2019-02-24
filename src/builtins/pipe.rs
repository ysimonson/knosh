use std::cell::RefCell;
use std::io;
use std::thread;

use ketos::Error;

use super::{Proc, ProcPromise};
use crate::error::ketos_err;

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct PipePromise(RefCell<Vec<ProcPromise>>);

impl PipePromise {
    pub fn new(children: Vec<&ProcPromise>) -> Self {
        Self {
            0: RefCell::new(children.into_iter().cloned().collect()),
        }
    }

    pub fn run(&self) -> Result<(), Error> {
        let pipe = self.spawn(0, 0, 0)?;

        if let Some(err) = pipe.wait().pop() {
            Err(err)
        } else {
            Ok(())
        }
    }

    pub fn spawn(&self, stdin: u8, stdout: u8, stderr: u8) -> Result<Pipe, Error> {
        let children = self.0.borrow_mut();
        debug_assert!(children.len() >= 2);
        let mut spawned_children = vec![children[0].spawn(stdin, 1, stderr)?];

        for child in children.iter().take(children.len() - 2).skip(1) {
            spawned_children.push(child.spawn(1, 1, stderr)?)
        }

        spawned_children.push(children[children.len() - 1].spawn(1, stdout, stderr)?);
        Ok(Pipe::new(spawned_children))
    }
}

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct Pipe {
    children: RefCell<Vec<Proc>>,
    threads: RefCell<Vec<thread::JoinHandle<Result<(), io::Error>>>>,
}

impl Pipe {
    pub fn new(children: Vec<Proc>) -> Self {
        let mut threads = Vec::new();

        for i in 1..children.len() {
            let src = &children[i - 1];
            let dest = &children[i];
            let mut stdout = src.take_stdout().expect("expected stdout");
            let mut stdin = dest.take_stdin().expect("expected stdin");

            threads.push(thread::spawn(move || match io::copy(&mut stdout, &mut stdin) {
                Ok(_) => Ok(()),
                Err(ref err) if err.kind() == io::ErrorKind::BrokenPipe => Ok(()),
                Err(err) => Err(err),
            }));
        }

        Self {
            children: RefCell::new(children),
            threads: RefCell::new(threads),
        }
    }

    pub fn children(&self) -> Vec<Proc> {
        let mut children = self.children.borrow_mut();
        children.drain(..).collect()
    }

    pub fn wait(&self) -> Vec<Error> {
        let mut threads = self.threads.borrow_mut();
        let mut errors = Vec::new();

        for t in threads.drain(..) {
            match t.join() {
                Ok(Err(err)) => {
                    let err = ketos_err(format!("pipe failed: {}", err));
                    errors.push(err);
                }
                Err(err) => {
                    let err = ketos_err(format!("pipe thread panic: {:?}", err));
                    errors.push(err);
                }
                Ok(_) => (),
            }
        }

        errors
    }
}
