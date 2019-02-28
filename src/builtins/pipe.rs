use std::cell::RefCell;
use std::io;
use std::thread;

use ketos::{Bytes, Error};

use super::Proc;
use crate::error::ketos_err;

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct Pipe {
    children: RefCell<Vec<Proc>>,
    threads: RefCell<Vec<thread::JoinHandle<Result<(), io::Error>>>>,
}

impl Pipe {
    pub fn new(children: Vec<Proc>) -> Result<Self, Error> {
        if children.len() < 2 {
            return Err(ketos_err("pipe must have at least two children"));
        }

        let mut threads = Vec::new();

        for i in 1..children.len() {
            let src = &children[i - 1];
            let dest = &children[i];
            let mut stdout = src.take_stdout()?;
            let mut stdin = dest.take_stdin()?;

            threads.push(thread::spawn(move || match io::copy(&mut stdout, &mut stdin) {
                Ok(_) => Ok(()),
                Err(ref err) if err.kind() == io::ErrorKind::BrokenPipe => Ok(()),
                Err(err) => Err(err),
            }));
        }

        let pipe = Self {
            children: RefCell::new(children),
            threads: RefCell::new(threads),
        };

        Ok(pipe)
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

    pub fn read_stdout(&self, limit: usize) -> Result<Bytes, Error> {
        let children = self.children.borrow_mut();
        children.last().unwrap().read_stdout(limit)
    }

    pub fn read_stdout_to_newline(&self) -> Result<String, Error> {
        let children = self.children.borrow_mut();
        children.last().unwrap().read_stdout_to_newline()
    }

    pub fn read_stdout_to_end(&self) -> Result<Bytes, Error> {
        let children = self.children.borrow_mut();
        children.last().unwrap().read_stdout_to_end()
    }

     pub fn read_stderr(&self, limit: usize) -> Result<Bytes, Error> {
        let children = self.children.borrow_mut();
        children.last().unwrap().read_stderr(limit)
    }

    pub fn read_stderr_to_newline(&self) -> Result<String, Error> {
        let children = self.children.borrow_mut();
        children.last().unwrap().read_stderr_to_newline()
    }

    pub fn read_stderr_to_end(&self) -> Result<Bytes, Error> {
        let children = self.children.borrow_mut();
        children.last().unwrap().read_stderr_to_end()
    }

    pub fn write(&self, bytes: &[u8]) -> Result<(), Error> {
        let children = self.children.borrow_mut();
        children.first().unwrap().write(bytes)
    }
}
