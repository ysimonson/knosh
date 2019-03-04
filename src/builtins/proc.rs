use std::cell::RefCell;
use std::ffi::OsString;
use std::io::{BufRead, BufReader, Read, Write};
#[cfg(unix)]
use std::os::unix::io::AsRawFd;
#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;
use std::process;

use ketos::{Bytes, Error};

use crate::error::ketos_err;

macro_rules! stdio_impl {
    ($name:ident, $underlying:ty) => {
        #[derive(Debug, ForeignValue, FromValue, FromValueRef, IntoValue)]
        pub struct $name(RefCell<Option<$underlying>>);

        impl $name {
            fn new(underlying: $underlying) -> Self {
                Self {
                    0: RefCell::new(Some(underlying)),
                }
            }

            #[cfg(unix)]
            pub fn fd(&self) -> Result<i32, Error> {
                Ok(self.take()?.as_raw_fd())
            }

            pub fn take(&self) -> Result<$underlying, Error> {
                self.0
                    .borrow_mut()
                    .take()
                    .ok_or_else(|| ketos_err("stdio unavailable"))
            }
        }
    };
}

macro_rules! output_impl {
    ($name:ty) => {
        impl $name {
            pub fn read(&self, limit: usize) -> Result<Bytes, Error> {
                let mut buf = Vec::with_capacity(limit);
                let mut underlying = self.0.borrow_mut();
                let stdio = underlying
                    .as_mut()
                    .ok_or_else(|| ketos_err("stdio unavailable"))?;
                let read = stdio
                    .read(&mut buf)
                    .map_err(|err| ketos_err(format!("could not read from stdio: {}", err)))?;
                Ok(Bytes::new(buf[..read].to_vec()))
            }

            pub fn read_to_newline(&self) -> Result<String, Error> {
                let mut buf = String::new();
                let mut underlying = self.0.borrow_mut();
                let stdio = underlying
                    .as_mut()
                    .ok_or_else(|| ketos_err("stdio unavailable"))?;
                BufReader::new(stdio)
                    .read_line(&mut buf)
                    .map_err(|err| ketos_err(format!("could not read string from stdio: {}", err)))?;
                Ok(buf)
            }

            pub fn read_to_end(&self) -> Result<Bytes, Error> {
                let mut buf = Vec::new();
                let mut underlying = self.0.borrow_mut();
                let stdio = underlying
                    .as_mut()
                    .ok_or_else(|| ketos_err("stdio unavailable"))?;
                let read = stdio
                    .read_to_end(&mut buf)
                    .map_err(|err| ketos_err(format!("could not read from stdio: {}", err)))?;
                Ok(Bytes::new(buf[..read].to_vec()))
            }
        }
    };
}

#[derive(Debug, ForeignValue, FromValue, FromValueRef, IntoValue)]
pub struct Proc(RefCell<process::Child>);

impl Proc {
    pub fn new(
        name: OsString,
        args: Vec<OsString>,
        stdin: process::Stdio,
        stdout: process::Stdio,
        stderr: process::Stdio,
    ) -> Result<Self, Error> {
        let child = process::Command::new(name)
            .args(args)
            .stdin(stdin)
            .stdout(stdout)
            .stderr(stderr)
            .spawn()
            .map_err(|err| ketos_err(format!("{}", err)))?;
        Ok(Self { 0: RefCell::new(child) })
    }

    pub fn wait(&self) -> Result<ExitStatus, Error> {
        match self.0.borrow_mut().wait() {
            Ok(status) => Ok(ExitStatus::new(status)),
            Err(err) => Err(ketos_err(format!("could not wait for proc: {}", err))),
        }
    }

    pub fn poll(&self) -> Result<Option<ExitStatus>, Error> {
        match self.0.borrow_mut().try_wait() {
            Ok(Some(status)) => Ok(Some(ExitStatus::new(status))),
            Ok(None) => Ok(None),
            Err(err) => Err(ketos_err(format!("could not wait for proc: {}", err))),
        }
    }

    pub fn pid(&self) -> u32 {
        self.0.borrow().id()
    }

    pub fn stdin(&self) -> Result<Stdin, Error> {
        let stdin = self
            .0
            .borrow_mut()
            .stdin
            .take()
            .ok_or_else(|| ketos_err("stdio unavailable"))?;
        Ok(Stdin::new(stdin))
    }

    pub fn stdout(&self) -> Result<Stdout, Error> {
        let stdout = self
            .0
            .borrow_mut()
            .stdout
            .take()
            .ok_or_else(|| ketos_err("stdio unavailable"))?;
        Ok(Stdout::new(stdout))
    }

    pub fn stderr(&self) -> Result<Stderr, Error> {
        let stderr = self
            .0
            .borrow_mut()
            .stderr
            .take()
            .ok_or_else(|| ketos_err("stdio unavailable"))?;
        Ok(Stderr::new(stderr))
    }
}

stdio_impl!(Stdin, process::ChildStdin);
impl Stdin {
    pub fn write(&self, bytes: &[u8]) -> Result<(), Error> {
        let mut underlying = self.0.borrow_mut();
        let stdio = underlying.as_mut().ok_or_else(|| ketos_err("stdio unavailable"))?;
        stdio
            .write_all(bytes)
            .map_err(|err| ketos_err(format!("could not write to stdin: {}", err)))
    }
}

stdio_impl!(Stdout, process::ChildStdout);
output_impl!(Stdout);

stdio_impl!(Stderr, process::ChildStderr);
output_impl!(Stderr);

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct ExitStatus(process::ExitStatus);

impl ExitStatus {
    pub fn new(status: process::ExitStatus) -> Self {
        Self { 0: status }
    }

    pub fn success(&self) -> bool {
        self.0.success()
    }

    pub fn code(&self) -> Option<i32> {
        self.0.code()
    }

    #[cfg(unix)]
    pub fn signal(&self) -> Option<i32> {
        self.0.signal()
    }
}
