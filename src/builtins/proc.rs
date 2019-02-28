use std::cell::RefCell;
use std::ffi::OsString;
use std::io::{BufReader, BufRead, Read, Write};
#[cfg(unix)]
use std::os::unix::io::AsRawFd;
#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;
use std::process;

use ketos::{Bytes, Error};

use crate::error::ketos_err;

#[derive(Debug, ForeignValue, FromValue, FromValueRef, IntoValue)]
pub struct Proc(RefCell<process::Child>);

impl Proc {
    pub fn new(name: OsString, args: Vec<OsString>, stdin: process::Stdio, stdout: process::Stdio, stderr: process::Stdio) -> Result<Self, Error> {
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

    pub fn read_stdout(&self, limit: usize) -> Result<Bytes, Error> {
        let mut buf = Vec::with_capacity(limit);
        let mut child = self.0.borrow_mut();
        let stdout = child.stdout.as_mut().ok_or_else(|| ketos_err("proc stdout not piped"))?;
        let read = stdout.read(&mut buf).map_err(|err| ketos_err(format!("could not read from stdout: {}", err)))?;
        Ok(Bytes::new(buf[..read].to_vec()))
    }

    pub fn read_stdout_to_newline(&self) -> Result<String, Error> {
        let mut buf = String::new();
        let mut child = self.0.borrow_mut();
        let stdout = child.stdout.as_mut().ok_or_else(|| ketos_err("proc stdout not piped"))?;
        BufReader::new(stdout).read_line(&mut buf).map_err(|err| ketos_err(format!("could not read string from stdout: {}", err)))?;
        Ok(buf)
    }

    pub fn read_stdout_to_end(&self) -> Result<Bytes, Error> {
        let mut buf = Vec::new();
        let mut child = self.0.borrow_mut();
        let stdout = child.stdout.as_mut().ok_or_else(|| ketos_err("proc stdout not piped"))?;
        let read = stdout.read_to_end(&mut buf).map_err(|err| ketos_err(format!("could not read from stdout: {}", err)))?;
        Ok(Bytes::new(buf[..read].to_vec()))
    }

     pub fn read_stderr(&self, limit: usize) -> Result<Bytes, Error> {
        let mut buf = Vec::with_capacity(limit);
        let mut child = self.0.borrow_mut();
        let stderr = child.stderr.as_mut().ok_or_else(|| ketos_err("proc stderr not piped"))?;
        let read = stderr.read(&mut buf).map_err(|err| ketos_err(format!("could not read from stderr: {}", err)))?;
        Ok(Bytes::new(buf[..read].to_vec()))
    }

    pub fn read_stderr_to_newline(&self) -> Result<String, Error> {
        let mut buf = String::new();
        let mut child = self.0.borrow_mut();
        let stderr = child.stderr.as_mut().ok_or_else(|| ketos_err("proc stderr not piped"))?;
        BufReader::new(stderr).read_line(&mut buf).map_err(|err| ketos_err(format!("could not read string from stderr: {}", err)))?;
        Ok(buf)
    }

    pub fn read_stderr_to_end(&self) -> Result<Bytes, Error> {
        let mut buf = Vec::new();
        let mut child = self.0.borrow_mut();
        let stderr = child.stderr.as_mut().ok_or_else(|| ketos_err("proc stderr not piped"))?;
        let read = stderr.read_to_end(&mut buf).map_err(|err| ketos_err(format!("could not read from stderr: {}", err)))?;
        Ok(Bytes::new(buf[..read].to_vec()))
    }

    pub fn write(&self, bytes: &[u8]) -> Result<(), Error> {
        let mut child = self.0.borrow_mut();
        let stdin = child.stdin.as_mut().ok_or_else(|| ketos_err("proc stdin not piped"))?;
        stdin
            .write_all(bytes)
            .map_err(|err| ketos_err(format!("could not write for proc: {}", err)))
    }

    #[cfg(unix)]
    pub fn stdin_fd(&self) -> Result<i32, Error> {
        let child = self.0.borrow();
        let stdin = child.stdin.as_ref().ok_or_else(|| ketos_err("proc stdin not piped"))?;
        Ok(stdin.as_raw_fd())
    }

    #[cfg(unix)]
    pub fn stdout_fd(&self) -> Result<i32, Error> {
        let child = self.0.borrow();
        let stdout = child.stdout.as_ref().ok_or_else(|| ketos_err("proc stdout not piped"))?;
        Ok(stdout.as_raw_fd())
    }

    #[cfg(unix)]
    pub fn stderr_fd(&self) -> Result<i32, Error> {
        let child = self.0.borrow();
        let stderr = child.stderr.as_ref().ok_or_else(|| ketos_err("proc stderr not piped"))?;
        Ok(stderr.as_raw_fd())
    }

    pub fn take_stdout(&self) -> Result<process::ChildStdout, Error> {
        self.0.borrow_mut().stdout.take().ok_or_else(|| ketos_err("proc stdout unavailable"))
    }

    pub fn take_stdin(&self) -> Result<process::ChildStdin, Error> {
        self.0.borrow_mut().stdin.take().ok_or_else(|| ketos_err("proc stdin unavailable"))
    }
}

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
