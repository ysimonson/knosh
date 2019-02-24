use std::cell::RefCell;
use std::ffi::OsString;
use std::io::Write;
#[cfg(unix)]
use std::os::unix::io::AsRawFd;
#[cfg(unix)]
use std::os::unix::process::{CommandExt, ExitStatusExt};
use std::process;

use ketos::Error;

use crate::error::ketos_err;

#[derive(Clone, Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct ProcPromise {
    name: OsString,
    args: Vec<OsString>,
}

impl ProcPromise {
    pub fn new(name: OsString, args: Vec<OsString>) -> Self {
        Self { name, args }
    }

    fn command(&self) -> process::Command {
        let mut cmd = process::Command::new(&self.name);
        cmd.args(self.args.iter());
        cmd
    }

    pub fn run(&self) -> Result<(), Error> {
        let child = self.spawn(0, 0, 0)?;
        child.wait().map(|_| ())
    }

    pub fn spawn(&self, stdin: u8, stdout: u8, stderr: u8) -> Result<Proc, Error> {
        let child = self
            .command()
            .stdin(to_stdio_value(stdin)?)
            .stdout(to_stdio_value(stdout)?)
            .stderr(to_stdio_value(stderr)?)
            .spawn()
            .map_err(|err| ketos_err(format!("{}", err)))?;
        Ok(Proc::new(child))
    }

    #[cfg(unix)]
    pub fn exec(&self) -> Result<(), Error> {
        let err = self.command().exec();
        Err(ketos_err(format!("{}", err)))
    }
}

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct Proc(RefCell<process::Child>);

impl Proc {
    pub fn new(child: process::Child) -> Self {
        Self { 0: RefCell::new(child) }
    }

    pub fn wait(&self) -> Result<ExitStatus, Error> {
        match self.0.borrow_mut().wait() {
            Ok(status) => Ok(ExitStatus::new(status)),
            Err(err) => Err(ketos_err(format!("could not wait for child: {}", err))),
        }
    }

    pub fn poll(&self) -> Result<ExitStatus, Error> {
        match self.0.borrow_mut().try_wait() {
            Ok(Some(status)) => Ok(ExitStatus::new(status)),
            Ok(None) => Err(ketos_err("child not finished")),
            Err(err) => Err(ketos_err(format!("could not wait for child: {}", err))),
        }
    }

    pub fn pid(&self) -> u32 {
        self.0.borrow().id()
    }

    pub fn write(&self, bytes: &[u8]) -> Result<(), Error> {
        let mut child = self.0.borrow_mut();
        let stdin = child.stdin.as_mut().expect("failed to get child stdin");
        stdin
            .write_all(bytes)
            .map_err(|err| ketos_err(format!("could not write to child: {}", err)))
    }

    #[cfg(unix)]
    pub fn stdin_fd(&self) -> i32 {
        let child = self.0.borrow();
        child.stdin.as_ref().expect("failed to get child stdin").as_raw_fd()
    }

    #[cfg(unix)]
    pub fn stdout_fd(&self) -> i32 {
        let child = self.0.borrow();
        child.stdout.as_ref().expect("failed to get child stdout").as_raw_fd()
    }

    #[cfg(unix)]
    pub fn stderr_fd(&self) -> i32 {
        let child = self.0.borrow();
        child.stderr.as_ref().expect("failed to get child stderr").as_raw_fd()
    }

    pub fn take_stdout(&self) -> Option<process::ChildStdout> {
        self.0.borrow_mut().stdout.take()
    }

    pub fn take_stdin(&self) -> Option<process::ChildStdin> {
        self.0.borrow_mut().stdin.take()
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

    pub fn code(&self) -> Result<i32, Error> {
        self.0.code().ok_or_else(|| ketos_err("no exit code"))
    }

    #[cfg(unix)]
    pub fn signal(&self) -> Result<i32, Error> {
        self.0.signal().ok_or_else(|| ketos_err("no exit signal"))
    }
}

fn to_stdio_value(i: u8) -> Result<process::Stdio, Error> {
    match i {
        0 => Ok(process::Stdio::inherit()),
        1 => Ok(process::Stdio::piped()),
        2 => Ok(process::Stdio::null()),
        _ => Err(ketos_err(format!("invalid stdio value: `{}`", i))),
    }
}
