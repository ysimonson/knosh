use std::process;
use std::ffi::OsString;
use std::cell::RefCell;
use std::io::Write;

#[cfg(unix)]
use std::os::unix::process::{CommandExt, ExitStatusExt};
#[cfg(unix)]
use std::os::unix::io::AsRawFd;

use ketos::Error;

#[cfg(unix)]
use nix::unistd::Pid;
#[cfg(unix)]
use nix::sys::wait::{waitpid, WaitStatus};

use crate::error::ketos_err;

#[derive(Clone, Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct ChildProcessPromise {
    name: OsString,
    args: Vec<OsString>
}

impl ChildProcessPromise {
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

    pub fn run_suppressed(&self) -> Result<(), Error> {
        let child = self.spawn(2, 2, 2)?;
        child.wait().map(|_| ())
    }

    pub fn spawn(&self, stdin: u8, stdout: u8, stderr: u8) -> Result<ChildProcess, Error> {
        let child = self.command()
            .stdin(to_stdio_value(stdin)?)
            .stdout(to_stdio_value(stdout)?)
            .stderr(to_stdio_value(stderr)?)
            .spawn()
            .map_err(|err| ketos_err(format!("{}", err)))?;
        Ok(ChildProcess::new(child))
    }

    #[cfg(unix)]
    pub fn exec(&self) -> Result<(), Error> {
        let err = self.command().exec();
        Err(ketos_err(format!("{}", err)))
    }
}

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct ChildProcess(RefCell<process::Child>);

impl ChildProcess {
    pub fn new(child: process::Child) -> Self {
        Self { 0: RefCell::new(child) }
    }

    pub fn wait(&self) -> Result<ChildExitStatus, Error> {
        match self.0.borrow_mut().wait() {
            Ok(status) => Ok(ChildExitStatus::new(status)),
            Err(err) => Err(ketos_err(format!("could not wait for child: {}", err)))
        }
    }

    pub fn poll(&self) -> Result<ChildExitStatus, Error> {
        match self.0.borrow_mut().try_wait() {
            Ok(Some(status)) => Ok(ChildExitStatus::new(status)),
            Ok(None) => Err(ketos_err("child not finished")),
            Err(err) => Err(ketos_err(format!("could not wait for child: {}", err)))
        }
    }

    pub fn pid(&self) -> u32 {
        self.0.borrow().id()
    }

    pub fn write(&self, bytes: &[u8]) -> Result<(), Error> {
        let mut child = self.0.borrow_mut();
        let stdin = child.stdin.as_mut().expect("failed to get child stdin");
        stdin.write_all(bytes).map_err(|err| ketos_err(format!("could not write to child: {}", err)))
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
pub struct ChildExitStatus(process::ExitStatus);

impl ChildExitStatus {
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

#[cfg(unix)]
#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct ChildInterpProcess(Pid);

impl ChildInterpProcess {
    pub fn new(pid: Pid) -> Self {
        Self { 0: pid }
    }

    pub fn wait(&self) -> Result<(), Error> {
        // Ideally we'd call `waitpid` once, but `nix` as of 0.13.0 doesn't
        // support `WIFSIGNALED`, so we have to repeatedly listen for all
        // signals
        loop {
            match waitpid(Some(self.0), None) {
                Ok(WaitStatus::Exited(_, code)) => if code == 0 {
                    return Ok(());
                } else {
                    return Err(ketos_err(format!("child interp return non-zero code: {}", code)));
                },
                Ok(WaitStatus::Signaled(_, _, _)) => return Ok(()),
                Ok(_) => (),
                Err(err) => return Err(ketos_err(format!("could not wait for child interp: {}", err))),
            }
        }
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
