use ketos::Error;
#[cfg(unix)]
use nix::sys::wait::{waitpid, WaitStatus};
#[cfg(unix)]
use nix::unistd::Pid;

use crate::error::ketos_err;

#[cfg(unix)]
#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct SubInterp(Pid);

impl SubInterp {
    pub fn new(pid: Pid) -> Self {
        Self { 0: pid }
    }

    pub fn wait(&self) -> Result<(), Error> {
        // Ideally we'd call `waitpid` once, but `nix` as of 0.13.0 doesn't
        // support `WIFSIGNALED`, so we have to repeatedly listen for all
        // signals
        loop {
            match waitpid(Some(self.0), None) {
                Ok(WaitStatus::Exited(_, code)) => {
                    if code == 0 {
                        return Ok(());
                    } else {
                        return Err(ketos_err(format!("child interp return non-zero code: {}", code)));
                    }
                }
                Ok(WaitStatus::Signaled(_, _, _)) => return Ok(()),
                Ok(_) => (),
                Err(err) => return Err(ketos_err(format!("could not wait for child interp: {}", err))),
            }
        }
    }
}
