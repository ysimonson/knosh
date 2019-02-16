use std::borrow::Borrow;
use std::rc::Rc;
use std::env;
use std::fmt;
use std::usize;
use std::process;
use std::ffi::{OsStr, OsString};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::io::Write;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;

#[cfg(unix)]
use std::os::unix::process::{CommandExt, ExitStatusExt};
#[cfg(unix)]
use std::os::unix::io::AsRawFd;

use ketos::{Error, Integer, Name, Value, Interpreter as KetosInterpreter};
use ketos::exec::ExecError;
use ketos::function::{Arity, Lambda};
use ketos::value::FromValueRef;
use linefeed::Signal;

#[cfg(unix)]
use nix::unistd::{fork, ForkResult, Pid as NixPid};
#[cfg(unix)]
use nix::sys::wait::{waitpid, WaitStatus};

use crate::error::ketos_err;
use crate::util;

pub struct Interpreter {
    pub interp: Rc<KetosInterpreter>,
    traps: [Rc<TrapMap>; 5],
    synced_child_processes: Rc<RefCell<Vec<ChildProcess>>>,
    synced_pipes: Rc<RefCell<Vec<thread::JoinHandle<Result<(), io::Error>>>>>,
    is_root: bool,
}

impl Interpreter {
    pub fn new(interp: KetosInterpreter, is_root: bool) -> Self {
        let traps = [
            Rc::new(TrapMap::new("signal-continue", 0)),
            Rc::new(TrapMap::new("signal-interrupt", 1)),
            Rc::new(TrapMap::new("signal-quit", 2)),
            Rc::new(TrapMap::new("signal-resize", 3)),
            Rc::new(TrapMap::new("signal-suspend", 4)),
        ];

        Self {
            interp: Rc::new(interp),
            traps,
            synced_child_processes: Rc::new(RefCell::new(Vec::new())),
            synced_pipes: Rc::new(RefCell::new(Vec::new())),
            is_root,
        }
    }

    pub fn add_builtins(&self) {
        let interp_scope = self.interp.clone();
        let scope = interp_scope.scope();
        let interp = self.interp.clone();
        let synced_child_processes = self.synced_child_processes.clone();
        let synced_pipes = self.synced_pipes.clone();

        for trap in self.traps.iter() {
            scope.add_named_value(&trap.name, Value::Foreign(trap.clone()));
        }

        scope.add_named_value("io-inherit", Value::Integer(Integer::from_u8(0)));
        scope.add_named_value("io-piped", Value::Integer(Integer::from_u8(1)));
        scope.add_named_value("io-null", Value::Integer(Integer::from_u8(2)));

        ketos_closure!(scope, "trap", |traps: &TrapMap, callback: &Lambda| -> usize {
            traps.add(callback)
        });

        ketos_closure!(scope, "untrap", |traps: &TrapMap, key: usize| -> bool {
            traps.remove(key)
        });

        ketos_closure!(scope, "pid", || -> u32 {
            Ok(process::id())
        });
        
        ketos_closure!(scope, "set-env", |key: &str, value: &str| -> () {
            env::set_var(key, value);
            Ok(())
        });

        ketos_closure!(scope, "env", |key: &str| -> OsString {
            Ok(env::var_os(key).unwrap_or_else(OsString::default))
        });

        ketos_closure!(scope, "del-env", |key: &str| -> () {
            env::remove_var(key);
            Ok(())
        });

        ketos_closure!(scope, "exit", |code: i32| -> () {
            process::exit(code);
        });

        ketos_closure!(scope, "cd", |dir: &str| -> OsString {
            let expanded = util::expand_path(dir)?;
            env::set_current_dir(expanded.clone()).map_err(|err| ketos_err(format!("{}: {}", dir, err)))?;
            Ok(expanded.into_os_string())
        });

        ketos_closure!(scope, "pwd", || -> OsString {
            let path = env::current_dir().map_err(|err| ketos_err(format!("{}", err)))?;
            Ok(path.into_os_string())
        });

        #[cfg(unix)]
        scope.add_value_with_name("fork", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Exact(1), args.len(), name)?;

            let mut iter = (&*args).iter();
            
            let callback = match iter.next() {
                Some(Value::Lambda(lambda)) => lambda,
                Some(value) => {
                    let err = ExecError::TypeError {
                        expected: "function",
                        found: value.type_name(),
                        value: None,
                    };
                    return Err(err.into());
                },
                None => unreachable!()
            };

            match fork() {
                Ok(ForkResult::Parent { child, .. }) => {
                    Ok(Pid::new(child).into())
                },
                Ok(ForkResult::Child) => {
                    let lambda = Value::Lambda(callback.clone());
                    match interp.call_value(lambda, vec![]) {
                        Ok(_) => process::exit(0),
                        Err(_) => process::exit(1),
                    }
                },
                Err(err) => {
                    Err(ketos_err(format!("could not fork: {}", err)))
                }
            }
        }));

        ketos_closure!(scope, "proc", |name: &OsStr, args: &[Value]| -> Command {
            let args_str: Result<Vec<OsString>, Error> = args.iter()
                .map(|value| {
                    match value {
                        Value::Bool(v) => Ok(format!("{}", v).into()),
                        Value::Float(v) => Ok(format!("{}", v).into()),
                        Value::Integer(v) => Ok(format!("{}", v).into()),
                        Value::Ratio(v) => Ok(format!("{}", v).into()),
                        Value::Char(v) => Ok(format!("{}", v).into()),
                        Value::String(v) => Ok(format!("{}", v).into()),
                        Value::Bytes(v) if cfg!(unix) => {
                            use std::os::unix::ffi::OsStringExt;
                            let bytes = v.clone().into_bytes();
                            Ok(OsString::from_vec(bytes))
                        },
                        Value::Path(v) => Ok(v.clone().into_os_string()),
                        _ => Err(ketos_err(format!("cannot use non-stringifiable value as an argument: `{:?}`", value)))
                    }
                })
                .collect();

            Ok(Command::new(name, args_str?).into())
        });

        scope.add_value_with_name("spawn", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Range(1, 4), args.len(), name)?;

            let mut iter = (&*args).iter();
            let p = <&Command>::from_value_ref(iter.next().unwrap())?;
            let (stdin, stdout, stderr) = match iter.collect::<Vec<&Value>>().as_slice() {
                [] => (process::Stdio::inherit(), process::Stdio::inherit(), process::Stdio::inherit()),
                [Value::Integer(stdout)] => (
                    process::Stdio::inherit(),
                    util::integer_to_stdio(stdout)?,
                    process::Stdio::inherit()
                ),
                [Value::Integer(stdout), Value::Integer(stderr)] => (
                    process::Stdio::inherit(),
                    util::integer_to_stdio(stdout)?,
                    util::integer_to_stdio(stderr)?
                ),
                [Value::Integer(stdin), Value::Integer(stdout), Value::Integer(stderr)] => (
                    util::integer_to_stdio(stdin)?,
                    util::integer_to_stdio(stdout)?,
                    util::integer_to_stdio(stderr)?
                ),
                _ => return Err(ketos_err("expected 0-3 stdio integers"))
            };

            Ok(p.spawn(stdin, stdout, stderr)?.into())
        }));

        #[cfg(unix)]
        ketos_closure!(scope, "exec", |cmd: &Command| -> () {
            cmd.exec()
        });

        #[cfg(unix)]
        ketos_closure!(scope, "wait", |pid: &Pid| -> () {
            pid.wait()
        });

        ketos_closure!(scope, "child-wait", |child: &ChildProcess| -> ChildExitStatus {
            child.wait()
        });

        ketos_closure!(scope, "child-poll", |child: &ChildProcess| -> ChildExitStatus {
            child.poll()
        });

        ketos_closure!(scope, "child-pid", |child: &ChildProcess| -> u32 {
            Ok(child.pid())
        });

        ketos_closure!(scope, "child-write", |child: &ChildProcess, bytes: &[u8]| -> () {
            child.write(bytes)
        });

        #[cfg(unix)]
        ketos_closure!(scope, "child-stdin-fd", |child: &ChildProcess| -> i32 {
            Ok(child.stdin_fd())
        });

        #[cfg(unix)]
        ketos_closure!(scope, "child-stdout-fd", |child: &ChildProcess| -> i32 {
            Ok(child.stdout_fd())
        });

        #[cfg(unix)]
        ketos_closure!(scope, "child-stderr-fd", |child: &ChildProcess| -> i32 {
            Ok(child.stderr_fd())
        });

        ketos_closure!(scope, "child-exit-success", |status: &ChildExitStatus| -> bool {
            Ok(status.success())
        });

        ketos_closure!(scope, "child-exit-code", |status: &ChildExitStatus| -> i32 {
            status.code()
        });

        #[cfg(unix)]
        ketos_closure!(scope, "child-exit-signal", |status: &ChildExitStatus| -> i32 {
            status.signal()
        });

        scope.add_value_with_name("|", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Min(2), args.len(), name)?;

            let procs: Result<Vec<&ChildProcess>, ExecError> = args.into_iter()
                .map(|arg| {
                    <&ChildProcess>::from_value_ref(arg)
                })
                .collect();

            let procs = procs?;
            let mut synced_pipes = synced_pipes.borrow_mut();

            for i in 1..procs.len() {
                let src = &procs[i - 1];
                let dest = &procs[i];
                let mut stdout = src.0.borrow_mut().stdout.take().expect("expected stdout");
                let mut stdin = dest.0.borrow_mut().stdin.take().expect("expected stdin");

                synced_pipes.push(thread::spawn(move || {
                    match io::copy(&mut stdout, &mut stdin) {
                        Ok(_) => Ok(()),
                        Err(ref err) if err.kind() == io::ErrorKind::BrokenPipe => Ok(()),
                        Err(err) => Err(err)
                    }
                }));
            }

            Ok(().into())
        }));
    }

    pub fn wait_synced(&self) -> Vec<Error> {
        let mut errors = Vec::new();

        {
            let synced_child_processes = self.synced_child_processes.clone();
            let mut synced_child_processes = synced_child_processes.borrow_mut();

            for child_process in synced_child_processes.drain(..) {
                match child_process.wait() {
                    Ok(ref status) if !status.success() => {
                        let err = ketos_err(format!("non-successful status code: {:?}", status));
                        errors.push(err);
                    }
                    Err(err) => {
                        errors.push(err);
                    }
                    Ok(_) => ()
                }
            }
        }

        {
            let synced_pipes = self.synced_pipes.clone();
            let mut synced_pipes = synced_pipes.borrow_mut();

            for synced_pipe in synced_pipes.drain(..) {
                match synced_pipe.join() {
                    Ok(Err(err)) => {
                        let err = ketos_err(format!("pipe failed: {}", err));
                        errors.push(err);
                    },
                    Err(err) => {
                        let err = ketos_err(format!("pipe thread panic: {:?}", err));
                        errors.push(err);
                    },
                    Ok(_) => ()
                }
            }
        }

        errors
    }

    pub fn trigger_signal(&self, sig: Signal) -> Vec<Result<Value, Error>> {
        if !self.is_root {
            panic!("Signals can only be triggered on the root interpreter");
        }

        let sig_int: u8 = match sig {
            Signal::Continue => 0,
            Signal::Interrupt => 1,
            Signal::Quit => 2,
            Signal::Resize => 3,
            Signal::Suspend => 4,
            _ => unimplemented!()
        };

        let args = vec![Value::Integer(Integer::from_u8(sig_int))];
        let interp = self.interp.clone();
        let interp = (*interp).borrow();

        self.traps.iter()
            .filter(|t| t.index == sig_int)
            .take(1)
            .flat_map(|t| t.get())
            .map(|t| interp.call_value(Value::Lambda(t), args.clone()))
            .collect()
    }
}

#[derive(ForeignValue, FromValueRef)]
pub struct TrapMap {
    name: String,
    index: u8,
    next_key: AtomicUsize,
    traps: RefCell<HashMap<usize, Lambda>>
}

impl TrapMap {
    fn new<S: Into<String>>(name: S, index: u8) -> Self {
        Self {
            name: name.into(),
            index,
            next_key: AtomicUsize::new(0),
            traps: RefCell::new(HashMap::new()),
        }
    }

    fn add(&self, callback: &Lambda) -> Result<usize, Error> {
        let key = self.next_key.fetch_add(1, Ordering::Relaxed);
        if key == usize::max_value() {
            return Err(ketos_err("out of trap keys"));
        }

        self.traps.borrow_mut().insert(key, callback.clone());
        Ok(key)
    }

    fn remove(&self, key: usize) -> Result<bool, Error> {
        Ok(self.traps.borrow_mut().remove(&key).is_some())
    }

    fn get(&self) -> Vec<Lambda> {
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

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct Command(RefCell<process::Command>);

impl Command {
    fn new<I, S>(name: &OsStr, args: I) -> Self
    where I: IntoIterator<Item=S>,
          S: AsRef<OsStr>
    {
        let mut cmd = process::Command::new(name);
        cmd.args(args.into_iter());
        Self { 0: RefCell::new(cmd) }
    }

    pub fn run(&self) -> Result<(), Error> {
        let child = self.spawn(process::Stdio::inherit(), process::Stdio::inherit(), process::Stdio::inherit())?;
        child.wait().map(|_| ())
    }

    fn spawn(&self, stdin: process::Stdio, stdout: process::Stdio, stderr: process::Stdio) -> Result<ChildProcess, Error> {
        let mut cmd = self.0.borrow_mut();
        let child = cmd
            .stdin(stdin)
            .stdout(stdout)
            .stderr(stderr)
            .spawn()
            .map_err(|err| ketos_err(format!("{}", err)))?;
        Ok(ChildProcess::new(child))
    }

    #[cfg(unix)]
    fn exec(&self) -> Result<(), Error> {
        let mut cmd = self.0.borrow_mut();
        let err = cmd.exec();
        Err(ketos_err(format!("{}", err)))
    }
}

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct ChildProcess(RefCell<process::Child>);

impl ChildProcess {
    fn new(child: process::Child) -> Self {
        Self { 0: RefCell::new(child) }
    }

    fn wait(&self) -> Result<ChildExitStatus, Error> {
        match self.0.borrow_mut().wait() {
            Ok(status) => Ok(ChildExitStatus::new(status)),
            Err(err) => Err(ketos_err(format!("could not wait for child: {}", err)))
        }
    }

    fn poll(&self) -> Result<ChildExitStatus, Error> {
        match self.0.borrow_mut().try_wait() {
            Ok(Some(status)) => Ok(ChildExitStatus::new(status)),
            Ok(None) => Err(ketos_err("child not finished")),
            Err(err) => Err(ketos_err(format!("could not wait for child: {}", err)))
        }
    }

    fn pid(&self) -> u32 {
        self.0.borrow().id()
    }

    fn write(&self, bytes: &[u8]) -> Result<(), Error> {
        let mut child = self.0.borrow_mut();
        let stdin = child.stdin.as_mut().expect("failed to get child stdin");
        stdin.write_all(bytes).map_err(|err| ketos_err(format!("could not write to child: {}", err)))
    }

    #[cfg(unix)]
    fn stdin_fd(&self) -> i32 {
        let child = self.0.borrow();
        child.stdin.as_ref().expect("failed to get child stdin").as_raw_fd()
    }

    #[cfg(unix)]
    fn stdout_fd(&self) -> i32 {
        let child = self.0.borrow();
        child.stdout.as_ref().expect("failed to get child stdout").as_raw_fd()
    }

    #[cfg(unix)]
    fn stderr_fd(&self) -> i32 {
        let child = self.0.borrow();
        child.stderr.as_ref().expect("failed to get child stderr").as_raw_fd()
    }
}

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct ChildExitStatus(pub process::ExitStatus);

impl ChildExitStatus {
    fn new(status: process::ExitStatus) -> Self {
        Self { 0: status }
    }

    fn success(&self) -> bool {
        self.0.success()
    }

    fn code(&self) -> Result<i32, Error> {
        self.0.code().ok_or_else(|| ketos_err("no exit code"))
    }

    #[cfg(unix)]
    fn signal(&self) -> Result<i32, Error> {
        self.0.signal().ok_or_else(|| ketos_err("no exit signal"))
    }
}

#[cfg(unix)]
#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct Pid(pub NixPid);

impl Pid {
    fn new(pid: NixPid) -> Self {
        Self { 0: pid }
    }

    fn wait(&self) -> Result<(), Error> {
        // Ideally we'd call `waitpid` once, but `nix` as of 0.13.0 doesn't
        // support `WIFSIGNALED`
        loop {
            match waitpid(Some(self.0), None) {
                Ok(WaitStatus::Exited(_, code)) if code == 0 => return Ok(()),
                Ok(WaitStatus::Signaled(_, _, _)) => return Ok(()),
                Ok(_) => (),
                Err(err) => return Err(ketos_err(format!("could not wait for child interpreter: {}", err))),
            }
        }
    }
}

fn check_arity(arity: Arity, len: usize, name: Name) -> Result<(), Error> {
    if arity.accepts(len as u32) {
        Ok(())
    } else {
        let err = ExecError::ArityError {
            name: Some(name),
            expected: arity,
            found: len as u32,
        };
        Err(err.into())
    }
}
