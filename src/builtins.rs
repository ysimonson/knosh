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
use nix::unistd::{fork, ForkResult, Pid};
#[cfg(unix)]
use nix::sys::wait::{waitpid, WaitStatus};

use crate::error::ketos_err;
use crate::util;

pub struct Interpreter {
    pub interp: Rc<KetosInterpreter>,
    traps: [Rc<TrapMap>; 5],
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
            is_root,
        }
    }

    // TODO: function to get children from a pipe
    pub fn add_builtins(&self) {
        let interp_scope = self.interp.clone();
        let scope = interp_scope.scope();
        let interp = self.interp.clone();

        for trap in self.traps.iter() {
            scope.add_named_value(&trap.name, Value::Foreign(trap.clone()));
        }

        scope.add_named_value("stdio/inherit", Value::Integer(Integer::from_u8(0)));
        scope.add_named_value("stdio/piped", Value::Integer(Integer::from_u8(1)));
        scope.add_named_value("stdio/null", Value::Integer(Integer::from_u8(2)));

        ketos_closure!(scope, "trap", |traps: &TrapMap, callback: &Lambda| -> usize {
            traps.add(callback)
        });

        ketos_closure!(scope, "untrap", |traps: &TrapMap, key: usize| -> bool {
            traps.remove(key)
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
                    Ok(ChildInterpProcess::new(child).into())
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

        ketos_closure!(scope, "proc", |name: &OsStr, args: &[Value]| -> ChildProcessPromise {
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

            Ok(ChildProcessPromise::new(name.to_os_string(), args_str?).into())
        });

        scope.add_value_with_name("spawn", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Range(1, 4), args.len(), name)?;

            let mut iter = (&*args).iter();
            let p = <&ChildProcessPromise>::from_value_ref(iter.next().unwrap())?;
            let (stdin, stdout, stderr) = match iter.collect::<Vec<&Value>>().as_slice() {
                [] => (0, 0, 0),
                [Value::Integer(stdout)] => (0, to_stdio_u8(stdout)?, 0),
                [Value::Integer(stdout), Value::Integer(stderr)] => (0, to_stdio_u8(stdout)?, to_stdio_u8(stderr)?),
                [Value::Integer(stdin), Value::Integer(stdout), Value::Integer(stderr)] => (to_stdio_u8(stdin)?, to_stdio_u8(stdout)?, to_stdio_u8(stderr)?),
                _ => return Err(ketos_err("expected 0-3 stdio integers"))
            };

            Ok(p.spawn(stdin, stdout, stderr)?.into())
        }));

        #[cfg(unix)]
        ketos_closure!(scope, "exec", |cmd: &ChildProcessPromise| -> () {
            cmd.exec()
        });

        scope.add_value_with_name("wait", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Exact(1), args.len(), name)?;

            let mut iter = (&*args).iter();
            let value = iter.next().unwrap();

            if let Ok(p) = <&ChildProcess>::from_value_ref(value) {
                p.wait()?;
                Ok(().into())
            } else if let Ok(p) = <&Pipe>::from_value_ref(value) {
                let mut errors = p.wait();

                if let Some(err) = errors.pop() {
                    Err(err.into())
                } else {
                    Ok(().into())
                }
            } else if let Ok(p) = <&ChildInterpProcess>::from_value_ref(value) {
                p.wait()?;
                Ok(().into())
            } else {
                Err(ketos_err("expected child process or child interp process"))
            }
        }));

        ketos_closure!(scope, "poll", |child: &ChildProcess| -> ChildExitStatus {
            child.poll()
        });

        scope.add_value_with_name("pid", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Range(0, 1), args.len(), name)?;

            let mut iter = (&*args).iter();

            if let Some(value) = iter.next() {
                let child = <&ChildProcess>::from_value_ref(value)?;
                Ok(child.pid().into())
            } else {
                Ok(process::id().into())
            }
        }));

        ketos_closure!(scope, "write", |child: &ChildProcess, bytes: &[u8]| -> () {
            child.write(bytes)
        });

        #[cfg(unix)]
        scope.add_value_with_name("stdin/fd", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Range(0, 1), args.len(), name)?;

            let mut iter = (&*args).iter();

            if let Some(value) = iter.next() {
                let child = <&ChildProcess>::from_value_ref(value)?;
                Ok(child.stdin_fd().into())
            } else {
                Ok(io::stdin().as_raw_fd().into())
            }
        }));

        #[cfg(unix)]
        scope.add_value_with_name("stdout/fd", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Range(0, 1), args.len(), name)?;

            let mut iter = (&*args).iter();

            if let Some(value) = iter.next() {
                let child = <&ChildProcess>::from_value_ref(value)?;
                Ok(child.stdout_fd().into())
            } else {
                Ok(io::stdout().as_raw_fd().into())
            }
        }));

        #[cfg(unix)]
        scope.add_value_with_name("stderr/fd", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Range(0, 1), args.len(), name)?;

            let mut iter = (&*args).iter();

            if let Some(value) = iter.next() {
                let child = <&ChildProcess>::from_value_ref(value)?;
                Ok(child.stderr_fd().into())
            } else {
                Ok(io::stderr().as_raw_fd().into())
            }
        }));

        ketos_closure!(scope, "exit/success?", |status: &ChildExitStatus| -> bool {
            Ok(status.success())
        });

        ketos_closure!(scope, "exit/code", |status: &ChildExitStatus| -> i32 {
            status.code()
        });

        #[cfg(unix)]
        ketos_closure!(scope, "exit/signal", |status: &ChildExitStatus| -> i32 {
            status.signal()
        });

        scope.add_value_with_name("|", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Min(2), args.len(), name)?;

            let ps: Result<Vec<&ChildProcessPromise>, ExecError> = args.into_iter()
                .map(|arg| {
                    <&ChildProcessPromise>::from_value_ref(arg)
                })
                .collect();

            Ok(PipePromise::new(ps?).into())
        }));
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
pub struct PipePromise(RefCell<Vec<ChildProcessPromise>>);

impl PipePromise {
    fn new(children: Vec<&ChildProcessPromise>) -> Self {
        Self { 0: RefCell::new(children.into_iter().cloned().collect()) }
    }

    pub fn run(&self) -> Result<(), Error> {
        let pipe = self.spawn(0, 0, 0)?;
        
        if let Some(err) = pipe.wait().pop() {
            Err(err)
        } else {
            Ok(())
        }
    }

    fn spawn(&self, stdin: u8, stdout: u8, stderr: u8) -> Result<Pipe, Error> {
        let children = self.0.borrow_mut();
        debug_assert!(children.len() >= 2);
        let mut spawned_children = vec![children[0].spawn(stdin, 1, stderr)?];

        for i in 1..children.len()-2 {
            spawned_children.push(children[i].spawn(1, 1, stderr)?)
        }

        spawned_children.push(children[children.len()-1].spawn(1, stdout, stderr)?);
        Ok(Pipe::new(spawned_children))
    }
}

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct Pipe {
    children: Vec<ChildProcess>,
    threads: RefCell<Vec<thread::JoinHandle<Result<(), io::Error>>>>
}

impl Pipe {
    fn new(children: Vec<ChildProcess>) -> Self {
        let mut threads = Vec::new();

        for i in 1..children.len() {
            let src = &children[i - 1];
            let dest = &children[i];
            let mut stdout = src.0.borrow_mut().stdout.take().expect("expected stdout");
            let mut stdin = dest.0.borrow_mut().stdin.take().expect("expected stdin");

            threads.push(thread::spawn(move || {
                match io::copy(&mut stdout, &mut stdin) {
                    Ok(_) => Ok(()),
                    Err(ref err) if err.kind() == io::ErrorKind::BrokenPipe => Ok(()),
                    Err(err) => Err(err)
                }
            }));
        }

        Self { children, threads: RefCell::new(threads) }
    }

    fn wait(&self) -> Vec<Error> {
        let mut threads = self.threads.borrow_mut();
        let mut errors = Vec::new();

        for t in threads.drain(..) {
            match t.join() {
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

        errors
    }
}

#[derive(Clone, Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct ChildProcessPromise {
    name: OsString,
    args: Vec<OsString>
}

impl ChildProcessPromise {
    fn new(name: OsString, args: Vec<OsString>) -> Self {
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

    fn spawn(&self, stdin: u8, stdout: u8, stderr: u8) -> Result<ChildProcess, Error> {
        let child = self.command()
            .stdin(to_stdio_value(stdin)?)
            .stdout(to_stdio_value(stdout)?)
            .stderr(to_stdio_value(stderr)?)
            .spawn()
            .map_err(|err| ketos_err(format!("{}", err)))?;
        Ok(ChildProcess::new(child))
    }

    #[cfg(unix)]
    fn exec(&self) -> Result<(), Error> {
        let err = self.command().exec();
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
pub struct ChildInterpProcess(pub Pid);

impl ChildInterpProcess {
    fn new(pid: Pid) -> Self {
        Self { 0: pid }
    }

    fn wait(&self) -> Result<(), Error> {
        // Ideally we'd call `waitpid` once, but `nix` as of 0.13.0 doesn't
        // support `WIFSIGNALED`, so we have to repeatedly listen for all
        // signals
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

fn to_stdio_u8(i: &Integer) -> Result<u8, Error> {
    match i.to_u8() {
        Some(value) if value <= 2 => Ok(value),
        _ => Err(ketos_err(format!("invalid stdio value: `{}`", i))),
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

