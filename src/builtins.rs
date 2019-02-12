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

use crate::error::{ketos_err, SimpleError};
use crate::util;

pub struct Interpreter {
    pub interp: Rc<KetosInterpreter>,
    pipe_operator_name: Name,
    traps: [Rc<TrapMap>; 5],
    synced_child_processes: Rc<RefCell<Vec<ChildProcess>>>,
    synced_pipes: Rc<RefCell<Vec<thread::JoinHandle<Result<(), io::Error>>>>>,
    is_root: bool,
    next_child_interp_key: Rc<RefCell<usize>>,
    child_interps: Rc<RefCell<HashMap<usize, thread::JoinHandle<Result<(), SimpleError>>>>>,
}

impl Interpreter {
    pub fn new(interp: KetosInterpreter, is_root: bool) -> Self {
        let scope = interp.scope();
        let pipe_operator_name = scope.borrow_names_mut().add("|");
        let traps = [
            Rc::new(TrapMap::new("signal-continue", 0)),
            Rc::new(TrapMap::new("signal-interrupt", 1)),
            Rc::new(TrapMap::new("signal-quit", 2)),
            Rc::new(TrapMap::new("signal-resize", 3)),
            Rc::new(TrapMap::new("signal-suspend", 4)),
        ];

        Self {
            interp: Rc::new(interp),
            pipe_operator_name,
            traps,
            synced_child_processes: Rc::new(RefCell::new(Vec::new())),
            synced_pipes: Rc::new(RefCell::new(Vec::new())),
            is_root,
            next_child_interp_key: Rc::new(RefCell::new(0)),
            child_interps: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn add_builtins(&self) {
        let interp1 = self.interp.clone();
        let interp2 = self.interp.clone();
        let scope = interp1.scope();
        let synced_child_processes = self.synced_child_processes.clone();
        let synced_pipes = self.synced_pipes.clone();
        let next_child_interp_key =self.next_child_interp_key.clone();
        let child_interps = self.child_interps.clone();

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
        
        ketos_closure!(scope, "setenv", |key: &str, value: &str| -> () {
            env::set_var(key, value);
            Ok(())
        });

        ketos_closure!(scope, "env", |key: &str| -> OsString {
            Ok(env::var_os(key).unwrap_or_else(OsString::default))
        });

        ketos_closure!(scope, "delenv", |key: &str| -> () {
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

        ketos_closure!(scope, "pwd", || -> OsString {
            let path = env::current_dir().map_err(|err| ketos_err(format!("{}", err)))?;
            Ok(path.into_os_string())
        });

        ketos_closure!(scope, "pwd", || -> OsString {
            let path = env::current_dir().map_err(|err| ketos_err(format!("{}", err)))?;
            Ok(path.into_os_string())
        });

        scope.add_value_with_name("go", |name| Value::new_foreign_fn(name, move |_, args| {
            check_arity(Arity::Exact(1), args.len(), name)?;

            let mut iter = (&*args).iter();
            
            let callback = match iter.next() {
                Some(Value::Name(callback)) => *callback,
                Some(value) => {
                    let err = ExecError::TypeError {
                        expected: "name",
                        found: value.type_name(),
                        value: None,
                    };
                    return Err(err.into());
                },
                None => unreachable!()
            };

            let key = {
                let mut key = next_child_interp_key.borrow_mut();
                *key += 1;
                *key
            };

            let context = interp2.clone().context();

            let handle = thread::spawn(move || {
                let interp = KetosInterpreter::with_context(context);
                let interp = Interpreter::new(interp, false);
                interp.add_builtins();
                interp.interp.call_value(Value::Name(callback), vec![])
                    .map_err(|err| SimpleError::new(format!("{}", err)))?;
                Ok(())
            });

            let mut child_interps = child_interps.borrow_mut();
            child_interps.insert(key, handle);
            Ok(().into())
        }));

        #[cfg(unix)]
        ketos_closure!(scope, "exec", |name: &OsStr, args: &[Value]| -> () {
            let args_str = util::values_to_osstrings(args)?;
            let err = process::Command::new(name).args(args_str).exec();
            Err(ketos_err(format!("{}", err)))
        });

        scope.add_value_with_name("spawn-async", |name| Value::new_foreign_fn(name, move |_, args| {
            let p = spawn(name, args, true)?;
            Ok(p.into())
        }));

        scope.add_value_with_name("spawn", |name| Value::new_foreign_fn(name, move |_, args| {
            let p = spawn(name, args, false)?;
            let pid = p.pid();
            let mut synced_child_processes = synced_child_processes.borrow_mut();
            synced_child_processes.push(p);
            Ok(pid.into())
        }));

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

        let pipe_name = scope.borrow_names_mut().add("|");
        scope.add_value(pipe_name, Value::new_foreign_fn(pipe_name, move |_, args| {
            check_arity(Arity::Min(2), args.len(), pipe_name)?;

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

    pub fn is_pipe_operator(&self, name: &Name) -> bool {
        name == &self.pipe_operator_name
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
    pub fn new<S: Into<String>>(name: S, index: u8) -> Self {
        Self {
            name: name.into(),
            index,
            next_key: AtomicUsize::new(0),
            traps: RefCell::new(HashMap::new()),
        }
    }

    pub fn add(&self, callback: &Lambda) -> Result<usize, Error> {
        let key = self.next_key.fetch_add(1, Ordering::Relaxed);
        if key == usize::max_value() {
            return Err(ketos_err("out of trap keys"));
        }

        self.traps.borrow_mut().insert(key, callback.clone());
        Ok(key)
    }

    pub fn remove(&self, key: usize) -> Result<bool, Error> {
        Ok(self.traps.borrow_mut().remove(&key).is_some())
    }

    pub fn get(&self) -> Vec<Lambda> {
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
pub struct ChildProcess(RefCell<process::Child>);

impl ChildProcess {
    pub fn new<I, S>(name: &OsStr, args: I, stdin: process::Stdio, stdout: process::Stdio, stderr: process::Stdio) -> Result<Self, Error>
    where I: IntoIterator<Item=S>,
          S: AsRef<OsStr>
    {
        let child = process::Command::new(name)
            .args(args.into_iter())
            .stdin(stdin)
            .stdout(stdout)
            .stderr(stderr)
            .spawn()
            .map_err(|err| ketos_err(format!("{}", err)))?;

        Ok(Self { 0: RefCell::new(child) })
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
}

#[derive(Debug, ForeignValue, FromValueRef, IntoValue)]
pub struct ChildExitStatus(pub process::ExitStatus);

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

fn spawn(name: Name, args: &mut [Value], is_async: bool) -> Result<ChildProcess, Error> {
    check_arity(Arity::Range(1, 3), args.len(), name)?;

    let mut iter = (&*args).iter();

    let proc_name = <&OsStr>::from_value_ref(iter.next().unwrap())?;

    let proc_args = match iter.next() {
        Some(value) => <&[Value]>::from_value_ref(value)?,
        None => &[],
    };
    let proc_args_str = util::values_to_osstrings(proc_args)?;

    let mut proc_stdin = process::Stdio::inherit();
    let mut proc_stdout = process::Stdio::inherit();
    let mut proc_stderr = process::Stdio::inherit();
    match iter.next() {
        Some(Value::Integer(stdout)) => {
            proc_stdout = util::integer_to_stdio(stdout, is_async)?;
        },
        Some(Value::List(value)) => match value.as_ref() {
            [Value::Integer(stdout), Value::Integer(stderr)] => {
                proc_stdout = util::integer_to_stdio(stdout, is_async)?;
                proc_stderr = util::integer_to_stdio(stderr, is_async)?;
            },
            [Value::Integer(stdin), Value::Integer(stdout), Value::Integer(stderr)] => {
                proc_stdin = util::integer_to_stdio(stdin, is_async)?;
                proc_stdout = util::integer_to_stdio(stdout, is_async)?;
                proc_stderr = util::integer_to_stdio(stderr, is_async)?;
            },
            _ => return Err(ketos_err("invalid stdio spec"))
        },
        None | Some(Value::Unit) => (),
        _ => return Err(ketos_err("invalid stdio spec"))
    };
    
    ChildProcess::new(proc_name, proc_args_str, proc_stdin, proc_stdout, proc_stderr)
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
