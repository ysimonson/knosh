use std::borrow::Borrow;
use std::env;
use std::ffi::{OsStr, OsString};
use std::io::{self, BufRead};
use std::process;
use std::rc::Rc;
use std::usize;
use std::slice::Iter;
use std::os::unix::process::CommandExt;

#[cfg(unix)]
use std::os::unix::io::AsRawFd;

use ketos::exec::ExecError;
use ketos::function::{Arity, Lambda};
use ketos::value::FromValueRef;
use ketos::{Bytes, Error, Integer, Interpreter as KetosInterpreter, Name, Value};
use linefeed::Signal;

#[cfg(unix)]
use nix::unistd::{fork, ForkResult};

use super::{ExitStatus, Pipe, Proc, SubInterp, TrapMap};
use crate::error::ketos_err;
use crate::util;

pub struct Interpreter {
    interp: Rc<KetosInterpreter>,
    pub pipe_name: Name,
    pub spawn_name: Name,
    pub spawn_with_stdio_name: Name,
    traps: [Rc<TrapMap>; 5],
    is_root: bool,
}

impl Interpreter {
    pub fn new(interp: KetosInterpreter, is_root: bool) -> Self {
        let (pipe_name, spawn_name, spawn_with_stdio_name) = {
            let scope = interp.scope();
            (scope.add_name("|"), scope.add_name("spawn"), scope.add_name("spawn-with-stdio"))
        };

        let traps = [
            Rc::new(TrapMap::new("signal/continue", 0)),
            Rc::new(TrapMap::new("signal/interrupt", 1)),
            Rc::new(TrapMap::new("signal/quit", 2)),
            Rc::new(TrapMap::new("signal/resize", 3)),
            Rc::new(TrapMap::new("signal/suspend", 4)),
        ];

        Self {
            interp: Rc::new(interp),
            pipe_name,
            spawn_name,
            spawn_with_stdio_name,
            traps,
            is_root,
        }
    }

    pub fn inner(&self) -> Rc<KetosInterpreter> {
        self.interp.clone()
    }

    pub fn add_builtins(&self) {
        let interp_scope = self.interp.clone();
        let scope = interp_scope.scope();
        let interp_fork = self.interp.clone();
        let pipe_name = self.pipe_name;
        let spawn_name = self.spawn_name;
        let spawn_with_stdio_name = self.spawn_with_stdio_name;

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

        scope.add_value_with_name("env", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Range(0, 1), args.len(), name)?;

                let mut iter = (&*args).iter();

                if let Some(value) = iter.next() {
                    let key = <&OsStr>::from_value_ref(value)?;
                    let value = env::var_os(key.to_os_string());
                    Ok(value.into())
                } else {
                    let mut values = Vec::new();

                    for (key, value) in env::vars_os() {
                        values.push((key, value));
                    }

                    Ok(values.into())
                }
            })
        });

        ketos_closure!(scope, "del-env", |key: &str| -> () {
            env::remove_var(key);
            Ok(())
        });

        ketos_closure!(scope, "exit", |code: i32| -> () {
            process::exit(code);
        });

        scope.add_value_with_name("d", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Range(0, 1), args.len(), name)?;

                let mut iter = (&*args).iter();

                let expanded = if let Some(value) = iter.next() {
                    let dir = <&str>::from_value_ref(value)?;
                    let expanded = util::expand_path(dir)?;
                    env::set_current_dir(expanded.clone()).map_err(|err| ketos_err(format!("{}: {}", dir, err)))?;
                    expanded
                } else {
                    env::current_dir().map_err(|err| ketos_err(format!("{}", err)))?
                };

                Ok(expanded.into_os_string().into())
            })
        });

        ketos_closure!(scope, "cd", |dir: &str| -> () {
            let expanded = util::expand_path(dir)?;
            env::set_current_dir(expanded.clone()).map_err(|err| ketos_err(format!("{}: {}", dir, err)))?;
            Ok(())
        });

        ketos_closure!(scope, "pwd", || -> OsString {
            let path = env::current_dir().map_err(|err| ketos_err(format!("{}", err)))?;
            Ok(path.into_os_string())
        });

        #[cfg(unix)]
        scope.add_value_with_name("fork", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Exact(1), args.len(), name)?;

                let mut iter = (&*args).iter();

                let callback = match iter.next() {
                    Some(Value::Lambda(lambda)) => lambda,
                    Some(value) => return Err(type_error("function", value))?,
                    None => unreachable!(),
                };

                match fork() {
                    Ok(ForkResult::Parent { child, .. }) => Ok(SubInterp::new(child).into()),
                    Ok(ForkResult::Child) => {
                        let lambda = Value::Lambda(callback.clone());
                        match interp_fork.call_value(lambda, vec![]) {
                            Ok(_) => process::exit(0),
                            Err(_) => process::exit(1),
                        }
                    }
                    Err(err) => Err(ketos_err(format!("could not fork: {}", err))),
                }
            })
        });

        scope.add_value(
            spawn_name,
            Value::new_foreign_fn(spawn_name, move |_, args| {
                check_arity(Arity::Min(1), args.len(), spawn_name)?;
                let iter = (&*args).iter();
                let (name, args) = proc_args(iter)?;
                let p = Proc::new(name, args, process::Stdio::inherit(), process::Stdio::inherit(), process::Stdio::inherit())?;
                Ok(p.into())
            }),
        );

        scope.add_value(
            spawn_with_stdio_name,
            Value::new_foreign_fn(spawn_with_stdio_name, move |_, args| {
                check_arity(Arity::Min(4), args.len(), spawn_with_stdio_name)?;
                let mut iter = (&*args).iter();
                let stdin = to_stdio_value(u8::from_value_ref(iter.next().unwrap())?)?;
                let stdout = to_stdio_value(u8::from_value_ref(iter.next().unwrap())?)?;
                let stderr = to_stdio_value(u8::from_value_ref(iter.next().unwrap())?)?;
                let (name, args) = proc_args(iter)?;
                let p = Proc::new(name, args, stdin, stdout, stderr)?;
                Ok(p.into())
            }),
        );

        #[cfg(unix)]
        scope.add_value_with_name("exec", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Min(1), args.len(), spawn_name)?;
                let iter = (&*args).iter();
                let (name, args) = proc_args(iter)?;
                let err = process::Command::new(name)
                    .args(args)
                    .exec();
                Err(ketos_err(format!("{}", err)))
            })
        });

        scope.add_value_with_name("wait", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Exact(1), args.len(), name)?;

                let mut iter = (&*args).iter();
                let value = iter.next().unwrap();

                if let Ok(p) = <&Proc>::from_value_ref(value) {
                    p.wait()?;
                    Ok(().into())
                } else if let Ok(p) = <&Pipe>::from_value_ref(value) {
                    let mut errors = p.wait();

                    if let Some(err) = errors.pop() {
                        Err(err)
                    } else {
                        Ok(().into())
                    }
                } else if let Ok(p) = <&SubInterp>::from_value_ref(value) {
                    p.wait()?;
                    Ok(().into())
                } else {
                    Err(type_error("waitable", value))
                }
            })
        });

        ketos_closure!(scope, "poll", |p: &Proc| -> Option<ExitStatus> {
            p.poll()
        });

        scope.add_value_with_name("pid", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Range(0, 1), args.len(), name)?;

                let mut iter = (&*args).iter();

                if let Some(value) = iter.next() {
                    let p = <&Proc>::from_value_ref(value)?;
                    Ok(p.pid().into())
                } else {
                    Ok(process::id().into())
                }
            })
        });

        ketos_closure!(scope, "stdout/read", |p: &Proc, limit: usize| -> Bytes {
            p.read_stdout(limit)
        });

        ketos_closure!(scope, "stdout/read-to-newline", |p: &Proc| -> String {
            p.read_stdout_to_newline()
        });

        ketos_closure!(scope, "stdout/read-to-end", |p: &Proc| -> Bytes {
            p.read_stdout_to_end()
        });

        ketos_closure!(scope, "stderr/read", |p: &Proc, limit: usize| -> Bytes {
            p.read_stderr(limit)
        });

        ketos_closure!(scope, "stderr/read-to-newline", |p: &Proc| -> String {
            p.read_stderr_to_newline()
        });

        ketos_closure!(scope, "stderr/read-to-end", |p: &Proc| -> Bytes {
            p.read_stderr_to_end()
        });

        ketos_closure!(scope, "stdin/read-to-newline", || -> String {
            let mut buf = String::new();
            io::BufReader::new(io::stdin()).read_line(&mut buf).map_err(|err| ketos_err(format!("could not read string from stdin: {}", err)))?;
            Ok(buf.into())
        });

        ketos_closure!(scope, "stdin/write", |p: &Proc, bytes: &[u8]| -> () {
            Ok(p.write(bytes)?.into())
        });

        #[cfg(unix)]
        scope.add_value_with_name("stdin/fd", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Range(0, 1), args.len(), name)?;

                let mut iter = (&*args).iter();

                if let Some(value) = iter.next() {
                    let p = <&Proc>::from_value_ref(value)?;
                    Ok(p.stdin_fd()?.into())
                } else {
                    Ok(io::stdin().as_raw_fd().into())
                }
            })
        });

        #[cfg(unix)]
        scope.add_value_with_name("stdout/fd", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Range(0, 1), args.len(), name)?;

                let mut iter = (&*args).iter();

                if let Some(value) = iter.next() {
                    let p = <&Proc>::from_value_ref(value)?;
                    Ok(p.stdout_fd()?.into())
                } else {
                    Ok(io::stdout().as_raw_fd().into())
                }
            })
        });

        #[cfg(unix)]
        scope.add_value_with_name("stderr/fd", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Range(0, 1), args.len(), name)?;

                let mut iter = (&*args).iter();

                if let Some(value) = iter.next() {
                    let p = <&Proc>::from_value_ref(value)?;
                    Ok(p.stderr_fd()?.into())
                } else {
                    Ok(io::stderr().as_raw_fd().into())
                }
            })
        });

        ketos_closure!(scope, "exit/success?", |status: &ExitStatus| -> bool {
            Ok(status.success())
        });

        ketos_closure!(scope, "exit/code", |status: &ExitStatus| -> Option<i32> {
            Ok(status.code())
        });

        #[cfg(unix)]
        ketos_closure!(scope, "exit/signal", |status: &ExitStatus| -> Option<i32> {
            Ok(status.signal())
        });

        scope.add_value(
            pipe_name,
            Value::new_foreign_fn(pipe_name, move |_, args| {
                check_arity(Arity::Min(2), args.len(), pipe_name)?;

                let iter = (&*args).iter();

                let ps: Result<Vec<&Proc>, ExecError> = iter.map(|arg| {
                    <&Proc>::from_value_ref(arg)
                }).collect();

                Ok(Pipe::new(ps?)?.into())
            }),
        );
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
            _ => unimplemented!(),
        };

        let args = vec![Value::Integer(Integer::from_u8(sig_int))];
        let interp = self.interp.clone();
        let interp = (*interp).borrow();

        self.traps
            .iter()
            .filter(|t| t.index == sig_int)
            .take(1)
            .flat_map(|t| t.get())
            .map(|t| interp.call_value(Value::Lambda(t), args.clone()))
            .collect()
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

fn type_error(expected: &'static str, value: &Value) -> Error {
    let err = ExecError::TypeError {
        expected,
        found: value.type_name(),
        value: None,
    };
    err.into()
}

fn to_stdio_value(i: u8) -> Result<process::Stdio, Error> {
    match i {
        0 => Ok(process::Stdio::inherit()),
        1 => Ok(process::Stdio::piped()),
        2 => Ok(process::Stdio::null()),
        _ => Err(ketos_err(format!("invalid stdio value: `{}`", i)))
    }
}

fn proc_args(mut iter: Iter<Value>) -> Result<(OsString, Vec<OsString>), Error> {
    let value = iter.next().unwrap();

    let name = match value {
        Value::String(v) => format!("{}", v).into(),
        Value::Bytes(v) if cfg!(unix) => {
            use std::os::unix::ffi::OsStringExt;
            let bytes = v.clone().into_bytes();
            OsString::from_vec(bytes)
        }
        Value::Path(v) => v.clone().into_os_string(),
        _ => {
            return Err(ketos_err(format!(
                "cannot use non-stringlike as a proc name: `{:?}`",
                value
            )));
        }
    };

    let args_str: Result<Vec<OsString>, Error> = iter
        .map(|value| match value {
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
            }
            Value::Path(v) => Ok(v.clone().into_os_string()),
            _ => Err(ketos_err(format!(
                "cannot use non-stringifiable as an argument: `{:?}`",
                value
            ))),
        })
        .collect();

    Ok((name, args_str?))
}
