use std::borrow::Borrow;
use std::env;
use std::ffi::OsString;
use std::io;
use std::process;
use std::rc::Rc;
use std::usize;

#[cfg(unix)]
use std::os::unix::io::AsRawFd;

use ketos::exec::ExecError;
use ketos::function::{Arity, Lambda};
use ketos::value::FromValueRef;
use ketos::{Error, Integer, Interpreter as KetosInterpreter, Name, Value};
use linefeed::Signal;

#[cfg(unix)]
use nix::unistd::{fork, ForkResult};

use super::{ExitStatus, Pipe, PipePromise, Proc, ProcPromise, SubInterp, TrapMap};
use crate::error::ketos_err;
use crate::util;

pub struct Interpreter {
    interp: Rc<KetosInterpreter>,
    pub proc_name: Name,
    traps: [Rc<TrapMap>; 5],
    is_root: bool,
}

impl Interpreter {
    pub fn new(interp: KetosInterpreter, is_root: bool) -> Self {
        let proc_name = {
            let scope = interp.scope();
            scope.add_name("proc")
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
            proc_name,
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
        let proc_name = self.proc_name;

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
            proc_name,
            Value::new_foreign_fn(proc_name, move |_, args| {
                check_arity(Arity::Min(1), args.len(), proc_name)?;

                let mut iter = (&*args).iter();
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
                            "cannot use non-stringifiable value as a proc name: `{:?}`",
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
                            "cannot use non-stringifiable value as an argument: `{:?}`",
                            value
                        ))),
                    })
                    .collect();

                Ok(ProcPromise::new(name, args_str?).into())
            }),
        );

        scope.add_value_with_name("spawn", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Range(1, 4), args.len(), name)?;

                let mut iter = (&*args).iter();
                let p = <&ProcPromise>::from_value_ref(iter.next().unwrap())?;
                let stdio = iter.collect::<Vec<&Value>>();

                let (stdin, stdout, stderr) = match stdio.as_slice() {
                    [] => (0, 0, 0),
                    [Value::Integer(stdout)] => (0, to_stdio_u8(stdout)?, 0),
                    [Value::Integer(stdout), Value::Integer(stderr)] => (0, to_stdio_u8(stdout)?, to_stdio_u8(stderr)?),
                    [Value::Integer(stdin), Value::Integer(stdout), Value::Integer(stderr)] => {
                        (to_stdio_u8(stdin)?, to_stdio_u8(stdout)?, to_stdio_u8(stderr)?)
                    }
                    _ => {
                        for value in &stdio {
                            if value.type_name() != "integer" {
                                return Err(type_error("integer", &value));
                            }
                        }

                        unreachable!();
                    }
                };

                Ok(p.spawn(stdin, stdout, stderr)?.into())
            })
        });

        #[cfg(unix)]
        ketos_closure!(scope, "exec", |cmd: &ProcPromise| -> () { cmd.exec() });

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
                    return Err(type_error("waitable", value));
                }
            })
        });

        ketos_closure!(scope, "poll", |child: &Proc| -> ExitStatus { child.poll() });

        scope.add_value_with_name("pid", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Range(0, 1), args.len(), name)?;

                let mut iter = (&*args).iter();

                if let Some(value) = iter.next() {
                    let child = <&Proc>::from_value_ref(value)?;
                    Ok(child.pid().into())
                } else {
                    Ok(process::id().into())
                }
            })
        });

        ketos_closure!(scope, "write", |child: &Proc, bytes: &[u8]| -> () {
            child.write(bytes)
        });

        #[cfg(unix)]
        scope.add_value_with_name("stdin/fd", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Range(0, 1), args.len(), name)?;

                let mut iter = (&*args).iter();

                if let Some(value) = iter.next() {
                    let child = <&Proc>::from_value_ref(value)?;
                    Ok(child.stdin_fd().into())
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
                    let child = <&Proc>::from_value_ref(value)?;
                    Ok(child.stdout_fd().into())
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
                    let child = <&Proc>::from_value_ref(value)?;
                    Ok(child.stderr_fd().into())
                } else {
                    Ok(io::stderr().as_raw_fd().into())
                }
            })
        });

        ketos_closure!(scope, "exit/success?", |status: &ExitStatus| -> bool {
            Ok(status.success())
        });

        ketos_closure!(scope, "exit/code", |status: &ExitStatus| -> i32 { status.code() });

        #[cfg(unix)]
        ketos_closure!(scope, "exit/signal", |status: &ExitStatus| -> i32 { status.signal() });

        scope.add_value_with_name("|", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Min(2), args.len(), name)?;

                let ps: Result<Vec<&ProcPromise>, ExecError> =
                    args.iter_mut().map(|arg| <&ProcPromise>::from_value_ref(arg)).collect();

                Ok(PipePromise::new(ps?).into())
            })
        });

        ketos_closure!(scope, "pipe/children", |pipe: &Pipe| -> Vec<Proc> {
            Ok(pipe.children())
        });
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

fn to_stdio_u8(i: &Integer) -> Result<u8, Error> {
    match i.to_u8() {
        Some(value) if value <= 2 => Ok(value),
        _ => Err(ketos_err(format!("invalid stdio value: `{}`", i))),
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
