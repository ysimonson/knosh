use std::collections::HashMap;
use std::env;
use std::ffi::{OsStr, OsString};
use std::io::{self, BufRead};
use std::os::unix::process::CommandExt;
use std::process;
use std::rc::Rc;
use std::slice::Iter;
use std::usize;

#[cfg(unix)]
use std::os::unix::io::AsRawFd;

use glob::glob;
use ketos::compile::compile;
use ketos::exec::ExecError;
use ketos::function::{Arity, Lambda};
use ketos::name::{is_system_fn, is_system_operator, standard_names};
use ketos::value::FromValueRef;
use ketos::rc_vec::RcVec;
use ketos::{Bytes, Error, Interpreter as KetosInterpreter, Name, Value};
use signal_hook;
use signal_hook::iterator::Signals;

#[cfg(unix)]
use nix::unistd::{fork, ForkResult};

use super::{ExitStatus, Proc, Stderr, Stdin, Stdout, SubInterp, TrapMap};
use crate::error::ketos_err;
use crate::util;

const REGISTERED_SIGNALS: [(&str, i32); 17] = [
    ("signal/abrt", signal_hook::SIGABRT),
    ("signal/alrm", signal_hook::SIGALRM),
    ("signal/bus", signal_hook::SIGBUS),
    ("signal/chld", signal_hook::SIGCHLD),
    ("signal/cont", signal_hook::SIGCONT),
    ("signal/hup", signal_hook::SIGHUP),
    ("signal/int", signal_hook::SIGINT),
    ("signal/io", signal_hook::SIGIO),
    ("signal/pipe", signal_hook::SIGPIPE),
    ("signal/prof", signal_hook::SIGPROF),
    ("signal/quit", signal_hook::SIGQUIT),
    ("signal/sys", signal_hook::SIGSYS),
    ("signal/term", signal_hook::SIGTERM),
    ("signal/trap", signal_hook::SIGTRAP),
    ("signal/usr1", signal_hook::SIGUSR1),
    ("signal/usr2", signal_hook::SIGUSR2),
    ("signal/winch", signal_hook::SIGWINCH),
];

pub struct Interpreter {
    interp: Rc<KetosInterpreter>,
    pipe_name: Name,
    pub spawn_name: Name,
    pub spawn_with_stdio_name: Name,
    stdio_inherit_name: Name,
    stdio_piped_name: Name,
    stdio_null_name: Name,
    signals: Signals,
    traps: Rc<HashMap<i32, TrapMap>>,
}

impl Interpreter {
    pub fn new(interp: KetosInterpreter) -> Self {
        let (pipe_name, spawn_name, spawn_with_stdio_name, stdio_inherit_name, stdio_piped_name, stdio_null_name) = {
            let scope = interp.scope();
            (
                scope.add_name("|"),
                scope.add_name("spawn"),
                scope.add_name("spawn-with-stdio"),
                scope.add_name("stdio/inherit"),
                scope.add_name("stdio/piped"),
                scope.add_name("stdio/null"),
            )
        };

        let mut signums = Vec::new();
        let mut traps = HashMap::new();

        for (_, signum) in &REGISTERED_SIGNALS {
            signums.push(*signum);
            traps.insert(*signum, TrapMap::default());
        }

        Self {
            interp: Rc::new(interp),
            pipe_name,
            spawn_name,
            spawn_with_stdio_name,
            stdio_inherit_name,
            stdio_piped_name,
            stdio_null_name,
            signals: Signals::new(signums).expect("could not register signal listeners"),
            traps: Rc::new(traps),
        }
    }

    pub fn inner(&self) -> Rc<KetosInterpreter> {
        self.interp.clone()
    }

    pub fn add_builtins(&self) {
        let interp_scope = self.interp.clone();
        let scope = interp_scope.scope();
        let interp_fork = self.interp.clone();
        let spawn_name = self.spawn_name;
        let spawn_with_stdio_name = self.spawn_with_stdio_name;
        let stdio_inherit_name = self.stdio_inherit_name;
        let stdio_piped_name = self.stdio_piped_name;
        let stdio_null_name = self.stdio_null_name;

        for (sigstr, signum) in &REGISTERED_SIGNALS {
            scope.add_named_value(&sigstr, (*signum).into());
        }

        let trap_traps = self.traps.clone();
        ketos_closure!(scope, "trap", |signum: i32, callback: &Lambda| -> usize {
            if let Some(trapmap) = trap_traps.get(&signum) {
                trapmap.add(callback)
            } else {
                Err(ketos_err(format!("unknown signum {}", signum)))
            }
        });

        let untrap_traps = self.traps.clone();
        ketos_closure!(scope, "untrap", |signum: i32, key: usize| -> () {
            if let Some(trapmap) = untrap_traps.get(&signum) {
                if trapmap.remove(key) {
                    Ok(())
                } else {
                    Err(ketos_err(format!("unknown key {} for signum {}", key, signum)))
                }
            } else {
                Err(ketos_err(format!("unknown signum {}", signum)))
            }
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

        let with_dir_interp = self.interp.clone();
        scope.add_value_with_name("with-dir", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Exact(2), args.len(), name)?;

                let mut iter = (&*args).iter();
                let new_dir = <&str>::from_value_ref(iter.next().unwrap())?;
                let value = iter.next().unwrap();

                let original_dir = env::current_dir().map_err(|err| ketos_err(format!("{}", err)))?;
                let original_dir_str = original_dir.to_str().map(|s| s.to_string());
                env::set_current_dir(util::expand_path(new_dir)?)
                    .map_err(|err| ketos_err(format!("{}: {}", new_dir, err)))?;
                let result = with_dir_interp.call_value(value.clone(), Vec::new());

                if let Some(original_dir_str) = original_dir_str {
                    env::set_current_dir(original_dir)
                        .map_err(|err| ketos_err(format!("{}: {}", original_dir_str, err)))?;
                } else {
                    env::set_current_dir(original_dir).map_err(|err| ketos_err(format!("{}", err)))?;
                }

                result
            })
        });

        let with_env_interp = self.interp.clone();
        scope.add_value_with_name("with-env", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Exact(3), args.len(), name)?;

                let mut iter = (&*args).iter();
                let env_key = <&str>::from_value_ref(iter.next().unwrap())?;
                let env_value = <&str>::from_value_ref(iter.next().unwrap())?;
                let value = iter.next().unwrap();

                let original_env_value = env::var(env_key).ok();
                env::set_var(env_key, env_value);
                let result = with_env_interp.call_value(value.clone(), Vec::new());

                if let Some(original_env_value) = original_env_value {
                    env::set_var(env_key, original_env_value);
                } else {
                    env::remove_var(env_key);
                }

                result
            })
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
                let p = Proc::new(
                    name,
                    args,
                    process::Stdio::inherit(),
                    process::Stdio::inherit(),
                    process::Stdio::inherit(),
                )?;
                Ok(p.into())
            }),
        );

        scope.add_value(
            spawn_with_stdio_name,
            Value::new_foreign_fn(spawn_with_stdio_name, move |_, args| {
                check_arity(Arity::Min(4), args.len(), spawn_with_stdio_name)?;
                let mut iter = (&*args).iter();
                let stdin = to_input_value(
                    iter.next().unwrap(),
                    stdio_inherit_name,
                    stdio_piped_name,
                    stdio_null_name,
                )?;
                let stdout = to_output_value(
                    iter.next().unwrap(),
                    stdio_inherit_name,
                    stdio_piped_name,
                    stdio_null_name,
                )?;
                let stderr = to_output_value(
                    iter.next().unwrap(),
                    stdio_inherit_name,
                    stdio_piped_name,
                    stdio_null_name,
                )?;
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
                let err = process::Command::new(name).args(args).exec();
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
                } else if let Ok(p) = <&SubInterp>::from_value_ref(value) {
                    p.wait()?;
                    Ok(().into())
                } else {
                    Err(type_error("waitable", value))
                }
            })
        });

        ketos_closure!(scope, "poll", |p: &Proc| -> Option<ExitStatus> { p.poll() });

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

        ketos_closure!(scope, "stdin", |p: &Proc| -> Stdin { p.stdin() });

        ketos_closure!(scope, "stdout", |p: &Proc| -> Stdout { p.stdout() });

        ketos_closure!(scope, "stderr", |p: &Proc| -> Stderr { p.stderr() });

        ketos_closure!(scope, "stdout/read", |stdout: &Stdout, limit: usize| -> Bytes {
            if limit > 0 {
                stdout.read(limit)
            } else {
                stdout.read_to_end()
            }
        });

        ketos_closure!(scope, "stdout/read-line", |stdout: &Stdout| -> String {
            stdout.read_to_newline()
        });

        ketos_closure!(scope, "stderr/read", |stderr: &Stderr, limit: usize| -> Bytes {
            if limit > 0 {
                stderr.read(limit)
            } else {
                stderr.read_to_end()
            }
        });

        ketos_closure!(scope, "stderr/read-line", |stderr: &Stderr| -> String {
            stderr.read_to_newline()
        });

        ketos_closure!(scope, "stdin/read-line", || -> String {
            let mut buf = String::new();
            io::BufReader::new(io::stdin())
                .read_line(&mut buf)
                .map_err(|err| ketos_err(format!("could not read string from stdin: {}", err)))?;
            Ok(buf)
        });

        ketos_closure!(scope, "stdin/write", |stdin: &Stdin, bytes: &[u8]| -> () {
            stdin.write(bytes)?;
            Ok(())
        });

        #[cfg(unix)]
        scope.add_value_with_name("stdin/fd", |name| {
            Value::new_foreign_fn(name, move |_, args| {
                check_arity(Arity::Range(0, 1), args.len(), name)?;

                let mut iter = (&*args).iter();

                if let Some(value) = iter.next() {
                    let stdin = <&Stdin>::from_value_ref(value)?;
                    Ok(stdin.fd()?.into())
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
                    let stdout = <&Stdout>::from_value_ref(value)?;
                    Ok(stdout.fd()?.into())
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
                    let stderr = <&Stderr>::from_value_ref(value)?;
                    Ok(stderr.fd()?.into())
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
    }

    pub fn execute(&self, exprs: &str, path: Option<String>) -> Result<Option<(Value, Value)>, Error> {
        let mut values = self.interp.parse_exprs(exprs, path)?;

        if values.is_empty() {
            return Ok(None);
        }

        // Automatically insert parens if they're not explicitly put
        let input_value = match values.as_slice() {
            [Value::List(_)] => values.pop().unwrap(),
            _ => values.into(),
        };

        let input_value = self.rewrite_exprs(input_value, None, None);
        // println!("{:#?}", input_value);
        let code = compile(self.interp.context(), &input_value)?;
        let output_value = self.interp.execute_code(Rc::new(code))?;
        Ok(Some((input_value, output_value)))
    }

    pub fn flush_signals(&self) -> Result<(), Error> {
        for signum in self.signals.pending() {
            let trapmap = self.traps.get(&signum).expect("expected a trapmap");

            for callback in trapmap.get() {
                self.interp.call_value(Value::Lambda(callback), vec![signum.into()])?;
            }
        }

        Ok(())
    }

    fn rewrite_exprs(&self, value: Value, stdin: Option<Value>, stdout: Option<Value>) -> Value {
        let scope = self.interp.scope();

        match value {
            Value::List(list) => {
                let list_v = list.into_vec();
                let list_count = list_v.len();
                let mut iter = list_v.into_iter();
                let first_value = iter.next().unwrap();
                let mut new_list = Vec::new();

                if let Value::Name(first_name) = first_value {
                    if first_name == self.pipe_name {
                        // This is a pipe call
                        let mut last: Option<Value> = None;
                        let mut i = 0;

                        while let Some(value) = iter.next() {
                            if i == list_count - 2 {
                                last = Some(self.rewrite_exprs(value, last, None));
                            } else {
                                last =
                                    Some(self.rewrite_exprs(value, last, Some(Value::Keyword(self.stdio_piped_name))));
                                i += 1;
                            }
                        }

                        if let Some(last) = last {
                            return last;
                        } else {
                            return Value::Unit;
                        }
                    } else if is_system_operator(first_name) {
                        // These system operators need special handling because they
                        // support lists where the first value can be a bare name

                        new_list.push(Value::Name(first_name));

                        match first_name {
                            standard_names::LET
                            | standard_names::DEFINE
                            | standard_names::MACRO
                            | standard_names::LAMBDA => {
                                if let Some(first_arg) = iter.next() {
                                    new_list.push(first_arg);
                                }
                            }
                            standard_names::STRUCT | standard_names::EXPORT | standard_names::USE => {
                                while let Some(arg) = iter.next() {
                                    new_list.push(arg);
                                }
                            }
                            _ => (),
                        }
                    } else if !is_system_fn(first_name) && !scope.contains_name(first_name) {
                        // Looks like this expr is shaped like a function call, to a
                        // function that does not exist. Change this into a call to
                        // `spawn-with-stdio`.

                        new_list.extend(vec![
                            Value::Name(self.spawn_with_stdio_name),
                            stdin.unwrap_or_else(|| Value::Keyword(self.stdio_inherit_name)),
                            stdout.unwrap_or_else(|| Value::Keyword(self.stdio_inherit_name)),
                            Value::Keyword(self.stdio_inherit_name),
                            {
                                let name_store = scope.borrow_names();
                                let first_name_str = name_store.get(first_name);
                                first_name_str.into()
                            },
                        ]);
                    } else {
                        new_list.push(first_value);
                    }
                } else {
                    new_list.push(first_value);
                }

                new_list.extend(iter.map(|value| self.rewrite_exprs(value, None, None)));
                new_list.into()
            }
            Value::Name(name) if !is_system_fn(name) && !is_system_operator(name) && !scope.contains_name(name) => {
                let name_store = scope.borrow_names();
                let arg_str = name_store.get(name);

                if arg_str.contains('*') || arg_str.contains('?') {
                    if let Ok(matches) = glob(arg_str) {
                        let matches: Vec<Value> = matches
                            .filter_map(|result| {
                                result.ok().map(Value::Path)
                            })
                            .collect();

                        if matches.is_empty() {
                            Value::Unit
                        } else {
                            Value::Quote(Box::new(Value::List(RcVec::new(matches))), 1)
                        }
                    } else {
                        arg_str.into()
                    }
                } else {
                    arg_str.into()
                }
            }
            _ => value,
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

fn type_error(expected: &'static str, value: &Value) -> Error {
    let err = ExecError::TypeError {
        expected,
        found: value.type_name(),
        value: None,
    };
    err.into()
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

    let args_str: Result<Vec<Vec<OsString>>, Error> = iter.map(proc_arg).collect();
    let args_str: Vec<OsString> = args_str?.into_iter().flat_map(|s| s).collect();
    Ok((name, args_str))
}

fn proc_arg(value: &Value) -> Result<Vec<OsString>, Error> {
match value {
        Value::Bool(v) => Ok(vec![format!("{}", v).into()]),
        Value::Float(v) => Ok(vec![format!("{}", v).into()]),
        Value::Integer(v) => Ok(vec![format!("{}", v).into()]),
        Value::Ratio(v) => Ok(vec![format!("{}", v).into()]),
        Value::Char(v) => Ok(vec![format!("{}", v).into()]),
        Value::String(v) => Ok(vec![format!("{}", v).into()]),
        Value::Bytes(v) if cfg!(unix) => {
            use std::os::unix::ffi::OsStringExt;
            let bytes = v.clone().into_bytes();
            Ok(vec![OsString::from_vec(bytes)])
        },
        Value::List(v) => {
            let result: Result<Vec<Vec<OsString>>, Error> = v.into_iter().map(proc_arg).collect();
            let result: Vec<OsString> = result?.into_iter().flat_map(|s| s).collect();
            Ok(result)
        }
        Value::Unit => Ok(vec![OsString::new()]),
        Value::Path(v) => Ok(vec![v.clone().into_os_string()]),
        _ => Err(ketos_err(format!(
            "cannot use non-stringifiable as an argument: `{:?}`",
            value
        )))
    }
}

fn to_input_value(
    value: &Value,
    inherit_name: Name,
    piped_name: Name,
    null_name: Name,
) -> Result<process::Stdio, Error> {
    if let Value::Foreign(_) = value {
        if let Ok(p) = <&Proc>::from_value_ref(value) {
            Ok(p.stdout()?.take()?.into())
        } else if let Ok(stdout) = <&Stdout>::from_value_ref(value) {
            Ok(stdout.take()?.into())
        } else if let Ok(stderr) = <&Stderr>::from_value_ref(value) {
            Ok(stderr.take()?.into())
        } else {
            Err(type_error("outputtable", value))
        }
    } else {
        to_stdio_value(value, inherit_name, piped_name, null_name)
    }
}

fn to_output_value(
    value: &Value,
    inherit_name: Name,
    piped_name: Name,
    null_name: Name,
) -> Result<process::Stdio, Error> {
    if let Value::Foreign(_) = value {
        if let Ok(p) = <&Proc>::from_value_ref(value) {
            Ok(p.stdin()?.take()?.into())
        } else if let Ok(stdin) = <&Stdin>::from_value_ref(value) {
            Ok(stdin.take()?.into())
        } else {
            Err(type_error("inputtable", value))
        }
    } else {
        to_stdio_value(value, inherit_name, piped_name, null_name)
    }
}

// TODO: add support for files
fn to_stdio_value(
    value: &Value,
    inherit_name: Name,
    piped_name: Name,
    null_name: Name,
) -> Result<process::Stdio, Error> {
    match value {
        Value::Keyword(name) if name == &inherit_name => Ok(process::Stdio::inherit()),
        Value::Keyword(name) if name == &piped_name => Ok(process::Stdio::piped()),
        Value::Keyword(name) if name == &null_name => Ok(process::Stdio::null()),
        _ => Err(type_error("stdio", value)),
    }
}
