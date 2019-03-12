#[macro_use]
extern crate clap;
extern crate dirs;
extern crate glob;
#[macro_use]
extern crate ketos;
#[macro_use]
extern crate ketos_derive;
extern crate linefeed;
#[cfg(unix)]
extern crate nix;
extern crate signal_hook;

mod builtins;
mod error;
mod tui;
mod util;

use std::env;
use std::fs::File;
use std::io::{self, stderr, BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

use clap::{App, Arg, AppSettings, Error as ClapError};
use ketos::io::{IoError, IoMode};
use ketos::{Builder, Error, FromValueRef, RestrictConfig, Value};
use linefeed::{Command, Interface, ReadResult, Signal};

use crate::error::ketos_err;
use crate::tui::{is_parseable, Accepter, Completer};

const SOFT_MAX_PROMPT_LENGTH: u8 = 10;

fn main() {
    match run() {
        Ok(code) => std::process::exit(code),
        Err(err) => err.exit()
    }
}

fn run() -> Result<i32, ClapError> {
    let matches = App::new("knosh")
        .version("0.1.0")
        .arg(Arg::with_name("expr")
            .short("e")
            .long("expr")
            .value_name("EXPR")
            .help("Evaluate the expression and exit")
            .takes_value(true))
        .arg(Arg::with_name("include")
            .short("i")
            .long("include")
            .value_name("DIR")
            .help("Add DIR to list of module search paths")
            .multiple(true)
            .takes_value(true))
        .arg(Arg::with_name("no_rc")
            .long("no-rc")
            .help("Do not run `~/.knoshrc.ket` on startup"))
        .arg(Arg::with_name("strict")
            .long("strict")
            .help("Applies \"strict\" execution restrictions"))
        .arg(Arg::with_name("execution_time")
            .long("execution-time")
            .help("Sets the maximum execution time")
            .takes_value(true))
        .arg(Arg::with_name("call_stack_size")
            .long("call-stack-size")
            .help("Sets the maximum call frames")
            .takes_value(true))
        .arg(Arg::with_name("value_stack_size")
            .long("value-stack-size")
            .help("Sets the maximum values stored on the stack")
            .takes_value(true))
        .arg(Arg::with_name("namespace_size")
            .long("namespace-size")
            .help("Sets the maximum values stored in global namespace")
            .takes_value(true))
        .arg(Arg::with_name("memory_limit")
            .long("memory-limit")
            .help("Sets the maximum total held memory, in abstract units")
            .takes_value(true))
        .arg(Arg::with_name("max_integer_size")
            .long("max_integer_size")
            .help("Sets the maximum integer size, in bits")
            .takes_value(true))
        .arg(Arg::with_name("max_syntax_nesting")
            .long("max_syntax_nesting")
            .help("Sets the maximum nested syntax elements")
            .takes_value(true))
        .arg(Arg::with_name("file")
            .index(1)
            .value_name("FILE")
            .help("Executes FILE")
            .takes_value(true))
        .arg(Arg::with_name("args")
            .help("Script args")
            .multiple(true)
            .takes_value(true)
            .last(true))
        .get_matches();

    let mut paths = vec![PathBuf::new()];
    if let Some(paths_str) = matches.values_of("include") {
        paths.extend(paths_str.into_iter().map(PathBuf::from).collect::<Vec<_>>());
    }

    let mut res = if matches.is_present("strict") {
        RestrictConfig::strict()        
    } else {
        RestrictConfig::permissive()
    };

    if matches.is_present("execution_time") {
        let micros = value_t!(matches, "execution_time", u64)?;
        res.execution_time = Some(Duration::from_micros(micros));
    }

    if matches.is_present("call_stack_size") {
        res.call_stack_size = value_t!(matches, "call_stack_size", usize)?;
    }

    if matches.is_present("value_stack_size") {
        res.value_stack_size = value_t!(matches, "value_stack_size", usize)?;
    }

    if matches.is_present("namespace_size") {
        res.namespace_size = value_t!(matches, "namespace_size", usize)?;
    }

    if matches.is_present("memory_limit") {
        res.memory_limit = value_t!(matches, "memory_limit", usize)?;
    }

    if matches.is_present("max_integer_size") {
        res.max_integer_size = value_t!(matches, "max_integer_size", usize)?;
    }

    if matches.is_present("max_syntax_nesting") {
        res.max_syntax_nesting = value_t!(matches, "max_syntax_nesting", usize)?;
    }

    let builder = Builder::new().search_paths(paths).restrict(res);
    let interp = builtins::Interpreter::new(builder.finish());
    interp.add_builtins();

    tui::set_thread_context({
        let ketos_interp = interp.inner();
        ketos_interp.context().clone()
    });

    if let Some(args) = matches.values_of("args") {
        let args = args.collect::<Vec<_>>();
        interp.inner().set_args(&args);
    }

    if !matches.is_present("no_rc") {
        if let Some(p) = dirs::home_dir() {
            let rc = p.join(".knoshrc");
            if rc.is_file() {
                // Do not bail on error in interactive mode
                run_file(&interp, &rc);
            }
        }
    }

    if let Some(expr) = matches.value_of("expr") {
        if !print_execution_result(&interp, None, &expr, "", None) {
            return Ok(1);
        }
    } else if let Some(file) = matches.value_of("file") {
        if !run_file(&interp, Path::new(file)) {
            return Ok(1);
        }
    } else {
        if let Err(err) = run_repl(&interp) {
            eprintln!("terminal device error: {}", err);
            return Ok(2);
        }
    }

    Ok(0)
}

fn display_error(interp: &builtins::Interpreter, prefix: &str, err: &Error) {
    let ketos_interp = interp.inner();
    if let Some(trace) = ketos_interp.take_traceback() {
        ketos_interp.display_trace(&trace);
    }
    eprintln!("{}{}", prefix, ketos_interp.format_error(err));
}

/// Executes a ketos file. This does not use ketos' built-in `run_file`
/// because we do some pre-processing before shipping it off to the
/// interpreter.
fn run_file(interp: &builtins::Interpreter, path: &Path) -> bool {
    let path_str = path.to_str().map(|s| s.to_string());

    let file = match File::open(path) {
        Ok(file) => file,
        Err(err) => {
            let err = Error::IoError(IoError::new(IoMode::Open, path, err));
            display_error(interp, "", &err);
            return false;
        }
    };

    let mut lines = BufReader::new(file).lines().enumerate().peekable();

    if let Some((_, Ok(first_line))) = lines.peek() {
        // ignore shebangs
        if first_line.starts_with("#!") {
            lines.next();
        }
    }

    let mut buf = String::new();
    let mut j = 0;

    for (i, line) in lines {
        let line = match line {
            Ok(line) => line,
            Err(err) => {
                let err = Error::IoError(IoError::new(IoMode::Read, path, err));
                display_error(interp, "", &err);
                return false;
            }
        };

        buf.push_str(&line);
        buf.push('\n');

        if is_parseable(&buf) {
            let error_prefix = format!("line {}: ", i + 1);
            if !print_execution_result(interp, None, &buf, &error_prefix, path_str.clone()) {
                return false;
            }
            buf = String::new();
        } else {
            // this might be the last line, in which case we want to save the
            // line number for error reporting
            j = i;
        }
    }

    // this ensures that the last line is still processed
    if !buf.is_empty() {
        let error_prefix = format!("line {}: ", j + 1);
        if !print_execution_result(interp, None, &buf, &error_prefix, path_str.clone()) {
            return false;
        }
    }

    true
}

fn run_repl(interp: &builtins::Interpreter) -> io::Result<()> {
    let interface = Interface::new("knosh")?;
    let ketos_interp = interp.inner();
    let completer = Arc::new(Completer::default());

    interface.set_completer(completer.clone());
    interface.set_report_signal(Signal::Interrupt, true);
    interface.define_function("knosh-accepter", Arc::new(Accepter));
    interface.bind_sequence("\r", Command::from_str("knosh-accepter"));
    interface.bind_sequence("\n", Command::from_str("knosh-accepter"));

    {
        let mut reader = interface.lock_reader();
        reader.set_word_break_chars(" \t\n'(),:;@[\\]`{}");
        reader.set_string_chars("\"");
        reader.set_blink_matching_paren(true);
    }

    loop {
        let cur_dir = env::current_dir().expect("expected to discover current directory");
        let cur_dir = util::shrink_path(cur_dir, SOFT_MAX_PROMPT_LENGTH - 2);
        let prompt = cur_dir.to_str().unwrap_or("λ");
        interface.set_prompt(&format!("{}> ", prompt))?;

        match interface.read_line()? {
            ReadResult::Eof => break,
            ReadResult::Input(mut line) => {
                interface.add_history(line.clone());
                line.push('\n');
                print_execution_result(interp, Some(completer.clone()), &line, "", None);
                ketos_interp.clear_codemap();
            }
            ReadResult::Signal(sig) => {
                if sig == Signal::Interrupt {
                    println!("^C");
                    interface.cancel_read_line()?;
                }
            }
        }
    }

    println!();

    Ok(())
}

pub fn print_execution_result(
    interp: &builtins::Interpreter,
    completer: Option<Arc<tui::Completer>>,
    line: &str,
    error_prefix: &str,
    path: Option<String>,
) -> bool {
    let successful = match interp.execute(line, path) {
        Ok(Some((input_value, output_value))) => {
            if let Ok(p) = <&builtins::Proc>::from_value_ref(&output_value) {
                match p.wait() {
                    Ok(exit_code) => {
                        if !exit_code.success() {
                            let err = ketos_err(format!("proc did not exit successfully: {:?}", exit_code));
                            display_error(&interp, error_prefix, &err);
                            false
                        } else {
                            if let Some(completer) = completer {
                                update_arg_completions(interp, completer, input_value);
                            }

                            true
                        }
                    }
                    Err(err) => {
                        display_error(&interp, error_prefix, &err);
                        false
                    }
                }
            } else if let Value::Unit = output_value {
                if let Some(completer) = completer {
                    update_arg_completions(interp, completer, input_value);
                }

                true
            } else {
                interp.inner().display_value(&output_value);

                if let Some(completer) = completer {
                    update_arg_completions(interp, completer, input_value);
                }

                true
            }
        }
        Ok(None) => true,
        Err(err) => {
            display_error(&interp, error_prefix, &err);
            false
        }
    };

    if let Err(err) = interp.flush_signals() {
        display_error(&interp, error_prefix, &err);
        false
    } else {
        successful
    }
}

fn update_arg_completions(interp: &builtins::Interpreter, completer: Arc<tui::Completer>, input_value: Value) {
    if let Value::List(list) = input_value {
        let mut iter = list.iter();

        match iter.next() {
            Some(Value::Name(first_name)) if first_name == &interp.spawn_name => {
                if let Some(Value::String(cmd)) = iter.next() {
                    for value in iter {
                        if let Value::String(arg) = value {
                            completer.add_arg(&cmd, &arg);
                        }
                    }
                }
            }
            Some(Value::Name(first_name)) if first_name == &interp.spawn_with_stdio_name => {
                let mut iter = iter.skip(3);

                if let Some(Value::String(cmd)) = iter.next() {
                    for value in iter {
                        if let Value::String(arg) = value {
                            completer.add_arg(&cmd, &arg);
                        }
                    }
                }
            }
            _ => {}
        }
    }
}
