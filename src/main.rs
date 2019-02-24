extern crate dirs;
extern crate gumdrop;
#[macro_use]
extern crate ketos;
#[macro_use]
extern crate ketos_derive;
extern crate linefeed;
#[cfg(unix)]
extern crate nix;

mod builtins;
mod error;
mod executor;
mod tui;
mod util;

use std::env;
use std::fs::File;
use std::io::{self, stderr, BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

use gumdrop::{Options, ParsingStyle};
use ketos::io::{IoError, IoMode};
use ketos::{Builder, Error, FromValueRef, RestrictConfig, Value};
use linefeed::{Command, Interface, ReadResult, Signal};

use crate::tui::{is_parseable, Accepter, Completer};

const SOFT_MAX_PROMPT_LENGTH: u8 = 10;

fn main() {
    let status = run();
    std::process::exit(status);
}

#[derive(Options)]
struct Opts {
    #[options(free)]
    free: Vec<String>,

    #[options(help = "Print this help message and exit")]
    help: bool,
    #[options(short = "V", help = "Print version and exit")]
    version: bool,

    #[options(no_long, help = "Evaluate one expression and exit", meta = "EXPR")]
    expr: Option<String>,
    #[options(help = "Run interactively even with a file")]
    interactive: bool,
    #[options(no_long, help = "Add DIR to list of module search paths", meta = "DIR")]
    include: Vec<String>,
    #[options(
        short = "R",
        help = "Configure execution restrictions; see `-R help` for more details",
        meta = "SPEC"
    )]
    restrict: Option<String>,
    #[options(no_short, help = "Do not run ~/.knoshrc.ket on startup")]
    no_rc: bool,
}

fn run() -> i32 {
    let args = std::env::args().collect::<Vec<_>>();

    // Allow arguments that appear to be options to be passed to scripts
    let opts = match Opts::parse_args(&args[1..], ParsingStyle::StopAtFirstFree) {
        Ok(opts) => opts,
        Err(e) => {
            let _ = writeln!(stderr(), "{}: {}", args[0], e);
            return 1;
        }
    };

    if opts.version {
        println!("{}", env!("CARGO_PKG_VERSION"));
        return 0;
    }
    if opts.help {
        println!("Usage: {} [OPTIONS] [FILE] [ARGS]", args[0]);
        println!();
        println!("{}", Opts::usage());
        return 0;
    }

    let mut paths = vec![PathBuf::new()];
    paths.extend(opts.include.into_iter().map(PathBuf::from));

    let mut builder = Builder::new().search_paths(paths);

    if let Some(ref res) = opts.restrict {
        if res == "help" {
            print_restrict_usage();
            return 0;
        }

        builder = match parse_restrict(res) {
            Ok(res) => builder.restrict(res),
            Err(e) => {
                println!("{}: {}", args[0], e);
                return 1;
            }
        }
    }

    let interp = builtins::Interpreter::new(builder.finish(), true);
    interp.add_builtins();

    tui::set_thread_context({
        let ketos_interp = interp.inner();
        ketos_interp.context().clone()
    });

    let interactive = opts.interactive || (opts.free.is_empty() && opts.expr.is_none());

    if let Some(ref expr) = opts.expr {
        if !interactive && !print_execution_result(&interp, None, &expr, "", None) {
            return 1;
        }
    } else if !opts.free.is_empty() {
        interp.inner().set_args(&opts.free);

        if !run_file(&interp, Path::new(&opts.free[0])) && !interactive {
            return 1;
        }
    }

    if interactive {
        if !opts.no_rc {
            if let Some(p) = dirs::home_dir() {
                let rc = p.join(".knoshrc");
                if rc.is_file() {
                    // Do not bail on error in interactive mode
                    run_file(&interp, &rc);
                }
            }
        }

        if let Err(e) = run_repl(&interp) {
            eprintln!("terminal device error: {}", e);
        }
    }

    0
}

fn parse_restrict(params: &str) -> Result<RestrictConfig, String> {
    let mut res = RestrictConfig::permissive();

    for param in params.split(',') {
        match param {
            "permissive" => res = RestrictConfig::permissive(),
            "strict" => res = RestrictConfig::strict(),
            _ => {
                let (name, value) = match param.find('=') {
                    Some(pos) => (&param[..pos], &param[pos + 1..]),
                    None => return Err(format!("unrecognized restrict option: {}", param)),
                };

                match name {
                    "execution_time" => res.execution_time = Some(Duration::from_millis(parse_param(name, value)?)),
                    "call_stack_size" => res.call_stack_size = parse_param(name, value)?,
                    "value_stack_size" => res.value_stack_size = parse_param(name, value)?,
                    "namespace_size" => res.namespace_size = parse_param(name, value)?,
                    "memory_limit" => res.memory_limit = parse_param(name, value)?,
                    "max_integer_size" => res.max_integer_size = parse_param(name, value)?,
                    "max_syntax_nesting" => res.max_syntax_nesting = parse_param(name, value)?,
                    _ => return Err(format!("unrecognized parameter: {}", name)),
                }
            }
        }
    }

    Ok(res)
}

fn parse_param<T: FromStr>(name: &str, value: &str) -> Result<T, String> {
    value
        .parse()
        .map_err(|_| format!("invalid `{}` value: {}", name, value))
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

    let lines: Vec<Result<String, io::Error>> = BufReader::new(file).lines().collect();
    let line_count = lines.len();
    let mut buf = String::new();

    for (i, line) in lines.into_iter().enumerate() {
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

        if is_parseable(&buf) || i == line_count - 1 {
            let error_prefix = format!("line {}: ", i + 1);
            if !print_execution_result(interp, None, &buf, &error_prefix, path_str.clone()) {
                return false;
            }
            buf = String::new();
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
    interface.set_report_signal(Signal::Continue, true);
    interface.set_report_signal(Signal::Resize, true);
    interface.set_report_signal(Signal::Suspend, true);
    interface.set_report_signal(Signal::Quit, true);
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
                for result in interp.trigger_signal(sig) {
                    match result {
                        Ok(value) => ketos_interp.display_value(&value),
                        Err(err) => display_error(&interp, "", &err),
                    }
                }

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
    match executor::exprs(interp, line, path) {
        Ok(Some((input_value, output_value))) => {
            if let Ok(p) = <&builtins::ProcPromise>::from_value_ref(&output_value) {
                if let Err(err) = p.run() {
                    display_error(&interp, error_prefix, &err);
                    return false;
                }
            } else if let Ok(p) = <&builtins::PipePromise>::from_value_ref(&output_value) {
                if let Err(err) = p.run() {
                    display_error(&interp, error_prefix, &err);
                    return false;
                }
            } else if let Value::Unit = output_value {
                // don't print anything
            } else {
                interp.inner().display_value(&output_value);
            }

            // Update the args completer
            if let Some(completer) = completer {
                update_arg_completions(interp, completer, input_value);
            }

            true
        }
        Ok(None) => true,
        Err(err) => {
            display_error(&interp, error_prefix, &err);
            false
        }
    }
}

fn update_arg_completions(interp: &builtins::Interpreter, completer: Arc<tui::Completer>, input_value: Value) {
    if let Value::List(list) = input_value {
        let mut iter = list.iter();
        let first_value = iter.next();
        let second_value = iter.next();

        if let (Some(Value::Name(first_name)), Some(Value::String(cmd))) = (first_value, second_value) {
            if first_name == &interp.proc_name {
                // This is a proc call - process the arguments
                for value in iter {
                    if let Value::String(arg) = value {
                        completer.add_arg(&cmd, &arg);
                    }
                }
            }
        }
    }
}

fn print_restrict_usage() {
    print!(
        r#"The `-R` / `--restrict` option accepts a comma-separated list of parameters:
  permissive
    Applies "permissive" restrictions (default)
  strict
    Applies "strict" restrictions
  key=value
    Assigns a value to the named restriction configuration parameter.
    Accepted keys are:
      execution_time          Maximum execution time, in milliseconds
      call_stack_size         Maximum call frames
      value_stack_size        Maximum values stored on the stack
      namespace_size          Maximum values stored in global namespace
      memory_limit            Maximum total held memory, in abstract units
      max_integer_size        Maximum integer size, in bits
      max_syntax_nesting      Maximum nested syntax elements
"#
    );
}
