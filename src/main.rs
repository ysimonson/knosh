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
mod util;

use std::cell::RefCell;
use std::env;
use std::io::{self, stderr, Write, BufRead, BufReader};
use std::iter::repeat;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use std::fs::File;
use std::rc::Rc;
use std::collections::{HashMap, BTreeMap};

use gumdrop::{Options, ParsingStyle};
use ketos::{
    complete_name, Builder, Context, Error, FromValueRef, Interpreter,
    ParseError, ParseErrorKind, RestrictConfig, Value
};
use ketos::name::{is_system_fn, is_system_operator, standard_names};
use ketos::compile::compile;
use ketos::io::{IoError, IoMode};
use ketos::rc_vec::{RcString, RcVec};
use linefeed::{
    Command, Completer, Completion, Function, Interface, Prompter, ReadResult,
    Signal, Suffix, Terminal,
};

const SOFT_MAX_PROMPT_LENGTH: u8 = 10;

fn main() {
    let status = run();
    std::process::exit(status);
}

#[derive(Options)]
struct KnoshOpts {
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
    let opts = match KnoshOpts::parse_args(&args[1..], ParsingStyle::StopAtFirstFree) {
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
        println!("{}", KnoshOpts::usage());
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

    CONTEXT.with(|key| {
        let ketos_interp = interp.interp.clone();
        *key.borrow_mut() = Some(ketos_interp.context().clone());
    });

    let interactive = opts.interactive || (opts.free.is_empty() && opts.expr.is_none());

    if let Some(ref expr) = opts.expr {
        if !interactive {
            match execute_exprs(&interp, &expr, None) {
                Ok(Some(value)) => if let Ok(p) = <&builtins::ChildProcessPromise>::from_value_ref(&value) {
                    if let Err(err) = p.run() {
                        display_error(&interp, "", &err);
                        return 1;
                    }
                } else if let Ok(p) = <&builtins::PipePromise>::from_value_ref(&value) {
                    if let Err(err) = p.run() {
                        display_error(&interp, "", &err);
                        return 1;
                    }
                } else {
                    interp.interp.clone().display_value(&value);
                },
                Ok(None) => {},
                Err(err) => {
                    display_error(&interp, "", &err);
                    return 1;
                }
            };
        }
    } else if !opts.free.is_empty() {
        interp.interp.clone().set_args(&opts.free);

        if !run_file(&interp, Path::new(&opts.free[0])) && !interactive {
            return 1;
        }
    }

    if interactive {
        if !opts.no_rc {
            if let Some(p) = dirs::home_dir() {
                let rc = p.join(".knoshrc.ket");
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
                    "execution_time" => {
                        res.execution_time = Some(Duration::from_millis(parse_param(name, value)?))
                    }
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
    let ketos_interp = interp.interp.clone();
    if let Some(trace) = ketos_interp.take_traceback() {
        ketos_interp.display_trace(&trace);
    }
    eprintln!("{}{}", prefix, ketos_interp.format_error(err));
}

fn execute_exprs(interp: &builtins::Interpreter, exprs: &str, path: Option<String>) -> Result<Option<Value>, Error> {
    let ketos_interp = interp.interp.clone();
    let mut values = ketos_interp.parse_exprs(exprs, path)?;

    if values.len() == 0 {
        return Ok(None);
    }

    // Automatically insert parens if they're not explicitly put
    let input_value = match values.as_slice() {
        [Value::List(_)] => values.pop().unwrap(),
        _ => Value::List(RcVec::new(values))
    };

    let input_value = rewrite_commands(interp, input_value);
    let code = compile(ketos_interp.context(), &input_value)?;
    let output_value = ketos_interp.execute_code(Rc::new(code))?;

    // update the completions
    ARGS_COMPLETER.with(move |key| {
        let mut completer = key.borrow_mut();
        update_command_completions(interp, &mut completer, &input_value);
    });

    Ok(Some(output_value))
}

fn rewrite_commands(interp: &builtins::Interpreter, value: Value) -> Value {
    let ketos_interp = interp.interp.clone();
    let scope = ketos_interp.scope();

    match value {
        Value::List(list) => {
            let list_v = list.into_vec();
            let mut iter = list_v.into_iter();
            let first_value = iter.next().unwrap();
            let mut new_list = vec![];

            if let Value::Name(first_name) = first_value {
                if is_system_operator(first_name) {
                    // These system operators need special handling because they
                    // support lists where the first value can be a bare name

                    new_list.push(Value::Name(first_name));

                    match first_name {
                        standard_names::LET | standard_names::DEFINE | standard_names::MACRO | standard_names::LAMBDA => {
                            if let Some(first_arg) = iter.next() {
                                new_list.push(first_arg);
                            }
                        }
                        standard_names::STRUCT | standard_names::EXPORT | standard_names::USE => {
                            while let Some(arg) = iter.next() {
                                new_list.push(arg);
                            }
                        }
                        _ => ()
                    }
                } else if !is_system_fn(first_name) && !scope.contains_name(first_name) {
                    // Looks like this expr is shaped like a function call, to a
                    // function that does not exist. Change this into a call to
                    // `proc`.

                    new_list.push(Value::Name(interp.proc_name));
                    new_list.push({
                        let name_store = scope.borrow_names();
                        let first_name_str = name_store.get(first_name).to_string();
                        Value::String(RcString::new(first_name_str.clone()))
                    });
                } else {
                    new_list.push(first_value);
                }
            } else {
                new_list.push(first_value);
            }

            new_list.extend(iter.map(|value| rewrite_commands(interp, value)));
            Value::List(RcVec::new(new_list))
        }
        Value::Name(name) if !is_system_fn(name) && !is_system_operator(name) && !scope.contains_name(name) => {
            let name_store = scope.borrow_names();
            let arg_str = name_store.get(name).to_string();
            Value::String(RcString::new(arg_str))
        }
        _ => value
    }
}

fn update_command_completions(interp: &builtins::Interpreter, completer: &mut ArgsCompleter, value: &Value) {
    if let Value::List(list) = value {
        let mut iter = list.iter();
        let first_value = iter.next();
        let second_value = iter.next();

        match (first_value, second_value) {
            (Some(Value::Name(first_name)), Some(Value::String(cmd))) if first_name == &interp.proc_name => {
                // This seems to be a proc call - process the arguments
                while let Some(value) = iter.next() {
                    if let Value::String(arg) = value {
                        completer.add(&cmd, &arg);
                    }
                }
            },
            _ => {}
        }
    }
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

            match execute_exprs(interp, &buf, path_str.clone()) {
                Ok(Some(value)) => if let Ok(p) = <&builtins::ChildProcessPromise>::from_value_ref(&value) {
                    if let Err(err) = p.run_suppressed() {
                        display_error(&interp, &error_prefix, &err);
                        return false;
                    }
                } else if let Ok(p) = <&builtins::PipePromise>::from_value_ref(&value) {
                    if let Err(err) = p.run_suppressed() {
                        display_error(&interp, &error_prefix, &err);
                        return false;
                    }
                }
                Ok(None) => {},
                Err(err) => {
                    display_error(&interp, &error_prefix, &err);
                    return false;
                }
            };

            buf = String::new();
        }
    }

    true
}

fn run_repl(interp: &builtins::Interpreter) -> io::Result<()> {
    let interface = Interface::new("knosh")?;
    let ketos_interp = interp.interp.clone();

    interface.set_completer(Arc::new(KnoshCompleter));
    interface.set_report_signal(Signal::Interrupt, true);
    interface.set_report_signal(Signal::Continue, true);
    interface.set_report_signal(Signal::Resize, true);
    interface.set_report_signal(Signal::Suspend, true);
    interface.set_report_signal(Signal::Quit, true);
    interface.define_function("knosh-accept", Arc::new(KnoshAccept));
    interface.bind_sequence("\r", Command::from_str("knosh-accept"));
    interface.bind_sequence("\n", Command::from_str("knosh-accept"));

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
            ReadResult::Eof => {
                break
            },
            ReadResult::Input(mut line) => {
                interface.add_history(line.clone());
                line.push('\n');

                match execute_exprs(interp, &line, None) {
                    Ok(Some(value)) => if let Ok(p) = <&builtins::ChildProcessPromise>::from_value_ref(&value) {
                        if let Err(err) = p.run() {
                            display_error(&interp, "", &err);
                        }
                    } else if let Ok(p) = <&builtins::PipePromise>::from_value_ref(&value) {
                        if let Err(err) = p.run() {
                            display_error(&interp, "", &err);
                        }
                    } else {
                        interp.interp.clone().display_value(&value);
                    },
                    Ok(None) => {},
                    Err(err) => {
                        display_error(&interp, "", &err);
                    }
                };

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

#[derive(Default)]
struct ArgsCompleter(HashMap<String, BTreeMap<String, usize>>);

impl ArgsCompleter {
    fn add(&mut self, cmd: &str, arg: &str) {
        let args = self.0.entry(cmd.to_string()).or_insert_with(BTreeMap::default);
        let count = args.entry(arg.to_string()).or_insert(0);
        *count += 1;
    }

    fn completions(&self, cmd: &str, word: &str) -> Vec<Completion> {
        if let Some(all_args) = self.0.get(cmd) {
            let mut candidate_args = BTreeMap::new();

            for (arg, count) in all_args.range(word.to_string()..) {
                if !arg.starts_with(word) {
                    break;
                }

                candidate_args.entry(count)
                    .or_insert_with(Vec::default)
                    .push(arg);
            }

            let mut completions: Vec<Completion> = candidate_args.values()
                .flatten()
                .map(|s| Completion::simple(s.to_string()))
                .collect();
            completions.reverse();
            completions
        } else {
            Vec::default()
        }
    }
}

thread_local! {
    // linefeed requires a Completer to impl Send + Sync.
    // Because a Context object contains Rc, it does not impl these traits.
    // Therefore, we must store the Context object in thread-local storage.
    // (We only use the linefeed Interface from a single thread, anyway.)
    static CONTEXT: RefCell<Option<Context>> = RefCell::new(None);

    static ARGS_COMPLETER: RefCell<ArgsCompleter> = RefCell::new(ArgsCompleter::default());
}

fn thread_context() -> Context {
    CONTEXT.with(|key| {
        key.borrow()
            .clone()
            .unwrap_or_else(|| panic!("no thread-local Context object set"))
    })
}

struct KnoshCompleter;

impl<Term: Terminal> Completer<Term> for KnoshCompleter {
    fn complete(
        &self,
        word: &str,
        prompter: &Prompter<Term>,
        start: usize,
        end: usize,
    ) -> Option<Vec<Completion>> {
        let line_start = prompter.buffer()[..start]
            .rfind('\n')
            .map(|pos| pos + 1)
            .unwrap_or(0);
        let is_whitespace = prompter.buffer()[line_start..start]
            .chars()
            .all(|ch| ch.is_whitespace());

        if is_whitespace && start == end {
            // Indent when there's no word to complete
            let n = 2 - (start - line_start) % 2;

            Some(vec![Completion {
                completion: repeat(' ').take(n).collect(),
                display: None,
                suffix: Suffix::None,
            }])
        } else {
            let ctx = thread_context();

            // complete names
            let mut completions: Vec<Completion> = complete_name(word, ctx.scope())
                .unwrap_or_else(Vec::default)
                .into_iter()
                .map(|w| Completion::simple(w))
                .collect();

            // complete paths
            if let Ok(path) = util::expand_path(word) {
                if !word.starts_with("~/") && !word.starts_with("./") && !word.starts_with("/") {
                    if let Ok(current_dir) = env::current_dir() {
                        if let Some(path) = path.to_str() {
                            completions.extend(self.complete_paths(&current_dir, &path));
                        }
                    }
                } else if word.ends_with("/") {
                    completions.extend(self.complete_paths(&path, ""));
                } else if let Some(Some(filename_prefix)) = path.file_name().map(|s| s.to_str()) {
                    if let Some(parent_path) = path.parent() {
                        completions.extend(self.complete_paths(parent_path, filename_prefix));
                    }
                }
            }

            // complete args
            if let Some(after_last_paren) = prompter.buffer()[0..start].rsplit("(").next() {
                if let Some(fn_name) = after_last_paren.split(char::is_whitespace).next() {
                    let args_completions = ARGS_COMPLETER.with(move |key| {
                        let args_completer = key.borrow();
                        args_completer.completions(fn_name, word)
                    });

                    completions.extend(args_completions);
                }
            }

            if completions.len() > 0 {
                Some(completions)
            } else {
                None
            }
        }
    }
}

impl KnoshCompleter {
    fn complete_paths(&self, parent_path: &Path, filename_prefix: &str) -> Vec<Completion> {
        let mut words = Vec::new();

        if let Ok(siblings) = parent_path.read_dir() {
            for sibling in siblings {
                if let Ok(sibling) = sibling {
                    if let Some(filename) = sibling.file_name().to_str() {
                        if filename.starts_with(filename_prefix) {
                            if let Some(s) = sibling.path().to_str() {
                                words.push(Completion {
                                    completion: s.to_string(),
                                    display: None,
                                    suffix: Suffix::None,
                                });
                            }
                        }
                    }
                }
            }
        }

        words
    }
}

struct KnoshAccept;

impl<Term: Terminal> Function<Term> for KnoshAccept {
    fn execute(&self, prompter: &mut Prompter<Term>, count: i32, _ch: char) -> io::Result<()> {
        if is_parseable(prompter.buffer()) {
            prompter.accept_input()
        } else if count > 0 {
            prompter.insert(count as usize, '\n')
        } else {
            Ok(())
        }
    }
}

fn is_parseable(text: &str) -> bool {
    let interp = Interpreter::with_context(thread_context());
    let r = interp.parse_exprs(text, None);
    interp.clear_codemap();

    match r {
        Err(Error::ParseError(ParseError {
            kind: ParseErrorKind::MissingCloseParen,
            ..
        }))
        | Err(Error::ParseError(ParseError {
            kind: ParseErrorKind::UnterminatedComment,
            ..
        }))
        | Err(Error::ParseError(ParseError {
            kind: ParseErrorKind::UnterminatedString,
            ..
        }))
        | Err(Error::ParseError(ParseError {
            kind: ParseErrorKind::DocCommentEof,
            ..
        })) => false,
        Ok(_) | Err(_) => true,
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
