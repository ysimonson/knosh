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

    let interactive = opts.interactive || (opts.free.is_empty() && opts.expr.is_none());

    if let Some(ref expr) = opts.expr {
        if !run_exprs(&interp, &expr, None) && !interactive {
            return 1;
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

fn display_error(interp: &builtins::Interpreter, e: &Error) {
    let ketos_interp = interp.interp.clone();
    if let Some(trace) = ketos_interp.take_traceback() {
        ketos_interp.display_trace(&trace);
    }
    ketos_interp.display_error(e);
}

fn execute_exprs(interp: &builtins::Interpreter, exprs: &str, path: Option<String>) -> Result<Value, Error> {
    let ketos_interp = interp.interp.clone();
    let mut values = ketos_interp.parse_exprs(exprs, path)?;

    // Automatically insert parens if they're not explicitly put
    let value = if values.len() > 1 {
        Value::List(RcVec::new(values))
    } else {
        values.pop().unwrap()
    };

    let value = walk_exprs(interp, value);
    let code = compile(ketos_interp.context(), &value)?;
    ketos_interp.execute_code(Rc::new(code))
}

// TODO: less value cloning
fn walk_exprs(interp: &builtins::Interpreter, value: Value) -> Value {
    if let Value::List(ref list) = value {
        // list is never empty according to ketos docs
        debug_assert!(!list.is_empty());

        if let Some(Value::Name(ref first_name)) = list.first() {
            let ketos_interp = interp.interp.clone();
            let scope = ketos_interp.scope();

            if is_system_operator(*first_name) {
                // These system operators need special handling because they
                // support lists where the first value can be a bare name
                match *first_name {
                    standard_names::LET | standard_names::DEFINE | standard_names::MACRO | standard_names::LAMBDA => {
                        let mut patched_list = list[..2].to_vec();
                        for value in &list[2..] {
                            patched_list.push(walk_exprs(interp, value.clone()));
                        }
                        return Value::List(RcVec::new(patched_list));
                    }
                    standard_names::STRUCT | standard_names::EXPORT | standard_names::USE => {
                        return value;
                    }
                    _ => ()
                }
            } else if !is_system_fn(*first_name) && !scope.contains_name(*first_name) {
                // Looks like this expr is shaped like a function call, to a
                // function that does not exist. Change this into a call to
                // `proc`.
                // Note: `proc_name` set before `name_store` to prevent a
                // runtime panic from multiple mut borrows of a `RefCell`.
                let proc_name = scope.add_name("proc");
                let name_store = scope.borrow_names();

                let mut args = vec![
                    Value::Name(proc_name),
                    Value::String(RcString::new(name_store.get(*first_name).to_string())),
                ];

                for value in &list[1..] {
                    let new_value = match value {
                        Value::Name(name) if !is_system_fn(*name) && !is_system_operator(*name) && !scope.contains_name(*name) => {
                            Value::String(RcString::new(name_store.get(*name).to_string()))
                        },
                        _ => walk_exprs(interp, value.clone())
                    };

                    args.push(new_value);
                }

                return Value::List(RcVec::new(args));
            }
        }

        let patched_list = list.into_iter()
            .map(|value| walk_exprs(interp, value.clone()))
            .collect();
        return Value::List(RcVec::new(patched_list));
    }

    value
}

fn run_exprs(interp: &builtins::Interpreter, exprs: &str, path: Option<String>) -> bool {
    let err = match execute_exprs(interp, exprs, path) {
        Ok(value) => if let Ok(p) = <&builtins::ChildProcessPromise>::from_value_ref(&value) {
            p.run()
        } else if let Ok(p) = <&builtins::PipePromise>::from_value_ref(&value) {
            p.run()
        } else {
            interp.interp.clone().display_value(&value);
            Ok(())
        }
        Err(err) => Err(err)
    };

    if let Err(err) = err {
        display_error(&interp, &err);
        false
    } else {
        true
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
            display_error(interp, &err);
            return false;
        }
    };

    let lines = BufReader::new(file).lines();
    let mut buf = String::new();

    for line in lines {
        let line = match line {
            Ok(line) => line,
            Err(err) => {
                let err = Error::IoError(IoError::new(IoMode::Read, path, err));
                display_error(interp, &err);
                return false;
            }
        };

        buf.push_str(&line);
        buf.push('\n');

        if is_parseable(&buf) {
            if !run_exprs(interp, &buf, path_str.clone()) {
                return false;
            }

            buf = String::new();
        }
    }

    if !buf.is_empty() {
        run_exprs(interp, &buf, path_str.clone())
    } else {
        true
    }
}

fn run_repl(interp: &builtins::Interpreter) -> io::Result<()> {
    let interface = Interface::new("knosh")?;
    let ketos_interp = interp.interp.clone();

    CONTEXT.with(|key| {
        *key.borrow_mut() = Some(ketos_interp.context().clone());
    });

    interface.set_completer(Arc::new(KnoshCompleter::default()));
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
                run_exprs(interp, &line, None);
                ketos_interp.clear_codemap();
            }
            ReadResult::Signal(sig) => {
                for result in interp.trigger_signal(sig) {
                    match result {
                        Ok(value) => ketos_interp.display_value(&value),
                        Err(err) => display_error(&interp, &err),
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

thread_local! {
    // linefeed requires a Completer to impl Send + Sync.
    // Because a Context object contains Rc, it does not impl these traits.
    // Therefore, we must store the Context object in thread-local storage.
    // (We only use the linefeed Interface from a single thread, anyway.)
    static CONTEXT: RefCell<Option<Context>> = RefCell::new(None);
}

fn thread_context() -> Context {
    CONTEXT.with(|key| {
        key.borrow()
            .clone()
            .unwrap_or_else(|| panic!("no thread-local Context object set"))
    })
}

#[derive(Default)]
struct KnoshCompleter {
    args: HashMap<String, BTreeMap<String, usize>>
}

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
            let mut words = complete_name(word, ctx.scope()).unwrap_or_else(Vec::default);

            // complete paths
            let mut path_word = word;
            if path_word.starts_with("#p\"") {
                path_word = &path_word[3..];
            }
            if path_word.ends_with("\"") {
                path_word = &path_word[..path_word.len()-1];
            }

            if let Ok(path) = util::expand_path(path_word) {
                if let Some(Some(filename_prefix)) = path.file_name().map(|s| s.to_str()) {
                    if let Some(parent_path) = path.parent() {
                        words.extend(self.complete_paths(ctx, parent_path, filename_prefix));
                    }
                }
            }

            // complete args
            if let Some(after_last_paren) = prompter.buffer()[0..start].rsplit("(").next() {
                if let Some(fn_name) = after_last_paren.split(char::is_whitespace).next() {
                    if let Some(args) = self.args.get(fn_name) {
                        words.extend(self.complete_args(args, word));
                    }
                }
            }

            if words.len() > 0 {
                Some(words.into_iter().map(Completion::simple).collect())
            } else {
                None
            }
        }
    }
}

impl KnoshCompleter {
    fn complete_paths(&self, ctx: Context, parent_path: &Path, filename_prefix: &str) -> Vec<String> {
        let interp = Interpreter::with_context(ctx);
        let mut words = Vec::new();

        if let Ok(siblings) = parent_path.read_dir() {
            for sibling in siblings {
                if let Ok(sibling) = sibling {
                    if let Some(filename) = sibling.file_name().to_str() {
                        if filename.starts_with(filename_prefix) {
                            let value = Value::Path(sibling.path());
                            let s = interp.format_value(&value);
                            words.push(s);
                        }
                    }
                }
            }
        }

        words
    }

    fn complete_args(&self, args: &BTreeMap<String, usize>, word: &str) -> Vec<String> {
        let mut candidates = BTreeMap::new();

        for (arg, count) in args.range(word.to_string()..) {
            if !arg.starts_with(word) {
                break;
            }

            candidates.entry(count)
                .or_insert_with(Vec::default)
                .push(word);
        }

        let mut words: Vec<String> = candidates.values()
            .flatten()
            .map(|s| s.to_string())
            .collect();
        words.reverse();
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

    if text.chars().all(|c| c.is_whitespace()) {
        return false;
    }

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
