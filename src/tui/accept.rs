use std::io;

use ketos::{Error, Interpreter, ParseError, ParseErrorKind};
use linefeed::{Function, Prompter, Terminal};

use super::context::thread_context;

pub struct KnoshAccept;

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

pub fn is_parseable(text: &str) -> bool {
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
