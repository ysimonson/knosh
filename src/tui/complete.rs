use std::cell::RefCell;
use std::env;
use std::iter::repeat;
use std::path::Path;
use std::collections::{HashMap, BTreeMap};

use ketos::complete_name;
use linefeed::{Completer, Completion, Prompter, Suffix, Terminal};

use super::context::thread_context;
use crate::util;

thread_local! {
    pub static ARGS_COMPLETER: RefCell<ArgsCompleter> = RefCell::new(ArgsCompleter::default());
}

#[derive(Default)]
pub struct ArgsCompleter(HashMap<String, BTreeMap<String, usize>>);

impl ArgsCompleter {
    pub fn add(&mut self, cmd: &str, arg: &str) {
        let args = self.0.entry(cmd.to_string()).or_insert_with(BTreeMap::default);
        let count = args.entry(arg.to_string()).or_insert(0);
        *count += 1;
    }

    pub fn completions(&self, cmd: &str, word: &str) -> Vec<Completion> {
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

pub struct KnoshCompleter;

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
                    if let Some(sibling_filename) = sibling.file_name().to_str() {
                        if sibling_filename.starts_with(filename_prefix) {
                            if let Some(sibling_path) = sibling.path().to_str() {
                                let is_dir = match sibling.file_type() {
                                    Ok(t) => t.is_dir(),
                                    Err(_) => false
                                };

                                let completion = if is_dir {
                                    format!("{}/", sibling_path)
                                } else {
                                    sibling_path.to_string()
                                };

                                words.push(Completion {
                                    completion: completion,
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