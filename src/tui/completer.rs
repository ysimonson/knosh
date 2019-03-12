use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::env;
use std::fs;
use std::iter::repeat;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use ketos::{complete_name, Context};
use linefeed::{Completer as LinefeedCompleter, Completion, Prompter, Suffix, Terminal};

use super::context::thread_context;
use crate::util;

pub struct Completer {
    procs: BTreeSet<String>,
    args: Mutex<HashMap<String, BTreeMap<String, usize>>>,
}

impl Default for Completer {
    fn default() -> Self {
        Completer {
            procs: executables(),
            args: Mutex::new(HashMap::new()),
        }
    }
}

impl<Term: Terminal> LinefeedCompleter<Term> for Completer {
    fn complete(&self, word: &str, prompter: &Prompter<Term>, start: usize, end: usize) -> Option<Vec<Completion>> {
        let line_start = prompter.buffer()[..start].rfind('\n').map(|pos| pos + 1).unwrap_or(0);
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
            let context = thread_context();
            let prior = &prompter.buffer()[0..start];
            let completions = self.completions(context, word, prior);

            if completions.is_empty() {
                None
            } else {
                Some(completions)
            }
        }
    }
}

impl Completer {
    fn completions(&self, context: Context, word: &str, prior: &str) -> Vec<Completion> {
        // complete names
        let mut completions: Vec<Completion> = complete_name(word, context.scope())
            .unwrap_or_else(Vec::default)
            .into_iter()
            .map(Completion::simple)
            .collect();

        // complete paths
        let current_dir = env::current_dir();

        if let Ok(current_dir) = current_dir {
            if let Ok(path) = util::expand_path(word) {
                let relative_to = if path.is_relative() {
                    Some(current_dir.clone())
                } else {
                    None
                };

                if word.ends_with("/") {
                    completions.extend(self.path_completions(&path, relative_to, ""));
                } else if let Some(parent) = path.parent() {
                    if parent.to_str() != Some("") {
                        if let Some(Some(s)) = path.file_name().map(|s| s.to_str()) {
                            completions.extend(self.path_completions(&parent, relative_to, s));
                        }
                    } else {
                        completions.extend(self.path_completions(&current_dir, relative_to, word));
                    }
                }
            }
        }

        // complete args
        if let Some(after_last_paren) = prior.rsplit('(').next() {
            if after_last_paren == "" {
                completions.extend(self.proc_completions(word));
            } else if let Some(fn_name) = after_last_paren.split(char::is_whitespace).next() {
                completions.extend(self.arg_completions(fn_name, word));
            }
        }

        completions
    }

    fn path_completions(
        &self,
        parent_path: &Path,
        relative_to: Option<PathBuf>,
        filename_prefix: &str,
    ) -> Vec<Completion> {
        let mut words = Vec::new();

        let siblings = match parent_path.read_dir() {
            Ok(s) => s,
            Err(_) => return words,
        };

        for sibling in siblings {
            let sibling = match sibling {
                Ok(s) => s,
                Err(_) => continue,
            };

            match sibling.file_name().to_str() {
                Some(s) if !s.starts_with(filename_prefix) => continue,
                Some(_) => {}
                None => continue,
            };

            let sibling_path = if let Some(relative_to) = relative_to.clone() {
                if let Ok(p) = sibling.path().strip_prefix(relative_to) {
                    p.to_path_buf()
                } else {
                    sibling.path()
                }
            } else {
                sibling.path()
            };

            if let Some(sibling_path_str) = sibling_path.to_str() {
                let is_dir = match sibling.file_type() {
                    Ok(t) => t.is_dir(),
                    Err(_) => false,
                };

                let completion = if is_dir {
                    format!("{}/", sibling_path_str)
                } else {
                    sibling_path_str.to_string()
                };

                words.push(Completion {
                    completion,
                    display: None,
                    suffix: Suffix::None,
                });
            }
        }

        words
    }

    fn proc_completions(&self, word: &str) -> Vec<Completion> {
        let mut procs = Vec::new();
        let word_cloned = word.to_string();

        for executable in self.procs.range(word_cloned..) {
            if !executable.starts_with(&word) {
                break;
            }

            procs.push(Completion::simple(executable.to_string()));
        }

        procs
    }

    fn arg_completions(&self, cmd: &str, word: &str) -> Vec<Completion> {
        let args = self.args.lock().unwrap();
        let cmd_args = args.get(cmd);

        if let Some(all_args) = cmd_args {
            let mut candidate_args = BTreeMap::new();

            for (arg, count) in all_args.range(word.to_string()..) {
                if !arg.starts_with(word) {
                    break;
                }

                candidate_args.entry(count).or_insert_with(Vec::default).push(arg);
            }

            let mut completions: Vec<Completion> = candidate_args
                .values()
                .flatten()
                .map(|s| Completion::simple(s.to_string()))
                .collect();
            completions.reverse();
            completions
        } else {
            Vec::default()
        }
    }

    pub fn add_arg(&self, cmd: &str, arg: &str) {
        let mut args = self.args.lock().unwrap();
        let cmd_args = args.entry(cmd.to_string()).or_insert_with(BTreeMap::default);
        let count = cmd_args.entry(arg.to_string()).or_insert(0);
        *count += 1;
    }
}

fn executables() -> BTreeSet<String> {
    let mut executables = BTreeSet::new();

    let path = match env::var("PATH") {
        Ok(p) => p,
        Err(_) => return executables,
    };

    for dirpath in path.split(':') {
        let entries = match fs::read_dir(dirpath) {
            Ok(e) => e,
            Err(_) => continue,
        };

        for entry in entries {
            let entry = match entry {
                Ok(e) => e,
                Err(_) => continue,
            };

            let metadata = match entry.metadata() {
                Ok(m) => m,
                Err(_) => continue,
            };

            if !metadata.is_file() {
                continue;
            }

            if let Ok(name) = entry.file_name().into_string() {
                executables.insert(name);
            }
        }
    }

    executables
}
