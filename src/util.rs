use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};

use dirs;
use ketos::Error;

use crate::error::ketos_err;

pub fn shrink_path(mut path: PathBuf, max_len: u8) -> OsString {
    if let Some(home) = dirs::home_dir() {
        if let Ok(home_relative) = path.strip_prefix(home) {
            path = Path::new("~").join(home_relative);
        }
    }

    if path.as_os_str().len() > max_len as usize {
        // If the path is too long, try to shorten parent directories
        let parts: Vec<&OsStr> = path.iter().collect();
        let len = parts.len();
        path = parts
            .into_iter()
            .enumerate()
            .map(|(i, d)| {
                if i == len - 1 {
                    d.to_os_string()
                } else {
                    match d.to_str() {
                        Some(s) if !s.is_empty() => s.chars().next().unwrap().to_string().into(),
                        _ => d.to_os_string(),
                    }
                }
            })
            .collect();
    }

    if path.as_os_str().len() > max_len as usize {
        // If the path is still too long, just display the current directory
        path.file_name().unwrap().to_os_string()
    } else {
        path.into_os_string()
    }
}

pub fn expand_path<S: AsRef<str>>(path: S) -> Result<PathBuf, Error> {
    let path = path.as_ref();

    let path_buf = if path.starts_with('~') {
        let home = dirs::home_dir().ok_or_else(|| ketos_err("no home directory"))?;

        if path.starts_with("~/") {
            home.join(&path[2..])
        } else {
            home.join(&path[1..])
        }
    } else {
        PathBuf::from(path)
    };

    Ok(path_buf)
}
