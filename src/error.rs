use std::fmt;
use ketos::Error;
use std::error::Error as StdError;

/// `SimpleError`s are just errors with string contents.
#[derive(Debug)]
pub struct SimpleError {
    description: String,
}

impl SimpleError {
    pub fn new(description: String) -> SimpleError {
        SimpleError {
            description,
        }
    }
}

impl StdError for SimpleError {
    fn description(&self) -> &str {
        &self.description[..]
    }

    fn cause(&self) -> Option<&StdError> {
        None
    }
}

impl fmt::Display for SimpleError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description)
    }
}

pub fn ketos_err<S: Into<String>>(description: S) -> Error {
    Error::Custom(Box::new(SimpleError::new(description.into())))
}
