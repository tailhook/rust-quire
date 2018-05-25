use std::io;
use std::fmt;
use std::rc::Rc;
use std::error::Error as StdError;
use std::slice::Iter;
use std::path::{Path, PathBuf};
use std::cell::RefCell;

use super::tokenizer::{self, Pos};

#[derive(Clone, Debug)]
pub struct ErrorPos(String, usize, usize);

quick_error! {
    /// Single error when of parsing configuration file
    ///
    /// Usually you use `ErrorList` which embeds multiple errors encountered
    /// during configuration file parsing
    #[derive(Debug)]
    pub enum Error wraps pub ErrorEnum {
        OpenError(filename: PathBuf, err: io::Error) {
            display("{}: Error reading file: {}", filename.display(), err)
        }
        TokenizerError(pos: ErrorPos, err: tokenizer::Error) {
            display("{filename}:{line}:{offset}: Tokenizer Error: {err}",
                    filename=pos.0, line=pos.1, offset=pos.2, err=err)
        }
        ParseError(pos: ErrorPos, msg: String) {
            display("{filename}:{line}:{offset}: Parse Error: {text}",
                    filename=pos.0, line=pos.1, offset=pos.2, text=msg)
        }
        ValidationError(pos: ErrorPos, msg: String) {
            display("{filename}:{line}:{offset}: Validation Error: {text}",
                    filename=pos.0, line=pos.1, offset=pos.2, text=msg)
        }
        PreprocessError(pos: ErrorPos, msg: String) {
            display("{filename}:{line}:{offset}: Preprocess Error: {text}",
                    filename=pos.0, line=pos.1, offset=pos.2, text=msg)
        }
        DecodeError(pos: ErrorPos, path: String, msg: String) {
            display("{filename}:{line}:{offset}: \
                Decode error at {path}: {text}",
                    filename=pos.0, line=pos.1, offset=pos.2,
                    path=path, text=msg)
        }
        SerdeError(msg: String) {
            display("{}", msg)
        }
        CustomError(pos: Option<ErrorPos>, err: Box<StdError>) {
            display(x) -> ("{loc}{err}",
                loc=if let &Some(ref p) = pos {
                    format!("{filename}:{line}:{offset}: ",
                        filename=p.0, line=p.1, offset=p.2)
                } else {
                    "".to_string()
                },
                err=err)
            cause(&**err)
        }
    }
}

impl ::serde::de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        ErrorEnum::SerdeError(format!("{}", msg)).into()
    }
}

impl Error {
    pub fn open_error(path: &Path, err: io::Error) -> Error {
        ErrorEnum::OpenError(path.to_path_buf(), err).into()
    }
    pub fn parse_error(pos: &Pos, message: String) -> Error {
        ErrorEnum::ParseError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message).into()
    }
    pub fn tokenizer_error((pos, err): (Pos, tokenizer::Error)) -> Error {
        ErrorEnum::TokenizerError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            err).into()
    }
    pub fn validation_error(pos: &Pos, message: String) -> Error {
        ErrorEnum::ValidationError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message).into()
    }
    pub fn decode_error(pos: &Pos, path: &String, message: String) -> Error {
        ErrorEnum::DecodeError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            path.clone(),
            message).into()
    }
    pub fn preprocess_error(pos: &Pos, message: String) -> Error {
        ErrorEnum::PreprocessError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message).into()
    }

    pub fn custom<T: StdError + 'static>(err: T)
        -> Error
    {
        ErrorEnum::CustomError(None, Box::new(err)).into()
    }

    pub fn custom_at<T: StdError + 'static>(pos: &Pos, err: T)
        -> Error
    {
        ErrorEnum::CustomError(
            Some(ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset)),
            Box::new(err)).into()
    }

    pub fn downcast_ref<T: StdError + 'static>(&self) -> Option<&T> {
        match self.0 {
            ErrorEnum::OpenError(_, ref e) => {
                (e as &StdError).downcast_ref::<T>()
            },
            ErrorEnum::CustomError(_, ref e) => e.downcast_ref::<T>(),
            _ => None,
        }
    }
}

/// List of errors that were encountered during configuration file parsing
#[must_use]
pub struct ErrorList {
    errors: Vec<Error>,
}

impl ErrorList {
    pub fn add_error(&mut self, err: Error) {
        self.errors.push(err);
    }
    pub fn errors(&self) -> Iter<Error> {
        self.errors.iter()
    }
}

impl fmt::Display for ErrorList {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for err in &self.errors {
            writeln!(fmt, "{}", err)?;
        }
        Ok(())
    }
}

impl fmt::Debug for ErrorList {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for err in &self.errors {
            writeln!(fmt, "{}", err)?;
        }
        Ok(())
    }
}


/// An internal structure to track list of errors
///
/// It's exposed only to handler of include file. Use `ErrorCollector`
/// to submit your errors from include file handler.
#[derive(Clone, Debug)]
pub struct ErrorCollector(Rc<RefCell<Option<ErrorList>>>);

impl ErrorCollector {

    /// New error collector
    pub fn new() -> ErrorCollector {
        ErrorCollector(Rc::new(RefCell::new(Some(ErrorList {
            errors: Vec::new()
        }))))
    }

    /// Add another error to error collector
    pub fn add_error(&self, err: Error) {
        self.0.borrow_mut().as_mut().unwrap().add_error(err)
    }

    /// Adds fatal (final) error into collection and return error list
    pub fn into_fatal(&self, err: Error) -> ErrorList {
        let mut lst = self.0.borrow_mut().take().unwrap();
        lst.add_error(err);
        return lst;
    }

    /// Converts collector into `Ok(val)` if no errors reported, into `Err`
    /// otherwise
    pub fn into_result<T>(&self, val: T) -> Result<T, ErrorList> {
        let lst = self.0.borrow_mut().take().unwrap();
        if lst.errors.len() > 0 {
            Err(lst)
        } else {
            Ok(val)
        }
    }

    /// Unwraps ErrorList from the collector
    pub fn unwrap(&self) -> ErrorList {
        self.0.borrow_mut().take().unwrap()
    }
}

pub fn add_info<T>(pos: &Pos, path: &String, result: Result<T, Error>)
    -> Result<T, Error>
{
    match result {
        Err(Error(ErrorEnum::SerdeError(e))) => {
            Err(Error::decode_error(pos, path, format!("{}", e)))
        }
        result => result,
    }
}
