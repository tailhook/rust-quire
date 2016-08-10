use std::io;
use std::fmt;
use std::rc::Rc;
use std::slice::Iter;
use std::path::PathBuf;
use std::cell::RefCell;

use super::tokenizer::{self, Pos};

#[derive(Clone, Debug)]
pub struct ErrorPos(String, usize, usize);

quick_error! {
    #[derive(Debug)]
    pub enum Error {
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
    }
}

unsafe impl Send for Error {}

impl Error {
    pub fn parse_error(pos: &Pos, message: String) -> Error {
        return Error::ParseError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message);
    }
    pub fn tokenizer_error((pos, err): (Pos, tokenizer::Error)) -> Error {
        return Error::TokenizerError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            err);
    }
    pub fn validation_error(pos: &Pos, message: String) -> Error {
        return Error::ValidationError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message);
    }
    pub fn decode_error(pos: &Pos, path: &String, message: String) -> Error {
        return Error::DecodeError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            path.clone(),
            message);
    }
    pub fn preprocess_error(pos: &Pos, message: String) -> Error {
        return Error::PreprocessError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message);
    }
}

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
            try!(writeln!(fmt, "{}", err));
        }
        Ok(())
    }
}

impl fmt::Debug for ErrorList {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for err in &self.errors {
            try!(writeln!(fmt, "{}", err));
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct ErrorCollector(Rc<RefCell<Option<ErrorList>>>);

impl ErrorCollector {
    pub fn new() -> ErrorCollector {
        ErrorCollector(Rc::new(RefCell::new(Some(ErrorList {
            errors: Vec::new()
        }))))
    }
    pub fn add_error(&self, err: Error) {
        self.0.borrow_mut().as_mut().unwrap().add_error(err)
    }
    pub fn into_fatal(&self, err: Error) -> ErrorList {
        let mut lst = self.0.borrow_mut().take().unwrap();
        lst.add_error(err);
        return lst;
    }
    pub fn into_result<T>(&self, val: T) -> Result<T, ErrorList> {
        let lst = self.0.borrow_mut().take().unwrap();
        if lst.errors.len() > 0 {
            Err(lst)
        } else {
            Ok(val)
        }
    }
    pub fn unwrap(&self) -> ErrorList {
        self.0.borrow_mut().take().unwrap()
    }
}
