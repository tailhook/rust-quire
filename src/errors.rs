use std::fmt::String as Show;
use std::fmt::Error as FormatError;
use std::fmt::{Formatter};

use super::tokenizer::Pos;

use self::Error::{TokenizerError, ParseError, ValidationError,
           PreprocessError, DecodeError};

#[derive(Clone, Debug)]
pub struct ErrorPos(String, usize, usize);

#[derive(Clone, Debug)]
pub enum Error {
    TokenizerError(ErrorPos, String),
    ParseError(ErrorPos, String),
    ValidationError(ErrorPos, String),
    PreprocessError(ErrorPos, String),
    DecodeError(ErrorPos, String, String),
}

unsafe impl Send for Error {}

impl Error {
    pub fn parse_error(pos: &Pos, message: String) -> Error {
        return ParseError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message);
    }
    pub fn tokenizer_error(pos: &Pos, message: String) -> Error {
        return TokenizerError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message);
    }
    pub fn validation_error(pos: &Pos, message: String) -> Error {
        return ValidationError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message);
    }
    pub fn decode_error(pos: &Pos, path: &String, message: String) -> Error {
        return DecodeError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            path.clone(),
            message);
    }
    pub fn preprocess_error(pos: &Pos, message: String) -> Error {
        return PreprocessError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message);
    }
}

impl Show for Error {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FormatError> {
        match *self {
            TokenizerError(ErrorPos(ref filename, line, offset), ref message)
            | ParseError(ErrorPos(ref filename, line, offset), ref message)
            | ValidationError(ErrorPos(ref filename, line, offset), ref message)
            | PreprocessError(ErrorPos(ref filename, line, offset), ref message)
            => {
                let prefix = match *self {
                    TokenizerError(_, _) => "Tokenizer Error",
                    ParseError(_, _) => "Parse Error",
                    PreprocessError(_, _) => "Preprocess Error",
                    ValidationError(_, _) => "Validation Error",
                    DecodeError(_, _, _) => unreachable!(),
                };
                write!(fmt, "{filename}:{line}:{offset}: {prefix}: {text}",
                    filename=filename,
                    line=line,
                    offset=offset,
                    prefix=prefix,
                    text=message)
            }
            DecodeError(ErrorPos(ref filename, line, offset),
                ref path, ref message)
            => {
                write!(fmt, "{filename}:{line}:{offset}: \
                    Decode error at {path}: {text}",
                    filename=filename,
                    line=line,
                    offset=offset,
                    path=path,
                    text=message)
            }
        }
    }
}

