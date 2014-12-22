use std::fmt::Show;
use std::fmt::Formatter;
use std::fmt::FormatError;

use super::tokenizer::Pos;

#[deriving(Send, Clone)]
pub struct ErrorPos(String, uint, uint);

#[deriving(Send, Clone)]
pub enum Error {
    TokenizerError(ErrorPos, String),
    ParseError(ErrorPos, String),
    ValidationError(ErrorPos, String),
    PreprocessError(ErrorPos, String),
    DecodeError(ErrorPos, String, String),
}

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
                try!(filename.fmt(fmt));
                try!(":".fmt(fmt));
                try!(line.fmt(fmt));
                try!(":".fmt(fmt));
                try!(offset.fmt(fmt));
                try!(": ".fmt(fmt));
                match *self {
                    TokenizerError(_, _) => try!("Tokenizer Error".fmt(fmt)),
                    ParseError(_, _) => try!("Parse Error".fmt(fmt)),
                    PreprocessError(_, _) => try!("Preprocess Error".fmt(fmt)),
                    ValidationError(_, _) => try!("Validation Error".fmt(fmt)),
                    DecodeError(_, _, _) => unreachable!(),
                }
                try!(": ".fmt(fmt));
                try!(message.fmt(fmt));
            }
            DecodeError(ErrorPos(ref filename, line, offset),
                ref path, ref message)
            => {

                try!(filename.fmt(fmt));
                try!(":".fmt(fmt));
                try!(line.fmt(fmt));
                try!(":".fmt(fmt));
                try!(offset.fmt(fmt));
                try!(": Decode error at ".fmt(fmt));
                try!(path.fmt(fmt));
                try!(": ".fmt(fmt));
                try!(message.fmt(fmt));
            }
        }
        return Ok(());
    }
}

