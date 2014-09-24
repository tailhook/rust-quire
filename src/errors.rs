use std::fmt::Show;
use std::fmt::Formatter;
use std::fmt::FormatError;

use super::tokenizer::Pos;

#[deriving(Show)]
pub enum Warning {
    //  AST transformation errors from ast.rs
    InvalidTag(Pos),
    NonScalarKey(Pos),
    UnsupportedTag(Pos),
    WrongNodeToMerge(Pos),
    //  Decoder errors from decode.rs
    UnexpectedNode(Pos, &'static str, String),
    CantParseValue(Pos, String),
    ValidationError(Pos, String),
    MissingFieldError(Pos, String),
    ExpectedSequence(Pos),
}

#[deriving(Clone)]
pub struct TokenError {
    position: Pos,
    error: &'static str,
}

#[deriving(Clone)]
pub struct ParserError {
    range: (Pos, Pos),
    error: &'static str,
}

pub enum Error {
    TokenError(TokenError),
    ParserError(ParserError),
}

impl TokenError {
    pub fn new(pos: Pos, err: &'static str) -> TokenError {
        return TokenError {
            position: pos,
            error: err,
            };
    }
}

impl ParserError {
    pub fn new(start: Pos, end: Pos, err: &'static str) -> ParserError {
        return ParserError {
            range: (start, end),
            error: err,
            };
    }
}

impl Show for TokenError {
    fn fmt(&self, fmt:&mut Formatter) -> Result<(), FormatError> {
        try!(self.position.line.fmt(fmt));
        try!(':'.fmt(fmt));
        try!(self.position.line_offset.fmt(fmt));
        try!(": ".fmt(fmt));
        try!(self.error.fmt(fmt));
        return Ok(());
    }
}

impl Show for ParserError {
    fn fmt(&self, fmt:&mut Formatter) -> Result<(), FormatError> {
        let (ref a, ref b) = self.range;
        try!(a.line.fmt(fmt));
        try!(':'.fmt(fmt));
        try!(a.line_offset.fmt(fmt));
        try!(": ".fmt(fmt));
        try!(b.line.fmt(fmt));
        try!(':'.fmt(fmt));
        try!(b.line_offset.fmt(fmt));
        try!(": ".fmt(fmt));
        try!(self.error.fmt(fmt));
        return Ok(());
    }
}

impl Show for Error {
    fn fmt(&self, fmt:&mut Formatter) -> Result<(), FormatError> {
        return match *self {
            TokenError(ref e) => e.fmt(fmt),
            ParserError(ref e) => e.fmt(fmt),
        }
    }
}
