use std::cell::RefCell;

use ast::Ast;
use errors::{Error, ErrorCollector};
use tokenizer::Pos;

pub type IncludeHandler<'a> =
    FnMut(&Pos, &Include, &ErrorCollector, &Options) -> Ast + 'a;

pub enum Include<'a> {
    /// Looks like `!Include some/file.yaml`
    File { filename: &'a str },
    // TODO(tailhook)
    // /// Looks like `!*Include some/file.yaml:some_key`
    // SubKey { filename: &'a str, key: &'a str },
    // /// Looks like `!*IncludeSeq some/*.yaml`
    // Sequence { directory: &'a str, prefix: &'a str, suffix: &'a str },
    // /// Looks like `!*IncludeMap some/*.yaml`.
    // /// Everything matched by star is used as a key
    // Mapping { directory: &'a str, prefix: &'a str, suffix: &'a str },
}


pub struct Options<'a> {
    include_handler: Box<RefCell<IncludeHandler<'a>>>,
}

pub trait DoInclude {
    fn include(&self, pos: &Pos, _: &Include, err: &ErrorCollector) -> Ast;
}

impl<'a> DoInclude for Options<'a> {
    fn include(&self, pos: &Pos, incl: &Include, err: &ErrorCollector) -> Ast {
        (&mut *(*self.include_handler).borrow_mut())(pos, incl, err, self)
    }
}


fn unsupported_include(pos: &Pos, _: &Include,
    err: &ErrorCollector, _: &Options)
    -> Ast
{
    err.add_error(Error::preprocess_error(pos,
        format!("Includes are not supported")));
    return Ast::void(pos);
}

impl<'a> Options<'a> {
    pub fn default() -> Options<'a> {
        Options {
            include_handler: Box::new(RefCell::new(unsupported_include)),
        }
    }
    pub fn allow_include<F>(&mut self, f: F)
        -> &mut Options<'a>
        where F: FnMut(&Pos, &Include, &ErrorCollector, &Options) -> Ast + 'a
    {
        self.include_handler = Box::new(RefCell::new(f));
        self
    }
}
