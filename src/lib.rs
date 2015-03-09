#![crate_name="quire"]
#![crate_type="lib"]

#![feature(old_io, rustc_private, collections)]

extern crate collections;
extern crate serialize;

pub use sky::parse_config;
pub use ast::Options;

mod chars;
mod errors;
mod tokenizer;
pub mod parser;
pub mod json;
pub mod emit;
pub mod ast;
pub mod decode;
pub mod validate;
pub mod sky;
#[cfg(test)] mod test_errors;
