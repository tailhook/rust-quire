#![crate_id="quire#0.1"]
#![crate_type="dylib"]

extern crate collections;
extern crate serialize;

pub use parser::parse;

mod chars;
mod errors;
mod tokenizer;
mod parser;
mod json;

