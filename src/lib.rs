#![crate_id="quire#0.1"]
#![crate_type="lib"]
#![feature(macro_rules)]

extern crate collections;
extern crate serialize;

pub use parser::parse;

mod chars;
mod errors;
mod tokenizer;
pub mod parser;
//mod json;
pub mod emit;
//pub mod decode;

