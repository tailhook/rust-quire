#![crate_name="quire"]
#![crate_type="lib"]


extern crate rustc_serialize;
#[macro_use] extern crate quick_error;

pub use sky::{parse_config, parse_string};
pub use options::{Options, Include};
pub use errors::{Error, ErrorList, ErrorCollector};

mod chars;
mod errors;
mod tokenizer;
mod options;
pub mod parser;
pub mod json;
pub mod emit;
pub mod ast;
pub mod decode;
pub mod validate;
pub mod sky;
#[cfg(test)] mod test_errors;
