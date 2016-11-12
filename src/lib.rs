#![crate_name="quire"]
#![crate_type="lib"]
#![warn(missing_docs)]


extern crate rustc_serialize;
#[macro_use] extern crate quick_error;

pub use sky::{parse_config, parse_string};
pub use options::{Options, Include};
pub use errors::{Error, ErrorList, ErrorCollector};
pub use tokenizer::{Pos};
pub use parser::{parse as raw_parse};
pub use emit::{emit_ast, emit_object};

mod chars;
mod errors;
mod tokenizer;
mod options;
mod parser;
mod json;
mod emit;
pub mod ast;
mod decode;
pub mod validate;
mod sky;
#[cfg(test)] mod test_errors;
