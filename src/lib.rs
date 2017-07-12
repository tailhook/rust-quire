//! Yaml-based configuration parsing and validation library
//!
//! # Example
//!
//! ```rust,ignore  # for some reason this crashes compiler
//! extern crate quire;
//! extern crate rustc_serialize;
//! use quire::{parse_config, Options};
//! use quire::validate::{Structure, Scalar};
//!
//! #[derive(RustcDecodable)]
//! struct Config {
//!     item1: String,
//!     item2: Option<String>,
//! }
//!
//! fn validator<'static>() -> Structure<'static> {
//!     Structure::new()
//!     .member("item1", Scalar::new())
//!     .member("item2", Scalar::new().optional())
//! }
//!
//! let cfg: Config;
//! cfg = parse_config("config.yaml", &validator(), &Options::default())
//!       .expect("valid config");
//!
//! ```
//!
//#![warn(missing_docs)]
#![allow(dead_code)]
#![allow(unused_variables)]


extern crate serde;
extern crate rustc_serialize;
extern crate regex;
extern crate humantime;
extern crate humannum;
extern crate num_traits;
#[macro_use] extern crate quick_error;
#[cfg(test)] #[macro_use] extern crate serde_derive;

pub use sky::{parse_config, parse_string};
pub use options::{Options, Include};
pub use errors::{Error, ErrorList, ErrorCollector};
pub use tokenizer::{Pos};
pub use parser::{parse as raw_parse};
pub use emit::{emit_ast, emit_object};
pub use special_cases::De;

mod chars;
mod errors;
mod tokenizer;
mod options;
mod parser;
mod json;
mod emit;
pub mod ast;
mod decode;
mod de;
pub mod validate;
mod sky;
mod special_cases;
#[cfg(test)] mod test_errors;
#[cfg(test)] mod test_util;
