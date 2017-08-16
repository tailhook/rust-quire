//! Yaml-based configuration parsing and validation library
//!
//! # Example
//!
//! ```rust,ignore  # for some reason this crashes compiler
//! extern crate quire;
//! #[macor_use] extern crate serde_derive;
//! use quire::{parse_config, Options};
//! use quire::validate::{Structure, Scalar};
//!
//! #[derive(Deserialize)]
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
extern crate humantime;
extern crate humannum;
extern crate num_traits;
#[macro_use] extern crate quick_error;
#[cfg(test)] #[macro_use] extern crate serde_derive;
#[cfg(feature="regex_expressions")] extern crate regex as re;

pub use sky::{parse_config, parse_string};
pub use options::{Options, Include};
pub use errors::{Error, ErrorList, ErrorCollector};
pub use tokenizer::{Pos};
pub use parser::{parse as raw_parse};
//pub use emit::{emit_ast, emit_object};

mod chars;
mod errors;
mod tokenizer;
mod options;
mod parser;
//mod emit;
mod de;
mod sky;
pub mod ast;
pub mod validate;
pub mod duration;
#[cfg(feature="regex_expressions")] pub mod regex;
#[cfg(feature="json")] mod json;
#[cfg(test)] mod test_errors;
#[cfg(test)] mod test_util;
