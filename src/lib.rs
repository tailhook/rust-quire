//! Yaml-based configuration parsing and validation library
//!
//! # Example
//!
//! ```rust,ignore  # for some reason this crashes compiler
//! extern crate quire;
//! #[macro_use] extern crate serde_derive;
//! use quire::{parse_config, Options};
//! use quire::validate::{Structure, Scalar};
//!
//! #[derive(Deserialize)]
//! #[allow(dead_code)]
//! struct Config {
//!     item1: String,
//!     item2: Option<String>,
//! }
//!
//! fn validator() -> Structure<'static> {
//!     Structure::new()
//!     .member("item1", Scalar::new())
//!     .member("item2", Scalar::new().optional())
//! }
//!
//! fn work(cfg: &Config) {
//!     println!("item1 is {}.", cfg.item1);
//!     //intln!("item2 is {}.", cfg.item2);
//!     // hey, this is just demonstration code ...
//! }
//!
//! fn main() {
//!     let cfg: Config;
//!     cfg = parse_config("config.yaml", &validator(), &Options::default())
//!         .expect("valid config");
//!     work(&cfg)
//! }
//! ```
//!
#![warn(missing_debug_implementations)]


extern crate humannum;
extern crate num_traits;
extern crate serde;
#[cfg(test)] #[macro_use] extern crate serde_derive;
#[cfg(test)] extern crate serde_humantime;
#[cfg(test)] extern crate serde_json;
#[cfg(test)] extern crate serde_transcode;
#[macro_use] extern crate quick_error;

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
#[cfg(test)] mod test_errors;
#[cfg(test)] mod test_transcode;
