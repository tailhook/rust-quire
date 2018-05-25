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


extern crate humannum;
extern crate humantime;
extern crate num_traits;
extern crate serde;
#[cfg(test)] #[macro_use] extern crate serde_derive;
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
pub mod duration;
#[cfg(test)] mod test_errors;
#[cfg(test)] mod test_transcode;
