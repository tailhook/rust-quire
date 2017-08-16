//! A module that parses regex from string
//!
//! # Example
//!
//! ```rust
//!
//! # extern crate quire;
//! # extern crate serde;
//! # extern crate regex;
//! # #[macro_use] extern crate serde_derive;
//!
//! #[derive(Serialize)]
//! struct SomeStruct {
//!     #[serde(with="quire::regex")]
//!     regex: regex::Regex,
//! }
//!
//! # fn main() {}
//!
//! ```

use std::fmt;
use re::Regex;

use serde::ser::{Serializer, Serialize};
use serde::de::{Deserializer, Error, Visitor};

struct RegexVisitor;

pub fn serialize<S>(re: &Regex, s: S) -> Result<S::Ok, S::Error>
    where S: Serializer
{
    re.as_str().serialize(s)
}

impl<'a> Visitor<'a> for RegexVisitor {
    type Value = Regex;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("regular expression")
    }

    fn visit_str<E>(self, val: &str) -> Result<Self::Value, E>
        where E: Error
    {
        Regex::new(val).map_err(E::custom)
    }
}

pub fn deserialize<'x, D>(d: D) -> Result<Regex, D::Error>
    where D: Deserializer<'x>
{
    d.deserialize_str(RegexVisitor)
}
