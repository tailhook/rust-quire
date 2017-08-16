//! A module that parses human-friendly duration
//!
//! # Example
//!
//! ```rust
//!
//! # extern crate quire;
//! # extern crate serde;
//! # #[macro_use] extern crate serde_derive;
//! # use std::time::Duration;
//!
//! #[derive(Serialize)]
//! struct SomeStruct {
//!     #[serde(with="quire::duration")]
//!     timeout: Duration,
//! }
//!
//! # fn main() {}
//!
//! ```

use std::fmt;
use std::time::Duration;

use humantime::parse_duration;
use serde::ser::{Serializer, Serialize};
use serde::de::{Deserializer, Error, Visitor};

struct DurationVisitor;

pub fn serialize<S>(duration: &Duration, s: S) -> Result<S::Ok, S::Error>
    where S: Serializer
{
    // TODO(tailhook) make nicers serialization
    let secs = duration.as_secs();
    let nanos = duration.subsec_nanos();
    if nanos > 0 {
        format!("{}s {}ns", secs, nanos).serialize(s)
    } else {
        format!("{}s", secs).serialize(s)
    }
}

impl<'a> Visitor<'a> for DurationVisitor {
    type Value = Duration;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("duration (like `1h 12m 35sec`)")
    }

    fn visit_str<E>(self, val: &str) -> Result<Self::Value, E>
        where E: Error
    {
        parse_duration(val).map_err(E::custom)
    }
}

pub fn deserialize<'x, D>(d: D) -> Result<Duration, D::Error>
    where D: Deserializer<'x>
{
    d.deserialize_str(DurationVisitor)
}
