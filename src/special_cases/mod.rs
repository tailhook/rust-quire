//! This module contains a wrapper for types that is used to provide
//! custom implementation for deserialization for things that either
//! have no Decodable implemented or maybe have wrong
//!
use std::borrow::Borrow;
use std::ops::Deref;

mod regex;
mod duration;

/// A wrapper around type that has Decodable implementation
///
/// This is a temporary solution that will go with the releasing of
/// macros 1.1 (and migration to serde) I think.
///
/// There are many ways to convert this to the real value:
///
/// * Deref value `*x`
/// * Default converter `x.into()` or `x.clone().into()`
/// * Convert to reference `x.as_ref()`
///
/// I.e. it many cases it should work seamlessly instead of the reference
/// to encopassed original type
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct De<T: ?Sized>(T);

impl<T> De<T> {
    pub fn new(val: T) -> De<T> {
        De(val)
    }
}

impl<T: ?Sized> Deref for De<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T: ?Sized, U> AsRef<U> for De<T>
    where T: AsRef<U>
{
    fn as_ref(&self) -> &U {
        self.0.as_ref()
    }
}

impl<T> From<T> for De<T> {
    fn from(value: T) -> De<T> {
        De(value)
    }
}
