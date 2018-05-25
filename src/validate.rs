//! Validators for your config
//!
//! All of the validators are configures using builder/fluent pattern.
//!
//! Also note that while validator can do various different transforms to the
//! AST, but it works on the AST level, so it must put something that decoder
//! is able to decode in the result.

use std::fmt::{Display};
use std::path::{PathBuf, Path, Component};
use std::collections::{BTreeMap, HashSet};

use num_traits::PrimInt;
use humannum::parse_integer;

use super::errors::{Error, ErrorCollector};
pub use super::tokenizer::Pos;
use super::ast::Ast as A;
use super::ast::Tag as T;
use super::ast::{Ast, NullKind};
use super::ast::ScalarKind::{Quoted, Plain};


/// The trait every validator implements
pub trait Validator {
    fn validate(&self, ast: Ast, err: &ErrorCollector) -> Ast;
    fn default(&self, pos: Pos) -> Option<Ast>;
}

/// Scalar YAML value
///
/// This may be different kind of value:
///
/// * string
/// * bool
/// * path
/// * number
///
/// But some of the scalars might have better validators, for example
/// `Numeric` has minimum and maximum value as well as decodes human-friendly
/// unit values
pub struct Scalar {
    optional: bool,
    default: Option<String>,
    min_length: Option<usize>,
    max_length: Option<usize>,
}

impl Scalar {
    pub fn new() -> Scalar {
        Scalar {
            optional: false,
            default: None,
            min_length: None,
            max_length: None,
        }
    }
    pub fn optional(mut self) -> Scalar {
        self.optional = true;
        self
    }
    pub fn default<S:ToString>(mut self, value: S) -> Scalar {
        self.default = Some(value.to_string());
        self
    }
    pub fn min_length(mut self, len: usize) -> Scalar {
        self.min_length = Some(len);
        self
    }
    pub fn max_length(mut self, len: usize) -> Scalar {
        self.max_length = Some(len);
        self
    }
}

impl Validator for Scalar {
    fn default(&self, pos: Pos) -> Option<Ast> {
        if self.default.is_none() && self.optional {
            return Some(A::Null(pos.clone(), T::NonSpecific, NullKind::Implicit));
        }
        self.default.as_ref().map(|val| {
            A::Scalar(pos.clone(), T::NonSpecific, Quoted, val.clone()) })
    }
    fn validate(&self, ast: Ast, err: &ErrorCollector) -> Ast {
        let (pos, kind, val) = match ast {
            A::Scalar(pos, _, kind, string) => {
                (pos, kind, string)
            }
            A::Null(_, _, _) if self.optional => {
                return ast;
            }
            ast => {
                err.add_error(Error::validation_error(&ast.pos(),
                    format!("Value must be scalar")));
                return ast;
            }
        };
        self.min_length.map(|minl| {
            if val.len() < minl {
                err.add_error(Error::validation_error(&pos,
                    format!("Value must be at least {} characters", minl)));
            }
        });
        self.max_length.map(|maxl| {
            if val.len() > maxl {
                err.add_error(Error::validation_error(&pos,
                    format!("Value must be at most {} characters", maxl)));
            }
        });
        return A::Scalar(pos, T::NonSpecific, kind, val);
    }
}

/// Numeric validator
///
/// Similar to `Scalar` but validates that value is a number and also allows
/// limit the range of the value.
pub struct Numeric<T:PrimInt=i64> {
    optional: bool,
    default: Option<T>,
    min: Option<T>,
    max: Option<T>,
}

impl<T: PrimInt> Numeric<T> {
    pub fn new() -> Numeric<T> {
        Numeric {
            optional: false,
            default: None,
            min: None,
            max: None,
        }
    }
    pub fn optional(mut self) -> Numeric<T> {
        self.optional = true;
        self
    }
    pub fn default(mut self, value: T) -> Numeric<T> {
        self.default = Some(value);
        self
    }
    pub fn min(mut self, val: T) -> Numeric<T> {
        self.min = Some(val);
        self
    }
    pub fn max(mut self, val: T) -> Numeric<T> {
        self.max = Some(val);
        self
    }
}

impl Validator for Numeric {

    fn default(&self, pos: Pos) -> Option<Ast> {
        if self.default.is_none() && self.optional {
            return Some(A::Null(pos.clone(), T::NonSpecific, NullKind::Implicit));
        }
        self.default.as_ref().map(|val| {
            A::Scalar(pos.clone(), T::NonSpecific, Quoted, val.to_string())
        })
    }
    fn validate(&self, ast: Ast, err: &ErrorCollector) -> Ast {
        let (pos, val): (Pos, i64)  = match ast {
            A::Scalar(pos, tag, kind, string)
            => match parse_integer(&string) {
                Ok(val) => (pos, val),
                Err(e) => {
                    err.add_error(Error::validation_error(&pos,
                        format!("number error: {}", e)));
                    return A::Scalar(pos, tag, kind, string);
                }
            },
            A::Null(_, _, _) if self.optional => {
                return ast;
            }
            ast => {
                err.add_error(Error::validation_error(&ast.pos(),
                    format!("Value must be scalar")));
                return ast;
            }
        };
        self.min.as_ref().map(|min| {
            if val < *min {
                err.add_error(Error::validation_error(&pos,
                    format!("Value must be at least {}", min)));
            }
        });
        self.max.as_ref().map(|max| {
            if val > *max {
                err.add_error(Error::validation_error(&pos,
                    format!("Value must be at most {}", max)));
            }
        });
        return A::Scalar(pos, T::NonSpecific, Plain, val.to_string());
    }
}


/// Directory validator
///
/// Similar to `Scalar` but also allows to force absolute or relative paths
pub struct Directory {
    optional: bool,
    default: Option<PathBuf>,
    absolute: Option<bool>,
}

impl Directory {
    pub fn new() -> Directory {
        Directory {
            optional: false,
            default: None,
            absolute: None,
        }
    }
    pub fn optional(mut self) -> Directory {
        self.optional = true;
        self
    }
    pub fn default<P:AsRef<Path>>(mut self, value: P) -> Directory {
        self.default = Some(value.as_ref().to_path_buf());
        self
    }
    pub fn absolute(mut self, value: bool) -> Directory {
        self.absolute = Some(value);
        self
    }
}

impl Validator for Directory {
    fn default(&self, pos: Pos) -> Option<Ast> {
        if self.default.is_none() && self.optional {
            return Some(A::Null(pos.clone(), T::NonSpecific, NullKind::Implicit));
        }
        self.default.as_ref().map(|val| {
            A::Scalar(pos.clone(), T::NonSpecific, Quoted,
                      val.display().to_string()) })
    }
    fn validate(&self, ast: Ast, err: &ErrorCollector) -> Ast {
        let (pos, kind, val) = match ast {
            A::Scalar(pos, _, kind, string) => {
                (pos, kind, string)
            }
            A::Null(_, _, _) if self.optional => {
                return ast;
            }
            ast => {
                err.add_error(Error::validation_error(&ast.pos(),
                    format!("Path expected")));
                return ast;
            }
        };
        {
            let path = Path::new(&val);
            match self.absolute {
                Some(true) => {
                    if !path.is_absolute() {
                        err.add_error(Error::validation_error(&pos,
                            format!("Path must be absolute")));
                    }
                }
                Some(false) => {
                    if path.is_absolute() {
                        err.add_error(Error::validation_error(&pos,
                            format!("Path must not be absolute")));
                    } else {
                        // Still for non-absolute paths we must check if
                        // there are ../../something
                        //
                        // If you don't want this check, just set self.absolute
                        // to None instead of Some(false)
                        for cmp in path.components() {
                            if cmp == Component::ParentDir {
                                err.add_error(Error::validation_error(&pos,
                                    format!("The /../ is not allowed in path")));
                            }
                        }
                    }
                }
                None => {}
            };
        }
        return A::Scalar(pos, T::NonSpecific, kind, val);
    }
}

/// Structure validator
///
/// In yaml terms this validates that value is a map (or a null value, if all
/// defaults are okay).
///
/// Additionally this validator allows to parse some scalar and convert it to
/// the structure. This feature is useful to upgrade scalar value to
/// a structure maintaining backwards compatiblity as well as for configuring
/// common case more easily.
pub struct Structure<'a> {
    members: Vec<(String, Box<Validator + 'a>)>,
    optional: bool,
    from_scalar: Option<fn (scalar: Ast) -> BTreeMap<String, Ast>>,
}

impl<'a> Structure<'a> {
    pub fn new() -> Structure<'a> {
        Structure {
            members: Vec::new(),
            optional: false,
            from_scalar: None,
        }
    }
    pub fn member<S: Display, V: Validator + 'a>(mut self, name: S, value: V)
        -> Structure<'a>
    {
        self.members.push((name.to_string(), Box::new(value)));
        self
    }
    pub fn optional(mut self) -> Structure<'a> {
        self.optional = true;
        self
    }
    pub fn parser(mut self,
        f: fn (scalar: Ast) -> BTreeMap<String, Ast>)
        -> Structure<'a>
    {
        self.from_scalar = Some(f);
        self
    }
}

impl<'a> Validator for Structure<'a> {
    fn default(&self, pos: Pos) -> Option<Ast> {
        if self.optional {
            return Some(A::Null(pos.clone(), T::NonSpecific, NullKind::Implicit));
        }
        let mut map = BTreeMap::new();
        for &(ref k, ref validator) in self.members.iter() {
            match validator.default(pos.clone()) {
                Some(val) => {
                    map.insert(k.clone(), val);
                }
                None => continue,
            }
        }
        return Some(A::Map(pos, T::NonSpecific, map));
    }
    fn validate(&self, ast: Ast, err: &ErrorCollector) -> Ast {
        let (pos, mut map) = match (ast, self.from_scalar) {
            (A::Map(pos, _, items), _) => {
                (pos, items)
            }
            (A::Null(pos, _, NullKind::Implicit), _) => {
                return self.default(pos).unwrap();
            }
            (ast@A::Scalar(..), Some(from_scalar)) => {
                (ast.pos(), from_scalar(ast))
            }
            (ast, _) => {
                err.add_error(Error::validation_error(&ast.pos(),
                    format!("Value must be mapping")));
                return ast;
            }
        };
        for &(ref k, ref validator) in self.members.iter() {
            let value = match map.remove(k)
                .or(map.remove(&k[..].replace("_", "-"))) {
                Some(src) => {
                    validator.validate(src, err)
                }
                None => {
                    match validator.default(pos.clone()) {
                        Some(x) => x,
                        None => {
                            err.add_error(Error::validation_error(&pos,
                                format!("Field {} is expected", k)));
                            continue;
                        }
                    }
                }
            };
            map.insert(k.clone(), value);
        }
        let mut keys: HashSet<String>;
        keys = map.keys()
            .filter(|s| !&s[..].starts_with("_"))
            .map(|s| s.clone()).collect();
        for &(ref k, _) in self.members.iter() {
            keys.remove(k);
        }
        if keys.len() > 0 {
            err.add_error(Error::validation_error(&pos,
                format!("Keys {:?} are not expected", keys)));
        }
        return A::Map(pos, T::NonSpecific, map);
    }
}

/// Enum validator
///
/// This validates that the value is enum in the rust meaning of enum.
///
/// Enumeration can contain:
///
/// * A selection from constants: `enum T {a, b, c}`,
///   if you enable `allow_plain()`, this allows to select both as scalar
///   value `a` and as a tag with null value `!a`
///
/// * A set of different values with their own data: `enum T { a(x), b(y) }`.
///   In this case value must be specified with tag `!a "hello"` and may
///   contain different types inside `!b ["x", "y"]` or `!c {x: y}`. Only
///   one enum field is supported. Structure enums `enum T { a { x: u8 }` are
///   equivalent to an option with that struct as single value
///   `struct A { x: u8 }; enum T { a(A) }`
pub struct Enum<'a> {
    options: Vec<(String, Box<Validator + 'a>)>,
    optional: bool,
    default_tag: Option<String>,
    default_value: Option<String>,
    allow_plain: bool,
}

impl<'a> Enum<'a> {
    pub fn new() -> Enum<'a> {
        Enum {
            options: Vec::new(),
            optional: false,
            default_tag: None,
            default_value: None,
            allow_plain: false,
        }
    }
    pub fn optional(mut self) -> Enum<'a> {
        self.optional = true;
        self
    }
    /// For variants that have no content (i.e. have validator of Nothing)
    pub fn allow_plain(mut self) -> Enum<'a> {
        assert!(self.default_tag.is_none(),
            "Default tag and allow_plain are not compatible");
        self.allow_plain = true;
        self
    }
    /// Set plain default value (i.e. option that have validator of Nothing)
    ///
    /// There is no way to set default to something complex at the moment
    pub fn plain_default<S: ToString>(mut self, value: S) -> Enum<'a> {
        assert!(self.allow_plain,
            "Plain default requires allow_plain to be enabled");
        self.default_value = Some(value.to_string());
        self
    }
    pub fn option<S: ToString, V: Validator + 'a>(mut self, name: S, value: V)
        -> Enum<'a>
    {
        self.options.push((name.to_string(), Box::new(value)));
        self
    }
    pub fn default_tag<S: ToString>(mut self, name: S) -> Enum<'a> {
        assert!(!self.allow_plain,
            "Default tag and allow_plain are not compatible");
        self.default_tag = Some(name.to_string());
        self
    }
}

impl<'a> Validator for Enum<'a> {
    fn default(&self, pos: Pos) -> Option<Ast> {
        if self.default_tag.is_some() && self.optional {
            return Some(A::Null(pos.clone(),
                T::LocalTag(self.default_tag.as_ref().unwrap().clone()),
                NullKind::Implicit));
        } else if self.default_value.is_some() {
            return self.default_value.as_ref().map(|val| {
                A::Scalar(pos.clone(), T::NonSpecific, Quoted, val.clone()) })
        } else if self.optional {
            return Some(A::Null(pos.clone(), T::NonSpecific,
                                NullKind::Implicit));
        }
        return None;
    }
    fn validate(&self, ast: Ast, err: &ErrorCollector) -> Ast {
        let tag_name = match ast.tag() {
            &T::LocalTag(ref tag_name) => {
                Some(tag_name.clone())
            }
            &T::NonSpecific => {
                if self.allow_plain {
                    if let A::Scalar(ref pos, _, _, ref val) = ast {
                        for &(ref k, ref validator) in self.options.iter() {
                            if &k[..] == val {
                                let value = validator.validate(
                                    A::Null(pos.clone(), T::NonSpecific,
                                            NullKind::Implicit),
                                    err);
                                return value.with_tag(
                                            T::LocalTag(k.to_string()));
                            }
                        }
                    }
                }
                if let Some(ref tag) = self.default_tag {
                    Some(tag.clone())
                } else if let Some(ref value) = self.default_value {
                    Some(value.clone())
                } else {
                    err.add_error(Error::validation_error(&ast.pos(),
                        format!("One of the tags {:?} expected",
                            self.options.iter().map(|&(ref k, _)| k)
                                .collect::<Vec<&String>>())));
                    None
                }
            }
            _ => unimplemented!(),
        };
        if let Some(tag_name) = tag_name {
            let pos = ast.pos().clone();
            for &(ref k, ref validator) in self.options.iter() {
                if &k[..] == &tag_name[..] {
                    let value = validator.validate(ast, err);
                    return value.with_tag(T::LocalTag(tag_name));
                }
            }
            err.add_error(Error::validation_error(&pos,
                format!("The tag {} is not expected", tag_name)));
        }
        return ast;
    }
}

/// Validates yaml mapping
///
/// This type has type for a key and value and also can be converted
/// from scalar as shortcut.
pub struct Mapping<'a> {
    key_element: Box<Validator + 'a>,
    value_element: Box<Validator + 'a>,
    from_scalar: Option<fn (scalar: Ast) -> BTreeMap<String, Ast>>,
}

impl<'a> Mapping<'a> {
    pub fn new<V: Validator + 'a, W: Validator + 'a>(key: V, val: W)
        -> Mapping<'a>
    {
        Mapping {
            key_element: Box::new(key),
            value_element: Box::new(val),
            from_scalar: None,
        }
    }
    pub fn parser(mut self,
        f: fn (scalar: Ast) -> BTreeMap<String, Ast>)
        -> Mapping<'a>
    {
        self.from_scalar = Some(f);
        self
    }
}

impl<'a> Validator for Mapping<'a> {
    fn default(&self, pos: Pos) -> Option<Ast> {
        return Some(A::Map(pos, T::NonSpecific, BTreeMap::new()));
    }
    fn validate(&self, ast: Ast, err: &ErrorCollector) -> Ast {
        let (pos, map) = match (ast, self.from_scalar) {
            (A::Map(pos, _, items), _) => {
                (pos, items)
            }
            (A::Null(pos, _, NullKind::Implicit), _) => {
                return A::Map(pos, T::NonSpecific, BTreeMap::new());
            }
            (ast@A::Scalar(..), Some(from_scalar)) => {
                (ast.pos(), from_scalar(ast))
            }
            (ast, _) => {
                err.add_error(Error::validation_error(&ast.pos(),
                    format!("Value must be mapping")));
                return ast;
            }
        };
        let mut res = BTreeMap::new();
        for (k, v) in map.into_iter() {
            let key = match self.key_element.validate(
                A::Scalar(v.pos().clone(), T::NonSpecific, Plain, k), err) {
                A::Scalar(_, _, _, val) => val,
                _ => unreachable!(),
            };
            let value = self.value_element.validate(v, err);
            res.insert(key, value);
        }
        return A::Map(pos, T::NonSpecific, res);
    }
}

/// Validates yaml sequence
///
/// Every element must be of single type, but it maybe an enum too.
///
/// This validator can also parse a scalar and convert it into a list in
/// application-specific way.
pub struct Sequence<'a> {
    element: Box<Validator + 'a>,
    from_scalar: Option<fn (scalar: Ast) -> Vec<Ast>>,
    min_length: usize,
}

impl<'a> Sequence<'a> {
    pub fn new<V: Validator + 'a>(el: V) -> Sequence<'a> {
        Sequence {
            element: Box::new(el),
            from_scalar: None,
            min_length: 0,
        }
    }
    pub fn min_length(mut self, len: usize) -> Sequence<'a> {
        self.min_length = len;
        self
    }
    pub fn parser(mut self, f: fn (scalar: Ast) -> Vec<Ast>) -> Sequence<'a> {
        self.from_scalar = Some(f);
        self
    }
}

impl<'a> Validator for Sequence<'a> {
    fn default(&self, pos: Pos) -> Option<Ast> {
        match self.min_length {
            0 => Some(A::Seq(pos, T::NonSpecific, Vec::new())),
            _ => None,
        }
    }
    fn validate(&self, ast: Ast, err: &ErrorCollector) -> Ast {
        let (pos, children) = match (ast, self.from_scalar) {
            (A::Seq(pos, _, items), _) => {
                (pos, items)
            }
            (A::Null(pos, _, NullKind::Implicit), _) => {
                if self.min_length > 0 {
                    err.add_error(Error::validation_error(&pos,
                        format!("Expected sequence with at least {} element(s)",
                                self.min_length)));
                }
                return A::Seq(pos, T::NonSpecific, Vec::new());
            }
            (ast@A::Scalar(_, _, _, _), Some(fun)) => {
                (ast.pos().clone(), fun(ast))
            }
            (ast, _) => {
                err.add_error(Error::validation_error(&ast.pos(),
                    format!("Value must be sequence")));
                return ast;
            }
        };
        let mut res = Vec::new();
        for val in children.into_iter() {
            let value = self.element.validate(val, err);
            res.push(value);
        }
        if self.min_length > 0 && res.len() < self.min_length {
            err.add_error(Error::validation_error(&pos,
                format!("Expected sequence with at least {} element(s)",
                        self.min_length)));
        };
        return A::Seq(pos, T::NonSpecific, res);
    }
}

/// Skips the validation of this value
///
/// It's useful to accept any value (of any type) at some place, or to
/// rely on `Deserialize::deserialize` for doing validation.
pub struct Anything;

impl Validator for Anything {
    fn default(&self, pos: Pos) -> Option<Ast> {
        return Some(A::Null(pos.clone(), T::NonSpecific, NullKind::Implicit));
    }
    fn validate(&self, ast: Ast, _err: &ErrorCollector) -> Ast {
        return ast;
    }
}

/// Only expect null at this place
///
/// This is mostly useful for enums, i.e. `!SomeTag null`
pub struct Nothing;

impl Validator for Nothing {
    fn default(&self, _: Pos) -> Option<Ast> {
        return None;
    }
    fn validate(&self, ast: Ast, err: &ErrorCollector) -> Ast {
        if let A::Null(_, _, _) = ast {
        } else {
            err.add_error(Error::parse_error(&ast.pos(),
                format!("Null expected, {} found", ast)));
        }
        return ast;
    }
}


#[cfg(test)]
mod test {
    use std::fmt;
    use std::rc::Rc;
    use std::path::PathBuf;
    use std::collections::BTreeMap;
    use std::collections::HashMap;
    use std::error::Error as StdError;

    use serde::Deserialize;

    use {Options};
    use de::Deserializer;
    use ast::{process, Ast as A};
    use ast::Tag::{NonSpecific};
    use ast::ScalarKind::{Plain};
    use parser::parse;
    use {parse_string, ErrorList};
    use validate::{Validator, Structure, Scalar, Numeric, Mapping, Sequence};
    use validate::{Enum, Nothing, Directory, Anything};
    use errors::{Error, ErrorCollector};
    use self::TestEnum::*;
    use super::Pos;

    #[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
    struct TestStruct {
        intkey: usize,
        strkey: String,
    }

    fn parse_str(body: &str) -> TestStruct {
        let str_val = Structure::new()
            .member("intkey", Numeric::new().default(123))
            .member("strkey", Scalar::new().default("default_value"));
        parse_string("<inline text>", body, &str_val, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_all_fields() {
        assert_eq!(parse_str("intkey: 1\nstrkey: test"), TestStruct {
            intkey: 1,
            strkey: "test".to_string(),
        });
    }

    #[test]
    fn test_no_fields() {
        assert_eq!(parse_str("{}"), TestStruct {
            intkey: 123,
            strkey: "default_value".to_string(),
        });
    }

    #[test]
    fn test_only_int() {
        assert_eq!(parse_str("intkey: 777"), TestStruct {
            intkey: 777,
            strkey: "default_value".to_string(),
        });
    }

    #[test]
    fn test_only_str() {
        assert_eq!(parse_str("strkey: strvalue"), TestStruct {
            intkey: 123,
            strkey: "strvalue".to_string(),
        });
    }

    #[test]
    fn test_unit() {
        assert_eq!(parse_str("intkey: 100M"), TestStruct {
            intkey: 100000000,
            strkey: "default_value".to_string(),
        });
    }

    fn parse_extra(body: &str) -> TestStruct {
        let str_val = Structure::new()
            .member("intkey", Numeric::new().default(123))
            .member("extra", Anything)
            .member("strkey", Scalar::new().default("default_value"));
        parse_string("<inline text>", body, &str_val, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_extra_exists() {
        assert_eq!(parse_extra("extra: 1"), TestStruct {
            intkey: 123,
            strkey: "default_value".to_string(),
        });
    }

    #[test]
    fn test_no_extra() {
        assert_eq!(parse_extra("intkey: 234"), TestStruct {
            intkey: 234,
            strkey: "default_value".to_string(),
        });
    }

    #[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
    struct TestDash {
        some_key: usize,
    }

    fn parse_dash_str(body: &str) -> TestDash {
        let str_val = Structure::new()
            .member("some_key", Numeric::new().default(123));
        parse_string("<inline text>", body, &str_val, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_dash_str() {
        assert_eq!(parse_dash_str("some-key: 13"), TestDash {
            some_key: 13,
        });
    }

    #[test]
    fn test_underscore_str() {
        assert_eq!(parse_dash_str("some_key: 7"), TestDash {
            some_key: 7,
        });
    }

    fn parse_with_warnings(body: &str) -> (TestDash, Vec<String>) {
        let str_val = Structure::new()
            .member("some_key", Numeric::new().default(123));
        let err = ErrorCollector::new();
        let ast = parse(
                Rc::new("<inline text>".to_string()),
                body,
                |doc| { process(&Options::default(), doc, &err) }
            ).map_err(|e| err.into_fatal(e)).unwrap();
        let ast = str_val.validate(ast, &err);
        match Deserialize::deserialize(&mut Deserializer::new(&ast, &err)) {
            Ok(val) => {
                (val, err.unwrap().errors().map(|x| x.to_string()).collect())
            }
            Err(e) => {
                panic!("{}", err.into_fatal(e));
            }
        }
    }

    #[test]
    fn test_underscore_keys() {
        assert_eq!(parse_with_warnings("some-key: 13\n_another-key: 12"),
            (TestDash {
                some_key: 13,
            }, vec!()));
    }

    #[test]
    fn test_additional_keys() {
        assert_eq!(parse_with_warnings("some-key: 13\nanother-key: 12"),
            (TestDash {
                some_key: 13,
            }, vec!("<inline text>:1:1: Validation Error: \
                 Keys {\"another-key\"} are not expected"
                 .to_string())));
    }

    #[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
    struct TestOpt {
        some_key: Option<usize>,
    }

    fn parse_opt_str(body: &str) -> TestOpt {
        let str_val = Structure::new()
            .member("some_key", Numeric::new().optional());
        parse_string("<inline text>", body, &str_val, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_opt_val() {
        assert_eq!(parse_opt_str("some-key: 13"), TestOpt {
            some_key: Some(13),
        });
    }

    #[test]
    fn test_opt_none() {
        assert_eq!(parse_opt_str("some_key:"), TestOpt {
            some_key: None,
        });
    }

    #[test]
    fn test_opt_no_key() {
        assert_eq!(parse_opt_str("{}"), TestOpt {
            some_key: None,
        });
    }

    fn parse_map<'x, T: Deserialize<'x>>(body: &str) -> T {
        fn parse_default(ast: A) -> BTreeMap<String, A> {
            match ast {
                A::Scalar(pos, _, style, value) => {
                    let mut map = BTreeMap::new();
                    map.insert("default_value".to_string(),
                        A::Scalar(pos.clone(), NonSpecific, style, value));
                    map
                },
                _ => unreachable!(),
            }
        }

        let validator = Mapping::new(
            Scalar::new(),
            Numeric::new().default(0)
        ).parser(parse_default);
        parse_string("<inline text>", body, &validator, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_map_1() {
        let mut m = BTreeMap::new();
        m.insert("a".to_string(), 1);
        let res: BTreeMap<String, usize> = parse_map("a: 1");
        assert_eq!(res, m);
    }

    #[test]
    fn test_map_2() {
        let mut m = BTreeMap::new();
        m.insert("a".to_string(), 1);
        m.insert("bc".to_string(), 2);
        let res: BTreeMap<String, usize> = parse_map("a: 1\nbc: 2");
        assert_eq!(res, m);
    }

    #[test]
    fn test_hash_map() {
        let mut m = HashMap::new();
        m.insert("a".to_string(), 1);
        m.insert("bc".to_string(), 2);
        let res: HashMap<String, usize> = parse_map("a: 1\nbc: 2");
        assert_eq!(res, m);
    }

    #[test]
    fn test_map_empty() {
        let m = BTreeMap::new();
        let res: BTreeMap<String, usize> = parse_map("{}");
        assert_eq!(res, m);
    }

    #[test]
    fn test_map_null() {
        let m = BTreeMap::new();
        let res: BTreeMap<String, usize> = parse_map("");
        assert_eq!(res, m);
    }

    #[test]
    fn test_map_with_parser() {
        let mut m = HashMap::new();
        m.insert("default_value".to_string(), 404);
        let res: HashMap<String, usize> = parse_map("404");
        assert_eq!(res, m);
    }

    fn parse_complex_map<'x, T: Deserialize<'x>>(body: &str) -> T {
        let validator = Mapping::new(
            Scalar::new(),
            Structure::new()
                .member("some_key", Numeric::new().default(123)));
        parse_string("<inline text>", body, &validator, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_cmap_1() {
        let mut m = BTreeMap::new();
        m.insert("a".to_string(), TestDash {
            some_key: 13,
        });
        let res: BTreeMap<String, TestDash>;
        res = parse_complex_map("a:\n some_key: 13");
        assert_eq!(res, m);
    }

    #[test]
    fn test_cmap_2() {
        let mut m = BTreeMap::new();
        m.insert("a".to_string(), TestDash {
            some_key: 123,
        });
        let res: BTreeMap<String, TestDash>;
        res = parse_complex_map("a:\n");
        assert_eq!(res, m);
    }

    #[test]
    fn test_cmap_3() {
        let mut m = BTreeMap::new();
        m.insert("a".to_string(), TestDash {
            some_key: 123,
        });
        let res: BTreeMap<String, TestDash> = parse_complex_map("a: {}");
        assert_eq!(res, m);
    }

    fn parse_seq(body: &str) -> Vec<usize> {
        fn split(ast: A) -> Vec<A> {
            match ast {
                A::Scalar(pos, _, style, value) => {
                    value
                        .split(" ")
                        .map(|v| {
                            A::Scalar(pos.clone(), NonSpecific, Plain, v.to_string())
                        })
                        .collect::<Vec<_>>()
                },
                _ => unreachable!(),
            }
        }

        let validator = Sequence::new(Numeric::new()).parser(split);
        parse_string("<inline text>", body, &validator, &Options::default())
        .unwrap()
    }

    fn parse_seq_min_length(body: &str, len: usize)
        -> Result<Vec<usize>, ErrorList>
    {
        fn split(ast: A) -> Vec<A> {
            match ast {
                A::Scalar(pos, _, style, value) => {
                    value
                        .split(" ")
                        .map(|v| {
                            A::Scalar(pos.clone(), NonSpecific, Plain, v.to_string())
                        })
                        .collect::<Vec<_>>()
                },
                _ => unreachable!(),
            }
        }

        let validator = Sequence::new(Numeric::new())
            .min_length(len).parser(split);
        parse_string("<inline text>", body, &validator, &Options::default())
    }

    #[test]
    fn test_seq_1() {
        let m = vec!(1);
        let res: Vec<usize> = parse_seq("- 1");
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_2() {
        let m = vec!(1, 2);
        let res: Vec<usize> = parse_seq("- 1\n- 2");
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_empty() {
        let m = Vec::<usize>::new();
        let res: Vec<usize> = parse_seq("[]");
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_null() {
        let m = Vec::<usize>::new();
        let res: Vec<usize> = parse_seq("");
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_parser() {
        let m = vec!(1, 2, 3);
        let res: Vec<usize> = parse_seq("1 2 3");
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_min_length() {
        let m = vec!(1);
        let res: Vec<usize> = parse_seq_min_length("- 1", 1).unwrap();
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_min_length_zero() {
        let m = Vec::<usize>::new();
        let res: Vec<usize> = parse_seq_min_length("[]", 0).unwrap();
        assert_eq!(res, m);

        let res: Vec<usize> = parse_seq_min_length("", 0).unwrap();
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_min_length_err() {
        let res = parse_seq_min_length("[]", 1);
        assert!(res.is_err());

        let res = parse_seq_min_length("- 1", 2);
        assert!(res.is_err());
    }

    #[test]
    fn test_numeric() {
        let m = vec!(100, 200, 300);
        let res: Vec<usize> = parse_seq("- 0o144\n- 0b11001000\n- 0x12c");
        assert_eq!(res, m);
    }

    #[derive(PartialEq, Eq, Deserialize, Debug)]
    enum TestEnum {
        Alpha,
        Beta,
        Gamma(isize),
        Delta(TestStruct),
        Epsilon(Option<TestStruct>),
    }

    fn enum_validator<'x>() -> Enum<'x> {
        Enum::new()
        .allow_plain()
        .option("Alpha", Nothing)
        .option("Beta", Nothing)
        .option("Gamma", Numeric::new().default(7).optional())
        .option("Delta", Structure::new()
            .member("intkey", Numeric::new().default(123))
            .member("strkey", Scalar::new().default("default_value")))
        .option("Epsilon", Structure::new()
            .optional()
            .member("intkey", Numeric::new().default(457))
            .member("strkey", Scalar::new().default("epsilon")))
    }

    fn parse_enum(body: &str) -> TestEnum {
        let validator = enum_validator();
        parse_string("<inline text>", body, &validator, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_enum_1() {
        assert_eq!(parse_enum("!Alpha"), Alpha);
    }

    #[test]
    fn test_enum_2() {
        assert_eq!(parse_enum("!Beta"), Beta);
    }

    #[test]
    fn test_enum_str() {
        assert_eq!(parse_enum("Alpha"), Alpha);
    }

    #[test]
    fn test_enum_3() {
        assert_eq!(parse_enum("!Alpha null"), Alpha);
    }

    #[test]
    fn test_enum_4() {
        assert_eq!(parse_enum("!Gamma 5"), Gamma(5));
    }

    #[test]
    #[should_panic]
    fn test_enum_5() {
        parse_enum("!Gamma");
    }

    #[test]
    fn test_enum_6() {
        assert_eq!(parse_enum("!Delta\nintkey: 1\nstrkey: a"),
            Delta(TestStruct {
                intkey: 1,
                strkey: "a".to_string(),
            }));
    }

    #[test]
    fn test_enum_7() {
        assert_eq!(parse_enum("!Delta"), Delta(TestStruct {
            intkey: 123,
            strkey: "default_value".to_string(),
            }));
    }

    #[test]
    fn test_enum_opt_struct() {
        assert_eq!(parse_enum("!Epsilon\nintkey: 457\nstrkey: a"),
            Epsilon(Some(TestStruct {
                intkey: 457,
                strkey: "a".to_string(),
            })));
    }

    #[test]
    fn test_enum_opt_struct_2() {
        assert_eq!(parse_enum("!Epsilon"), Epsilon(None));
    }

    #[test]
    fn test_enum_opt_struct_3() {
        assert_eq!(parse_enum("!Epsilon {}"),
            Epsilon(Some(TestStruct {
                intkey: 457,
                strkey: "epsilon".to_string(),
            })));
    }

    #[derive(Clone, PartialEq, Eq, Deserialize)]
    struct TestPath {
        path: PathBuf,
    }

    fn parse_path(body: &str, abs: Option<bool>) -> TestPath {
        let mut dir = Directory::new().default("/test");
        if let Some(abs) = abs {
            dir = dir.absolute(abs);
        }
        let str_val = Structure::new().member("path", dir);
        parse_string("<inline text>", body, &str_val, &Options::default())
        .unwrap()
    }

    #[test]
    #[should_panic(expected = "Path expected")]
    fn test_path_null() {
        assert!(parse_path("path:", None) == TestPath {
            path: PathBuf::from("/test"),
        });
    }

    #[test]
    fn test_path_abs() {
        assert!(parse_path("path: /root/dir", None) == TestPath {
            path: PathBuf::from("/root/dir"),
        });
    }

    #[test]
    fn test_path_rel() {
        assert!(parse_path("path: root/dir", None) == TestPath {
            path: PathBuf::from("root/dir"),
        });
    }

    #[test]
    fn test_path_down() {
        assert!(parse_path("path: ../root/dir", None) == TestPath {
            path: PathBuf::from("../root/dir"),
        });
    }

    #[test]
    #[cfg(unix)]
    fn test_path_abs_abs() {
        assert!(parse_path("path: /root/dir", Some(true)) == TestPath {
            path: PathBuf::from("/root/dir"),
        });
    }

    #[test]
    #[cfg(windows)]
    fn test_path_abs_abs() {
        assert!(parse_path(r#"path: c:\\root\dir"#, Some(true)) == TestPath {
            path: PathBuf::from(r#"c:\\root\dir"#),
        });
    }

    #[test]
    #[should_panic(expected = "must be absolute")]
    fn test_path_rel_abs() {
        assert!(parse_path("path: root/dir", Some(true)) == TestPath {
            path: PathBuf::from("root/dir"),
        });
    }

    #[test]
    #[should_panic(expected = "must be absolute")]
    fn test_path_down_abs() {
        assert!(parse_path("path: ../root/dir", Some(true)) == TestPath {
            path: PathBuf::from("../root/dir"),
        });
    }

    #[test]
    #[cfg(unix)]
    #[should_panic(expected = "must not be absolute")]
    fn test_path_abs_rel() {
        assert!(parse_path("path: /root/dir", Some(false)) == TestPath {
            path: PathBuf::from("/root/dir"),
        });
    }

    #[test]
    #[cfg(windows)]
    #[should_panic(expected = "must not be absolute")]
    fn test_path_abs_rel() {
        assert!(parse_path(r#"path: c:\\root\dir"#, Some(false)) == TestPath {
            path: PathBuf::from(r#":\\root\dir"#),
        });
    }

    #[test]
    fn test_path_rel_rel() {
        assert!(parse_path("path: root/dir", Some(false)) == TestPath {
            path: PathBuf::from("root/dir"),
        });
    }

    #[test]
    #[should_panic(expected = "/../ is not allowed")]
    fn test_path_down_rel() {
        assert!(parse_path("path: ../root/dir", Some(false)) == TestPath {
            path: PathBuf::from("../root/dir"),
        });
    }

    fn parse_enum_list(body: &str) -> Vec<TestEnum> {
        let validator = Sequence::new(enum_validator());
        parse_string("<inline text>", body, &validator, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_enum_list_null() {
        assert_eq!(parse_enum_list("- !Delta\n- !Epsilon"), vec!(
            Delta(TestStruct {
                intkey: 123,
                strkey: "default_value".to_string(),
                }),
            Epsilon(None)
            ));
    }

    #[derive(PartialEq, Eq, Deserialize, Debug)]
    struct EnumOpt {
        val: Option<TestEnum>,
    }

    fn parse_enum_opt(body: &str) -> EnumOpt {
        let validator = Structure::new()
            .member("val", enum_validator().optional());
        parse_string("<inline text>", body, &validator, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_enum_val_none() {
        assert_eq!(parse_enum_opt("{}"), EnumOpt { val: None });
    }

    #[test]
    fn test_enum_val_some() {
        assert_eq!(parse_enum_opt("val: !Epsilon"),
                   EnumOpt { val: Some(Epsilon(None)) });
    }

    #[derive(PartialEq, Eq, Deserialize, Debug)]
    struct Parsed {
        value: String,
    }

    fn parse_struct_with_parser(body: &str) -> Parsed {
        fn value_parser(ast: A) -> BTreeMap<String, A> {
            match ast {
                A::Scalar(pos, _, style, value) => {
                    let mut map = BTreeMap::new();
                    map.insert("value".to_string(),
                        A::Scalar(pos.clone(), NonSpecific, style, value));
                    map
                }
                _ => unreachable!(),
            }
        }

        let validator = Structure::new()
            .member("value", Scalar::new())
            .parser(value_parser);
        parse_string("<inline text>", body, &validator, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_struct_with_parser() {
        assert_eq!(parse_struct_with_parser("value: test"),
                   Parsed { value: "test".to_string() });
    }

    #[test]
    fn test_struct_with_parser_custom() {
        assert_eq!(parse_struct_with_parser("test"),
                   Parsed { value: "test".to_string() });
    }

    #[derive(PartialEq, Eq, Deserialize, Debug)]
    enum TestEnumDef {
        Alpha,
        Beta,
    }

    fn enum_def_validator<'x>() -> Enum<'x> {
        Enum::new()
        .allow_plain()
        .plain_default("Beta")
        .option("Alpha", Nothing)
        .option("Beta", Nothing)
    }

    fn parse_enum_def(body: &str) -> TestEnumDef {
        let validator = enum_def_validator();
        parse_string("<inline text>", body, &validator, &Options::default())
        .unwrap()
    }

    #[test]
    fn test_enum_def_default() {
        assert_eq!(parse_enum_def(""), TestEnumDef::Beta);
    }

    #[test]
    fn test_enum_def_norm() {
        assert_eq!(parse_enum_def("Alpha"), TestEnumDef::Alpha);
    }

    #[test]
    fn test_enum_def_same_as_def() {
        assert_eq!(parse_enum_def("Beta"), TestEnumDef::Beta);
    }

    #[test]
    fn test_enum_def_tag() {
        assert_eq!(parse_enum_def("!Alpha"), TestEnumDef::Alpha);
    }

    #[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
    struct Version;

    #[derive(Debug)]
    struct VersionError(&'static str);

    impl StdError for VersionError {
        fn description(&self) -> &str { "Version Error" }
        fn cause(&self) -> Option<&StdError> { None }
    }

    impl fmt::Display for VersionError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}: {}", self.description(), self.0)
        }
    }

    impl Version {
        fn new() -> Version {
            Version {}
        }
    }

    impl Validator for Version {
        fn default(&self, pos: Pos) -> Option<A> {
            None
        }

        fn validate(&self, ast: A, err: &ErrorCollector) -> A {
            match ast {
                A::Scalar(pos, tag, kind, version) => {
                    if !version.starts_with("v") {
                        err.add_error(Error::custom_at(
                            &pos,
                            VersionError("Version must start with 'v'")))
                    }
                    A::Scalar(pos, tag, kind, version)
                },
                ast => {
                    err.add_error(Error::validation_error(
                        &ast.pos(), format!("Version must be a scalar value")));
                    ast
                },
            }
        }
    }

    fn parse_version(body: &str) -> Result<Version, ErrorList> {
        let validator = Version::new();
        parse_string("<inline text>", body, &validator, &Options::default())
    }

    #[test]
    fn test_custom_error() {
        let err = parse_version("0.0.1").unwrap_err();
        let error = err.errors().nth(0).unwrap();
        assert_eq!(
            format!("{}", error),
            "<inline text>:1:1: Version Error: Version must start with 'v'");
        match error.downcast_ref::<VersionError>() {
            Some(&VersionError(msg)) => {
                assert_eq!(msg, "Version must start with 'v'")
            },
            e => panic!("Custom error must be VersionError but was: {:?}", e),
        }
    }
}
