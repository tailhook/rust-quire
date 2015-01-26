use std::str::FromStr;
use std::ops::Mul;
use std::fmt::String as Show;
use std::num::{FromStrRadix, from_str_radix, FromPrimitive};
use std::default::Default;
use std::collections::{BTreeMap, HashSet};

use regex::Regex;

use super::errors::Error;
pub use super::tokenizer::Pos;
use super::ast::Ast as A;
use super::ast::Tag as T;
use super::ast::{Ast, NullKind};
use super::ast::ScalarKind::{Quoted, Plain};


static NUMERIC_SUFFIXES: &'static [(&'static str, u64)] = &[
    ("k", 1000),
    ("M", 1000000),
    ("G", 1000000000),
    ("ki", 1024),
    ("Mi", 1048576),
    ("Gi", 1024*1024*1024),
    ];

pub trait Validator {
    fn validate(&self, ast: Ast) -> (Ast, Vec<Error>);
    fn default(&self, pos: Pos) -> Option<Ast>;
}

#[derive(Default)]
pub struct Scalar {
    pub descr: Option<String>,
    pub optional: bool,
    pub default: Option<String>,
    pub min_length: Option<usize>,
    pub max_length: Option<usize>,
    pub regex: Option<Regex>,
}

impl Validator for Scalar {
    fn default(&self, pos: Pos) -> Option<Ast> {
        if self.default.is_none() && self.optional {
            return Some(A::Null(pos.clone(), T::NonSpecific, NullKind::Implicit));
        }
        self.default.as_ref().map(|val| {
            A::Scalar(pos.clone(), T::NonSpecific, Quoted, val.clone()) })
    }
    fn validate(&self, ast: Ast) -> (Ast, Vec<Error>) {
        let mut warnings = vec!();
        let (pos, kind, val) = match ast {
            A::Scalar(pos, _, kind, string) => {
                (pos, kind, string)
            }
            A::Null(_, _, _) if self.optional => {
                return (ast, warnings);
            }
            ast => {
                warnings.push(Error::validation_error(&ast.pos(),
                    format!("Value must be scalar")));
                return (ast, warnings);
            }
        };
        self.min_length.map(|minl| {
            if val.len() < minl {
                warnings.push(Error::validation_error(&pos,
                    format!("Value must be at least {} characters", minl)));
            }
        });
        self.max_length.map(|maxl| {
            if val.len() > maxl {
                warnings.push(Error::validation_error(&pos,
                    format!("Value must be at most {} characters", maxl)));
            }
        });
        self.regex.as_ref().map(|regex| {
            if regex.is_match(val.as_slice()) {
                warnings.push(Error::validation_error(&pos,
                    format!("Value must match regular expression {}",
                            regex)));
            }
        });
        return (A::Scalar(pos, T::NonSpecific, kind, val), warnings);
    }
}

#[derive(Default)]
pub struct Numeric<T> {
    pub descr: Option<String>,
    pub optional: bool,
    pub default: Option<T>,
    pub min: Option<T>,
    pub max: Option<T>,
}

fn from_numeric<T>(mut src: &str) -> Option<T>
    where T: FromStr+FromStrRadix+Mul<T, Output=T>+FromPrimitive+Copy

{
    let mut mult = 1;
    for &(suffix, value) in NUMERIC_SUFFIXES.iter() {
        if suffix.len() < src.len() &&
            src.slice_from(src.len() - suffix.len()) == suffix
        {
            mult = value;
            src = src.slice_to(src.len() - suffix.len());
            break;
        }
    }
    let mut val: Option<T> = FromStr::from_str(src);
    if val.is_none() && src.len() > 2 {
        val = match src.slice_to(2) {
            "0x" => from_str_radix(src.slice_from(2), 16),
            "0o" => from_str_radix(src.slice_from(2), 8),
            "0b" => from_str_radix(src.slice_from(2), 2),
            _    => None,
        };
    }
    return val.and_then(|x| FromPrimitive::from_u64(mult).map(|m| x*m));
}

impl<T> Validator for Numeric<T>
    where T: PartialOrd+Show+FromStr+FromStrRadix+Mul<T, Output=T>
        +FromPrimitive+Copy
{

    fn default(&self, pos: Pos) -> Option<Ast> {
        if self.default.is_none() && self.optional {
            return Some(A::Null(pos.clone(), T::NonSpecific, NullKind::Implicit));
        }
        self.default.as_ref().map(|val| {
            A::Scalar(pos.clone(), T::NonSpecific, Quoted, val.to_string())
        })
    }
    fn validate(&self, ast: Ast) -> (Ast, Vec<Error>) {
        let mut warnings = vec!();
        let (pos, val): (Pos, T)  = match ast {
            A::Scalar(pos, tag, kind, string)
            => match from_numeric::<T>(string.as_slice()) {
                Some(val) => (pos, val),
                None => {
                    warnings.push(Error::validation_error(&pos,
                        format!("Value must be numeric")));
                    return (A::Scalar(pos, tag, kind, string), warnings);
                }
            },
            A::Null(_, _, _) if self.optional => {
                return (ast, warnings);
            }
            ast => {
                warnings.push(Error::validation_error(&ast.pos(),
                    format!("Value must be scalar")));
                return (ast, warnings);
            }
        };
        self.min.as_ref().map(|min| {
            if val < *min {
                warnings.push(Error::validation_error(&pos,
                    format!("Value must be at least {}", min)));
            }
        });
        self.max.as_ref().map(|max| {
            if val > *max {
                warnings.push(Error::validation_error(&pos,
                    format!("Value must be at most {}", max)));
            }
        });
        return (A::Scalar(pos, T::NonSpecific, Plain, val.to_string()),
                warnings);
    }
}

#[derive(Default)]
pub struct Directory {
    pub descr: Option<String>,
    pub optional: bool,
    pub default: Option<Path>,
    pub absolute: Option<bool>,
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
    fn validate(&self, ast: Ast) -> (Ast, Vec<Error>) {
        let mut warnings = vec!();
        let (pos, kind, val) = match ast {
            A::Scalar(pos, _, kind, string) => {
                (pos, kind, string)
            }
            A::Null(_, _, _) if self.optional => {
                return (ast, warnings);
            }
            ast => {
                warnings.push(Error::validation_error(&ast.pos(),
                    format!("Path expected")));
                return (ast, warnings);
            }
        };
        let path = Path::new(val);
        match self.absolute {
            Some(true) => {
                if !path.is_absolute() {
                    warnings.push(Error::validation_error(&pos,
                        format!("Path must be absolute")));
                }
            }
            Some(false) => {
                if path.is_absolute() {
                    warnings.push(Error::validation_error(&pos,
                        format!("Path must not be absolute")));
                } else {
                    // Still for non-absolute paths we must check if
                    // there are ../../something
                    //
                    // If you don't want this check, just set self.absolute
                    // to None instead of Some(false)
                    for cmp in path.str_components() {
                        if cmp == Some("..") {
                            warnings.push(Error::validation_error(&pos,
                                format!("The /../ is not allowed in path")));
                        }
                    }
                }
            }
            None => {}
        };
        return (A::Scalar(pos, T::NonSpecific, kind,
                          path.display().to_string()),
                warnings);
    }
}

#[derive(Default)]
pub struct Structure<'a> {
    pub descr: Option<String>,
    pub members: Vec<(String, Box<Validator + 'a>)>,
    pub optional: bool,
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
    fn validate(&self, ast: Ast) -> (Ast, Vec<Error>) {
        let mut warnings = vec!();
        let (pos, mut map) = match ast {
            A::Map(pos, _, items) => {
                (pos, items)
            }
            A::Null(pos, _, NullKind::Implicit) => {
                return (self.default(pos).unwrap(), warnings);
            }
            ast => {
                warnings.push(Error::validation_error(&ast.pos(),
                    format!("Value must be mapping")));
                return (ast, warnings);
            }
        };
        for &(ref k, ref validator) in self.members.iter() {
            let value = match map.remove(k)
                .or(map.remove(&k.as_slice().replace("_", "-"))) {
                Some(src) => {
                    let (value, wrn) = validator.validate(src);
                    warnings.extend(wrn.into_iter());
                    value
                }
                None => {
                    match validator.default(pos.clone()) {
                        Some(x) => x,
                        None => {
                            warnings.push(Error::validation_error(&pos,
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
            .filter(|s| !s.as_slice().starts_with("_"))
            .map(|s| s.clone()).collect();
        for &(ref k, _) in self.members.iter() {
            keys.remove(k);
        }
        if keys.len() > 0 {
            warnings.push(Error::validation_error(&pos,
                format!("Keys {:?} are not expected", keys)));
        }
        return (A::Map(pos, T::NonSpecific, map), warnings);
    }
}

#[derive(Default)]
pub struct Enum<'a> {
    pub descr: Option<String>,
    pub options: Vec<(String, Box<Validator + 'a>)>,
    pub optional: bool,
    pub default_tag: Option<String>,
}

impl<'a> Validator for Enum<'a> {
    fn default(&self, pos: Pos) -> Option<Ast> {
        if self.default_tag.is_some() && self.optional {
            return Some(A::Null(pos.clone(),
                T::LocalTag(self.default_tag.as_ref().unwrap().clone()),
                NullKind::Implicit));
        }
        return None;
    }
    fn validate(&self, ast: Ast) -> (Ast, Vec<Error>) {
        let mut warnings = vec!();
        let tag_name = match ast.tag() {
            &T::LocalTag(ref tag_name) => {
                tag_name.clone()
            }
            _ => unimplemented!(),
        };
        let pos = ast.pos().clone();
        for &(ref k, ref validator) in self.options.iter() {
            if k.as_slice() == tag_name.as_slice() {
                let (value, wrn) = validator.validate(ast);
                warnings.extend(wrn.into_iter());
                return (value.with_tag(T::LocalTag(tag_name)), warnings);
            }
        }
        warnings.push(Error::validation_error(&pos,
            format!("The tag {} is not expected", tag_name)));
        return (ast, warnings);
    }
}

#[derive(Default)]
pub struct Mapping<'a, 'b> {
    pub descr: Option<String>,
    pub key_element: Box<Validator + 'a>,
    pub value_element: Box<Validator + 'b>,
}

impl<'a, 'b> Validator for Mapping<'a, 'b> {
    fn default(&self, pos: Pos) -> Option<Ast> {
        return Some(A::Map(pos, T::NonSpecific, BTreeMap::new()));
    }
    fn validate(&self, ast: Ast) -> (Ast, Vec<Error>) {
        let mut warnings = vec!();
        let (pos, map) = match ast {
            A::Map(pos, _, items) => {
                (pos, items)
            }
            A::Null(pos, _, NullKind::Implicit) => {
                return (A::Map(pos, T::NonSpecific, BTreeMap::new()), warnings);
            }
            ast => {
                warnings.push(Error::validation_error(&ast.pos(),
                    format!("Value must be mapping")));
                return (ast, warnings);
            }
        };
        let mut res = BTreeMap::new();
        for (k, v) in map.into_iter() {
            let (key, wrn) = match self.key_element.validate(
                A::Scalar(v.pos().clone(), T::NonSpecific, Plain, k)) {
                (A::Scalar(_, _, _, val), wrn) => (val, wrn),
                _ => unreachable!(),
            };
            warnings.extend(wrn.into_iter());
            let (value, wrn) = self.value_element.validate(v);
            warnings.extend(wrn.into_iter());
            res.insert(key, value);
        }
        return (A::Map(pos, T::NonSpecific, res), warnings);
    }
}

#[derive(Default)]
pub struct Sequence<'a> {
    pub descr: Option<String>,
    pub element: Box<Validator + 'a>,
    pub from_scalar: Option<fn (scalar: Ast) -> Vec<Ast>>,
}

impl<'a> Validator for Sequence<'a> {
    fn default(&self, pos: Pos) -> Option<Ast> {
        return Some(A::List(pos, T::NonSpecific, Vec::new()));
    }
    fn validate(&self, ast: Ast) -> (Ast, Vec<Error>) {
        let mut warnings = vec!();
        let (pos, children) = match (ast, self.from_scalar) {
            (A::List(pos, _, items), _) => {
                (pos, items)
            }
            (A::Null(pos, _, NullKind::Implicit), _) => {
                return (A::List(pos, T::NonSpecific, Vec::new()), warnings);
            }
            (ast@A::Scalar(_, _, _, _), Some(fun)) => {
                (ast.pos().clone(), fun(ast))
            }
            (ast, _) => {
                warnings.push(Error::validation_error(&ast.pos(),
                    format!("Value must be sequence")));
                return (ast, warnings);
            }
        };
        let mut res = Vec::new();
        for val in children.into_iter() {
            let (value, wrn) = self.element.validate(val);
            warnings.extend(wrn.into_iter());
            res.push(value);
        }
        return (A::List(pos, T::NonSpecific, res), warnings);
    }
}

pub struct Anything;

impl Validator for Anything {
    fn default(&self, _: Pos) -> Option<Ast> {
        return None;
    }
    fn validate(&self, ast: Ast) -> (Ast, Vec<Error>) {
        return (ast, Vec::new());
    }
}

pub struct Nothing;

impl Validator for Nothing {
    fn default(&self, _: Pos) -> Option<Ast> {
        return None;
    }
    fn validate(&self, ast: Ast) -> (Ast, Vec<Error>) {
        let mut wrn = vec!();
        if let A::Null(_, _, _) = ast {
        } else {
            wrn.push(Error::parse_error(&ast.pos(),
                format!("Null expected, {} found", ast)));
        }
        return (ast, wrn);
    }
}

impl<'a> Default for Box<Validator + 'a> {
    fn default() -> Box<Validator + 'a> {
        return Box::new(Nothing) as Box<Validator>;
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::sync::mpsc::channel;
    use std::default::Default;
    use serialize::Decodable;
    use std::collections::BTreeMap;
    use std::collections::HashMap;

    use super::super::decode::YamlDecoder;
    use super::super::ast::process;
    use super::super::parser::parse;
    use super::super::errors::Error;
    use super::{Validator, Structure, Scalar, Numeric, Mapping, Sequence};
    use super::{Enum, Nothing, Directory};
    use self::TestEnum::*;

    #[derive(Clone, Show, PartialEq, Eq, Decodable)]
    struct TestStruct {
        intkey: usize,
        strkey: String,
    }

    fn parse_str(body: &str) -> TestStruct {
        let str_val = Structure { members: vec!(
            ("intkey".to_string(), Box::new(Numeric {
                default: Some(123us),
                .. Default::default() }) as Box<Validator>),
            ("strkey".to_string(), Box::new(Scalar {
                default: Some("default_value".to_string()),
                .. Default::default() }) as Box<Validator>),
        ), .. Default::default()};
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        assert_eq!(warnings.len(), 0);
        let (ast, warnings) = str_val.validate(ast);
        assert_eq!(warnings.len(), 0);
        let (tx, _) = channel();
        let mut dec = YamlDecoder::new(ast, tx);
        return Decodable::decode(&mut dec).unwrap();
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

    #[derive(Clone, Show, PartialEq, Eq, Decodable)]
    struct TestDash {
        some_key: usize,
    }

    fn parse_dash_str(body: &str) -> TestDash {
        let str_val = Structure { members: vec!(
            ("some_key".to_string(), Box::new(Numeric {
                default: Some(123us),
                .. Default::default() }) as Box<Validator>),
        ), .. Default::default()};
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        assert_eq!(warnings.len(), 0);
        let (ast, warnings) = str_val.validate(ast);
        assert_eq!(warnings.len(), 0);
        let (tx, _) = channel();
        let mut dec = YamlDecoder::new(ast, tx);
        return Decodable::decode(&mut dec).unwrap();
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
        let str_val = Structure { members: vec!(
            ("some_key".to_string(), Box::new(Numeric {
                default: Some(123us),
                .. Default::default() }) as Box<Validator>),
        ), .. Default::default()};
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        let (ast, warnings) = str_val.validate(ast);
        let (tx, _) = channel();
        let mut dec = YamlDecoder::new(ast, tx);
        return (Decodable::decode(&mut dec).unwrap(),
                warnings.iter().map(|x| x.to_string()).collect());
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
                 Keys HashSet {\"another-key\"} are not expected"
                 .to_string())));
    }

    #[derive(Clone, Show, PartialEq, Eq, Decodable)]
    struct TestOpt {
        some_key: Option<usize>,
    }

    fn parse_opt_str(body: &str) -> TestOpt {
        let str_val = Structure { members: vec!(
            ("some_key".to_string(), Box::new(Numeric {
                default: None::<usize>,
                optional: true,
                .. Default::default() }) as Box<Validator>),
        ), .. Default::default()};
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        assert_eq!(warnings.len(), 0);
        let (ast, warnings) = str_val.validate(ast);
        println!("WARNINGS {:?}", warnings);
        assert_eq!(warnings.len(), 0);
        let (tx, _) = channel();
        let mut dec = YamlDecoder::new(ast, tx);
        return Decodable::decode(&mut dec).unwrap();
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

    fn parse_map<T:Decodable>(body: &str) -> T {
        let validator = Mapping {
            key_element: Box::new(Scalar { .. Default::default()}),
            value_element: Box::new(Numeric::<usize> {
                default: Some(0us),
                .. Default::default()}),
            .. Default::default()
        };
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        assert_eq!(warnings.len(), 0);
        let (ast, warnings) = validator.validate(ast);
        assert_eq!(warnings.len(), 0);
        let (tx, _) = channel();
        let mut dec = YamlDecoder::new(ast, tx);
        return Decodable::decode(&mut dec).unwrap();
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

    fn parse_complex_map<T:Decodable>(body: &str)
        -> T
    {
        let validator = Mapping {
            key_element: Box::new(Scalar { .. Default::default()}),
            value_element: Box::new(Structure { members: vec!(
                ("some_key".to_string(), Box::new(Numeric {
                    default: Some(123us),
                    .. Default::default() }) as Box<Validator>),
            ),.. Default::default()}) as Box<Validator>,
            .. Default::default()
        };
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        warnings.iter().all(|w| { println!("WARNING: {}", w); true });
        assert_eq!(warnings.len(), 0);
        let (ast, warnings) = validator.validate(ast);
        warnings.iter().all(|w| { println!("WARNING: {}", w); true });
        assert_eq!(warnings.len(), 0);
        let (tx, _) = channel();
        let mut dec = YamlDecoder::new(ast, tx);
        return Decodable::decode(&mut dec).unwrap();
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
        let validator = Sequence {
            element: Box::new(Numeric {
                default: None::<usize>,
                .. Default::default()}),
            .. Default::default()
        };
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        assert_eq!(warnings.len(), 0);
        let (ast, warnings) = validator.validate(ast);
        assert_eq!(warnings.len(), 0);
        let (tx, _) = channel();
        let mut dec = YamlDecoder::new(ast, tx);
        return Decodable::decode(&mut dec).unwrap();
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
        let m = Vec::new();
        let res: Vec<usize> = parse_seq("[]");
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_null() {
        let m = Vec::new();
        let res: Vec<usize> = parse_seq("");
        assert_eq!(res, m);
    }

    #[test]
    fn test_numeric() {
        let m = vec!(100, 200, 300);
        let res: Vec<usize> = parse_seq("- 0o144\n- 0b11001000\n- 0x12c");
        assert_eq!(res, m);
    }

    #[derive(PartialEq, Eq, Decodable, Show)]
    enum TestEnum {
        Alpha,
        Beta,
        Gamma(isize),
        Delta(TestStruct),
        Epsilon(Option<TestStruct>),
    }

    fn parse_enum(body: &str) -> TestEnum {
        let validator = Enum {
            options: vec!(
                ("Alpha".to_string(), Box::new(Nothing) as Box<Validator>),
                ("Beta".to_string(), Box::new(Nothing) as Box<Validator>),
                ("Gamma".to_string(), Box::new(Numeric {
                    optional: true,
                    default: Some(7us),
                    .. Default::default() }) as Box<Validator>),
                ("Delta".to_string(), Box::new(Structure { members: vec!(
                    ("intkey".to_string(), Box::new(Numeric {
                        default: Some(123us),
                        .. Default::default() }) as Box<Validator>),
                    ("strkey".to_string(), Box::new(Scalar {
                        default: Some("default_value".to_string()),
                        .. Default::default()}) as Box<Validator>),
                    ), .. Default::default()}) as Box<Validator>),
                ("Epsilon".to_string(), Box::new(Structure {
                    optional: true,
                    members: vec!(
                    ("intkey".to_string(), Box::new(Numeric {
                        default: Some(457us),
                        .. Default::default() }) as Box<Validator>),
                    ("strkey".to_string(), Box::new(Scalar {
                        default: Some("epsilon".to_string()),
                        .. Default::default() }) as Box<Validator>),
                    ), .. Default::default()}) as Box<Validator>),
            ), .. Default::default()
        };
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        assert_eq!(warnings.len(), 0);
        let (ast, warnings) = validator.validate(ast);
        assert_eq!(warnings.len(), 0);
        let (tx, _) = channel();
        let mut dec = YamlDecoder::new(ast, tx);
        return Decodable::decode(&mut dec).unwrap();
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
    fn test_enum_3() {
        assert_eq!(parse_enum("!Alpha null"), Alpha);
    }

    #[test]
    fn test_enum_4() {
        assert_eq!(parse_enum("!Gamma 5"), Gamma(5));
    }

    #[test]
    #[should_fail]
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


    #[derive(Clone, PartialEq, Eq, Decodable)]
    struct TestPath {
        path: Path,
    }

    fn parse_path(body: &str, abs: Option<bool>) -> TestPath {
        let str_val = Structure { members: vec!(
            ("path".to_string(), Box::new(Directory {
                default: Some(Path::new("/test")),
                absolute: abs,
                .. Default::default() }) as Box<Validator>),
        ), .. Default::default()};
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        assert_eq!(warnings.len(), 0);
        let (ast, warnings) = str_val.validate(ast);
        for w in warnings.iter() {
            panic!(w.to_string());
        }
        let (tx, _) = channel();
        let mut dec = YamlDecoder::new(ast, tx);
        return Decodable::decode(&mut dec).unwrap();
    }

    #[test]
    #[should_fail(expected = "Path expected")]
    fn test_path_null() {
        assert!(parse_path("path:", None) == TestPath {
            path: Path::new("/test"),
        });
    }

    #[test]
    fn test_path_abs() {
        assert!(parse_path("path: /root/dir", None) == TestPath {
            path: Path::new("/root/dir"),
        });
    }

    #[test]
    fn test_path_rel() {
        assert!(parse_path("path: root/dir", None) == TestPath {
            path: Path::new("root/dir"),
        });
    }

    #[test]
    fn test_path_down() {
        assert!(parse_path("path: ../root/dir", None) == TestPath {
            path: Path::new("../root/dir"),
        });
    }

    #[test]
    fn test_path_abs_abs() {
        assert!(parse_path("path: /root/dir", Some(true)) == TestPath {
            path: Path::new("/root/dir"),
        });
    }

    #[test]
    #[should_fail(expected = "must be absolute")]
    fn test_path_rel_abs() {
        assert!(parse_path("path: root/dir", Some(true)) == TestPath {
            path: Path::new("root/dir"),
        });
    }

    #[test]
    #[should_fail(expected = "must be absolute")]
    fn test_path_down_abs() {
        assert!(parse_path("path: ../root/dir", Some(true)) == TestPath {
            path: Path::new("../root/dir"),
        });
    }

    #[test]
    #[should_fail(expected = "must not be absolute")]
    fn test_path_abs_rel() {
        assert!(parse_path("path: /root/dir", Some(false)) == TestPath {
            path: Path::new("/root/dir"),
        });
    }

    #[test]
    fn test_path_rel_rel() {
        assert!(parse_path("path: root/dir", Some(false)) == TestPath {
            path: Path::new("root/dir"),
        });
    }

    #[test]
    #[should_fail(expected = "/../ is not allowed")]
    fn test_path_down_rel() {
        assert!(parse_path("path: ../root/dir", Some(false)) == TestPath {
            path: Path::new("../root/dir"),
        });
    }
}
