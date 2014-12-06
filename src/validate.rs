use std::from_str::FromStr;
use std::fmt::Show;
use std::num::{FromStrRadix,from_str_radix};
use std::default::Default;
use std::collections::{TreeMap, HashSet};
use std::str::replace;

use regex::Regex;

use super::errors::Error;
pub use super::tokenizer::Pos;
use super::ast as A;

pub trait Validator {
    fn validate(&self, ast: A::Ast) -> (A::Ast, Vec<Error>);
    fn default(&self, pos: Pos) -> Option<A::Ast>;
}

#[deriving(Default)]
pub struct Scalar {
    pub descr: Option<String>,
    pub optional: bool,
    pub default: Option<String>,
    pub min_length: Option<uint>,
    pub max_length: Option<uint>,
    pub regex: Option<Regex>,
}

impl Validator for Scalar {
    fn default(&self, pos: Pos) -> Option<A::Ast> {
        if self.default.is_none() && self.optional {
            return Some(A::Null(pos.clone(), A::NonSpecific, A::Implicit));
        }
        self.default.as_ref().map(|val| {
            A::Scalar(pos.clone(), A::NonSpecific, A::Quoted, val.clone()) })
    }
    fn validate(&self, ast: A::Ast) -> (A::Ast, Vec<Error>) {
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
        return (A::Scalar(pos, A::NonSpecific, kind, val), warnings);
    }
}

#[deriving(Default)]
pub struct Numeric<T> {
    pub descr: Option<String>,
    pub optional: bool,
    pub default: Option<T>,
    pub min: Option<T>,
    pub max: Option<T>,
}

fn from_numeric<T:FromStr+FromStrRadix>(src: &str) -> Option<T> {
    if let Some(x) = FromStr::from_str(src) {
        return Some(x);
    }
    match src.slice_to(2) {
        "0x" => { return from_str_radix(src.slice_from(2), 16); }
        "0o" => { return from_str_radix(src.slice_from(2), 8); }
        "0b" => { return from_str_radix(src.slice_from(2), 2); }
        _    => { return None; }
    }
}

impl<T:PartialOrd+Show+FromStr+FromStrRadix> Validator for Numeric<T> {
    fn default(&self, pos: Pos) -> Option<A::Ast> {
        if self.default.is_none() && self.optional {
            return Some(A::Null(pos.clone(), A::NonSpecific, A::Implicit));
        }
        self.default.as_ref().map(|val| {
            A::Scalar(pos.clone(), A::NonSpecific, A::Quoted, val.to_string())
        })
    }
    fn validate(&self, ast: A::Ast) -> (A::Ast, Vec<Error>) {
        let mut warnings = vec!();
        let (pos, val): (Pos, T)  = match ast {
            A::Scalar(pos, tag, kind, string)
            => match from_numeric(string.as_slice()) {
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
        return (A::Scalar(pos, A::NonSpecific, A::Plain, val.to_string()),
                warnings);
    }
}

#[deriving(Default)]
pub struct Structure<'a> {
    pub descr: Option<String>,
    pub members: Vec<(String, Box<Validator + 'a>)>,
}

impl<'a> Validator for Structure<'a> {
    fn default(&self, pos: Pos) -> Option<A::Ast> {
        let mut map = TreeMap::new();
        for &(ref k, ref validator) in self.members.iter() {
            match validator.default(pos.clone()) {
                Some(val) => {
                    map.insert(k.clone(), val);
                }
                None => continue,
            }
        }
        return Some(A::Map(pos, A::NonSpecific, map));
    }
    fn validate(&self, ast: A::Ast) -> (A::Ast, Vec<Error>) {
        let mut warnings = vec!();
        let (pos, mut map) = match ast {
            A::Map(pos, _, items) => {
                (pos, items)
            }
            A::Null(pos, _, A::Implicit) => {
                return (self.default(pos).unwrap(), warnings);
            }
            ast => {
                warnings.push(Error::validation_error(&ast.pos(),
                    format!("Value must be mapping")));
                return (ast, warnings);
            }
        };
        for &(ref k, ref validator) in self.members.iter() {
            let value = match map.pop(k)
                .or(map.pop(&replace(k.as_slice(), "_", "-"))) {
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
                format!("Keys {} are not expected", keys)));
        }
        return (A::Map(pos, A::NonSpecific, map), warnings);
    }
}

#[deriving(Default)]
pub struct Enum<'a> {
    pub descr: Option<String>,
    pub options: Vec<(String, Box<Validator + 'a>)>,
    pub optional: bool,
    pub default_tag: Option<String>,
}

impl<'a> Validator for Enum<'a> {
    fn default(&self, pos: Pos) -> Option<A::Ast> {
        if self.default_tag.is_some() && self.optional {
            return Some(A::Null(pos.clone(),
                A::LocalTag(self.default_tag.as_ref().unwrap().clone()),
                A::Implicit));
        }
        return None;
    }
    fn validate(&self, ast: A::Ast) -> (A::Ast, Vec<Error>) {
        let mut warnings = vec!();
        let tag_name = match ast.tag() {
            &A::LocalTag(ref tag_name) => {
                tag_name.clone()
            }
            _ => unimplemented!(),
        };
        let pos = ast.pos().clone();
        for &(ref k, ref validator) in self.options.iter() {
            if k.as_slice() == tag_name.as_slice() {
                let (value, wrn) = validator.validate(ast);
                warnings.extend(wrn.into_iter());
                return (value.with_tag(A::LocalTag(tag_name)), warnings);
            }
        }
        warnings.push(Error::validation_error(&pos,
            format!("The tag {} is not expected", tag_name)));
        return (ast, warnings);
    }
}

#[deriving(Default)]
pub struct Mapping<'a, 'b> {
    pub descr: Option<String>,
    pub key_element: Box<Validator + 'a>,
    pub value_element: Box<Validator + 'b>,
}

impl<'a, 'b> Validator for Mapping<'a, 'b> {
    fn default(&self, pos: Pos) -> Option<A::Ast> {
        return Some(A::Map(pos, A::NonSpecific, TreeMap::new()));
    }
    fn validate(&self, ast: A::Ast) -> (A::Ast, Vec<Error>) {
        let mut warnings = vec!();
        let (pos, map) = match ast {
            A::Map(pos, _, items) => {
                (pos, items)
            }
            A::Null(pos, _, A::Implicit) => {
                return (A::Map(pos, A::NonSpecific, TreeMap::new()), warnings);
            }
            ast => {
                warnings.push(Error::validation_error(&ast.pos(),
                    format!("Value must be mapping")));
                return (ast, warnings);
            }
        };
        let mut res = TreeMap::new();
        for (k, v) in map.into_iter() {
            let (key, wrn) = match self.key_element.validate(
                A::Scalar(v.pos().clone(), A::NonSpecific, A::Plain, k)) {
                (A::Scalar(_, _, _, val), wrn) => (val, wrn),
                _ => unreachable!(),
            };
            warnings.extend(wrn.into_iter());
            let (value, wrn) = self.value_element.validate(v);
            warnings.extend(wrn.into_iter());
            res.insert(key, value);
        }
        return (A::Map(pos, A::NonSpecific, res), warnings);
    }
}

#[deriving(Default)]
pub struct Sequence<'a> {
    pub descr: Option<String>,
    pub element: Box<Validator + 'a>,
    pub from_scalar: Option<fn (scalar: A::Ast) -> Vec<A::Ast>>,
}

impl<'a> Validator for Sequence<'a> {
    fn default(&self, pos: Pos) -> Option<A::Ast> {
        return Some(A::List(pos, A::NonSpecific, Vec::new()));
    }
    fn validate(&self, ast: A::Ast) -> (A::Ast, Vec<Error>) {
        let mut warnings = vec!();
        let (pos, children) = match (ast, self.from_scalar) {
            (A::List(pos, _, items), _) => {
                (pos, items)
            }
            (A::Null(pos, _, A::Implicit), _) => {
                return (A::List(pos, A::NonSpecific, Vec::new()), warnings);
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
        return (A::List(pos, A::NonSpecific, res), warnings);
    }
}

pub struct Anything;

impl Validator for Anything {
    fn default(&self, _: Pos) -> Option<A::Ast> {
        return None;
    }
    fn validate(&self, ast: A::Ast) -> (A::Ast, Vec<Error>) {
        return (ast, Vec::new());
    }
}

pub struct Nothing;

impl Validator for Nothing {
    fn default(&self, _: Pos) -> Option<A::Ast> {
        return None;
    }
    fn validate(&self, ast: A::Ast) -> (A::Ast, Vec<Error>) {
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
        return box Nothing as Box<Validator>;
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::default::Default;
    use serialize::Decodable;
    use std::collections::TreeMap;
    use std::collections::HashMap;

    use super::super::decode::YamlDecoder;
    use super::super::ast::process;
    use super::super::parser::parse;
    use super::super::errors::Error;
    use super::{Validator, Structure, Scalar, Numeric, Mapping, Sequence};
    use super::{Enum, Nothing};

    #[deriving(Clone, Show, PartialEq, Eq, Decodable)]
    struct TestStruct {
        intkey: uint,
        strkey: String,
    }

    fn parse_str(body: &str) -> TestStruct {
        let str_val = Structure { members: vec!(
            ("intkey".to_string(), box Numeric {
                default: Some(123u),
                .. Default::default() } as Box<Validator>),
            ("strkey".to_string(), box Scalar {
                default: Some("default_value".to_string()),
                .. Default::default() } as Box<Validator>),
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

    #[deriving(Clone, Show, PartialEq, Eq, Decodable)]
    struct TestDash {
        some_key: uint,
    }

    fn parse_dash_str(body: &str) -> TestDash {
        let str_val = Structure { members: vec!(
            ("some_key".to_string(), box Numeric {
                default: Some(123u),
                .. Default::default() } as Box<Validator>),
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
            ("some_key".to_string(), box Numeric {
                default: Some(123u),
                .. Default::default() } as Box<Validator>),
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
                     Keys {another-key} are not expected".to_string())));
    }

    #[deriving(Clone, Show, PartialEq, Eq, Decodable)]
    struct TestOpt {
        some_key: Option<uint>,
    }

    fn parse_opt_str(body: &str) -> TestOpt {
        let str_val = Structure { members: vec!(
            ("some_key".to_string(), box Numeric {
                default: None::<uint>,
                optional: true,
                .. Default::default() } as Box<Validator>),
        ), .. Default::default()};
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        assert_eq!(warnings.len(), 0);
        let (ast, warnings) = str_val.validate(ast);
        println!("WARNINGS {}", warnings);
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

    fn parse_map<T:Decodable<YamlDecoder, Error>>(body: &str) -> T {
        let validator = Mapping {
            key_element: box Scalar { .. Default::default()},
            value_element: box Numeric::<uint> { default: Some(0u), .. Default::default()},
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
        let mut m = TreeMap::new();
        m.insert("a".to_string(), 1);
        let res: TreeMap<String, uint> = parse_map("a: 1");
        assert_eq!(res, m);
    }

    #[test]
    fn test_map_2() {
        let mut m = TreeMap::new();
        m.insert("a".to_string(), 1);
        m.insert("bc".to_string(), 2);
        let res: TreeMap<String, uint> = parse_map("a: 1\nbc: 2");
        assert_eq!(res, m);
    }

    #[test]
    fn test_hash_map() {
        let mut m = HashMap::new();
        m.insert("a".to_string(), 1);
        m.insert("bc".to_string(), 2);
        let res: HashMap<String, uint> = parse_map("a: 1\nbc: 2");
        assert_eq!(res, m);
    }

    #[test]
    fn test_map_empty() {
        let m = TreeMap::new();
        let res: TreeMap<String, uint> = parse_map("{}");
        assert_eq!(res, m);
    }

    #[test]
    fn test_map_null() {
        let m = TreeMap::new();
        let res: TreeMap<String, uint> = parse_map("");
        assert_eq!(res, m);
    }

    fn parse_complex_map<T:Decodable<YamlDecoder, Error>>(body: &str)
        -> T
    {
        let validator = Mapping {
            key_element: box Scalar { .. Default::default()},
            value_element: box Structure { members: vec!(
                ("some_key".to_string(), box Numeric {
                    default: Some(123u),
                    .. Default::default() } as Box<Validator>),
            ), .. Default::default()} as Box<Validator>,
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
        let mut m = TreeMap::new();
        m.insert("a".to_string(), TestDash {
            some_key: 13,
        });
        let res: TreeMap<String, TestDash>;
        res = parse_complex_map("a:\n some_key: 13");
        assert_eq!(res, m);
    }

    #[test]
    fn test_cmap_2() {
        let mut m = TreeMap::new();
        m.insert("a".to_string(), TestDash {
            some_key: 123,
        });
        let res: TreeMap<String, TestDash>;
        res = parse_complex_map("a:\n");
        assert_eq!(res, m);
    }

    #[test]
    fn test_cmap_3() {
        let mut m = TreeMap::new();
        m.insert("a".to_string(), TestDash {
            some_key: 123,
        });
        let res: TreeMap<String, TestDash> = parse_complex_map("a: {}");
        assert_eq!(res, m);
    }

    fn parse_seq(body: &str) -> Vec<uint> {
        let validator = Sequence {
            element: box Numeric { default: None::<uint>, .. Default::default()},
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
        let res: Vec<uint> = parse_seq("- 1");
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_2() {
        let m = vec!(1, 2);
        let res: Vec<uint> = parse_seq("- 1\n- 2");
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_empty() {
        let m = Vec::new();
        let res: Vec<uint> = parse_seq("[]");
        assert_eq!(res, m);
    }

    #[test]
    fn test_seq_null() {
        let m = Vec::new();
        let res: Vec<uint> = parse_seq("");
        assert_eq!(res, m);
    }

    #[test]
    fn test_numeric() {
        let m = vec!(100, 200, 300);
        let res: Vec<uint> = parse_seq("- 0o144\n- 0b11001000\n- 0x12c");
        assert_eq!(res, m);
    }

    #[deriving(PartialEq, Eq, Decodable, Show)]
    enum TestEnum {
        Alpha,
        Beta,
        Gamma(int),
        Delta(TestStruct),
    }

    fn parse_enum(body: &str) -> TestEnum {
        let validator = Enum {
            options: vec!(
                ("Alpha".to_string(), box Nothing as Box<Validator>),
                ("Beta".to_string(), box Nothing as Box<Validator>),
                ("Gamma".to_string(), box Numeric {
                    optional: true,
                    default: Some(7u),
                    .. Default::default() } as Box<Validator>),
                ("Delta".to_string(), box Structure { members: vec!(
                    ("intkey".to_string(), box Numeric {
                        default: Some(123u),
                        .. Default::default() } as Box<Validator>),
                    ("strkey".to_string(), box Scalar {
                        default: Some("default_value".to_string()),
                        .. Default::default() } as Box<Validator>),
                    ), .. Default::default()} as Box<Validator>),
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

}
