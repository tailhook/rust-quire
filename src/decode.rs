use std::ops::Deref;
use std::any::Any;
use std::mem::swap;
use std::fmt::Show;
use std::fmt::Display;
use std::fmt::Error as FormatError;
use std::fmt::{Formatter};
use std::str::FromStr;
use std::default::Default;
use std::sync::mpsc::Sender;
use std::intrinsics::get_tydesc;
use serialize::{Decoder, Decodable};
use serialize::json::{Json, ToJson, from_str};
use serialize::json::Json as J;

use super::ast::Ast as A;
use super::ast::Tag as T;
use super::ast::NullKind;
use super::ast::Ast;
use super::errors::Error;
use super::tokenizer::Pos;
use self::ParserState::*;


pub type DecodeResult<T> = Result<T, Error>;

struct AnyJson(Json);

impl Deref for AnyJson {
    type Target = Json;
    fn deref<'x>(&'x self) -> &'x Json {
        let AnyJson(ref val) = *self;
        return val;
    }
}

impl Decodable for AnyJson {
    fn decode<D: Decoder + 'static>(dec: &mut D)
        -> Result<AnyJson, <Self as Decodable>::Error>
    {
        let dec: &mut YamlDecoder = (dec as &mut Any).downcast_mut().unwrap();
        match dec.state {
            Node(ref node) => {
                return Ok(AnyJson(node.to_json()));
            }
            Byte(_, _) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_, _) => unreachable!(),
            Key(_, ref val) => return Ok(AnyJson(J::String(val.clone()))),
        }
    }
}

impl PartialEq for AnyJson {
    fn eq(&self, other: &AnyJson) -> bool {
        let AnyJson(ref selfj) = *self;
        let AnyJson(ref otherj) = *other;
        return selfj == otherj;
    }
}
impl Eq for AnyJson {}
impl Show for AnyJson {
    fn fmt(&self, fmt:&mut Formatter) -> Result<(), FormatError> {
        let AnyJson(ref selfj) = *self;
        write!(fmt, "{}", selfj)
    }
}

enum ParserState {
    Node(Ast),
    Map(Vec<(String, Ast)>),  // used only in read_map_elt_key/elt_val
    Seq(Vec<Ast>),  // used only in read_seq_elt
    ByteSeq(Pos, Vec<u8>),  // used for decoding Path
    Byte(Pos, u8),     // used for decoding Path
    Key(Pos, String),
}

pub struct YamlDecoder {
    state: ParserState,
    sender: Sender<Error>,
    path: String,
}

impl YamlDecoder {

    pub fn new(ast: Ast, sender: Sender<Error>)
        -> YamlDecoder
    {
        return YamlDecoder {
            state: Node(ast),
            sender: sender,
            path: "".to_string(),
        }
    }

    fn from_str<T, E>(&mut self) -> DecodeResult<T>
        where T: FromStr<Err=E>+Default+'static,
              E: Display
    {
        match self.state {
            Node(A::Scalar(ref pos, _, _, ref val)) | Key(ref pos, ref val) => {
                match FromStr::from_str(&val[..]) {
                    Ok(x) => Ok(x),
                    Err(err) => {
                        return Err(Error::decode_error(pos, &self.path,
                            format!("Can't parse value of type {}: {}",
                                unsafe { (*get_tydesc::<T>()).name },
                                err)));
                    }
                }
            }
            Node(ref node) => {
                return Err(Error::decode_error(&node.pos(), &self.path,
                    format!("Expected scalar, got {}", node)));
            }
            Byte(ref pos, _) => {
                // The string is a sequence of bytes to make Path (which
                // decodes from a sequence of bytes) work
                // But if string specified instead of sequence of scalars
                // we should emit an error
                return Err(Error::decode_error(pos, &self.path,
                    format!("Expected sequence, got string")));
            }
            Map(_) | Seq(_) | ByteSeq(_, _) => unreachable!(),
        }
    }
}


impl Decoder for YamlDecoder {
    type Error = Error;
    fn read_nil(&mut self) -> DecodeResult<()> {
        match self.state {
            Node(A::Null(_, _, _)) => return Ok(()),
            Node(ref node) => {
                self.sender.send(Error::decode_error(&node.pos(), &self.path,
                    format!("Expected null")));
                return Ok(())
            }
            Key(_, _) => unimplemented!(),
            Byte(_, _) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_, _) => unreachable!(),
        }
    }


    fn read_u64(&mut self)  -> DecodeResult<u64> {
        Ok(try!(self.from_str()))
    }
    fn read_u32(&mut self)  -> DecodeResult<u32> {
        Ok(try!(self.from_str()))
    }
    fn read_u16(&mut self)  -> DecodeResult<u16> {
        Ok(try!(self.from_str()))
    }
    fn read_u8 (&mut self)  -> DecodeResult<u8> {
        if let Byte(_, x) = self.state {
            return Ok(x);
        }
        Ok(try!(self.from_str()))
    }
    fn read_uint(&mut self) -> DecodeResult<usize> {
        Ok(try!(self.from_str()))
    }

    fn read_i64(&mut self) -> DecodeResult<i64> {
        Ok(try!(self.from_str()))
    }
    fn read_i32(&mut self) -> DecodeResult<i32> {
        Ok(try!(self.from_str()))
    }
    fn read_i16(&mut self) -> DecodeResult<i16> {
        Ok(try!(self.from_str()))
    }
    fn read_i8 (&mut self) -> DecodeResult<i8 > {
        Ok(try!(self.from_str()))
    }
    fn read_int(&mut self) -> DecodeResult<isize> {
        Ok(try!(self.from_str()))
    }

    fn read_bool(&mut self) -> DecodeResult<bool> {
        Ok(try!(self.from_str()))
    }

    fn read_f64(&mut self) -> DecodeResult<f64> {
        Ok(try!(self.from_str()))
    }

    fn read_f32(&mut self) -> DecodeResult<f32> {
        Ok(try!(self.from_str()))
    }


    fn read_char(&mut self) -> DecodeResult<char> {
        unimplemented!();
    }

    fn read_str(&mut self) -> DecodeResult<String> {
        // TODO(tailhook) Is it fast enought?
        match self.state {
            Node(A::Scalar(ref pos, _, _, ref val)) | Key(ref pos, ref val) => {
                return Ok(val.clone());
            }
            Node(ref node) => {
                return Err(Error::decode_error(&node.pos(), &self.path,
                    format!("Expected scalar, got {}", node)));
            }
            Byte(ref pos, _) => {
                // The string is a sequence of bytes to make Path (which
                // decodes from a sequence of bytes) work
                // But if string specified instead of sequence of scalars
                // we should emit an error
                return Err(Error::decode_error(pos, &self.path,
                    format!("Expected sequence, got string")));
            }
            Map(_) | Seq(_) | ByteSeq(_, _) => unreachable!(),
        }
    }

    fn read_enum<T, F>(&mut self, _name: &str,
        f: F) -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T>
    {
        return f(self);
    }

    fn read_enum_variant<T, F>(&mut self,
        names: &[&str], mut f: F)
        -> DecodeResult<T>
        where F: FnMut(&mut Self, usize) -> DecodeResult<T>
    {
        let mut idx = None;
        match self.state {
            Node(ref node) if node.tag().is_specific() => {
                match node.tag() {
                    &T::NonSpecific => unreachable!(),
                    &T::LocalTag(ref tag) => {
                        for (i, name) in names.iter().enumerate() {
                            if *name == tag.as_slice() {
                                idx = Some(i);
                            }
                        }
                        if idx.is_none() {
                            return Err(Error::decode_error(&node.pos(),
                                &self.path,
                                format!("{} is not one of {:?}", tag, names)));
                        }
                    }
                    &T::GlobalTag(_) => unimplemented!(),
                }
            }
            Node(A::Scalar(ref pos, _, _, ref value)) => {
                let programmatic_name = value.replace("-", "_");
                for (i, name) in names.iter().enumerate() {
                    if *name == value.as_slice() ||
                        *name == programmatic_name.as_slice() {
                        idx = Some(i);
                    }
                }
                if idx.is_none() {
                    return Err(Error::decode_error(pos, &self.path,
                        format!("{} is not one of {:?}", value, names)));
                }
            }
            Node(ref node) => {
                return Err(Error::decode_error(&node.pos(), &self.path,
                    format!("Scalar or tagged value expected")));
            }
            _ => unimplemented!(),
        }
        return f(self, idx.unwrap());
    }

    fn read_enum_variant_arg<T, F>(&mut self, idx: usize, mut f: F)
        -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T>
    {
        if idx == 0 {
            return f(self);
        }
        unimplemented!();
    }

    fn read_enum_struct_variant<T, F>(&mut self, names: &[&str], f: F)
        -> DecodeResult<T>
    {
        unimplemented!();
    }


    fn read_enum_struct_variant_field<T, F>(&mut self,
        _name: &str, _idx: usize, _f: F)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_struct<T, F>(&mut self, _name: &str, _len: usize, f: F)
        -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T>
    {
        match self.state {
            Node(A::Map(_, _, _)) => {}
            Node(A::Null(ref pos, _, _)) => {
                return f(&mut YamlDecoder {
                    state: Node(A::Map(pos.clone(), T::NonSpecific,
                        Default::default())),
                    sender: self.sender.clone(),
                    path: self.path.clone(),
                });
            }
            Node(ref node) => {
                return Err(Error::decode_error(&node.pos(), &self.path,
                    "Mapping expected".to_string()));
            }
            Byte(_, _) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_, _) => unreachable!(),
            Key(_, _) => unimplemented!(),
        };
        return f(self);
    }

    fn read_struct_field<T, F>(&mut self,
        name: &str, _idx: usize, f: F)
        -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> DecodeResult<T>
    {
        if let Node(A::Map(ref pos, _, ref mut children)) = self.state {
            match children.remove(&name.to_string()) {
                None => {
                    return f(&mut YamlDecoder {
                        state: Node(A::Null(pos.clone(), T::NonSpecific,
                            NullKind::Implicit)),
                        sender: self.sender.clone(),
                        path: format!("{}.{}", self.path, name),
                    });
                }
                Some(node) => {
                    return f(&mut YamlDecoder {
                        state: Node(node),
                        sender: self.sender.clone(),
                        path: format!("{}.{}", self.path, name),
                    });
                }
            };
        }
        unreachable!();
    }

    fn read_tuple<T, F>(&mut self, _len: usize, _f: F)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_tuple_arg<T, F>(&mut self, _idx: usize, _f: F)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_tuple_struct<T, F>(&mut self, _name: &str, _len: usize, _f: F)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_tuple_struct_arg<T, F>(&mut self, _idx: usize, _f: F)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_option<T, F>(&mut self, f: F)
        -> DecodeResult<T>
        where F: FnOnce(&mut Self, bool) -> Result<T, Error>
    {
        match self.state {
            Node(A::Null(_, _, _)) => f(self, false),
            Node(_) => f(self, true),
            Key(_, _) => unimplemented!(),
            Byte(_, _) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_, _) => unreachable!(),
        }
    }

    fn read_seq<T, F>(&mut self, f: F)
        -> DecodeResult<T>
        where F: FnOnce(&mut Self, usize) -> Result<T, Error>
    {
        let items = match self.state {
            Node(A::List(_, _, ref mut children)) => {
                let mut ch = Default::default();
                swap(children, &mut ch);
                ch
            }
            Node(A::Scalar(ref pos, _, _, ref val)) => {
                let bytes = val.as_bytes();
                return f(&mut YamlDecoder {
                    state: ByteSeq(pos.clone(), bytes.to_vec()),
                    sender: self.sender.clone(),
                    path: self.path.clone(),
                }, bytes.len());
            }
            Node(A::Null(_, _, _)) => Vec::new(),
            Node(ref node) => {
                return Err(Error::decode_error(&node.pos(), &self.path,
                    "Sequence expected".to_string()));
            }
            Byte(_, _) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_, _) => unreachable!(),
            Key(ref pos, ref val) => {
                let bytes = val.as_bytes();
                return f(&mut YamlDecoder {
                    state: ByteSeq(pos.clone(), bytes.to_vec()),
                    sender: self.sender.clone(),
                    path: self.path.clone(),
                }, bytes.len());
            }
        };
        let len = items.len();
        return f(&mut YamlDecoder {
            state: Seq(items),
            sender: self.sender.clone(),
            path: self.path.clone(),
        }, len);
    }

    fn read_seq_elt<T, F>(&mut self, idx: usize, f: F)
        -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> Result<T, Error>
    {
        match self.state {
            Seq(ref mut els) => {
                let val = els.remove(0);
                return f(&mut YamlDecoder {
                    state: Node(val),
                    sender: self.sender.clone(),
                    path: format!("{}[{}]", self.path, idx),
                });
            }
            ByteSeq(ref pos, ref vec) => {
                return f(&mut YamlDecoder {
                    state: Byte(pos.clone(), vec[idx]),
                    sender: self.sender.clone(),
                    path: format!("{}[{}]", self.path, idx),
                });
            }
            _ => unreachable!(),
        }
    }

    fn read_map<T, F>(&mut self, f: F)
        -> DecodeResult<T>
        where F: FnOnce(&mut Self, usize) -> Result<T, Error>
    {
        let items = match self.state {
            Node(A::Map(_, _, ref mut children)) => {
                let mut ch = Default::default();
                swap(children, &mut ch);
                ch.into_iter().collect()
            }
            Node(A::Null(_, _, _)) => Vec::new(),
            Node(ref node) => {
                return Err(Error::decode_error(&node.pos(), &self.path,
                    "Mapping expected".to_string()));
            }
            Byte(_, _) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_, _) => unreachable!(),
            Key(_, _) => unimplemented!(),
        };
        let len = items.len();
        return f(&mut YamlDecoder {
            state: Map(items),
            sender: self.sender.clone(),
            path: self.path.clone(),
        }, len);
    }

    fn read_map_elt_key<T, F>(&mut self, _idx: usize, f: F)
        -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> Result<T, Error>
    {
        if let Map(ref mut vec) = self.state {
            let (ref key, ref val) = (*vec)[0];
            return f(&mut YamlDecoder {
                state: Key(val.pos().clone(), key.clone()),
                sender: self.sender.clone(),
                path: self.path.clone() + ".",
            });
        }
        unreachable!();
    }

    fn read_map_elt_val<T, F>(&mut self, _idx: usize, f: F)
        -> DecodeResult<T>
        where F: FnOnce(&mut Self) -> Result<T, Error>
    {
        if let Map(ref mut els) = self.state {
            let (key, val) = els.remove(0);
            return f(&mut YamlDecoder {
                state: Node(val),
                sender: self.sender.clone(),
                path: self.path.clone() + "." + key.as_slice(),
            });
        }
        unreachable!();
    }

    fn error(&mut self, err: &str) -> Error {
        let pos = match self.state {
            Node(ref node) => node.pos().clone(),
            Byte(_, _) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_, _) => unimplemented!(),
            Key(ref pos, _) => pos.clone(),
        };
        return Error::decode_error(&pos, &self.path, err.to_string())
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::default::Default;
    use std::collections::BTreeMap;
    use std::sync::mpsc::channel;
    use serialize::{Decodable, Decoder};
    use serialize::json::{from_str};
    use serialize::json as J;

    use super::YamlDecoder;
    use super::super::parser::parse;
    use super::super::ast::process;
    use super::AnyJson;
    use self::TestEnum::*;

    #[derive(Clone, Show, PartialEq, Eq, Decodable)]
    struct TestStruct {
        a: usize,
        b: String,
    }

    #[test]
    fn decode_struct() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "a: 1\nb: hello",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: TestStruct = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        assert_eq!(val, TestStruct {
            a: 1,
            b: "hello".to_string(),
            });
        assert_eq!(warnings.len(), 0);
    }

    #[test]
    fn decode_list() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "- a\n- b",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: Vec<String> = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        assert_eq!(val, vec!("a".to_string(), "b".to_string()));
        assert_eq!(warnings.len(), 0);
    }

    #[test]
    fn decode_list_error() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "test",
            |doc| { process(Default::default(), doc) }).unwrap();
        let (tx, _) = channel();
        let mut dec = YamlDecoder::new(ast, tx);
        let res: Result<Vec<String>, String> = Decodable::decode(&mut dec)
                                               .map_err(|e| format!("{}", e));
        assert_eq!(res, Err("<inline text>:1:1: Decode error at [0]: \
                             Expected sequence, got string".to_string()));
    }

    #[test]
    fn decode_map() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "a: 1\nb: 2",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: BTreeMap<String, isize> = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        let mut res =  BTreeMap::new();
        res.insert("a".to_string(), 1);
        res.insert("b".to_string(), 2);
        assert_eq!(val, res);
        assert_eq!(warnings.len(), 0);
    }

    #[derive(Show, PartialEq, Eq, Decodable)]
    struct TestJson {
        json: AnyJson,
    }

    #[test]
    fn decode_json() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "json:\n a: 1\n b: test",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: TestJson = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        assert_eq!(val, TestJson {
            json: AnyJson(from_str(r#"{"a": 1, "b": "test"}"#).unwrap()),
            });
        assert_eq!(warnings.len(), 0);
    }

    #[derive(PartialEq, Eq, Decodable, Show)]
    struct TestOption {
        path: Option<String>,
    }

    #[test]
    fn decode_option_some() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "path: test/value",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: TestOption = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        assert!(val.path == Some("test/value".to_string()));
        assert_eq!(warnings.len(), 0);
    }

    #[test]
    fn decode_option_none() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "path:",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: TestOption = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        assert!(val.path == None);
        assert_eq!(warnings.len(), 0);
    }

    #[test]
    fn decode_option_no_key() {
        // This one failed in rust0.11 but works in rust0.12 for unknown reason
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "{}",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: TestOption = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        assert!(val.path == None);
        assert_eq!(warnings.len(), 0);
    }

    #[derive(PartialEq, Eq, Decodable)]
    struct TestPath {
        path: Path,
    }

    #[test]
    fn decode_path() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "path: test/dir",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: TestPath = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        assert!(val.path == Path::new("test/dir"));
        assert_eq!(warnings.len(), 0);
    }

    #[derive(PartialEq, Eq, Decodable)]
    struct TestPathMap {
        paths: BTreeMap<Path, isize>,
    }

    #[test]
    fn decode_path_map() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "paths: {test/dir: 1}",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: TestPathMap = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        let tree: BTreeMap<Path, isize>;
        tree = vec!((Path::new("test/dir"), 1)).into_iter().collect();
        assert!(val.paths == tree);
        assert_eq!(warnings.len(), 0);
    }

    #[derive(PartialEq, Eq, Decodable, Show)]
    #[allow(non_camel_case_types)]
    enum TestEnum {
        Alpha,
        Beta,
        beta_gamma,
        Gamma(isize),
        Delta(TestStruct),
        Sigma(Vec<isize>),
    }

    fn decode_enum(text: &str) -> TestEnum {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            text,
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: TestEnum = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        assert_eq!(warnings.len(), 0);
        return val;
    }

    #[test]
    fn test_enum_1() {
        assert_eq!(decode_enum("Alpha"), Alpha);
    }

    #[test]
    fn test_enum_2() {
        assert_eq!(decode_enum("Beta"), Beta);
    }

    #[test]
    fn test_enum_2_e() {
        assert_eq!(decode_enum("beta-gamma"), beta_gamma);
    }

    #[test]
    fn test_enum_3() {
        assert_eq!(decode_enum("!Beta"), Beta);
    }

    #[test]
    fn test_enum_4() {
        assert_eq!(decode_enum("!Alpha"), Alpha);
    }

    #[test]
    fn test_enum_5() {
        assert_eq!(decode_enum("!Gamma 5"), Gamma(5));
    }

    #[test]
    fn test_enum_map() {
        assert_eq!(decode_enum("!Delta\na: 1\nb: a"), Delta(TestStruct {
            a: 1,
            b: "a".to_string(),
            }));
    }

    #[test]
    fn test_enum_map_flow() {
        assert_eq!(decode_enum("!Delta {a: 2, b: b}"), Delta(TestStruct {
            a: 2,
            b: "b".to_string(),
            }));
    }

    #[test]
    fn test_enum_seq_flow() {
        assert_eq!(decode_enum("!Sigma [1, 2]"), Sigma(vec!(1, 2)));
    }

    #[test]
    fn test_enum_seq() {
        assert_eq!(decode_enum("!Sigma\n- 1\n- 2"), Sigma(vec!(1, 2)));
    }

}
