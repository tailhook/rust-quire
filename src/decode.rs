use std::mem::swap;
use std::any::{Any, AnyMutRefExt};
use std::fmt::{Show, Formatter, FormatError};
use std::default::Default;
use std::from_str::FromStr;
use std::intrinsics::TypeId;
use serialize::{Decoder, Decodable};
use serialize::json::ToJson;
use serialize::json as J;

use super::ast as A;
use super::errors::Error;
use super::tokenizer::Pos;


pub type DecodeResult<T> = Result<T, Error>;

struct AnyJson(J::Json);

impl Deref<J::Json> for AnyJson {
    fn deref<'x>(&'x self) -> &'x J::Json {
        let AnyJson(ref val) = *self;
        return val;
    }
}

impl<D:Decoder<E> + 'static, E> Decodable<D, E> for AnyJson {
    fn decode(dec: &mut D) -> Result<AnyJson, E> {
        let dec: &mut YamlDecoder = (dec as &mut Any).downcast_mut().unwrap();
        match dec.state {
            Node(ref node) => {
                return Ok(AnyJson(node.to_json()));
            }
            Byte(_) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_) => unreachable!(),
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
        selfj.fmt(fmt)
    }
}

enum ParserState {
    Node(A::Ast),
    Map(Vec<(String, A::Ast)>),  // used only in read_map_elt_key/elt_val
    Seq(Vec<A::Ast>),  // used only in read_seq_elt
    ByteSeq(Vec<u8>),  // used for decoding Path
    Byte(u8),     // used for decoding Path
    Key(Pos, String),
}

pub struct YamlDecoder {
    state: ParserState,
    sender: Sender<Error>,
    path: String,
}

impl YamlDecoder {

    pub fn new(ast: A::Ast, sender: Sender<Error>)
        -> YamlDecoder
    {
        return YamlDecoder {
            state: Node(ast),
            sender: sender,
            path: "".to_string(),
        }
    }

    fn from_str<T: FromStr+Default+'static>(&mut self) -> DecodeResult<T> {
        match self.state {
            Node(A::Scalar(ref pos, _, _, ref val)) | Key(ref pos, ref val) => {
                match FromStr::from_str(val.as_slice()) {
                    Some(x) => Ok(x),
                    None => {
                        return Err(Error::decode_error(pos, &self.path,
                            format!("Can't parse value of type: {}",
                                    TypeId::of::<T>())));
                    }
                }
            }
            Node(ref node) => {
                return Err(Error::decode_error(&node.pos(), &self.path,
                    format!("Expected scalar, got {}", node)));
            }
            Byte(_) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_) => unreachable!(),
        }
    }
}


impl Decoder<Error> for YamlDecoder {
    fn read_nil(&mut self) -> DecodeResult<()> {
        match self.state {
            Node(A::Null(_, _, _)) => return Ok(()),
            Node(ref node) => {
                self.sender.send(Error::decode_error(&node.pos(), &self.path,
                    format!("Expected null")));
                return Ok(())
            }
            Key(_, _) => unimplemented!(),
            Byte(_) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_) => unreachable!(),
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
        if let Byte(x) = self.state {
            return Ok(x);
        }
        Ok(try!(self.from_str()))
    }
    fn read_uint(&mut self) -> DecodeResult<uint> {
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
    fn read_int(&mut self) -> DecodeResult<int> {
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
        Ok(try!(self.from_str()))
    }

    fn read_enum<T>(&mut self, _name: &str,
        f: |&mut YamlDecoder| -> DecodeResult<T>) -> DecodeResult<T>
    {
        return f(self);
    }

    fn read_enum_variant<T>(&mut self,
        names: &[&str], f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        let mut idx = None;
        match self.state {
            Node(ref node) if node.tag().is_specific() => {
                match node.tag() {
                    &A::NonSpecific => unreachable!(),
                    &A::LocalTag(ref tag) => {
                        for (i, name) in names.iter().enumerate() {
                            if *name == tag.as_slice() {
                                idx = Some(i);
                            }
                        }
                        if idx.is_none() {
                            return Err(Error::decode_error(&node.pos(),
                                &self.path,
                                format!("{} is not one of {}", tag, names)));
                        }
                    }
                    &A::GlobalTag(_) => unimplemented!(),
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
                        format!("{} is not one of {}", value, names)));
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

    fn read_enum_variant_arg<T>(&mut self, idx: uint,
        f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        if idx == 0 {
            return f(self);
        }
        unimplemented!();
    }

    fn read_enum_struct_variant<T>(&mut self,
        names: &[&str], f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        unimplemented!();
    }


    fn read_enum_struct_variant_field<T>(&mut self,
        _name: &str, _idx: uint, _f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_struct<T>(&mut self,
        _name: &str, _len: uint, f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        match self.state {
            Node(A::Map(_, _, _)) => {}
            Node(A::Null(ref pos, _, _)) => {
                return f(&mut YamlDecoder {
                    state: Node(A::Map(pos.clone(), A::NonSpecific,
                        Default::default())),
                    sender: self.sender.clone(),
                    path: self.path.clone(),
                });
            }
            Node(ref node) => {
                return Err(Error::decode_error(&node.pos(), &self.path,
                    "Mapping expected".to_string()));
            }
            Byte(_) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_) => unreachable!(),
            Key(_, _) => unimplemented!(),
        };
        return f(self);
    }

    fn read_struct_field<T>(&mut self,
        name: &str, _idx: uint, f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        if let Node(A::Map(ref pos, _, ref mut children)) = self.state {
            match children.pop(&name.to_string()) {
                None => {
                    return f(&mut YamlDecoder {
                        state: Node(A::Null(pos.clone(), A::NonSpecific,
                            A::Implicit)),
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

    fn read_tuple<T>(&mut self,
        _f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_tuple_arg<T>(&mut self,
        idx: uint, f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_tuple_struct<T>(&mut self,
        name: &str, f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_tuple_struct_arg<T>(&mut self,
        idx: uint, f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_option<T>(&mut self,
        f: |&mut YamlDecoder, bool| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        match self.state {
            Node(A::Null(_, _, _)) => f(self, false),
            Node(_) => f(self, true),
            Key(_, _) => unimplemented!(),
            Byte(_) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_) => unreachable!(),
        }
    }

    fn read_seq<T>(&mut self,
        f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        let items = match self.state {
            Node(A::List(_, _, ref mut children)) => {
                let mut ch = Default::default();
                swap(children, &mut ch);
                ch
            }
            Node(A::Scalar(_, _, _, ref val)) => {
                let bytes = val.as_bytes();
                return f(&mut YamlDecoder {
                    state: ByteSeq(bytes.to_vec()),
                    sender: self.sender.clone(),
                    path: self.path.clone(),
                }, bytes.len());
            }
            Node(A::Null(_, _, _)) => Vec::new(),
            Node(ref node) => {
                return Err(Error::decode_error(&node.pos(), &self.path,
                    "Sequence expected".to_string()));
            }
            Byte(_) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_) => unreachable!(),
            Key(_, _) => unimplemented!(),
        };
        let len = items.len();
        return f(&mut YamlDecoder {
            state: Seq(items),
            sender: self.sender.clone(),
            path: self.path.clone(),
        }, len);
    }

    fn read_seq_elt<T>(&mut self, idx: uint,
        f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        match self.state {
            Seq(ref mut els) => {
                let val = els.remove(0).unwrap();
                return f(&mut YamlDecoder {
                    state: Node(val),
                    sender: self.sender.clone(),
                    path: format!("{}[{}]", self.path, idx),
                });
            }
            ByteSeq(ref vec) => {
                return f(&mut YamlDecoder {
                    state: Byte(vec[idx]),
                    sender: self.sender.clone(),
                    path: format!("{}[{}]", self.path, idx),
                });
            }
            _ => unreachable!(),
        }
    }

    fn read_map<T>(&mut self,
        f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
        -> DecodeResult<T>
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
            Byte(_) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_) => unreachable!(),
            Key(_, _) => unimplemented!(),
        };
        let len = items.len();
        return f(&mut YamlDecoder {
            state: Map(items),
            sender: self.sender.clone(),
            path: self.path.clone(),
        }, len);
    }

    fn read_map_elt_key<T>(&mut self, _idx: uint,
        f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
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

    fn read_map_elt_val<T>(&mut self, _idx: uint,
        f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        if let Map(ref mut els) = self.state {
            let (key, val) = els.remove(0).unwrap();
            return f(&mut YamlDecoder {
                state: Node(val),
                sender: self.sender.clone(),
                path: self.path.clone() + "." + key,
            });
        }
        unreachable!();
    }

    fn error(&mut self, err: &str) -> Error {
        let pos = match self.state {
            Node(ref node) => node.pos().clone(),
            Byte(_) => unimplemented!(),
            Map(_) | Seq(_) | ByteSeq(_) => unimplemented!(),
            Key(ref pos, _) => pos.clone(),
        };
        return Error::decode_error(&pos, &self.path, err.to_string())
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::default::Default;
    use std::collections::TreeMap;
    use super::YamlDecoder;
    use super::super::parser::parse;
    use super::super::ast::process;
    use serialize::{Decodable, Decoder};
    use super::AnyJson;
    use serialize::json as J;

    #[deriving(Clone, Show, PartialEq, Eq, Decodable)]
    struct TestStruct {
        a: uint,
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
    fn decode_map() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "a: 1\nb: 2",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut warnings = vec!();
        let (tx, rx) = channel();
        let val: TreeMap<String, int> = {
            let mut dec = YamlDecoder::new(ast, tx);
            Decodable::decode(&mut dec).unwrap()
        };
        warnings.extend(rx.iter());
        let mut res =  TreeMap::new();
        res.insert("a".to_string(), 1);
        res.insert("b".to_string(), 2);
        assert_eq!(val, res);
        assert_eq!(warnings.len(), 0);
    }

    #[deriving(Show, PartialEq, Eq, Decodable)]
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
            json: AnyJson(J::from_str(r#"{"a": 1, "b": "test"}"#).unwrap()),
            });
        assert_eq!(warnings.len(), 0);
    }

    #[deriving(PartialEq, Eq, Decodable, Show)]
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

    #[deriving(PartialEq, Eq, Decodable)]
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

    #[deriving(PartialEq, Eq, Decodable, Show)]
    #[allow(non_camel_case_types)]
    enum TestEnum {
        Alpha,
        Beta,
        beta_gamma,
        Gamma(int),
        Delta(TestStruct),
        Sigma(Vec<int>),
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
