use std::vec::MoveItems;
use std::any::{Any, AnyMutRefExt};
use std::fmt::{Show, Formatter, FormatError};
use std::default::Default;
use std::from_str::FromStr;
use std::intrinsics::TypeId;
use serialize::{Decoder, Decodable};
use serialize::json::ToJson;
use serialize::json as J;

use super::ast as A;
use super::errors as E;

pub type DecodeResult<T> = Result<T, E::Warning>;

struct AnyJson(J::Json);

impl Deref<J::Json> for AnyJson {
    fn deref<'x>(&'x self) -> &'x J::Json {
        let AnyJson(ref val) = *self;
        return val;
    }
}

impl<D:Decoder<E> + 'static, E> Decodable<D, E> for AnyJson {
    fn decode(dec: &mut D) -> Result<AnyJson, E> {
        let dec: &mut YamlDecoder = (dec as &mut Any).as_mut().unwrap();
        return Ok(AnyJson(dec.pop().to_json()));
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

/// A structure to decode Yaml to values in rust.
pub struct YamlDecoder {
    stack: Vec<A::Ast>,
    bytes: Option<MoveItems<u8>>,
    warnings: Vec<E::Warning>,
}

impl YamlDecoder {

    pub fn new(ast: A::Ast) -> YamlDecoder {
        return YamlDecoder {
            stack: vec!(ast),
            bytes: None,
            warnings: Vec::new(),
        }
    }

    fn pop(&mut self) -> A::Ast {
        self.stack.pop().unwrap()
    }

    fn push(&mut self, ast: A::Ast) {
        self.stack.push(ast);
    }

    fn from_str<T: FromStr+Default+'static>(&mut self) -> DecodeResult<T> {
        match self.pop() {
            A::Scalar(ref pos, _, _, ref val) => {
                match FromStr::from_str(val.as_slice()) {
                    Some(x) => Ok(x),
                    None => {
                        return Err(E::CantParseValue(pos.clone(),
                            format!("{}", TypeId::of::<T>())));
                    }
                }
            }
            node => {
                return Err(E::UnexpectedNode(node.pos(),
                    "Plain Scalar",
                    format!("{}", node)));
            }
        }
    }
}


impl Decoder<E::Warning> for YamlDecoder {
    fn read_nil(&mut self) -> DecodeResult<()> {
        match self.pop() {
            A::Null(_, _, _) => Ok(()),
            node => {
                self.warnings.push(E::UnexpectedNode(node.pos(),
                    "null",
                    format!("{}", node)));
                Ok(())
            }
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
        match self.bytes {
            Some(ref mut iter) => {
                return Ok(iter.next().unwrap());
            }
            None => {
                return Ok(try!(self.from_str()))
            }
        };
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

    fn read_enum<T>(&mut self, name: &str,
        f: |&mut YamlDecoder| -> DecodeResult<T>) -> DecodeResult<T>
    {
        return f(self);
    }

    fn read_enum_variant<T>(&mut self,
                            names: &[&str],
                            f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
                            -> DecodeResult<T> {
        match self.pop() {
            ref node if node.tag().is_specific() => {
                match node.tag() {
                    &A::NonSpecific => unreachable!(),
                    &A::LocalTag(ref tag) => {
                        for (i, name) in names.iter().enumerate() {
                            if *name == tag.as_slice() {
                                return f(self, i);
                            }
                        }
                        return Err(E::UnexpectedNode(node.pos(),
                            "One of the supported tags",
                            format!("{} is not one of {}", tag, names)));
                    }
                    &A::GlobalTag(_) => unimplemented!(),
                }
            }
            A::Scalar(ref pos, _, _, ref value) => {
                for (i, name) in names.iter().enumerate() {
                    if *name == value.as_slice() {
                        return f(self, i);
                    }
                }
                return Err(E::UnexpectedNode(pos.clone(),
                    "One of the supported tags",
                    format!("{} is not one of {}", value, names)));
            }
            node => {
                return Err(E::UnexpectedNode(node.pos(),
                    "Scalar or tagged value", format!("{}", node)));
            }
        }
    }

    fn read_enum_variant_arg<T>(&mut self, idx: uint, f: |&mut YamlDecoder| -> DecodeResult<T>)
                                -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_enum_struct_variant<T>(&mut self,
                                   names: &[&str],
                                   f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
                                   -> DecodeResult<T> {
        unimplemented!();
    }


    fn read_enum_struct_variant_field<T>(&mut self,
                                         name: &str,
                                         idx: uint,
                                         f: |&mut YamlDecoder| -> DecodeResult<T>)
                                         -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_struct<T>(&mut self,
                      _name: &str,
                      _len: uint,
                      f: |&mut YamlDecoder| -> DecodeResult<T>)
                      -> DecodeResult<T>
    {
        let value = try!(f(self));
        self.pop();
        return Ok(value);
    }

    fn read_struct_field<T>(&mut self,
                            name: &str,
                            idx: uint,
                            f: |&mut YamlDecoder| -> DecodeResult<T>)
                            -> DecodeResult<T> {
        let (pos, tag, mut children) = match self.pop() {
            A::Map(pos, tag, children) => (pos, tag, children),
            _ => unimplemented!(),
        };

        let value = match children.pop(&name.to_string()) {
            None => return Err(E::MissingFieldError(
                pos.clone(), name.to_string())),
            Some(json) => {
                self.stack.push(json);
                try!(f(self))
            }
        };
        self.stack.push(A::Map(pos, tag, children));
        Ok(value)
    }

    fn read_tuple<T>(&mut self, f: |&mut YamlDecoder, uint| -> DecodeResult<T>) -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_tuple_arg<T>(&mut self,
                         idx: uint,
                         f: |&mut YamlDecoder| -> DecodeResult<T>) -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_tuple_struct<T>(&mut self,
                            name: &str,
                            f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
                            -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_tuple_struct_arg<T>(&mut self,
                                idx: uint,
                                f: |&mut YamlDecoder| -> DecodeResult<T>)
                                -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_option<T>(&mut self, f: |&mut YamlDecoder, bool| -> DecodeResult<T>) -> DecodeResult<T> {
        match self.pop() {
            A::Null(_, _, _) => f(self, false),
            node => { self.stack.push(node); f(self, true) }
        }
    }

    fn read_seq<T>(&mut self, f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        let node = self.pop();
        let len = match node {
            A::List(_, _, ref children) => {
                children.len()
            }
            A::Scalar(_, _, _, ref value) => {
                // TODO(tailhook) check for !!binary tag
                let vec = value.as_bytes().into_owned();
                let len = vec.len();
                self.bytes = Some(vec.move_iter());
                let value = try!(f(self, len));
                self.bytes = None;
                return Ok(value);
            }
            ref node => {
                self.warnings.push(E::UnexpectedNode(node.pos(),
                    "Sequence", format!("{}", node)));
                0
            }
        };
        self.push(node);
        let value = try!(f(self, len));
        self.pop();
        return Ok(value);
    }

    fn read_seq_elt<T>(&mut self, _idx: uint,
        f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        if self.bytes.is_some() {
            let value = try!(f(self));
            return Ok(value);
        }
        let (pos, tag, mut children) = match self.pop() {
            A::List(pos, tag, children) => (pos, tag, children),
            _ => unimplemented!(),
        };

        let ast = children.shift().unwrap();
        self.stack.push(ast);
        let value = try!(f(self));
        self.stack.push(A::List(pos, tag, children));
        Ok(value)
    }

    fn read_map<T>(&mut self, f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        let node = self.pop();
        let len = match node {
            A::Map(_, _, ref children) => {
                children.len()
            }
            ref node => {
                self.warnings.push(E::UnexpectedNode(node.pos(),
                    "Mapping", format!("{}", node)));
                0
            }
        };
        self.push(node);
        let value = try!(f(self, len));
        self.pop();
        return Ok(value);
    }

    fn read_map_elt_key<T>(&mut self, _idx: uint,
        f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        let (pos, tag, mut children) = match self.pop() {
            A::Map(pos, tag, children) => (pos, tag, children),
            _ => unimplemented!(),
        };

        let key = {
            let (key, _) = children.iter().next().unwrap();
            key.clone()
        };
        let val = children.pop(&key).unwrap();
        self.stack.push(A::Map(pos.clone(), tag, children));
        self.stack.push(val);
        self.stack.push(A::Scalar(pos, A::NonSpecific, A::Quoted, key));
        let value = try!(f(self));
        Ok(value)
    }

    fn read_map_elt_val<T>(&mut self, _idx: uint,
        f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        f(self)
    }

    fn error(&mut self, err: &str) -> E::Warning {
        E::DecoderError(err.to_string())
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
    use super::super::errors::Warning;
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
        let mut dec = YamlDecoder::new(ast);
        let val: TestStruct = Decodable::decode(&mut dec).unwrap();
        assert_eq!(val, TestStruct {
            a: 1,
            b: "hello".to_string(),
            });
    }

    #[test]
    fn decode_list() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "- a\n- b",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut dec = YamlDecoder::new(ast);
        let val: Vec<String> = Decodable::decode(&mut dec).unwrap();
        assert_eq!(val, vec!("a".to_string(), "b".to_string()));
    }

    #[test]
    fn decode_map() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "a: 1\nb: 2",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut dec = YamlDecoder::new(ast);
        let val: TreeMap<String, int> = Decodable::decode(&mut dec).unwrap();
        let mut res =  TreeMap::new();
        res.insert("a".to_string(), 1);
        res.insert("b".to_string(), 2);
        assert_eq!(val, res);
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
        let mut dec = YamlDecoder::new(ast);
        let val: TestJson = Decodable::decode(&mut dec).unwrap();
        assert_eq!(val, TestJson {
            json: AnyJson(J::from_str(r#"{"a": 1, "b": "test"}"#).unwrap()),
            });
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
        let mut dec = YamlDecoder::new(ast);
        let val: TestOption = Decodable::decode(&mut dec).unwrap();
        assert!(val.path == Some("test/value".to_string()));
    }

    #[test]
    fn decode_option_none() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "path:",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut dec = YamlDecoder::new(ast);
        let val: TestOption = Decodable::decode(&mut dec).unwrap();
        assert!(val.path == None);
    }

    #[test]
    fn decode_option_no_key() {
        // This one should fail, this would be worked out by validators
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "{}",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut dec = YamlDecoder::new(ast);
        let val: Result<TestOption, Warning>;
        val = Decodable::decode(&mut dec);
        val.unwrap_err();
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
        let mut dec = YamlDecoder::new(ast);
        let val: TestPath = Decodable::decode(&mut dec).unwrap();
        assert!(val.path == Path::new("test/dir"));
    }

    #[deriving(PartialEq, Eq, Decodable, Show)]
    enum TestEnum {
        Alpha,
        Beta,
    }

    fn decode_enum(text: &str) -> TestEnum {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            text,
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut dec = YamlDecoder::new(ast);
        return Decodable::decode(&mut dec).unwrap();
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
    fn test_enum_3() {
        assert_eq!(decode_enum("!Beta"), Beta);
    }

    #[test]
    fn test_enum_4() {
        assert_eq!(decode_enum("!Alpha"), Alpha);
    }

}
