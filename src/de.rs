use std::marker::PhantomData;
use std::ops::{Neg, AddAssign, MulAssign};
use std::str::FromStr;


use serde::de::{self, Deserialize, DeserializeSeed, Visitor, SeqAccess,
                MapAccess, EnumAccess, VariantAccess, IntoDeserializer};

use ast::Ast;
use ast::Ast as A;
use errors::{Error, ErrorCollector};

type Result<T> = ::std::result::Result<T, Error>;

pub struct Deserializer<'de> {
    ast: Ast,
    err: ErrorCollector,
    path: String,
    phantom: PhantomData<&'de ()>,
}

impl<'de> Deserializer<'de> {
    // By convention, `Deserializer` constructors are named like `from_xyz`.
    // That way basic use cases are satisfied by something like
    // `serde_json::from_str(...)` while advanced use cases that require a
    // deserializer can make one with `serde_json::Deserializer::from_str(...)`.
    pub fn new<'x>(ast: Ast, err: &ErrorCollector) -> Deserializer<'x> {
        Deserializer {
            ast,
            err: err.clone(),
            path: "".to_string(),
            phantom: PhantomData,
        }
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    // Look at the input data to decide what Serde data model type to
    // deserialize as. Not all data formats are able to support this operation.
    // Formats that support `deserialize_any` are known as self-describing.
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!();
    }

    // Uses the `parse_bool` parsing function defined above to read the JSON
    // identifier `true` or `false` from the input.
    //
    // Parsing refers to looking at the input and deciding that it contains the
    // JSON value `true` or `false`.
    //
    // Deserialization refers to mapping that JSON value into Serde's data
    // model by invoking one of the `Visitor` methods. In the case of JSON and
    // bool that mapping is straightforward so the distinction may seem silly,
    // but in other cases Deserializers sometimes perform non-obvious mappings.
    // For example the TOML format has a Datetime type and Serde's data model
    // does not. In the `toml` crate, a Datetime in the input is deserialized by
    // mapping it to a Serde data model "struct" type with a special name and a
    // single field containing the Datetime represented as a string.
    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        let value = match self.ast {
            A::Scalar(ref pos, _, _, ref val) => {
                match &val[..] {
                    "true" => true,
                    "false" => false,
                    _ => {
                        return Err(Error::decode_error(pos, &self.path,
                            format!("bad boolean {:?}", val)));
                    }
                }
            }
            ref node => {
                return Err(Error::decode_error(&node.pos(), &self.path,
                    format!("Can't parse {:?} as boolean", node)));
            }
        };
        visitor.visit_bool(value)
    }

    // The `parse_signed` function is generic over the integer type `T` so here
    // it is invoked with `T=i8`. The next 8 methods are similar.
    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        visitor.visit_i8(from_str(self, "i8")?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        visitor.visit_i16(from_str(self, "i16")?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        visitor.visit_i32(from_str(self, "i32")?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        visitor.visit_i64(from_str(self, "i64")?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        visitor.visit_u8(from_str(self, "u8")?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        visitor.visit_u16(from_str(self, "u16")?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        visitor.visit_u32(from_str(self, "u32")?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        visitor.visit_u64(from_str(self, "u64")?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        visitor.visit_f32(from_str(self, "f32")?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        visitor.visit_f64(from_str(self, "f64")?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        let c = match self.ast {
            A::Scalar(ref pos, _, _, ref val) => {
                if val.len() != 1 {
                    Err(Error::decode_error(pos, &self.path,
                        format!("should be single char: {:?}", val)))
                } else {
                    Ok(val.chars().next().unwrap())
                }
            }
            ref node => {
                Err(Error::decode_error(&node.pos(), &self.path,
                    format!("Can't parse {:?} as char", node)))
            }
        };
        visitor.visit_char(c?)
    }

    // Refer to the "Understanding deserializer lifetimes" page for information
    // about the three deserialization flavors of strings in Serde.
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!()
    }

    // The `Serializer` implementation on the previous page serialized byte
    // arrays as JSON arrays of bytes. Handle that representation here.
    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!()
    }

    // An absent optional is represented as the JSON `null` and a present
    // optional is represented as just the contained value.
    //
    // As commented in `Serializer` implementation, this is a lossy
    // representation. For example the values `Some(())` and `None` both
    // serialize as just `null`. Unfortunately this is typically what people
    // expect when working with JSON. Other formats are encouraged to behave
    // more intelligently if possible.
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!()
    }

    // In Serde, unit means an anonymous value containing no data.
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!()
    }

    // Unit struct means a named value containing no data.
    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V
    ) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!()
    }

    // As is done here, serializers are encouraged to treat newtype structs as
    // insignificant wrappers around the data they contain. That means not
    // parsing anything other than the contained value.
    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V
    ) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!()
    }

    // Deserialization of compound types like sequences and maps happens by
    // passing the visitor an "Access" object that gives it the ability to
    // iterate through the data contained in the sequence.
    fn deserialize_seq<V>(mut self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!();
    }

    // Tuples look just like sequences in JSON. Some formats may be able to
    // represent tuples more efficiently.
    //
    // As indicated by the length parameter, the `Deserialize` implementation
    // for a tuple in the Serde data model is required to know the length of the
    // tuple before even looking at the input data.
    fn deserialize_tuple<V>(
        self,
        _len: usize,
        visitor: V
    ) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!();
    }

    // Tuple structs look just like sequences in JSON.
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V
    ) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!();
    }

    // Much like `deserialize_seq` but calls the visitors `visit_map` method
    // with a `MapAccess` implementation, rather than the visitor's `visit_seq`
    // method with a `SeqAccess` implementation.
    fn deserialize_map<V>(mut self, visitor: V) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!();
    }

    // Structs look just like maps in JSON.
    //
    // Notice the `fields` parameter - a "struct" in the Serde data model means
    // that the `Deserialize` implementation is required to know what the fields
    // are before even looking at the input data. Any key-value pairing in which
    // the fields cannot be known ahead of time is probably a map.
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V
    ) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!();
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V
    ) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!();
    }

    // An identifier in Serde is the type that identifies a field of a struct or
    // the variant of an enum. In JSON, struct fields and enum variants are
    // represented as strings. In other formats they may be represented as
    // numeric indices.
    fn deserialize_identifier<V>(
        self,
        visitor: V
    ) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!();
    }

    // Like `deserialize_any` but indicates to the `Deserializer` that it makes
    // no difference which `Visitor` method is called because the data is
    // ignored.
    //
    // Some deserializers are able to implement this more efficiently than
    // `deserialize_any`, for example by rapidly skipping over matched
    // delimiters without paying close attention to the data in between.
    //
    // Some formats are not able to implement this at all. Formats that can
    // implement `deserialize_any` and `deserialize_ignored_any` are known as
    // self-describing.
    fn deserialize_ignored_any<V>(
        self,
        visitor: V
    ) -> Result<V::Value>
        where V: Visitor<'de>
    {
        unimplemented!();
    }
}

fn from_str<T: FromStr>(dec: &mut Deserializer, typename: &str) -> Result<T> {
    match dec.ast {
        A::Scalar(ref pos, _, _, ref val) => {
            match FromStr::from_str(val) {
                Ok(val) => Ok(val),
                _ => {
                    Err(Error::decode_error(pos, &dec.path,
                        format!("bad {}: {:?}", typename, val)))
                }
            }
        }
        ref node => {
            Err(Error::decode_error(&node.pos(), &dec.path,
                format!("Can't parse {:?} as {}", node, typename)))
        }
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::path::PathBuf;
    use std::collections::BTreeMap;

    use serde::Deserialize;

    use parser::parse;
    use ast::process;
    use errors::ErrorCollector;
    use {Options};
    use super::Deserializer;
    use self::TestEnum::*;

    fn decode<'x, T: Deserialize<'x>>(data: &str) -> T {
        let err = ErrorCollector::new();
        let ast = parse(
                Rc::new("<inline text>".to_string()),
                data,
                |doc| { process(&Options::default(), doc, &err) }
            ).map_err(|e| err.into_fatal(e)).unwrap();
        T::deserialize(&mut Deserializer::new(ast, &err))
        .map_err(|e| err.into_fatal(e))
        .unwrap()
    }

    #[derive(Clone, Debug, PartialEq, Eq, Deserialize)]
    struct TestStruct {
        a: usize,
        b: String,
    }

    #[test]
    fn decode_bool() {
        assert_eq!(decode::<bool>("true"), true);
        assert_eq!(decode::<bool>("false"), false);
    }

    #[test]
    fn decode_i8() {
        assert_eq!(decode::<i8>("1"), 1);
        assert_eq!(decode::<i8>("123"), 123);
        assert_eq!(decode::<i8>("0"), 0);
    }

    #[test]
    fn decode_char() {
        assert_eq!(decode::<char>("1"), '1');
        assert_eq!(decode::<char>("x"), 'x');
        assert_eq!(decode::<char>("\"y\""), 'y');
    }

    #[test]
    fn decode_struct() {
        assert_eq!(decode::<TestStruct>("a: 1\nb: hello"), TestStruct {
            a: 1,
            b: "hello".to_string(),
            });
    }

    #[test]
    fn decode_list() {
        assert_eq!(decode::<Vec<String>>("- a\n- b"),
                   vec!("a".to_string(), "b".to_string()));
    }

    #[test]
    #[should_panic(expected="Expected sequence, got string")]
    fn decode_list_error() {
        decode::<Vec<String>>("test");
    }

    #[test]
    fn decode_map() {
        let mut res =  BTreeMap::new();
        res.insert("a".to_string(), 1);
        res.insert("b".to_string(), 2);
        assert_eq!(decode::<BTreeMap<String, isize>>("a: 1\nb: 2"), res);
    }


    #[derive(PartialEq, Eq, Deserialize, Debug)]
    struct TestOption {
        path: Option<String>,
    }
    /*
    #[derive(Debug, PartialEq, Eq, Deserialize)]
    struct TestJson {
        json: AnyJson,
    }


    This test does not compile for some reason
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
*/

    #[test]
    fn decode_option_some() {
        let val: TestOption = decode("path: test/value");
        assert!(val.path == Some("test/value".to_string()));
    }

    #[test]
    fn decode_option_none() {
        let val: TestOption = decode("path:");
        assert!(val.path == None);
    }

    #[test]
    fn decode_option_no_key() {
        let val: TestOption = decode("{}");
        assert!(val.path == None);
    }

    #[derive(PartialEq, Eq, Deserialize)]
    struct TestPath {
        path: PathBuf,
    }

    #[test]
    fn decode_path() {
        let val: TestPath = decode("path: test/dir");
        assert!(val.path == PathBuf::from("test/dir"));
    }

    #[derive(PartialEq, Eq, Deserialize)]
    struct TestPathMap {
        paths: BTreeMap<PathBuf, isize>,
    }

    #[test]
    fn decode_path_map() {
        let val: TestPathMap = decode("paths: {test/dir: 1}");
        let tree: BTreeMap<PathBuf, isize>;
        tree = vec!((PathBuf::from("test/dir"), 1)).into_iter().collect();
        assert!(val.paths == tree);
    }

    #[derive(PartialEq, Eq, Deserialize, Debug)]
    #[allow(non_camel_case_types)]
    enum TestEnum {
        Alpha,
        Beta,
        beta_gamma,
        Gamma(isize),
        Delta(TestStruct),
        Sigma(Vec<isize>),
    }

    #[test]
    fn test_enum_1() {
        assert_eq!(decode::<TestEnum>("Alpha"), Alpha);
    }

    #[test]
    fn test_enum_2() {
        assert_eq!(decode::<TestEnum>("Beta"), Beta);
    }

    #[test]
    fn test_enum_2_e() {
        assert_eq!(decode::<TestEnum>("beta-gamma"), beta_gamma);
    }

    #[test]
    fn test_enum_3() {
        assert_eq!(decode::<TestEnum>("!Beta"), Beta);
    }

    #[test]
    fn test_enum_4() {
        assert_eq!(decode::<TestEnum>("!Alpha"), Alpha);
    }

    #[test]
    fn test_enum_5() {
        assert_eq!(decode::<TestEnum>("!Gamma 5"), Gamma(5));
    }

    #[test]
    fn test_enum_map() {
        assert_eq!(decode::<TestEnum>("!Delta\na: 1\nb: a"), Delta(TestStruct {
            a: 1,
            b: "a".to_string(),
            }));
    }

    #[test]
    fn test_enum_map_flow() {
        assert_eq!(decode::<TestEnum>("!Delta {a: 2, b: b}"), Delta(TestStruct {
            a: 2,
            b: "b".to_string(),
            }));
    }

    #[test]
    fn test_enum_seq_flow() {
        assert_eq!(decode::<TestEnum>("!Sigma [1, 2]"), Sigma(vec!(1, 2)));
    }

    #[test]
    fn test_enum_seq() {
        assert_eq!(decode::<TestEnum>("!Sigma\n- 1\n- 2"), Sigma(vec!(1, 2)));
    }

    #[derive(PartialEq, Eq, Deserialize, Debug)]
    struct TestStruct2 {
        items: Vec<TestEnum>,
    }

    #[test]
    #[should_panic(expected = "Expected sequence, got string")]
    fn test_struct_items_tag() {
        decode::<TestStruct2>("items:\n  'hello'");
    }

}
