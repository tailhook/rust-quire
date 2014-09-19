use std::f64;
use std::str;
use std::default::Default;
use std::from_str::FromStr;
use std::intrinsics::TypeId;
use serialize::Decoder;

use A = super::ast;
use E = super::errors;

pub type DecodeResult<T> = Result<T, ()>;

/// A structure to decode Yaml to values in rust.
pub struct YamlDecoder {
    stack: Vec<A::Ast>,
    warnings: Vec<E::Warning>,
}

impl YamlDecoder {

    fn new(ast: A::Ast) -> YamlDecoder {
        return YamlDecoder {
            stack: vec!(ast),
            warnings: Vec::new(),
        }
    }

    fn pop(&mut self) -> A::Ast {
        self.stack.pop().unwrap()
    }

    fn from_str<T: FromStr+Default+'static>(&mut self) -> DecodeResult<T> {
        match self.pop() {
            ref node@A::Scalar(ref pos, _, A::Plain, ref val) => {
                match FromStr::from_str(val.as_slice()) {
                    Some(x) => Ok(x),
                    None => {
                        self.warnings.push(E::CantParseValue(node.pos(),
                            format!("{}", TypeId::of::<T>())));
                        Ok(Default::default())
                    }
                }
            }
            node => {
                self.warnings.push(E::UnexpectedNode(node.pos(),
                    "Plain Scalar",
                    format!("{}", node)));
                Ok(Default::default())
            }
        }
    }
}


impl Decoder<()> for YamlDecoder {
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

    fn read_enum<T>(&mut self,
                    name: &str,
                    f: |&mut YamlDecoder| -> DecodeResult<T>) -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_enum_variant<T>(&mut self,
                            names: &[&str],
                            f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
                            -> DecodeResult<T> {
        unimplemented!();
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
                      name: &str,
                      len: uint,
                      f: |&mut YamlDecoder| -> DecodeResult<T>)
                      -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_struct_field<T>(&mut self,
                            name: &str,
                            idx: uint,
                            f: |&mut YamlDecoder| -> DecodeResult<T>)
                            -> DecodeResult<T> {
        unimplemented!();
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

    fn read_seq<T>(&mut self, f: |&mut YamlDecoder, uint| -> DecodeResult<T>) -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_seq_elt<T>(&mut self,
                       idx: uint,
                       f: |&mut YamlDecoder| -> DecodeResult<T>) -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_map<T>(&mut self, f: |&mut YamlDecoder, uint| -> DecodeResult<T>) -> DecodeResult<T> {
        unimplemented!();
    }

    fn read_map_elt_key<T>(&mut self, idx: uint,
        f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T>
    {
        unimplemented!();
    }

    fn read_map_elt_val<T>(&mut self, idx: uint, f: |&mut YamlDecoder| -> DecodeResult<T>)
        -> DecodeResult<T> {
        unimplemented!();
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::default::Default;
    use super::YamlDecoder;
    use super::super::parser::parse;
    use super::super::ast::process;
    use serialize::Decodable;

    #[deriving(Clone, Show, PartialEq, Eq, Default, Decodable)]
    struct TestStruct {
        a: uint,
        b: String,
    }

    #[test]
    fn decode_struct() {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()),
            "a: 1, b: hello",
            |doc| { process(Default::default(), doc) }).unwrap();
        let mut dec = YamlDecoder::new(ast);
        let val: TestStruct = Decodable::decode(&mut dec).unwrap();
        assert_eq!(val, TestStruct {
            a: 1,
            b: "hello".to_string(),
            });
    }
}
