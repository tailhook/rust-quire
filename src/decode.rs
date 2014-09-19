use std::f64;
use std::str;
use serialize::Decoder;

use A = super::Ast;
use E = super::errors;

pub type DecodeResult<T> = Result<T, ()>;

/// A structure to decode Yaml to values in rust.
pub struct YamlDecoder<'a> {
    stack: Vec<Ast>,
    warnings: Vec<Warning>,
}

impl<'a> YamlDecoder<'a> {
    fn pop(&mut self) -> Node<'a> {
        self.stack.pop().unwrap()
    }
}


impl<'a> Decoder<DecoderError> for YamlDecoder<'a> {
    fn read_nil(&mut self) -> DecodeResult<()> {
        match self.pop() {
            A::Null(_, _, _) => Ok(()),
            node => {
                self.warnings.push(UnexpectedNode(node.pos(),
                    "null",
                    format!("{}", node)));
                Ok(())
            }
        }
    }

    fn from_str<T: FromStr+Default>(&mut self) -> DecodeResult<T> {
        match self.pop() {
            node@A::Scalar(pos, A::Plain, val) => {
                match FromStr::from_str() {
                    Some(x) => Ok(x),
                    None => {
                        self.warnings.push(CantParseValue(node.pos(),
                            format!("{}", TypeId.of::<T>())));
                        Ok(Default::default())
                    }
                }
            }
            node => {
                self.warnings.push(UnexpectedNode(node.pos(),
                    "Plain Scalar",
                    format!("{}", node)));
                Ok(Default::default())
            }
        }
    }

    fn read_u64(&mut self)  -> DecodeResult<u64> {
        Ok(try!(self.from_str()) as u64)
    }
    fn read_u32(&mut self)  -> DecodeResult<u32> {
        Ok(try!(self.from_str()) as u32)
    }
    fn read_u16(&mut self)  -> DecodeResult<u16> {
        Ok(try!(self.from_str()) as u16)
    }
    fn read_u8 (&mut self)  -> DecodeResult<u8> {
        Ok(try!(self.from_str()) as u8)
    }
    fn read_uint(&mut self) -> DecodeResult<uint> {
        Ok(try!(self.from_str()) as uint)
    }

    fn read_i64(&mut self) -> DecodeResult<i64> {
        Ok(try!(self.from_str()) as i64)
    }
    fn read_i32(&mut self) -> DecodeResult<i32> {
        Ok(try!(self.from_str()) as i32)
    }
    fn read_i16(&mut self) -> DecodeResult<i16> {
        Ok(try!(self.from_str()) as i16)
    }
    fn read_i8 (&mut self) -> DecodeResult<i8 > {
        Ok(try!(self.from_str()) as i8)
    }
    fn read_int(&mut self) -> DecodeResult<int> {
        Ok(try!(self.from_str()) as int)
    }

    fn read_bool(&mut self) -> DecodeResult<bool> {
        Ok(try!(self.from_str()) as bool)
    }

    fn read_f64(&mut self) -> DecodeResult<f64> {
        Ok(try!(self.from_str()) as f64)
    }

    fn read_f32(&mut self) -> DecodeResult<f32> {
        Ok(try!(self.from_str()))
    }


    fn read_char(&mut self) -> DecodeResult<char> {
        let s = try!(self.read_str());
        {
            let mut it = s.as_slice().chars();
            match (it.next(), it.next()) {
                // exactly one character
                (Some(c), None) => return Ok(c),
                _ => ()
            }
        }
        Err(ExpectedError("single character string".to_string(),
                          format!("{}", s)))
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
        println!("read_struct(name={}, len={})", name, len);
        let value = try!(f(self));
        self.pop();
        Ok(value)
    }

    fn read_struct_field<T>(&mut self,
                            name: &str,
                            idx: uint,
                            f: |&mut YamlDecoder| -> DecodeResult<T>)
                            -> DecodeResult<T> {
        println!("read_struct_field(name={}, idx={})", name, idx);
        let mut obj = try!(expect!(self.pop(), Map));

        let value = match obj.pop(&name.to_string()) {
            None => return Err(MissingFieldError(name.to_string())),
            Some(json) => {
                self.stack.push(json);
                try!(f(self))
            }
        };
        self.stack.push(Map(obj));
        Ok(value)
    }

    fn read_tuple<T>(&mut self, f: |&mut YamlDecoder, uint| -> DecodeResult<T>) -> DecodeResult<T> {
        println!("read_tuple()");
        self.read_seq(f)
    }

    fn read_tuple_arg<T>(&mut self,
                         idx: uint,
                         f: |&mut YamlDecoder| -> DecodeResult<T>) -> DecodeResult<T> {
        println!("read_tuple_arg(idx={})", idx);
        self.read_seq_elt(idx, f)
    }

    fn read_tuple_struct<T>(&mut self,
                            name: &str,
                            f: |&mut YamlDecoder, uint| -> DecodeResult<T>)
                            -> DecodeResult<T> {
        println!("read_tuple_struct(name={})", name);
        self.read_tuple(f)
    }

    fn read_tuple_struct_arg<T>(&mut self,
                                idx: uint,
                                f: |&mut YamlDecoder| -> DecodeResult<T>)
                                -> DecodeResult<T> {
        println!("read_tuple_struct_arg(idx={})", idx);
        self.read_tuple_arg(idx, f)
    }

    fn read_option<T>(&mut self, f: |&mut YamlDecoder, bool| -> DecodeResult<T>) -> DecodeResult<T> {
        match self.pop() {
            Null => f(self, false),
            value => { self.stack.push(value); f(self, true) }
        }
    }

    fn read_seq<T>(&mut self, f: |&mut YamlDecoder, uint| -> DecodeResult<T>) -> DecodeResult<T> {
        println!("read_seq()");
        let list = try!(expect!(self.pop(), List));
        let len = list.len();
        for v in list.move_iter().rev() {
            self.stack.push(v);
        }
        f(self, len)
    }

    fn read_seq_elt<T>(&mut self,
                       idx: uint,
                       f: |&mut YamlDecoder| -> DecodeResult<T>) -> DecodeResult<T> {
        println!("read_seq_elt(idx={})", idx);
        f(self)
    }

    fn read_map<T>(&mut self, f: |&mut YamlDecoder, uint| -> DecodeResult<T>) -> DecodeResult<T> {
        println!("read_map()");
        let obj = try!(expect!(self.pop(), Map));
        let len = obj.len();
        for (key, value) in obj.move_iter() {
            self.stack.push(value);
            self.stack.push(Scalar(key));
        }
        f(self, len)
    }

    fn read_map_elt_key<T>(&mut self, idx: uint, f: |&mut YamlDecoder| -> DecodeResult<T>)
                           -> DecodeResult<T> {
        println!("read_map_elt_key(idx={})", idx);
        f(self)
    }

    fn read_map_elt_val<T>(&mut self, idx: uint, f: |&mut YamlDecoder| -> DecodeResult<T>)
                           -> DecodeResult<T> {
        println!("read_map_elt_val(idx={})", idx);
        f(self)
    }
}
