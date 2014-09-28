macro_rules! Structure {
    ( $($nname:ident = $typ:ident { $($item:ident : $value:expr),* },)+ ) => {
        box Structure {
            members: vec!(
                $( ( stringify!($nname).to_string(),
                    validator!($typ { $($item : $value),* }) ) ),+
            ),
            .. Default::default()
        }
    };
    ( $($nname:ident = $typ:ident { $($item:ident : $value:expr),* }),+ ) => {
        box Structure {
            members: vec!(
                $( ( stringify!($nname).to_string(),
                    validator!($typ { $($item : $value),* }) ) ),+
            ),
            .. Default::default()
        } as Box<Validator>
    };
    ( $($nname:ident = $typ:ident { $($item:ident : $value:expr,)* },)+ ) => {
        box Structure {
            members: vec!(
                $( ( stringify!($nname).to_string(),
                    validator!($typ { $($item : $value),* }) ) ),+
            ),
            .. Default::default()
        }
    };
    ( $($nname:ident = $typ:ident { $($item:ident : $value:expr,)* }),+ ) => {
        box Structure {
            members: vec!(
                $( ( stringify!($nname).to_string(),
                    validator!($typ { $($item : $value),* }) ) ),+
            ),
            .. Default::default()
        } as Box<Validator>
    };
}

macro_rules! validator {
    ( $name:ident { $($item:ident : $value:expr),* } ) => {
        box $name {
            $( $item : Some($value), )*
        .. Default::default() } as Box<Validator>
    };
    ( $name:ident { $($item:ident : $value:expr,)* } ) => {
        box $name {
            $( $item : Some($value), )*
        .. Default::default() } as Box<Validator>
    };
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::default::Default;
    use serialize::Decodable;

    use super::super::decode::YamlDecoder;
    use super::super::ast::process;
    use super::super::parser::parse;
    use super::super::validate::{Validator, Structure, Scalar, Numeric};

    #[deriving(Clone, Show, PartialEq, Eq, Decodable)]
    struct TestStruct {
        intkey: uint,
        strkey: String,
    }

    fn parse_str(body: &str) -> TestStruct {
        Structure!(
            intkey=Numeric {
                default: 123u
            },
        );
        Structure!(
            intkey=Numeric {
                default: 123u
            }
        );
        Structure!(
            intkey=Numeric {
                default: 123u
            },
            strkey=Scalar {
                default: "default_value".to_string()
            }
        );
        let str_val = Structure!(
            intkey=Numeric {
                default: 123u
            },
            strkey=Scalar {
                descr: "Hello world".to_string(),
                default: "default_value".to_string()
            },
        );
        let (ast, warnings) = parse(Rc::new("<inline text>".to_string()), body,
            |doc| { process(Default::default(), doc) }).unwrap();
        assert_eq!(warnings.len(), 0);
        let (ast, warnings) = str_val.validate(ast);
        assert_eq!(warnings.len(), 0);
        let mut dec = YamlDecoder::new(ast);
        return Decodable::decode(&mut dec).unwrap();
    }

    #[test]
    fn test_all_fields() {
        assert_eq!(parse_str("intkey: 1\nstrkey: test"), TestStruct {
            intkey: 1,
            strkey: "test".to_string(),
        });
    }
}
