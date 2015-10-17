use std::rc::Rc;
use std::default::Default;
use std::sync::mpsc::channel;
use rustc_serialize::Decodable;

use super::decode::YamlDecoder;
use super::ast::process;
use super::parser::parse;


#[derive(RustcDecodable, PartialEq, Eq, Debug)]
struct Struct1 {
    list: Vec<Struct2>,
}

#[derive(RustcDecodable, PartialEq, Eq, Debug)]
struct Struct2 {
    value: String,
}


fn decode_struct(data: &str) -> Result<Struct1, String> {
    let (ast, _) = try!(parse(Rc::new("<inline text>".to_string()),
        data,
        |doc| { process(Default::default(), doc) })
        .map_err(|e| e.to_string()));
    let mut warnings = vec!();
    let (tx, rx) = channel();
    let val: Struct1 = {
        let mut dec = YamlDecoder::new(ast, tx);
        try!(Decodable::decode(&mut dec)
            .map_err(|e| format!("{}", e)))
    };
    warnings.extend(rx.iter());
    if warnings.len() > 0 {
        return Err(format!("{:?}", warnings));
    } else {
        return Ok(val);
    }
}


#[test]
fn test_path() {
    assert_eq!(decode_struct("list:\n- {}"),
        Err("<inline text>:2:3: Decode error at .list[0].value: \
            Expected scalar, got Null".to_string()));
}

#[test]
fn test_unknown_alias() {
    assert_eq!(decode_struct("- *x"),
        Err("<inline text>:1:3: Parse Error: \
            Unknown alias \"x\"".to_string()));
}

#[test]
fn test_unknown_alias_flow() {
    assert_eq!(decode_struct("- [ *x ]"),
        Err("<inline text>:1:5: Parse Error: \
            Unknown alias \"x\"".to_string()));
}
