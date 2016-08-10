use std::rc::Rc;
use std::default::Default;
use rustc_serialize::Decodable;

use super::decode::YamlDecoder;
use super::ast::process;
use super::parser::parse;
use super::errors::ErrorCollector;


#[derive(RustcDecodable, PartialEq, Eq, Debug)]
struct Struct1 {
    list: Vec<Struct2>,
}

#[derive(RustcDecodable, PartialEq, Eq, Debug)]
struct Struct2 {
    value: String,
}


fn decode_struct(data: &str) -> Result<Struct1, String> {
    let err = ErrorCollector::new();
    parse(
            Rc::new("<inline text>".to_string()),
            data,
            |doc| { process(Default::default(), doc, &err) }
    ).map_err(|e| err.into_fatal(e))
    .and_then(|ast| {
        Decodable::decode(&mut YamlDecoder::new(ast, &err))
        .map_err(|e| err.into_fatal(e))
        .and_then(|v| err.into_result(v))
    })
    .map_err(|e| format!("{}", e))
}


#[test]
fn test_path() {
    assert_eq!(decode_struct("list:\n- {}"),
        Err("<inline text>:2:3: Decode error at .list[0].value: \
            Expected scalar, got Null\n".to_string()));
}

#[test]
fn test_unknown_alias() {
    assert_eq!(decode_struct("- *x"),
        Err("<inline text>:1:3: Parse Error: \
            Unknown alias \"x\"\n".to_string()));
}

#[test]
fn test_unknown_alias_flow() {
    assert_eq!(decode_struct("- [ *x ]"),
        Err("<inline text>:1:5: Parse Error: \
            Unknown alias \"x\"\n".to_string()));
}
