use std::rc::Rc;

use ast::process;
use de::Deserializer;
use errors::ErrorCollector;
use parser::parse;
use serde::Deserialize;
use {Options};


#[derive(Deserialize, PartialEq, Eq, Debug)]
struct Struct1 {
    list: Vec<Struct2>,
}

#[derive(Deserialize, PartialEq, Eq, Debug)]
struct Struct2 {
    value: String,
}


fn decode<'x, T: Deserialize<'x>>(data: &str) -> Result<T, String> {
    let err = ErrorCollector::new();
    let ast = parse(
            Rc::new("<inline text>".to_string()),
            data,
            |doc| { process(&Options::default(), doc, &err) }
        ).map_err(|e| err.into_fatal(e).to_string())?;
    T::deserialize(&mut Deserializer::new(&ast, &err))
    .map_err(|e| err.into_fatal(e))
    .and_then(|v| err.into_result(v))
    .map_err(|e| e.to_string())
}

fn decode_struct(data: &str) -> Result<Struct1, String> {
    decode(data)
}


#[test]
fn test_path() {
    assert_eq!(decode_struct("list:\n- {}"),
        Err("missing field `value`\n".to_string()));
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

#[test]
fn test_non_working_includes() {
    assert_eq!(decode_struct("list: !*Include 'y.yaml'"),
        Err("<inline text>:1:17: Preprocess Error: \
            Includes are not supported\n".to_string()));
}
