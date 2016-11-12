use std::rc::Rc;
use std::path::PathBuf;
use std::collections::BTreeMap;
use rustc_serialize::Decodable;

use decode::YamlDecoder;
use parser::parse;
use ast::process;
use errors::ErrorCollector;

use {Options};

pub fn decode<T: Decodable>(data: &str) -> T {
    let err = ErrorCollector::new();
    let ast = parse(
            Rc::new("<inline text>".to_string()),
            data,
            |doc| { process(&Options::default(), doc, &err) }
        ).map_err(|e| err.into_fatal(e)).unwrap();
    Decodable::decode(&mut YamlDecoder::new(ast, &err))
    .map_err(|e| err.into_fatal(e))
    .unwrap()
}
