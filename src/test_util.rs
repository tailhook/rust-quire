use std::rc::Rc;

use serde::Deserialize;

use de::Deserializer;
use parser::parse;
use ast::process;
use errors::ErrorCollector;

use {Options};

pub fn decode<'x, T: Deserialize<'x>>(data: &str) -> T {
    let err = ErrorCollector::new();
    let ast = parse(
            Rc::new("<inline text>".to_string()),
            data,
            |doc| { process(&Options::default(), doc, &err) }
        ).map_err(|e| err.into_fatal(e)).unwrap();
    Deserialize::deserialize(&mut Deserializer::new(&ast, &err))
    .map_err(|e| err.into_fatal(e))
    .unwrap()
}
