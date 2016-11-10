use std::rc::Rc;
use std::io::Read;
use std::fs::File;
use std::path::Path;
use rustc_serialize::{Decodable};

use super::ast;
pub use super::errors::{Error, ErrorList};
use super::errors::ErrorCollector;
use super::parser::parse;
use super::decode::YamlDecoder;
use super::validate::Validator;
use {Options};


pub fn parse_config<T: Decodable, P: AsRef<Path>>(
    filename: P, validator: &Validator, options: &Options)
    -> Result<T, ErrorList>
{
    let filename = filename.as_ref();
    let err = ErrorCollector::new();
    let mut file = File::open(filename).map_err(
        |e| err.into_fatal(Error::OpenError(filename.to_path_buf(), e)))?;
    let mut body = String::new();
    file.read_to_string(&mut body).map_err(
        |e| err.into_fatal(Error::OpenError(filename.to_path_buf(), e)))?;
    let filename = Rc::new(format!("{}", filename.display()));
    let ast = parse(filename, &body,
        |doc| { ast::process(options, doc, &err) }
        ).map_err(|e| err.into_fatal(e))?;
    let ast = validator.validate(ast, &err);
    let res = Decodable::decode(&mut YamlDecoder::new(ast, &err))
        .map_err(|e| err.into_fatal(e))?;
    return err.into_result(res);
}

pub fn parse_string<T: Decodable>(filename: &str, data: &str,
    validator: &Validator, options: &Options)
    -> Result<T, ErrorList>
{
    let err = ErrorCollector::new();
    let ast = parse(Rc::new(filename.to_string()), data,
            |doc| { ast::process(options, doc, &err) }
        ).map_err(|e| err.into_fatal(e))?;
    let ast = validator.validate(ast, &err);
    let res = Decodable::decode(&mut YamlDecoder::new(ast, &err))
        .map_err(|e| err.into_fatal(e))?;
    return err.into_result(res);
}

