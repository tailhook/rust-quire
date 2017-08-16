use std::rc::Rc;
use std::io::Read;
use std::fs::File;
use std::path::Path;
use serde::de::{Deserialize};

use ast;
use de::Deserializer;
use super::errors::ErrorCollector;
use super::parser::parse;
use super::validate::Validator;
use {Options};
use errors::{ErrorEnum, ErrorList};


/// Parse configuration from a file
pub fn parse_config<'x, T: Deserialize<'x>, P: AsRef<Path>>(
    filename: P, validator: &Validator, options: &Options)
    -> Result<T, ErrorList>
{
    let filename = filename.as_ref();
    let err = ErrorCollector::new();
    let mut file = File::open(filename).map_err(
        |e| err.into_fatal(ErrorEnum::OpenError(
            filename.to_path_buf(), e).into()))?;
    let mut body = String::new();
    file.read_to_string(&mut body).map_err(
        |e| err.into_fatal(ErrorEnum::OpenError(
            filename.to_path_buf(), e).into()))?;
    let filename = Rc::new(format!("{}", filename.display()));
    let ast = parse(filename, &body,
        |doc| { ast::process(options, doc, &err) }
        ).map_err(|e| err.into_fatal(e))?;
    let ast = validator.validate(ast, &err);
    let res = Deserialize::deserialize(&mut Deserializer::new(&ast, &err))
        .map_err(|e| err.into_fatal(e))?;
    return err.into_result(res);
}

/// Parse configuration from a string
pub fn parse_string<'x, T: Deserialize<'x>>(filename: &str, data: &str,
    validator: &Validator, options: &Options)
    -> Result<T, ErrorList>
{
    let err = ErrorCollector::new();
    let ast = parse(Rc::new(filename.to_string()), data,
            |doc| { ast::process(options, doc, &err) }
        ).map_err(|e| err.into_fatal(e))?;
    let ast = validator.validate(ast, &err);
    let res = Deserialize::deserialize(&mut Deserializer::new(&ast, &err))
        .map_err(|e| err.into_fatal(e))?;
    return err.into_result(res);
}

