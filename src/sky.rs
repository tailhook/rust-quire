use std::rc::Rc;
use std::io::{Read, Write};
use std::io::stderr;
use std::fs::File;
use std::path::Path;
use std::sync::mpsc::channel;
use rustc_serialize::{Decodable};

use super::ast;
pub use super::errors::Error;
use super::parser::parse;
use super::decode::YamlDecoder;
use super::validate::Validator;


pub fn parse_config<T: Decodable, P: AsRef<Path>>(
    filename: &P, validator: &Validator, options: ast::Options)
    -> Result<T, String>
{
    let mut file = try!(File::open(filename)
        .map_err(|e| format!("Error opening config {}: {}",
            filename.display(), e)));
    let mut body = String::new();
    try!(file.read_to_string(&mut body)
        .map_err(|e| format!("Error reading config {}: {}",
            filename.display(), e)));
    let (ast, mut warnings) = try!(parse(
            Rc::new(format!("{}", filename.display())),
            &body,
            |doc| { ast::process(options, doc) })
        .map_err(|e| format!("Error parsing config {}: {}",
            filename.display(), e)));
    let (ast, nwarn) = validator.validate(ast);
    warnings.extend(nwarn.into_iter());
    let (tx, rx) = channel();
    let res = {
        let mut dec = YamlDecoder::new(ast, tx);
        Decodable::decode(&mut dec)
    };
    warnings.extend(rx.iter());
    if options.print_warnings {
        let mut err = stderr();
        for warning in warnings.iter() {
            (writeln!(&mut err, "config: {}", warning)).ok();
        }
    }
    match res {
        Ok(val) => {
            if warnings.len() > 0 {
                return Err(format!("Multiple errors in configuration file {}",
                    filename.display()));
            }
            return Ok(val);
        }
        Err(err) => {
                return Err(format!("Fatal error in {}: {}",
                    filename.display(), err));
        }
    }
}
