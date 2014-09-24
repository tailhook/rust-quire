use std::rc::Rc;
use std::io::stderr;
use std::io::fs::File;
use serialize::Decodable;

use super::ast;
pub use super::errors::Warning;
use super::parser::parse;
use super::decode::YamlDecoder;
use super::validate::Validator;


pub fn parse_config<T:Decodable<YamlDecoder, Warning>>(
    filename: &Path, validator: &Validator, options: ast::Options)
    -> Result<T, String>
{
    let mut file = try!(File::open(filename)
        .map_err(|e| format!("Error opending config {}: {}",
            filename.display(), e)));
    let body = try!(file.read_to_str()
        .map_err(|e| format!("Error reading config {}: {}",
            filename.display(), e)));
    let (ast, mut warnings) = try!(parse(
            Rc::new(format!("{}", filename.display())),
            body.as_slice(),
            |doc| { ast::process(options, doc) })
        .map_err(|e| format!("Error parsing config {}: {}",
            filename.display(), e)));
    let (ast, nwarn) = validator.validate(ast);
    warnings.extend(nwarn.move_iter());
    let mut dec = YamlDecoder::new(ast);
    let res = Decodable::decode(&mut dec);
    if options.print_warnings {
        let mut err = stderr();
        for warning in warnings.iter() {
            (writeln!(err, "config: {}", warning)).ok();
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
