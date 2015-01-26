extern crate quire;
extern crate argparse;
extern crate serialize;

use std::os;
use std::rc::Rc;
use std::str::from_utf8;
use std::io::fs::File;
use std::default::Default;
use std::io::stdio::stderr;
use std::io::stdio::stdout;

use serialize::json::{ToJson, as_json, as_pretty_json};

use argparse::{ArgumentParser, StoreConst, Store};
use quire::parser::parse;
use quire::ast::process;
use quire::emit::emit_ast;

#[derive(Copy)]
enum Action {
    NoAction,
    AsJson,
    AsYaml,
}


fn main() {
    let mut action = Action::NoAction;
    let mut pretty = false;
    let mut filename = Path::new("");
    let parse_result = {
        let mut ap = ArgumentParser::new();
        ap.refer(&mut action)
            .add_option(&["-J", "--to-json"],
                Box::new(StoreConst(Action::AsJson)),
                "Print parsed YAML as a JSON")
            .add_option(&["-Y", "--to-yaml"],
                Box::new(StoreConst(Action::AsYaml)),
                "Print parsed YAML as a YAML
                 (probably with some transormations)")
            .required();
        ap.refer(&mut pretty)
            .add_option(&["-p", "--pretty"], Box::new(StoreConst(true)),
                        "Pretty print result");
        ap.refer(&mut filename)
            .add_option(&["-f", "--filename"], Box::new(Store::<Path>),
                        "File name of the YAML file to parse")
            .add_argument("filename", Box::new(Store::<Path>),
                          "File name of the YAML file to parse")
            .required();
        match ap.parse_args() {
            Ok(()) => {}
            Err(x) => {
                os::set_exit_status(x);
                return;
            }
        }
    };

    let data = File::open(&filename).read_to_end()
        .ok().expect("Can't read file");
    let string = from_utf8(data.as_slice())
        .ok().expect("File is not utf-8 encoded");
    let mut out = stdout();

    let (ast, warnings) = match parse(
        Rc::new(format!("{}", filename.display())),
        string.as_slice(),
        |doc| { process(Default::default(), doc) })
    {
        Ok(pair) => pair,
        Err(e) => {
            (write!(&mut stderr(),
                "Error parsing file {}: {}\n",
                filename.display(), e)
            ).ok().expect("Error formatting error");
            return;
        }
    };
    warnings.iter().all(|&:e| write!(&mut stderr(),
        "Error parsing file {}: {}\n",
        filename.display(), e).is_ok());
    match action {
        Action::NoAction => unreachable!(),
        Action::AsJson => {
            let json = ast.to_json();
            if pretty {
                (write!(&mut out, "{}", as_pretty_json(&json))).ok();
            } else {
                (write!(&mut out, "{}", as_json(&json))).ok();
            }
        }
        Action::AsYaml => {
            match emit_ast(&ast, &mut out) {
                Ok(()) => {}
                Err(e) => {
                    (write!(&mut stderr(), "Error printing file: {}\n",e)
                    ).ok().expect("Error formatting error");
                }
            }
        }
    }
}
