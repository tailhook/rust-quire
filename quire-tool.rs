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

use serialize::json::ToJson;

use argparse::{ArgumentParser, StoreConst, Store};
use quire::parser::parse;
use quire::ast::process;
use quire::emit::emit_ast;

enum Action {
    NoAction,
    AsJson,
    AsYaml,
}


fn main() {
    let mut ap = ArgumentParser::new();
    let mut action = NoAction;
    let mut pretty = false;
    let mut filename = Path::new("");
    ap.refer(&mut action)
        .add_option(["-J", "--to-json"], box StoreConst(AsJson),
                    "Print parsed YAML as a JSON")
        .add_option(["-Y", "--to-yaml"], box StoreConst(AsYaml),
                    "Print parsed YAML as a YAML
                     (probably with some transormations)");
    ap.refer(&mut pretty)
        .add_option(["-p", "--pretty"], box StoreConst(true),
                    "Pretty print result");
    ap.refer(&mut filename)
        .add_option(["-f", "--filename"], box Store::<Path>,
                    "File name of the YAML file to parse")
        .add_argument("filename", box Store::<Path>,
                      "File name of the YAML file to parse")
        .required();
    match (ap.parse_args(), action) {
        (Ok(()), NoAction) => {
            let mut out = stderr();
            ap.print_help("quire-tool", &mut out).unwrap();
            os::set_exit_status(1);
            return;
        }
        (Ok(()), _) => {}
        (Err(x), _) => {
            os::set_exit_status(x);
            return;
        }
    }

    let data = File::open(&filename).read_to_end()
        .ok().expect("Can't read file");
    let string = from_utf8(data.as_slice())
        .expect("File is not utf-8 encoded");
    let mut out = stdout();

    let (ast, warnings) = match parse(
        Rc::new(format!("{}", filename.display())),
        string.as_slice(),
        |doc| { process(Default::default(), doc) })
    {
        Ok(pair) => pair,
        Err(e) => {
            (write!(stderr(),
                "Error parsing file {}: {}\n",
                filename.display(), e)
            ).ok().expect("Error formatting error");
            return;
        }
    };
    warnings.iter().all(|e| write!(stderr(),
        "Error parsing file {}: {}\n",
        filename.display(), e).is_ok());
    match action {
        NoAction => unreachable!(),
        AsJson => {
            let json = ast.to_json();
            if pretty {
                json.to_pretty_writer(&mut out).unwrap();
            } else {
                json.to_writer(&mut out).unwrap();
            }
        }
        AsYaml => {
            match emit_ast(&ast, &mut out) {
                Ok(()) => {}
                Err(e) => {
                    (write!(stderr(), "Error printing file: {}\n",e)
                    ).ok().expect("Error formatting error");
                }
            }
        }
    }
}
