extern crate quire;
extern crate argparse;
extern crate serialize;

use std::os;
use std::str::from_utf8;
use std::io::fs::File;
use std::io::stdio::stderr;
use std::io::stdio::stdout;

use serialize::json::ToJson;

use argparse::{ArgumentParser, StoreConst, Store};
use quire::parse;

enum Action {
    NoAction,
    ToJson,
}


fn main() {
    let mut ap = ArgumentParser::new();
    let mut action = NoAction;
    let mut pretty = false;
    let mut filename = Path::new("");
    ap.refer(&mut action)
        .add_option(["-J", "--to-json"], box StoreConst(ToJson),
                    "Print parsed YAML as a JSON");
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

    match action {
        NoAction => unreachable!(),
        ToJson => {
            let data = File::open(&filename).read_to_end()
                .ok().expect("Can't read file");
            let string = from_utf8(data.as_slice())
                .expect("File is not utf-8 encoded");
            let mut out = stdout();
            match parse(string.as_slice(), |doc| { doc.to_json() })  {
                Ok(json) => {
                    if pretty {
                        json.to_pretty_writer(&mut out).unwrap();
                    } else {
                        json.to_writer(&mut out).unwrap();
                    }
                }
                Err(e) => {
                    (write!(stderr(),
                        "Error parsing file {}: {}\n",
                        filename.display(), e)
                    ).ok().expect("Error formatting error");
                }
            }

        }
    }
}
