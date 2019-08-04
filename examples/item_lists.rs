extern crate quire;
#[macro_use] extern crate serde_derive;
use quire::{parse_config, Options};
use quire::validate::{Structure, Scalar};

#[derive(Deserialize)]
#[allow(dead_code)]
struct Journey {
    name: String,
    year: String,
    team: Members,
}

#[derive(Deserialize)]
#[allow(dead_code)]
struct Members(Vec<String>);

// give it a method
// so we can create a iterator for it
#[allow(dead_code)]
impl Members {
    fn new() -> Members {
        Members(Vec::new())
    }
}

// and implement IntoIterator
impl IntoIterator for Members {
    type Item = String;
    type IntoIter = ::std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

fn validator() -> Structure<'static> {
    Structure::new()
    .member("name", Scalar::new())
    .member("year", Scalar::new())
    .member("team", Scalar::new())
}

fn work(jrny: &Journey) {
    println!("name is {}.", jrny.name);
    println!("year is {}.", jrny.year);
    /*
    for tm in jrny.team {
        println!("team member {}.", tm);
    }
    */
}

fn main() {
    let jrny: Journey;
    jrny = parse_config("examples/journey.yaml",
                       &validator(), &Options::default())
        .expect("valid config");
    work(&jrny)
}
