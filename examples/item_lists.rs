extern crate quire;
#[macro_use] extern crate serde_derive;
use quire::{parse_config, Options};
use quire::validate::{Structure, Scalar, Sequence};
//e quire::validate::{Structure, Scalar, Sequence, Enum};
// TODO  Marker_002  Validate against known vehicles


#[derive(Deserialize)]
#[allow(dead_code)]
struct Journey {
    name: String,
    year: String,
    team: Members,
    vehicles: Vehicles,
    locations: Locations,
}

#[derive(Debug,Deserialize)]
struct Members(Vec<String>);


/* TODO  Marker_002  Validate against known vehicles
enum KnownVehicles {
    Bobcat,
    Jeep,
    Landrover,
    Unimog,
}
// TODO  Marker_002  Validate against known vehicles
*/

#[derive(Deserialize)]
struct Vehicles(Vec<String>);
// TODO  Marker_002  Validate against known vehicles

//#[derive(Deserialize)]
#[derive(Debug,Deserialize)]
struct Locations(Vec<String>);


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
    .member("team", Sequence::new(Scalar::new()))
    .member("vehicles", Sequence::new(Scalar::new()))
    .member("locations", Sequence::new(Scalar::new()))
}

fn work(jrny: &Journey) {
    println!("name is {}.", jrny.name);
    println!("year is {}.", jrny.year);
    /*
    for tm in jrny.team {
        println!("team member {}.", tm);
    }
    TODO make team iterable
    */
    println!("team members {:?} DBG.", jrny.team);  // TODO make team iterable
    println!("{:?} DBG a.k.a. DeBuG", jrny.locations);
    //
    //  TODO  show more of what has been read from the YAML configuration
    //
}

fn main() {
    let jrny: Journey;
    jrny = parse_config("examples/journey.yaml",
                       &validator(), &Options::default())
        .expect("valid config");
    work(&jrny)
}
