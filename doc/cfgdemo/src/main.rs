extern crate quire;
#[macro_use] extern crate serde_derive;
use quire::{parse_config, Options};
use quire::validate::{Structure, Scalar};

#[derive(Deserialize)]
#[allow(dead_code)]
struct Config {
    item1: String,
    item2: Option<String>,
}

fn validator() -> Structure<'static> {
    Structure::new()
    .member("item1", Scalar::new())
    .member("item2", Scalar::new().optional())
}

fn work(cfg: &Config) {
    println!("item1 is {}.", cfg.item1);
    //intln!("item2 is {}.", cfg.item2);
    // hey, this is just demonstration code ...
}

fn main() {
    let cfg: Config;
    cfg = parse_config("config.yaml", &validator(), &Options::default())
        .expect("valid config");
    work(&cfg)
}
