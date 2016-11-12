use std::time::Duration;

use humantime::parse_duration;
use rustc_serialize::{Decoder, Decodable};

use super::De;

impl Decodable for De<Duration> {
    fn decode<D: Decoder>(dec: &mut D)
        -> Result<De<Duration>, D::Error>
    {
        let value = dec.read_str()?;
        parse_duration(&value)
        .map(De)
        .map_err(|e| dec.error(
            &format!("error decoding duration {:?}: {}", value, e)))
    }
}

#[cfg(test)]
mod test {

    use std::time::Duration;
    use test_util::decode;
    use De;

    #[test]
    fn decode_15min() {
        assert_eq!(decode::<De<Duration>>("15 min"),
            De::from(Duration::new(900, 0)));
    }
}
