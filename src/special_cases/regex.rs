use regex::Regex;
use rustc_serialize::{Decoder, Decodable};

use super::De;

impl Decodable for De<Regex> {
    fn decode<D: Decoder>(dec: &mut D)
        -> Result<De<Regex>, D::Error>
    {
        let value = dec.read_str()?;
        Regex::new(&value)
        .map(De)
        .map_err(|e| dec.error(
            &format!("error decoding regex {:?}: {}", value, e)))
    }
}

#[cfg(test)]
mod test {

    use regex::Regex;
    use test_util::decode;
    use De;

    #[test]
    fn decode_simple_regex() {
        let re = decode::<De<Regex>>("a..a");
        assert!(re.is_match("abba"));
        assert!(!re.is_match("baab"));
    }
}
