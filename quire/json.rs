use std::from_str::FromStr;

use serialize::json::{ToJson, Json};
use J = serialize::json;

use T = super::tokenizer;
use super::parser::{parse, Node, Document, Directive};
use super::parser::{Map, List, Scalar, Null, Alias};
use super::tokenizer;


impl<'a> tokenizer::Token<'a> {
    fn plain_value(&self) -> StrBuf {
        let mut res = StrBuf::with_capacity(self.value.len());
        match self.kind {
            T::PlainString => { res.push_str(self.value); }
            T::SingleString => unimplemented!(),
            T::DoubleString => unimplemented!(),
            _ => unreachable!(),
        }
        return res;
    }
}

impl<'a> ToJson for Node<'a> {
    fn to_json(&self) -> Json {
        return match *self {
            Map(_, _, ref tm, _) => {
                unimplemented!();
            },
            List(_, _, ref vec, _) => {
                unimplemented!();
            },
            Null(_, _) => J::Null,
            Alias(_) => unimplemented!(),
            Scalar(_, _, ref tok) => {
                if tok.kind == T::PlainString {
                    match FromStr::from_str(tok.value) {
                        Some(x) => return J::Number(x),
                        None => {}
                    }
                }
                return J::String(tok.plain_value());
            }
        };
    }
}

impl<'a> ToJson for Document<'a> {
    fn to_json(&self) -> Json {
        return self.root.to_json();
    }
}

#[cfg(test)]
fn assert_yaml_eq_json(a: &'static str, b: &'static str) {
    let aj = parse(a, |doc| { doc.to_json() }).unwrap();
    let bj = J::from_str(b).unwrap();
    assert_eq!(aj, bj);
}

#[test]
fn test_to_json_1() {
    assert_yaml_eq_json("1", "1");
}

#[test]
fn test_to_json_str() {
    assert_yaml_eq_json("test", "\"test\"");
}
