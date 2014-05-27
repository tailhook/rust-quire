use std::from_str::FromStr;

use serialize::json::{ToJson, Json};
use J = serialize::json;

use T = super::tokenizer;
use super::parser::{parse, Node, Document, Directive};
use super::parser::{Map, List, Scalar, Null, Alias};
use super::tokenizer;


impl<'a> T::Token<'a> {
    fn plain_value(&self) -> StrBuf {
        let mut res = StrBuf::with_capacity(self.value.len());
        match self.kind {
            T::PlainString => { res.push_str(self.value); }
            T::DoubleString => {
                let mut iter = self.value.chars();
                assert_eq!(iter.next(), Some('"'));
                loop {
                    match iter.next() {
                        None => break,
                        Some('\\') => {
                            match iter.next() {
                                None => res.push_char('\\'),  // fixme
                                Some('0') => res.push_char('\0'),
                                Some('a') => res.push_char('\x07'),
                                Some('b') => res.push_char('\x08'),
                                Some('t') | Some('\t') => res.push_char('\t'),
                                Some('n') => res.push_char('\n'),
                                Some('r') => res.push_char('\r'),
                                Some('v') => res.push_char('\x0b'),
                                Some('f') => res.push_char('\x0c'),
                                Some('e') => res.push_char('\x1b'),
                                Some(' ') => res.push_char(' '),
                                Some('"') => res.push_char('"'),
                                Some('/') => res.push_char('/'),
                                Some('\\') => res.push_char('\\'),
                                Some('N') => res.push_char('\x85'),
                                Some('_') => res.push_char('\xa0'),
                                Some('L') => res.push_char('\u2028'),
                                Some('P') => res.push_char('\u2029'),
                                Some('x') => {
                                    unimplemented!();
                                },
                                Some('u') => {
                                    unimplemented!();
                                },
                                Some('U') => {
                                    unimplemented!();
                                },
                                Some(x) => {
                                    res.push_char('\\');
                                    res.push_char(x);
                                }
                            }
                        }
                        Some('"') => break,
                        Some(x) => res.push_char(x),
                    }
                }
            },
            T::SingleString => {
                let mut iter = self.value.chars();
                assert_eq!(iter.next(), Some('\''));
                loop {
                    match iter.next() {
                        None => break,
                        Some('\'') => {
                            match iter.next() {
                                None => break,
                                Some('\'') => res.push_char('\''),
                                Some(x) => unreachable!(),
                            }
                        }
                        Some(x) => res.push_char(x),
                    }
                }
            }
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

#[test]
fn test_to_json_str_quoted() {
    assert_yaml_eq_json("\"abc\"", "\"abc\"");
}

#[test]
fn test_to_json_str_apos() {
    assert_yaml_eq_json("'abc'", "\"abc\"");
}
