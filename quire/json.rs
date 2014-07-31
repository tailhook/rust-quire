use std::from_str::FromStr;
use std::string::String;

use collections::treemap::TreeMap;
use serialize::json::{ToJson, Json};
use J = serialize::json;

use T = super::tokenizer;
use super::parser::{parse, Node, Document, Directive};
use super::parser::{Map, List, Scalar, Null, Alias};
use super::tokenizer;


impl<'a> ToJson for Node<'a> {
    fn to_json(&self) -> Json {
        return match *self {
            Map(_, _, ref tm, _) => {
                let mut ob = TreeMap::new();
                for (k, v) in tm.iter() {
                    match *k {
                        Scalar(_, _, ref val, _) => {
                            ob.insert(val.clone(), v.to_json());
                        }
                        // Unfortunately we don't have a way to report an
                        // error, should we serialized key to string?
                        _ => unimplemented!(),
                    }
                }
                J::Object(box ob)
            },
            List(_, _, ref lst, _) => {
                J::List(lst.iter().map(|ref val| val.to_json()).collect())
            }
            Null(_, _) => J::Null,
            Alias(_) => unimplemented!(),
            Scalar(_, _, ref val, ref tok) => {
                if tok.kind == T::PlainString {
                    match FromStr::from_str(tok.value) {
                        Some(x) => return J::Number(x),
                        None => {}
                    }
                    if val.as_slice() == "~" || val.as_slice() == "null" {
                        return J::Null;
                    }
                }
                J::String(val.clone())
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
fn test_to_json_str_1_sq() {
    assert_yaml_eq_json("'1'", r#""1""#);
}

#[test]
fn test_to_json_str_1_dq() {
    assert_yaml_eq_json(r#""1""#, r#""1""#);
}

#[test]
fn test_to_json_str() {
    assert_yaml_eq_json("test", r#""test""#);
}

#[test]
fn test_to_json_str_quoted() {
    assert_yaml_eq_json(r#""abc""#, r#""abc""#);
}

#[test]
fn test_to_json_str_apos() {
    assert_yaml_eq_json("'abc'", "\"abc\"");
}

#[test]
fn test_to_json_map1() {
    assert_yaml_eq_json("a: b", "{\"a\": \"b\"}");
}

#[test]
fn test_to_json_map2() {
    assert_yaml_eq_json("1: 2", "{\"1\": 2}");
}

#[test]
fn test_to_json_map3() {
    assert_yaml_eq_json("'a':", "{\"a\": null}");
}

#[test]
fn test_to_json_map4() {
    assert_yaml_eq_json("\"a\":  ", "{\"a\": null}");
}

#[test]
fn test_to_json_map5() {
    assert_yaml_eq_json("abc: ~", "{\"abc\": null}");
}

#[test]
fn test_to_json_two_keys() {
    assert_yaml_eq_json("a: 1\nb: 2", "{\"a\": 1, \"b\": 2}");
}

#[test]
fn test_to_json_two_nested() {
    assert_yaml_eq_json("a:\n b:\n  c:\nd:",
        r#"{"a": {"b": {"c": null}}, "d": null}"#);
}

#[test]
fn test_to_json_nested() {
    assert_yaml_eq_json("a:\n b: 2", "{\"a\": {\"b\": 2}}");
}

#[test]
fn test_to_json_nested_2() {
    assert_yaml_eq_json("a:\n b: 2\n c: 3\nd: 4",
        "{\"a\": {\"b\": 2, \"c\": 3}, \"d\": 4}");
}


#[test]
fn test_to_json_list_1() {
    assert_yaml_eq_json("-", "[null]");
}

#[test]
fn test_to_json_list_2() {
    assert_yaml_eq_json("- 1", "[1]");
}

#[test]
fn test_to_json_list_3() {
    assert_yaml_eq_json("- '1'", "[\"1\"]");
}

#[test]
fn test_to_json_list_4() {
    assert_yaml_eq_json("-\n-", "[null, null]");
}

#[test]
fn test_to_json_list_5() {
    assert_yaml_eq_json("- ab\n- cd", "[\"ab\", \"cd\"]");
}

#[test]
fn test_to_json_list_6() {
    assert_yaml_eq_json("-\n -", "[[null]]");
}

#[test]
fn test_to_json_list_7() {
    assert_yaml_eq_json("-\n- -", "[null, [null]]");
}

#[test]
fn test_to_json_list_8() {
    assert_yaml_eq_json("-\n - a\n - b", "[[\"a\", \"b\"]]");
}

#[test]
fn test_to_json_list_map() {
    assert_yaml_eq_json("- a:", r#"[{"a": null}]"#);
}

#[test]
fn test_to_json_list_map2() {
    assert_yaml_eq_json("- a: 1\n  b: 2", r#"[{"a": 1, "b": 2}]"#);
}

#[test]
fn test_to_json_list_map3() {
    assert_yaml_eq_json("- a: 1\n- b: 2", r#"[{"a": 1}, {"b": 2}]"#);
}

#[test]
fn test_to_json_map_list_1() {
    assert_yaml_eq_json("a:\n-", r#"{"a": [null]}"#);
}

#[test]
fn test_to_json_map_list_2() {
    assert_yaml_eq_json("a:\n -", r#"{"a": [null]}"#);
}

#[test]
fn test_flow_list_1() {
    assert_yaml_eq_json("[]", "[]");
}

#[test]
fn test_flow_list_2() {
    assert_yaml_eq_json(r#"[a]"#, r#"["a"]"#);
}

#[test]
fn test_flow_list_3() {
    assert_yaml_eq_json(r#"[a,]"#, r#"["a"]"#);
}

#[test]
fn test_flow_list_4() {
    assert_yaml_eq_json(r#"[a,b]"#, r#"["a", "b"]"#);
}

#[test]
fn test_flow_list_5() {
    assert_yaml_eq_json(r#"[[a],b]"#, r#"[["a"], "b"]"#);
}

#[test]
fn test_flow_map_1() {
    assert_yaml_eq_json("{}", "{}");
}

#[test]
fn test_flow_map_2() {
    assert_yaml_eq_json(r#"{a: 1}"#, r#"{"a":1}"#);
}

#[test]
fn test_flow_map_3() {
    assert_yaml_eq_json(r#"{a: 1,}"#, r#"{"a":1}"#);
}

#[test]
fn test_flow_map_4() {
    assert_yaml_eq_json(r#"{a: 1,b: 2}"#, r#"{"a":1, "b":2}"#);
}

#[test]
fn test_flow_map_5() {
    assert_yaml_eq_json(r#"{a:{c: 1},b: 2}"#, r#"{"a":{"c":1}, "b": 2}"#);
}

#[test]
fn test_flow_map_quotes_no_space() {
    assert_yaml_eq_json(r#"{"a":1}"#, r#"{"a":1}"#);
}

#[test]
fn test_combined() {
    assert_yaml_eq_json("a: {}", r#"{"a":{}}"#);
}
