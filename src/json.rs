use std::str::FromStr;

use std::collections::BTreeMap;
use rustc_serialize::json::{ToJson, Json};
use rustc_serialize::json::Json as J;

use super::ast::Ast;
use super::ast::Ast as A;
use super::ast::ScalarKind::{Quoted, Plain};


impl ToJson for Ast {
    fn to_json(&self) -> Json {
        return match *self {
            A::Map(_, _, ref tm) => {
                let mut ob = BTreeMap::new();
                for (k, v) in tm.iter() {
                    ob.insert(k.clone(), v.to_json());
                }
                J::Object(ob)
            },
            A::List(_, _, ref lst) => {
                J::Array(lst.iter().map(|ref val| val.to_json()).collect())
            }
            A::Null(_, _, _) => J::Null,
            A::Scalar(_, _, Plain, ref val) => {
                match FromStr::from_str(val) {
                    Ok(x) => return J::U64(x),
                    Err(_) => {}
                }
                match FromStr::from_str(val) {
                    Ok(x) => return J::I64(x),
                    Err(_) => {}
                }
                match FromStr::from_str(val) {
                    Ok(x) => return J::F64(x),
                    Err(_) => {}
                }
                if &val[..] == "~" || &val[..] == "null" {
                    return J::Null;
                }
                J::String(val.clone())
            }
            A::Scalar(_, _, Quoted, ref val) => {
                J::String(val.clone())
            }
        };
    }
}


#[cfg(test)]
mod test {
    use std::rc::Rc;
    use std::default::Default;
    use rustc_serialize::json::ToJson;
    use rustc_serialize::json as J;
    use super::super::parser::parse;
    use super::super::ast::process;
    use errors::ErrorCollector;

    fn assert_yaml_eq_json(a: &'static str, b: &'static str) {
        let err = ErrorCollector::new();
        let ast = parse(Rc::new("<inline text>".to_string()), a,
            |doc| { process(Default::default(), doc, &err) },
            ).map_err(|e| err.into_fatal(e)).unwrap();
        err.into_result(()).unwrap();
        let aj = ast.to_json();
        let bj = J::Json::from_str(&b).unwrap();
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
    fn test_merge1() {
        assert_yaml_eq_json("a: 1\n<<:\n b: 2", "{\"a\": 1, \"b\": 2}");
    }

    #[test]
    fn test_multiple_merge() {
        assert_yaml_eq_json("<<: [{a: 1, b: 2}, {b: 3, c: 4}]",
            r#"{"a": 1, "b": 2, "c": 4}"#);
    }

    #[test]
    fn test_no_merge1() {
        assert_yaml_eq_json("a: 1\n'<<':\n b: 2",
            "{\"a\": 1, \"<<\": {\"b\": 2}}");
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
    fn test_to_json_1level() {
        assert_yaml_eq_json("abc:\ndef:", "{\"abc\": null, \"def\": null}");
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

    #[test]
    fn test_nl_dquoted() {
        assert_yaml_eq_json("\"a   \nb\"", r#""a b""#);
    }

    #[test]
    fn test_nl_quoted() {
        assert_yaml_eq_json("'a   \nb'", r#""a b""#);
    }

    #[test]
    fn test_nl_plain() {
        assert_yaml_eq_json("a   \nb", r#""a b""#);
    }

    #[test]
    fn test_nl2_dquoted() {
        assert_yaml_eq_json("\"a   \n   \n b\"", r#""a\nb""#);
    }

    #[test]
    fn test_nl_slash_dquoted() {
        assert_yaml_eq_json("\"a   \\\n   \n b\"", r#""a   \nb""#);
    }

    #[test]
    fn test_nl_slash_middle_dquoted() {
        assert_yaml_eq_json("\"a   \\  \n   \n b\"", r#""a    \nb""#);
    }

    #[test]
    fn test_slash_dquoted() {
        assert_yaml_eq_json("\"a   \\\n   b\"", r#""a   b""#);
    }

    #[test]
    fn test_slash_dquoted_nospace() {
        assert_yaml_eq_json("\"a\\\n   b\"", r#""ab""#);
    }

    #[test]
    fn test_slash_middle_dquoted() {
        assert_yaml_eq_json("\"a   \\  \nb\"", r#""a    b""#);
    }

    #[test]
    fn test_nl2_quoted() {
        assert_yaml_eq_json("'a   \n   \n b'", r#""a\nb""#);
    }

    #[test]
    fn test_nl2_plain() {
        assert_yaml_eq_json("a   \n   \n b", r#""a\nb""#);
    }

    #[test]
    fn test_literal() {
        assert_yaml_eq_json(
            "a: |\n hello\n world\n",
            r#"{"a": "hello\nworld\n"}"#);
    }

    #[test]
    fn test_map_and_scalar() {
        assert_yaml_eq_json(
            "a:\n b:\n  hello\n  world\n c: end",
            r#"{"a": {"b": "hello world", "c": "end"}}"#);
    }

    #[test]
    fn yaml_words_in_list() {
        assert_yaml_eq_json("- a\n b\n", r#"["a b"]"#);
    }

    #[test]
    fn yaml_literal_in_list() {
        assert_yaml_eq_json("- |\n  val\n", r#"["val\n"]"#);
    }

    #[test]
    fn yaml_literal_with_empty_line_in_a_list() {
        assert_yaml_eq_json("- |\n  val\n\n  line2", r#"["val\n\nline2\n"]"#);
    }

    #[test]
    fn yaml_words_with_space() {
        assert_yaml_eq_json("   a\nb", r#""a b""#);
    }

    #[test]
    fn indented_map() {
        assert_yaml_eq_json("   a: 1\n   b: 2\n", r#"{"a": 1, "b": 2}"#);
    }

    #[test]
    fn map_map() {
        assert_yaml_eq_json("a: {\n  b: {}}",
            r#"{"a": {"b": {}}}"#);
    }

    #[test]
    fn test_alias() {
        assert_yaml_eq_json("- &a hello\n- *a", r#"["hello", "hello"]"#);
    }

    #[test]
    fn test_unpack() {
        assert_yaml_eq_json("- !*Unpack [[hello]]\n", r#"["hello"]"#);
    }

    #[test]
    fn test_multiple_alias_merge() {
        assert_yaml_eq_json("- &a {hello: world, foo: bar}\n- &b {foo: 123}\n- <<: [*a, *b]",
            r#"[{"hello": "world", "foo": "bar"}, {"foo": 123}, {"hello": "world", "foo": "bar"}]"#);
    }

    #[test]
    fn wrong_escape_incrorrect_tofix() {
        assert_yaml_eq_json(r#"a: "a\.b""#, r#"{"a": "a\\.b"}"#);
    }

    #[test]
    fn wrong_escape_raw_incorrect_tofix() {
        assert_yaml_eq_json(r#"a: a\.b"#, r#"{"a": "a\\.b"}"#);
    }

    #[test]
    fn wrong_escape_correct() {
        assert_yaml_eq_json(r#"a: "a\\.b""#, r#"{"a": "a\\.b"}"#);
    }

    #[test]
    fn wrong_escape_raw_correct() {
        assert_yaml_eq_json(r#"a: "a\\.b""#, r#"{"a": "a\\.b"}"#);
    }

    #[test]
    fn yaml_tag_null_in_map() {
        assert_yaml_eq_json("x: \n  a: !Tag\n  b: x",
            r#"{"x": {"a": null, "b": "x"}}"#);
    }
}
