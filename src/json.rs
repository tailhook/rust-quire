use std::from_str::FromStr;
use std::string::String;

use collections::treemap::TreeMap;
use serialize::json::{ToJson, Json};
use serialize::json as J;

use super::tokenizer as T;
use super::ast as A;
use super::tokenizer;


impl ToJson for A::Ast {
    fn to_json(&self) -> Json {
        return match *self {
            A::Map(_, _, ref tm) => {
                let mut ob = TreeMap::new();
                for (k, v) in tm.iter() {
                    ob.insert(k.clone(), v.to_json());
                }
                J::Object(ob)
            },
            A::List(_, _, ref lst) => {
                J::List(lst.iter().map(|ref val| val.to_json()).collect())
            }
            A::Null(_, _, _) => J::Null,
            A::Scalar(_, _, A::Plain, ref val) => {
                match FromStr::from_str(val.as_slice()) {
                    Some(x) => return J::I64(x),
                    None => {}
                }
                match FromStr::from_str(val.as_slice()) {
                    Some(x) => return J::U64(x),
                    None => {}
                }
                match FromStr::from_str(val.as_slice()) {
                    Some(x) => return J::F64(x),
                    None => {}
                }
                if val.as_slice() == "~" || val.as_slice() == "null" {
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
    use serialize::json::ToJson;
    use serialize::json as J;
    use super::super::parser::parse;
    use super::super::ast::process;

    fn assert_yaml_eq_json(a: &'static str, b: &'static str) {
        let (ast, _) = parse(Rc::new("<inline text>".to_string()), a,
            |doc| { process(Default::default(), doc) }).unwrap();
        let aj = ast.to_json();
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
    fn test_merge1() {
        assert_yaml_eq_json("a: 1\n<<:\n b: 2", "{\"a\": 1, \"b\": 2}");
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
}
