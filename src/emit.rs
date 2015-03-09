use std::old_io::{IoResult, IoError, Writer};
use std::string::ToString;
use serialize::{Encodable, Encoder};

use super::parser::Node;

use self::State as S;
use self::Line as L;
use self::ScalarStyle::{Plain};
use super::parser::Node as N;  // Node enum constants
use super::ast::Tag::{LocalTag, GlobalTag, NonSpecific};
use super::ast::Tag as AstTag;
use super::ast::Ast;
use super::ast::Ast as A;
use super::ast::NullKind::{Explicit, Implicit};
use super::errors::Error;

pub type Tag<'a> = &'a str;
pub type Anchor<'a> = &'a str;

#[derive(Copy)]
enum ScalarStyle {
    Auto,
    Plain,
    SingleQuoted,
    DoubleQuoted,
    Literal,
    Folded,
}

#[derive(Copy)]
enum Null {
    Nothing,
    Tilde,
    Null,
}

#[derive(Copy)]
enum State {
    New,
    MapKey,
    MapSimpleKeyValue,
    MapValue,
    SeqItem,
    Fin,
}

#[derive(PartialEq, Eq)]
enum Line {
    Start,
    AfterIndent,  // Like Start, but already indented
    AfterScalar,  // Means, insert newline unless you emitting comment
}

#[derive(Copy)]
enum Opcode<'a> {
    MapStart(Option<Tag<'a>>, Option<Anchor<'a>>),
    MapEnd,
    SeqStart(Option<Tag<'a>>, Option<Anchor<'a>>),
    SeqEnd,
    Null(Option<Tag<'a>>, Option<Anchor<'a>>, Null),
    Scalar(Option<Tag<'a>>, Option<Anchor<'a>>, ScalarStyle, &'a str),
    Comment(&'a str),
    Alias(&'a str),
}

pub struct Context<'a> {
    cur_indent: usize,
    stream: &'a mut (Writer + 'a),
    stack: Vec<(State, usize)>,
    state: State,
    line: Line,
}


fn tag_as_string<'x>(tag: &'x AstTag) -> Option<&'x str> {
    return (match *tag {
        NonSpecific => None,
        LocalTag(ref value) => Some(value),
        GlobalTag(_) => unimplemented!(),
    }).map(|t| t.as_slice());
}


impl<'a> Context<'a> {
    pub fn new<'x>(stream: &'x mut Writer) -> Context<'x> {
        return Context {
            cur_indent: 0,
            stream: stream,
            stack: Vec::new(),
            state: S::New,
            line: L::Start,
        };
    }

    fn emit_scalar(&mut self, style: ScalarStyle, value: &str)
        -> IoResult<()>
    {
        match style {
            ScalarStyle::Auto|ScalarStyle::Plain => {
                //  Check for allowed characters
                self.line = L::AfterScalar;
                return self.stream.write_str(value);
            }
            ScalarStyle::SingleQuoted => {
                unimplemented!();
            }
            ScalarStyle::DoubleQuoted => {
                unimplemented!();
            }
            ScalarStyle::Literal => {
                unimplemented!();
            }
            ScalarStyle::Folded => {
                unimplemented!();
            }
        }
    }

    fn emit_null(&mut self, space:bool, style: Null) -> IoResult<()> {
        return match style {
            Null::Nothing => {
                if self.line == L::Start {
                    Ok(())
                } else {
                    self.stream.write_char('\n')
                }
            }
            Null::Tilde =>
                self.stream.write_str(if space { " ~\n" } else { "~\n" }),
            Null::Null =>
                self.stream.write_str(if space { " null\n" } else { "null\n"}),
        };
    }

    fn push_indent(&mut self, state: State, value: usize) {
        // TODO(tailhook) allow to custimize indent width at each nesting level
        self.stack.push((state, self.cur_indent));
        self.cur_indent += value;
    }
    fn pop_indent(&mut self) -> State {
        let (val, indent) = self.stack.pop().unwrap();
        self.cur_indent = indent;
        return val;
    }

    fn ensure_line_start(&mut self) -> IoResult<()> {
        match self.line {
            L::Start => {
                return Ok(());
            }
            L::AfterScalar | L::AfterIndent => {
                self.line = L::Start;
                return self.stream.write_char('\n');
            }
        }
    }
    fn ensure_indented(&mut self) -> IoResult<()> {
        match self.line {
            L::AfterIndent => return Ok(()),
            _ => {}
        }
        try!(self.ensure_line_start());
        for _ in 0..self.cur_indent {
            try!(self.stream.write_char(' '));
        }
        return Ok(());
    }

    pub fn emit_tag_anchor(&mut self, tag: Option<Tag>,
        anchor: Option<Tag>, space: bool) -> IoResult<()> {
        match tag {
            Some(x) => {
                try!(self.stream.write_char('!'));
                try!(self.stream.write_str(x));
                if space {
                    try!(self.stream.write_char(' '));
                } else {
                    self.line = L::Start;
                    return self.stream.write_char('\n');
                }
            }
            None => {}
        }
        match anchor {
            Some(_) => {
                unimplemented!();
            }
            None => {}
        }
        return Ok(());
    }

    fn emit(&mut self, op: Opcode) -> IoResult<()> {
        self.state = match (self.state, op) {
            (S::Fin, _) => unreachable!(),
            (S::New, Opcode::Scalar(tag, anchor, style, value)) => {
                try!(self.emit_tag_anchor(tag, anchor, true));
                try!(self.emit_scalar(style, value));
                try!(self.ensure_line_start());
                S::Fin }
            (S::New, Opcode::Null(tag, anchor, style)) => {
                try!(self.emit_tag_anchor(tag, anchor, false));
                try!(self.emit_null(false, style));
                try!(self.ensure_line_start());
                S::Fin }
            (S::New, Opcode::MapStart(tag, anchor)) => {
                try!(self.emit_tag_anchor(tag, anchor, false));
                if tag.is_some() || anchor.is_some() {
                    try!(self.ensure_line_start());
                }
                self.push_indent(S::Fin, 0);
                S::MapKey }
            (S::MapKey, Opcode::Scalar(tag, anchor, style, value)) => {
                try!(self.ensure_indented());
                // TODO(tailhook) check for complex key
                try!(self.emit_tag_anchor(tag, anchor, true));
                try!(self.emit_scalar(style, value));
                S::MapSimpleKeyValue }
            (S::MapKey, Opcode::Null(tag, anchor, style)) => {
                try!(self.ensure_indented());
                // TODO(tailhook) check for complex key
                try!(self.emit_tag_anchor(tag, anchor, false));
                try!(self.emit_null(false, style));
                try!(self.ensure_line_start());
                S::MapKey }
            (S::MapSimpleKeyValue, Opcode::Scalar(tag, anchor, style, value))
            => {
                try!(self.stream.write_str(": "));
                try!(self.emit_tag_anchor(tag, anchor, true));
                try!(self.emit_scalar(style, value));
                S::MapKey }
            (S::MapSimpleKeyValue, Opcode::Null(tag, anchor, style)) => {
                try!(self.stream.write_str(": "));
                try!(self.emit_tag_anchor(tag, anchor, false));
                try!(self.emit_null(false, style));
                try!(self.ensure_line_start());
                S::MapKey }
            (S::MapSimpleKeyValue, Opcode::MapStart(tag, anchor)) => {
                try!(self.stream.write_char(':'));
                if tag.is_some() || anchor.is_some() {
                    try!(self.stream.write_str(" "));
                    try!(self.emit_tag_anchor(tag, anchor, false));
                } else {
                    self.line = L::AfterScalar;
                }
                self.push_indent(S::MapKey, 2);
                S::MapKey }
            (S::MapSimpleKeyValue, Opcode::SeqStart(tag, anchor)) => {
                try!(self.stream.write_char(':'));
                self.line = L::AfterScalar;
                self.push_indent(S::MapKey, 0);
                S::SeqItem }
            (S::MapKey, Opcode::MapEnd) => {
                let nstate = self.pop_indent();
                match nstate {
                    S::Fin => try!(self.ensure_line_start()),
                    _ => {}
                }
                nstate }
            (S::New, Opcode::SeqStart(tag, anchor)) => {
                try!(self.emit_tag_anchor(tag, anchor, false));
                if tag.is_some() || anchor.is_some() {
                    try!(self.ensure_line_start());
                }
                self.push_indent(S::Fin, 0);
                S::SeqItem }
            (S::SeqItem, Opcode::Scalar(tag, anchor, style, value)) => {
                try!(self.ensure_indented());
                try!(self.stream.write_str("- "));
                try!(self.emit_tag_anchor(tag, anchor, true));
                try!(self.emit_scalar(style, value));
                S::SeqItem }
            (S::SeqItem, Opcode::MapStart(tag, anchor)) => {
                try!(self.emit_tag_anchor(tag, anchor, false));
                if tag.is_some() || anchor.is_some() {
                    try!(self.ensure_line_start());
                }
                try!(self.ensure_indented());
                try!(self.stream.write_str("- "));
                self.line = L::AfterIndent;
                self.push_indent(S::SeqItem, 2);
                S::MapKey }
            (S::SeqItem, Opcode::SeqEnd) => {
                let nstate = self.pop_indent();
                match nstate {
                    S::Fin => try!(self.ensure_line_start()),
                    _ => {}
                }
                nstate }
            (_, _) => unimplemented!(),
        };
        return Ok(());
    }

    pub fn emit_node(&mut self, node: &Node) -> IoResult<()> {
        match node {
            &N::Map(tag, anchor, ref map, _) => {
                try!(self.emit(Opcode::MapStart(tag.map(|t| t.slice_from(1)),
                                        anchor)));
                for (k, v) in map.iter() {
                    try!(self.emit_node(k));
                    try!(self.emit_node(v));
                }
                try!(self.emit(Opcode::MapEnd));
            }
            &N::List(tag, anchor, ref items, _) => {
                try!(self.emit(Opcode::SeqStart(tag.map(|t| t.slice_from(1)),
                                        anchor)));
                for i in items.iter() {
                    try!(self.emit_node(i));
                }
                try!(self.emit(Opcode::SeqEnd));
            },
            &N::Scalar(ref tag, _anchor, ref value, _) => {
                // TODO(tailhook) fix anchor
                try!(self.emit(Opcode::Scalar(tag.map(|t| t.slice_from(1)),
                    None, ScalarStyle::Auto, value.as_slice())));
            }
            &N::ImplicitNull(ref tag, ref _anchor, ref _token) => {
                // TODO(tailhook) fix anchor
                try!(self.emit(Opcode::Null(tag.map(|t| t.slice_from(1)),
                    None, Null::Nothing)));
            }
            &N::Alias(name, _) => unimplemented!(),
        }
        return Ok(());
    }

    pub fn emit_ast(&mut self, node: &Ast) -> IoResult<()> {
        match node {
            &A::Map(_, ref tag, ref map) => {
                try!(self.emit(Opcode::MapStart(tag_as_string(tag), None)));
                for (k, v) in map.iter() {
                    try!(self.emit(Opcode::Scalar(None, None,
                        ScalarStyle::Auto, k.as_slice())));
                    try!(self.emit_ast(v));
                }
                try!(self.emit(Opcode::MapEnd));
            }
            &A::List(_, ref tag, ref items) => {
                try!(self.emit(Opcode::SeqStart(tag_as_string(tag), None)));
                for i in items.iter() {
                    try!(self.emit_ast(i));
                }
                try!(self.emit(Opcode::SeqEnd));
            },
            &A::Scalar(_, ref tag, _, ref value) => {
                // TODO(tailhook) fix tag and anchor
                try!(self.emit(Opcode::Scalar(tag_as_string(tag), None,
                                      ScalarStyle::Auto, value.as_slice())));
            }
            &A::Null(_, ref tag, ref kind) => {
                try!(self.emit(Opcode::Null(tag_as_string(tag), None, match *kind {
                    Explicit => Null::Null,
                    Implicit => Null::Nothing,
                })));
            }
        }
        return Ok(());
    }

    fn to_buffer<'x, T: Encodable, W: Writer>(
        val: &T, wr: &'x mut W)
    {
        let mut encoder = Context::new(wr);
        val.encode(&mut encoder).unwrap();
    }

}

pub fn emit_parse_tree(tree: &Node, stream: &mut Writer)
    -> IoResult<()>
{
    let mut ctx = Context::new(stream);
    return ctx.emit_node(tree);
}

pub fn emit_ast(tree: &Ast, stream: &mut Writer)
    -> IoResult<()>
{
    let mut ctx = Context::new(stream);
    return ctx.emit_ast(tree);
}

pub fn emit_object<'x, T: Encodable>(
    val: &T, wr: &'x mut Writer) -> Result<(), IoError>
{
    let mut encoder = Context::new(wr);
    val.encode(&mut encoder)
}


impl<'a> Encoder for Context<'a> {
    type Error = IoError;
    fn emit_nil(&mut self) -> Result<(), IoError> {
        return self.emit(Opcode::Null(None, None, Null::Nothing));
    }
    fn emit_uint(&mut self, v: usize) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_u64(&mut self, v: u64) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_u32(&mut self, v: u32) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_u16(&mut self, v: u16) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_u8(&mut self, v: u8) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_int(&mut self, v: isize) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_i64(&mut self, v: i64) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_i32(&mut self, v: i32) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_i16(&mut self, v: i16) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_i8(&mut self, v: i8) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_bool(&mut self, v: bool) -> Result<(), IoError> {
        return self.emit(Opcode::Scalar(None, None, Plain,
            if v { "true" } else { "false" }));
    }
    fn emit_f64(&mut self, v: f64) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_f32(&mut self, v: f32) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_char(&mut self, v: char) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Opcode::Scalar(None, None, ScalarStyle::Auto, val.as_slice()));
    }
    fn emit_str(&mut self, v: &str) -> Result<(), IoError> {
        return self.emit(Opcode::Scalar(None, None, ScalarStyle::Auto, v));
    }
    fn emit_enum<F>(&mut self, name: &str, f: F) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_enum_variant<F>(&mut self, v_name: &str, v_id: usize, len: usize, f: F)
        -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_enum_variant_arg<F>(&mut self, a_idx: usize, f: F)
        -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_enum_struct_variant<F>(&mut self, v_name: &str,
        v_id: usize, len: usize, f: F)
        -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_enum_struct_variant_field<F>(&mut self, f_name: &str, f_idx: usize,
        f: F)
        -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_struct<F>(&mut self, name: &str, len: usize, f: F)
        -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        self.emit(Opcode::MapStart(None, None))
        .and(f(self))
        .and(self.emit(Opcode::MapEnd))
    }
    fn emit_struct_field<F>(&mut self, f_name: &str, f_idx: usize, f: F)
        -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        self.emit(Opcode::Scalar(None, None, ScalarStyle::Auto, f_name))
        .and(f(self))
    }
    fn emit_tuple<F>(&mut self, len: usize, f: F)
        -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_tuple_arg<F>(&mut self, idx: usize, f: F)
        -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_tuple_struct<F>(&mut self, name: &str, len: usize, f: F)
        -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_tuple_struct_arg<F>(&mut self, f_idx: usize, f: F)
        -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_option<F>(&mut self, f: F) -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_option_none(&mut self) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_option_some<F>(&mut self, f: F) -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_seq<F>(&mut self, len: usize, f: F) -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        self.emit(Opcode::SeqStart(None, None))
            .and(f(self))
            .and(self.emit(Opcode::SeqEnd))
    }
    fn emit_seq_elt<F>(&mut self, idx: usize, f: F) -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        f(self)
    }
    fn emit_map<F>(&mut self, len: usize, f: F) -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_map_elt_key<F>(&mut self, idx: usize, f: F) -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        unimplemented!();
    }
    fn emit_map_elt_val<F>(&mut self, idx: usize, f: F) -> Result<(), IoError>
        where F: FnOnce(&mut Self) -> Result<(), IoError>
    {
        unimplemented!();
    }
}

#[cfg(test)]
mod test {
    use std::str::{from_utf8};
    use std::rc::Rc;
    use std::default::Default;
    use serialize::{Encodable, Encoder};

    use super::super::parser::parse;
    use super::{Null, Opcode};
    use super::super::ast::process;
    use super::Context;
    use super::ScalarStyle;

    fn emit_and_compare(list: &[Opcode], output: &str) {
        let mut bytes = Vec::new();
        {
            let mut ctx = Context::new(&mut bytes);
            for op in list.iter() {
                ctx.emit(*op).unwrap();
            }
        }
        let value = from_utf8(&bytes[..]).unwrap();
        assert_eq!(value, output);
    }

    #[test]
    fn test_empty() {
        emit_and_compare(&[
            Opcode::Null(None, None, Null::Nothing),
        ], "");
    }

    #[test]
    fn test_plain() {
        emit_and_compare(&[
            Opcode::Scalar(None, None, ScalarStyle::Auto, "hello"),
        ], "hello\n");
    }

    #[test]
    fn test_map() {
        emit_and_compare(&[
            Opcode::MapStart(None, None),
            Opcode::Scalar(None, None, ScalarStyle::Auto, "a"),
            Opcode::Scalar(None, None, ScalarStyle::Auto, "val"),
            Opcode::Scalar(None, None, ScalarStyle::Auto, "b"),
            Opcode::Scalar(None, None, ScalarStyle::Auto, "2"),
            Opcode::MapEnd,
        ], "a: val\nb: 2\n");
    }

    fn assert_yaml_eq_yaml(source: &'static str, output: &'static str) {
        let mut bytes = Vec::new();
        let filen = Rc::new("<inline test>".to_string());
        parse(filen.clone(), source, |doc| {
            let mut ctx = Context::new(&mut bytes);
            ctx.emit_node(&doc.root).unwrap();
        }).unwrap();
        let value = from_utf8(&bytes[..]).unwrap();
        assert_eq!(value, output);

        let mut bytes = Vec::new();
        let (ast, _) = parse(filen, source, |doc| {
            process(Default::default(), doc)
        }).unwrap();

        {
            let mut ctx = Context::new(&mut bytes);
            ctx.emit_ast(&ast).unwrap();
        }

        let value = from_utf8(&bytes[..]).unwrap();
        assert_eq!(value, output);
    }

    #[test]
    fn yaml_scalar() {
        assert_yaml_eq_yaml("Hello", "Hello\n");
    }

    #[test]
    fn yaml_tag_scalar() {
        assert_yaml_eq_yaml("!Tag Hello", "!Tag Hello\n");
    }

    #[test]
    fn yaml_tag_null() {
        assert_yaml_eq_yaml("!Tag", "!Tag\n");
    }

    #[test]
    fn yaml_map() {
        assert_yaml_eq_yaml("a: b\nc: d", "a: b\nc: d\n");
    }

    #[test]
    fn yaml_map_map() {
        assert_yaml_eq_yaml("a:\n b: c", "a:\n  b: c\n");
    }

    #[test]
    fn yaml_list() {
        assert_yaml_eq_yaml("- a\n- b", "- a\n- b\n");
    }

    #[test]
    fn yaml_map_list() {
        assert_yaml_eq_yaml("a:\n- b\n- c", "a:\n- b\n- c\n");
    }

    #[test]
    fn yaml_list_map() {
        assert_yaml_eq_yaml("- a: b\n  c: d", "- a: b\n  c: d\n");
    }

    #[test]
    fn yaml_tag_map() {
        assert_yaml_eq_yaml("!Tag {a: b}", "!Tag\na: b\n");
    }

    #[test]
    fn yaml_list_tag() {
        assert_yaml_eq_yaml("- !Tag a", "- !Tag a\n");
    }

    #[test]
    fn yaml_tag_map_map() {
        assert_yaml_eq_yaml("a: !Tag\n a: b", "a: !Tag\n  a: b\n");
    }

    #[test]
    fn yaml_tag_map2() {
        assert_yaml_eq_yaml("!Tag {a: b, c: d}", "!Tag\na: b\nc: d\n");
    }

    #[test]
    fn yaml_tag_map3() {
        assert_yaml_eq_yaml("!Tag { a: b, c: d }", "!Tag\na: b\nc: d\n");
    }

    #[test]
    fn yaml_tag_list() {
        assert_yaml_eq_yaml("!Tag [a, b, c]", "!Tag\n- a\n- b\n- c\n");
    }


    #[test]
    fn encode_int() {
        let mut bytes = Vec::new();
        Context::to_buffer(&1us, &mut bytes);
        let value = from_utf8(&bytes[..]).unwrap();
        assert_eq!(value, "1\n");
    }

    #[test]
    fn encode_seq() {
        let mut bytes = Vec::new();
        Context::to_buffer(&vec!(1us, 2us), &mut bytes);
        let value = from_utf8(&bytes[..]).unwrap();
        assert_eq!(value, "- 1\n- 2\n");
    }

    #[derive(Encodable)]
    struct Something {
        key1: isize,
        key2: String,
    }

    #[test]
    fn encode_struct() {
        let mut bytes = Vec::new();
        Context::to_buffer(&Something{
            key1: -123,
            key2: "hello".to_string(),
            }, &mut bytes);
        let value = from_utf8(&bytes[..]).unwrap();
        assert_eq!(value, "key1: -123\nkey2: hello\n");
    }
}
