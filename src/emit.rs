use std::io::{IoResult, IoError, MemWriter};
use std::io::Writer;
use std::to_string::ToString;
use std::str::{from_char, from_utf8};
use serialize::{Encodable, Encoder};

use super::parser::Node;

use self::State as S;
use self::Line as L;
use super::parser as N;  // Node enum constants
use super::ast as A;

pub type Tag<'a> = &'a str;
pub type Anchor<'a> = &'a str;

pub enum ScalarStyle {
    Auto,
    Plain,
    SingleQuoted,
    DoubleQuoted,
    Literal,
    Folded,
}

pub mod Null {
    pub enum Style {
        Nothing,
        Tilde,
        Null,
    }
}

mod State {
    pub enum Opcode {
        New,
        MapKey,
        MapSimpleKeyValue,
        MapValue,
        SeqItem,
        Fin,
    }
}

mod Line {
    pub enum State {
        Start,
        AfterIndent,  // Like Start, but already indented
        AfterScalar,  // Means, insert newline unless you emitting comment
    }
}

pub enum Opcode<'a> {
    MapStart(Option<Tag<'a>>, Option<Anchor<'a>>),
    MapEnd,
    SeqStart(Option<Tag<'a>>, Option<Anchor<'a>>),
    SeqEnd,
    Null(Option<Tag<'a>>, Option<Anchor<'a>>, Null::Style),
    Scalar(Option<Tag<'a>>, Option<Anchor<'a>>, ScalarStyle, &'a str),
    Comment(&'a str),
    Alias(&'a str),
}

pub struct Context<'a> {
    cur_indent: uint,
    want_newline: bool,
    stream: &'a mut Writer + 'a,
    stack: Vec<(State::Opcode, uint)>,
    state: State::Opcode,
    line: Line::State,
}


fn tag_as_string<'x>(tag: &'x A::Tag) -> Option<&'x str> {
    return (match *tag {
        A::NonSpecific => None,
        A::LocalTag(ref value) => Some(value),
        A::GlobalTag(_) => unimplemented!(),
    }).map(|t| t.as_slice());
}


impl<'a> Context<'a> {
    pub fn new<'x>(stream: &'x mut Writer) -> Context<'x> {
        return Context {
            cur_indent: 0,
            want_newline: false,
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
            Auto|Plain => {
                //  Check for allowed characters
                self.line = L::AfterScalar;
                return self.stream.write_str(value);
            }
            SingleQuoted => {
                unimplemented!();
            }
            DoubleQuoted => {
                unimplemented!();
            }
            Literal => {
                unimplemented!();
            }
            Folded => {
                unimplemented!();
            }
        }
    }

    fn emit_null(&mut self, space:bool, style: Null::Style) -> IoResult<()> {
        return match style {
            Null::Nothing => self.stream.write_char('\n'),
            Null::Tilde =>
                self.stream.write_str(if space { " ~\n" } else { "~\n" }),
            Null::Null =>
                self.stream.write_str(if space { " null\n" } else { "null\n"}),
        };
    }

    fn push_indent(&mut self, state: S::Opcode, value: uint) {
        // TODO(tailhook) allow to custimize indent width at each nesting level
        self.stack.push((state, self.cur_indent));
        self.cur_indent += value;
    }
    fn pop_indent(&mut self) -> State::Opcode {
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
        self.ensure_line_start();
        for i in range(0, self.cur_indent) {
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
                }
            }
            None => {}
        }
        match anchor {
            Some(x) => {
                unimplemented!();
            }
            None => {}
        }
        return Ok(());
    }

    pub fn emit(&mut self, op: Opcode) -> IoResult<()> {
        self.state = match (self.state, op) {
            (S::Fin, _) => unreachable!(),
            (S::New, Scalar(tag, anchor, style, value)) => {
                try!(self.emit_tag_anchor(tag, anchor, true));
                try!(self.emit_scalar(style, value));
                try!(self.ensure_line_start())
                S::Fin }
            (S::New, Null(tag, anchor, style)) => {
                try!(self.emit_tag_anchor(tag, anchor, false));
                try!(self.emit_null(false, style));
                try!(self.ensure_line_start())
                S::Fin }
            (S::New, MapStart(tag, anchor)) => {
                self.push_indent(S::Fin, 0);
                S::MapKey }
            (S::MapKey, Scalar(tag, anchor, style, value)) => {
                try!(self.ensure_indented());
                // TODO(tailhook) check for complex key
                try!(self.emit_tag_anchor(tag, anchor, true));
                try!(self.emit_scalar(style, value));
                S::MapSimpleKeyValue }
            (S::MapKey, Null(tag, anchor, style)) => {
                try!(self.ensure_indented());
                // TODO(tailhook) check for complex key
                try!(self.emit_tag_anchor(tag, anchor, false));
                try!(self.emit_null(false, style));
                try!(self.ensure_line_start())
                S::MapKey }
            (S::MapSimpleKeyValue, Scalar(tag, anchor, style, value)) => {
                try!(self.stream.write_str(": "));
                try!(self.emit_tag_anchor(tag, anchor, true));
                try!(self.emit_scalar(style, value));
                S::MapKey }
            (S::MapSimpleKeyValue, Null(tag, anchor, style)) => {
                try!(self.stream.write_str(": "));
                try!(self.emit_tag_anchor(tag, anchor, false));
                try!(self.emit_null(false, style));
                try!(self.ensure_line_start())
                S::MapKey }
            (S::MapSimpleKeyValue, MapStart(tag, anchor)) => {
                try!(self.stream.write_char(':'));
                self.line = L::AfterScalar;
                self.push_indent(S::MapKey, 2);
                S::MapKey }
            (S::MapSimpleKeyValue, SeqStart(tag, anchor)) => {
                try!(self.stream.write_char(':'));
                self.line = L::AfterScalar;
                self.push_indent(S::MapKey, 0);
                S::SeqItem }
            (S::MapKey, MapEnd) => {
                let nstate = self.pop_indent();
                match nstate {
                    S::Fin => try!(self.ensure_line_start()),
                    _ => {}
                }
                nstate }
            (S::New, SeqStart(tag, anchor)) => {
                self.push_indent(S::Fin, 0);
                S::SeqItem }
            (S::SeqItem, Scalar(tag, anchor, style, value)) => {
                try!(self.ensure_indented());
                try!(self.stream.write_str("- "));
                try!(self.emit_tag_anchor(tag, anchor, true));
                try!(self.emit_scalar(style, value));
                S::SeqItem }
            (S::SeqItem, MapStart(tag, anchor)) => {
                try!(self.ensure_indented());
                try!(self.stream.write_str("- "));
                self.line = L::AfterIndent;
                self.push_indent(S::SeqItem, 2);
                S::MapKey }
            (S::SeqItem, SeqEnd) => {
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
                try!(self.emit(MapStart(tag, anchor)));
                for (k, v) in map.iter() {
                    try!(self.emit_node(k));
                    try!(self.emit_node(v));
                }
                try!(self.emit(MapEnd));
            }
            &N::List(tag, anchor, ref items, _) => {
                try!(self.emit(SeqStart(tag, anchor)));
                for i in items.iter() {
                    try!(self.emit_node(i));
                }
                try!(self.emit(SeqEnd));
            },
            &N::Scalar(ref tag, _anchor, ref value, _) => {
                // TODO(tailhook) fix anchor
                try!(self.emit(Scalar(tag.map(|t| t.slice_from(1)),
                    None, Auto, value.as_slice())));
            }
            &N::ImplicitNull(ref tag, ref _anchor, ref _token) => {
                // TODO(tailhook) fix anchor
                println!("TAGTAG {}", tag);
                try!(self.emit(Null(tag.map(|t| t.slice_from(1)),
                    None, Null::Nothing)));
            }
            &N::Alias(name, _) => unimplemented!(),
        }
        return Ok(());
    }

    pub fn emit_ast(&mut self, node: &A::Ast) -> IoResult<()> {
        match node {
            &A::Map(_, ref _tag, ref map) => {
                try!(self.emit(MapStart(None, None)));
                for (k, v) in map.iter() {
                    try!(self.emit(Scalar(None, None, Auto, k.as_slice())));
                    try!(self.emit_ast(v));
                }
                try!(self.emit(MapEnd));
            }
            &A::List(_, ref _tag, ref items) => {
                try!(self.emit(SeqStart(None, None)));
                for i in items.iter() {
                    try!(self.emit_ast(i));
                }
                try!(self.emit(SeqEnd));
            },
            &A::Scalar(_, ref tag, _, ref value) => {
                // TODO(tailhook) fix tag and anchor
                try!(self.emit(Scalar(tag_as_string(tag), None,
                                      Auto, value.as_slice())));
            }
            &A::Null(_, ref tag, kind) => {
                try!(self.emit(Null(tag_as_string(tag), None, match kind {
                    A::Explicit => Null::Null,
                    A::Implicit => Null::Nothing,
                })));
            }
        }
        return Ok(());
    }

    fn to_buffer<'x, T: Encodable<Context<'x>, IoError>>(
        val: &T, wr: &'x mut MemWriter)
    {
        let mut encoder = Context::new(wr);
        val.encode(&mut encoder);
    }

}

pub fn emit_parse_tree(tree: &Node, stream: &mut Writer)
    -> IoResult<()>
{
    let mut ctx = Context::new(stream);
    return ctx.emit_node(tree);
}

pub fn emit_ast(tree: &A::Ast, stream: &mut Writer)
    -> IoResult<()>
{
    let mut ctx = Context::new(stream);
    return ctx.emit_ast(tree);
}

pub fn emit_object<'x, T: Encodable<Context<'x>, IoError>>(
    val: &T, wr: &'x mut Writer) -> Result<(), IoError>
{
    let mut encoder = Context::new(wr);
    val.encode(&mut encoder)
}


impl<'a> Encoder<IoError> for Context<'a> {
    fn emit_nil(&mut self) -> Result<(), IoError> {
        return self.emit(Null(None, None, Null::Nothing));
    }
    fn emit_uint(&mut self, v: uint) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_u64(&mut self, v: u64) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_u32(&mut self, v: u32) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_u16(&mut self, v: u16) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_u8(&mut self, v: u8) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_int(&mut self, v: int) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_i64(&mut self, v: i64) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_i32(&mut self, v: i32) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_i16(&mut self, v: i16) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_i8(&mut self, v: i8) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_bool(&mut self, v: bool) -> Result<(), IoError> {
        return self.emit(Scalar(None, None, Plain,
            if v { "true" } else { "false" }));
    }
    fn emit_f64(&mut self, v: f64) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_f32(&mut self, v: f32) -> Result<(), IoError> {
        let val = v.to_string();
        return self.emit(Scalar(None, None, Plain, val.as_slice()));
    }
    fn emit_char(&mut self, v: char) -> Result<(), IoError> {
        let val = from_char(v);
        return self.emit(Scalar(None, None, Auto, val.as_slice()));
    }
    fn emit_str(&mut self, v: &str) -> Result<(), IoError> {
        return self.emit(Scalar(None, None, Auto, v));
    }
    fn emit_enum(&mut self, name: &str, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_enum_variant(&mut self, v_name: &str, v_id: uint, len: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_enum_variant_arg(&mut self, a_idx: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_enum_struct_variant(&mut self, v_name: &str, v_id: uint, len: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_enum_struct_variant_field(&mut self, f_name: &str, f_idx: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_struct(&mut self, name: &str, len: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        self.emit(MapStart(None, None))
        .and(f(self))
        .and(self.emit(MapEnd))
    }
    fn emit_struct_field(&mut self, f_name: &str, f_idx: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        self.emit(Scalar(None, None, Auto, f_name))
        .and(f(self))
    }
    fn emit_tuple(&mut self, len: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_tuple_arg(&mut self, idx: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_tuple_struct(&mut self, name: &str, len: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_tuple_struct_arg(&mut self, f_idx: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_option(&mut self, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_option_none(&mut self) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_option_some(&mut self, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_seq(&mut self, len: uint, f: |this: &mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        self.emit(SeqStart(None, None))
            .and(f(self))
            .and(self.emit(SeqEnd))
    }
    fn emit_seq_elt(&mut self, idx: uint, f: |this: &mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        f(self)
    }
    fn emit_map(&mut self, len: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_map_elt_key(&mut self, idx: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
    fn emit_map_elt_val(&mut self, idx: uint, f: |&mut Context<'a>| -> Result<(), IoError>) -> Result<(), IoError> {
        unimplemented!();
    }
}

#[cfg(test)]
mod test {
    use std::io::{MemWriter, IoError};
    use std::str::{from_utf8};
    use std::mem::transmute;
    use std::rc::Rc;
    use std::default::Default;
    use serialize::{Encodable, Encoder};

    use super::super::parser::parse;
    use super::{Opcode, Context, Null, Auto, Scalar, MapStart, MapEnd};
    use super::super::ast::process;

    fn emit_and_compare(list: &[Opcode], output: &str) {
        let mut buf = MemWriter::new();
        {
            let mut ctx = Context::new(&mut buf);
            for op in list.iter() {
                ctx.emit(*op);
            }
        }
        let bytes = buf.unwrap();
        let value = from_utf8(bytes.as_slice()).unwrap();
        assert_eq!(value, output);
    }

    #[test]
    fn test_empty() {
        emit_and_compare([
            Null(None, None, Null::Nothing),
        ], "\n");
    }

    #[test]
    fn test_plain() {
        emit_and_compare([
            Scalar(None, None, Auto, "hello"),
        ], "hello\n");
    }

    #[test]
    fn test_map() {
        emit_and_compare([
            MapStart(None, None),
            Scalar(None, None, Auto, "a"),
            Scalar(None, None, Auto, "val"),
            Scalar(None, None, Auto, "b"),
            Scalar(None, None, Auto, "2"),
            MapEnd,
        ], "a: val\nb: 2\n");
    }

    fn assert_yaml_eq_yaml(source: &'static str, output: &'static str) {
        let mut buf = MemWriter::new();
        let filen = Rc::new("<inline test>".to_string());
        parse(filen.clone(), source, |doc| {
            let mut ctx = Context::new(&mut buf);
            ctx.emit_node(&doc.root).unwrap();
        }).unwrap();
        let bytes = buf.unwrap();
        let value = from_utf8(bytes.as_slice()).unwrap();
        assert_eq!(value, output);

        let mut buf = MemWriter::new();
        let (ast, _) = parse(filen, source, |doc| {
            process(Default::default(), doc)
        }).unwrap();

        {
            let mut ctx = Context::new(&mut buf);
            ctx.emit_ast(&ast).unwrap();
        }

        let bytes = buf.unwrap();
        let value = from_utf8(bytes.as_slice()).unwrap();
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
    fn encode_int() {
        let mut buf = MemWriter::new();
        Context::to_buffer(&1u, &mut buf);
        let bytes = buf.unwrap();
        let value = from_utf8(bytes.as_slice()).unwrap();
        assert_eq!(value, "1\n");
    }

    #[test]
    fn encode_seq() {
        let mut buf = MemWriter::new();
        Context::to_buffer(&vec!(1u, 2u), &mut buf);
        let bytes = buf.unwrap();
        let value = from_utf8(bytes.as_slice()).unwrap();
        assert_eq!(value, "- 1\n- 2\n");
    }

    #[deriving(Encodable)]
    struct Something {
        key1: int,
        key2: String,
    }

    #[test]
    fn encode_struct() {
        let mut buf = MemWriter::new();
        Context::to_buffer(&Something{
            key1: -123,
            key2: "hello".to_string(),
            }, &mut buf);
        let bytes = buf.unwrap();
        let value = from_utf8(bytes.as_slice()).unwrap();
        assert_eq!(value, "key1: -123\nkey2: hello\n");
    }
}
