use std::io::IoResult;
use std::str::from_utf8;
#[cfg(test)] use std::io::MemWriter;

use super::parser::Node;
#[cfg(test)] use super::parser::parse;

use S = self::State;
use L = self::Line;
use N = super::parser;  // Node enum constants

type Tag<'a> = &'a str;
type Anchor<'a> = &'a str;

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
    stream: &'a mut Writer,
    stack: Vec<State::Opcode>,
    state: State::Opcode,
    line: Line::State,
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
                self.stream.write_str(if space { "null\n" } else { " null\n"}),
        };
    }

    fn push_indent(&mut self, state: S::Opcode) {
        // TODO(tailhook) allow to custimize indent width at each nesting level
        self.stack.push(state);
        self.cur_indent = (self.stack.len() - 1) * 2;
    }
    fn pop_indent(&mut self) -> State::Opcode {
        let val = self.stack.pop().unwrap();
        self.cur_indent = (self.stack.len() - 1) * 2;
        return val;
    }

    fn ensure_line_start(&mut self) -> IoResult<()> {
        match self.line {
            L::Start => {
                return Ok(());
            }
            L::AfterScalar => {
                self.line = L::Start;
                return self.stream.write_char('\n');
            }
        }
    }
    fn ensure_indented(&mut self) -> IoResult<()> {
        self.ensure_line_start();
        for i in range(0, self.cur_indent) {
            try!(self.stream.write_char(' '));
        }
        return Ok(());
    }

    pub fn emit(&mut self, op: Opcode) -> IoResult<()> {
        self.state = match (self.state, op) {
            (S::Fin, _) => unreachable!(),
            (S::New, Scalar(tag, anchor, style, value)) => {
                try!(self.emit_scalar(style, value));
                try!(self.ensure_line_start())
                S::Fin }
            (S::New, Null(tag, anchor, style)) => {
                try!(self.emit_null(false, style));
                try!(self.ensure_line_start())
                S::Fin }
            (S::New, MapStart(tag, anchor)) => {
                self.push_indent(S::Fin);
                S::MapKey }
            (S::MapKey, Scalar(tag, anchor, style, value)) => {
                try!(self.ensure_indented());
                // TODO(tailhook) check for complex key
                try!(self.emit_scalar(style, value));
                S::MapSimpleKeyValue }
            (S::MapSimpleKeyValue, Scalar(tag, anchor, style, value)) => {
                try!(self.stream.write_str(": "));
                try!(self.emit_scalar(style, value));
                S::MapKey }
            (S::MapKey, MapEnd) => {
                let nstate = self.pop_indent();
                match nstate {
                    S::Fin => try!(self.ensure_line_start()),
                    _ => {}
                }
                nstate }
            (S::New, SeqStart(tag, anchor)) => {
                self.push_indent(S::Fin);
                S::SeqItem }
            (S::SeqItem, Scalar(tag, anchor, style, value)) => {
                try!(self.ensure_indented());
                try!(self.stream.write_str("- "));
                try!(self.emit_scalar(style, value));
                S::SeqItem }
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
            &N::Scalar(ref tag, ref anchor, ref value, tok) => {
                // TODO(tailhook) fix tag and anchor
                try!(self.emit(Scalar(None, None, Auto, value.as_slice())));
            }
            &N::Null(tag, anchor) => { }
            &N::Alias(name) => unimplemented!(),
        }
        return Ok(());
    }
}

pub fn emit_parse_tree(tree: &Node, stream: &mut Writer)
    -> IoResult<()>
{
    let mut ctx = Context::new(stream);
    return ctx.emit_node(tree);
}

#[cfg(test)]
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
    ], r#"
"#);
}

#[test]
fn test_plain() {
    emit_and_compare([
        Scalar(None, None, Auto, "hello"),
    ], r#"hello
"#);
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
    ], r#"a: val
b: 2
"#);
}

#[cfg(test)]
fn assert_yaml_eq_yaml(source: &'static str, output: &'static str) {
    let mut buf = MemWriter::new();
    parse(source, |doc| {
        let mut ctx = Context::new(&mut buf);
        ctx.emit_node(&doc.root).unwrap();
    }).unwrap();
    let bytes = buf.unwrap();
    let value = from_utf8(bytes.as_slice()).unwrap();
    assert_eq!(value, output);
}

#[test]
fn yaml_scalar() {
    assert_yaml_eq_yaml("Hello", "Hello\n");
}

#[test]
fn yaml_map() {
    assert_yaml_eq_yaml("a: b\nc: d", "a: b\nc: d\n");
}

#[test]
fn yaml_list() {
    assert_yaml_eq_yaml("- a\n- b", "- a\n- b\n");
}
