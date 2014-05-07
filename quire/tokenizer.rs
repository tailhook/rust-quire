use std::vec::Vec;
use std::str::CharOffsets;
use std::iter::Peekable;

use chars::is_indicator;
use chars::is_whitespace;
use chars::is_printable;

mod chars;

#[deriving(Eq, Show)]
enum TokenType {
    DocumentStart,
    DocumentEnd,
    Indent,
    Whitespace,
    PlainString,
    SingleString,
    DoubleString,
    Literal,
    Folded,
    Comment,
    Tag,
    Alias,
    Anchor,
    SequenceEntry,  // '-'
    MappingKey,  // '?'
    MappingValue,  // ':'
    FlowSeqStart,  // '['
    FlowSeqEnd,  // ']'
    FlowMapStart,  // '{'
    FlowMapEnd,  // '}'
    FlowEntry,  // ','
    Directive,  // '%...'
    Reserved,  // '@' or '`'
}

struct Pos {
    indent: uint,
    line: uint,
    line_start: bool,
    line_offset: uint,
    offset: uint,
}

struct Token<'tok> {
    kind: TokenType,
    start: Pos,
    end: Pos,
    value: &'tok str,
}

struct YamlIter<'a> {
    buf: &'a str,
    chars: Peekable<(uint, char), CharOffsets<'a>>,
    position: Pos,
    value: Option<char>,
}

struct Tokenizer<'a, 'b> {
    result: &'a mut Vec<Token<'b>>,
    data: &'b str,
    iter: YamlIter<'b>,
}

impl<'a> YamlIter<'a> {
    fn new<'x>(buf: &'x str) -> YamlIter<'x> {
        return YamlIter {
            buf: buf,
            chars: buf.char_indices().peekable(),
            position: Pos {
                indent: 0,
                offset: 0,
                line: 1,
                line_start: true,
                line_offset: 1,
                },
            value: None,
            };
    }
}

impl<'a> Iterator<(Pos, char)> for YamlIter<'a> {
    fn next(&mut self) -> Option<(Pos, char)> {
        let pos = self.position;  // Current position is returned one
        let npos = &mut self.position;  // Store new position in self
        match self.chars.next() {
            None => {
                return None;
            }
            Some((_, value)) => {
                npos.offset = match self.chars.peek() {
                    Some(&(off, _)) => off,
                    None => self.buf.len(),
                };
                match value {
                    '\r' | '\n' => {
                        match (self.value, value) {
                            (Some('\r'), '\n') => {}
                            _ => {
                                npos.line += 1;
                                npos.line_offset = 0;
                                npos.line_start = true;
                                npos.indent = 0;
                            }
                        }
                    }
                    ' ' if pos.line_start => {
                        npos.indent += 1;

                    }
                    '\t' if npos.line_start => {
                        // TODO: Emit error somehow
                        return None;
                    }
                    ch if !is_printable(ch) => {
                        // TODO: Emit error somehow
                        return None;
                    }
                    _ => {
                        npos.line_start = false;
                    }
                };
                return Some((pos, value));
            }
        };
    }
}


impl<'a, 'b> Tokenizer<'a, 'b> {

    fn new<'x, 'y>(result: &'x mut Vec<Token<'y>>, data: &'y str)
        -> Tokenizer<'x, 'y>
    {
        return Tokenizer {
            result: result,
            data: data,
            iter: YamlIter::new(data),
        }
    }

    fn skip_whitespace(&self) -> YamlIter<'b> {
        let mut iter = self.iter;
        loop {
            match iter.chars.peek() {
                Some(&(_, ch)) => match ch {
                    ' ' | '\t' | '\n' | '\r' => {}
                    _ => break,
                },
                None => break,
            }
            match iter.next() {
                Some((_, _)) => continue,
                None => break,
            }
        }
        return iter;
    }

    fn read_plain(&mut self, start: Pos) {
        loop {
            match self.iter.next() {
                Some((pos, ch)) => match ch {
                    ':' => {
                        // may end plainstring if followed by WS
                        match self.iter.chars.peek() {
                            Some(&(_, nchar)) => match nchar {
                                ' ' | '\t' | '\n' | '\r' => {}
                                _ => continue,
                            },
                            None => {}
                        }
                        self.add_token(PlainString, start, pos);
                        self.add_token(MappingValue, pos, self.iter.position);
                        return;
                    }
                    ' ' | '\t' | '\n' | '\r' => {
                        // may end plainstring if next block is not indented
                        // as much
                        let niter = self.skip_whitespace();
                        self.iter = niter;
                        if niter.position.indent >= start.indent {
                            continue;
                        } else {
                            self.add_token(PlainString, start, pos);
                            self.add_token(Whitespace, pos, niter.position);
                            return;
                        }
                    }
                    _ => continue,
                },
                None => break,
            }
        }
        self.add_token(PlainString, start, self.iter.position);
    }

    fn add_token(&mut self, kind: TokenType, start: Pos, end: Pos) {
        self.result.push(Token {
            kind: kind,
            start: start,
            end: end,
            value: self.data.slice(start.offset, end.offset),
            });
    }


    fn tokenize(&mut self) {
        loop {
            match self.iter.next() {
                Some((start, '-')) => { // list element, doc start, plainstring
                    match self.iter.next() {
                        Some((_, '-')) => { // maybe document end
                            match self.iter.next() {
                                Some((_, '-')) => self.add_token(DocumentStart,
                                    start, self.iter.position),
                                _ => self.read_plain(start),
                            }
                        }
                        Some((cur, ' ')) | Some((cur, '\t'))
                        | Some((cur, '\r')) | Some((cur, '\n')) => {
                            self.add_token(SequenceEntry, start, cur);
                            self.iter = self.skip_whitespace();
                            self.add_token(Whitespace, cur,
                                self.iter.position);
                        }
                        Some(_) => {
                            self.read_plain(start);
                        }
                        None => { // list element at end of stream
                            self.add_token(SequenceEntry,
                                start, self.iter.position);
                            return;
                            }
                        };
                    }
                Some((start, '?')) => { // key, plainstring
                    // TODO(tailhook) in flow context space is not required
                    match self.iter.next() {
                        Some((cur, ' ')) | Some((cur, '\t'))
                        | Some((cur, '\r')) | Some((cur, '\n')) => {
                            self.add_token(MappingKey, start, cur);
                            self.iter = self.skip_whitespace();
                            self.add_token(Whitespace, cur,
                                self.iter.position);
                        }
                        None => {
                            self.add_token(MappingKey, start,
                                self.iter.position);
                            break;
                        }
                        Some(_) =>  {
                            self.read_plain(start);
                        }
                    }
                }
                Some((start, ':')) => { // key, plainstring
                    // TODO(tailhook) in flow context space is not required
                    match self.iter.next() {
                        Some((cur, ' ')) | Some((cur, '\t'))
                        | Some((cur, '\r')) | Some((cur, '\n')) => {
                            self.add_token(MappingValue, start, cur);
                            self.iter = self.skip_whitespace();
                            self.add_token(Whitespace, cur,
                                self.iter.position);
                        }
                        None => {
                            self.add_token(MappingValue, start,
                                self.iter.position);
                            break;
                        }
                        Some(_) =>  {
                            self.read_plain(start);
                        }
                    }
                }
                // TODO: ":"  // mapping value or plainstring
                // TODO: "%"  // directive
                // TODO: "@" "`"  // not allowed
                // TODO: '"' // Quoted string
                // TODO: "'" // Quoted string
                // TODO: "#" // Comment
                // TODO: "&" // Anchor
                // TODO: "*" // Alias
                // TODO: "!" // Tag
                // TODO: "|" // BlockScalar
                // TODO: ">" // Folded Scalar
                // TODO: "," // Flow Entry
                // TODO: "[" // Flow Seq Start
                // TODO: "]" // Flow Seq End
                // TODO: "{" // Flow Map Start
                // TODO: "}" // Flow Map End
                Some((start, ' ')) | Some((start, '\t'))
                | Some((start, '\r')) | Some((start, '\n')) => {
                    // Just skipping it for now
                    self.iter = self.skip_whitespace();
                    self.add_token(Whitespace, start, self.iter.position);
                }
                Some((start, _)) => { self.read_plain(start); }
                None => break,
            }
        };
    }
}

fn tokenize<'x, 'y>(data: &'x str) -> Vec<Token<'x>> {
    let mut result: Vec<Token<'x>> = Vec::new();
    //let iter = data.char_indices();
    Tokenizer::new(&mut result, data).tokenize();
    return result;
}

#[cfg(test)]
fn simple_tokens<'a>(vec: &'a Vec<Token>) -> ~[(TokenType, &'a str)] {
    return vec.iter().map(|&tok| {
        return (tok.kind, tok.value);
    }).collect();
}

#[test]
fn test_tokenize() {
    let tokens = tokenize("a:  b");
    let strings = simple_tokens(&tokens);
    assert_eq!(strings, ~[
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, "  "),
        (PlainString, "b")]);
}

#[test]
fn test_list() {
    let tokens = tokenize("-");
    assert_eq!(simple_tokens(&tokens),
        ~[(SequenceEntry, "-")]);
    let tokens = tokenize("---");
    assert_eq!(simple_tokens(&tokens),
        ~[(DocumentStart, "---")]);
    let tokens = tokenize("- something");
    assert_eq!(simple_tokens(&tokens),
        ~[(SequenceEntry, "-"), (Whitespace, " "),
            (PlainString, "something")]);
}

#[test]
fn test_map_key() {
    let tokens = tokenize("?");
    assert_eq!(simple_tokens(&tokens),
        ~[(MappingKey, "?")]);
    let tokens = tokenize("?something");
    assert_eq!(simple_tokens(&tokens),
        ~[(PlainString, "?something")]);
    let tokens = tokenize("? something");
    assert_eq!(simple_tokens(&tokens),
        ~[(MappingKey, "?"), (Whitespace, " "), (PlainString, "something")]);
}

#[test]
fn test_map_value() {
    let tokens = tokenize(":");
    assert_eq!(simple_tokens(&tokens),
        ~[(MappingValue, ":")]);
    let tokens = tokenize(":something");
    assert_eq!(simple_tokens(&tokens),
        ~[(PlainString, ":something")]);
    let tokens = tokenize(": something");
    assert_eq!(simple_tokens(&tokens),
        ~[(MappingValue, ":"), (Whitespace, " "), (PlainString, "something")]);
}

#[test]
fn test_plain() {
    let tokens = tokenize("a");
    assert_eq!(simple_tokens(&tokens),
        ~[(PlainString, "a")]);
    let tokens = tokenize("abc");
    assert_eq!(simple_tokens(&tokens),
        ~[(PlainString, "abc")]);
    let tokens = tokenize("abc\ndef");
    assert_eq!(simple_tokens(&tokens),
        ~[(PlainString, "abc\ndef")]);
    let tokens = tokenize("a#bc");
    assert_eq!(simple_tokens(&tokens),
        ~[(PlainString, "a#bc")]);
    let tokens = tokenize(" a\nbc");
    assert_eq!(simple_tokens(&tokens),
        ~[(Whitespace, " "), (PlainString, "a"),
          (Whitespace, "\n"), (PlainString, "bc")]);
}

