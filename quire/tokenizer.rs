use std::io::IoResult;
use std::vec::Vec;
use std::str::CharOffsets;
use std::iter::Peekable;
use std::fmt::Show;
use std::fmt::Formatter;
use std::fmt::FormatError;
use std::vec::FromVec;

use super::chars::is_indicator;
use super::chars::is_whitespace;
use super::chars::is_printable;
use super::chars::is_tag_char;
use super::chars::is_flow_indicator;

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

struct TokenError {
    position: Pos,
    error: &'static str,
}

impl TokenError {
    fn new(pos: Pos, err: &'static str) -> TokenError {
        return TokenError {
            position: pos,
            error: err,
            };
    }
}

impl Show for TokenError {
    fn fmt(&self, fmt:&mut Formatter) -> Result<(), FormatError> {
        try!(self.position.line.fmt(fmt));
        try!(':'.fmt(fmt));
        try!(self.position.line_offset.fmt(fmt));
        try!(": ".fmt(fmt));
        try!(self.error.fmt(fmt));
        return Ok(());
    }
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
    error: Option<TokenError>,
}

struct Tokenizer<'a, 'b> {
    result: &'a mut Vec<Token<'b>>,
    data: &'b str,
    iter: YamlIter<'b>,
    error: Option<TokenError>,
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
            error: None,
            };
    }
}

impl<'a> Iterator<(Pos, char)> for YamlIter<'a> {
    fn next(&mut self) -> Option<(Pos, char)> {
        let pos = self.position;  // Current position is returned one
        let npos = &mut self.position;  // Store new position in self
        match self.chars.next() {
            None => {
                self.value = None;
                return None;
            }
            Some((_, value)) => {
                self.value = Some(value);
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
                    ch if !is_printable(ch) => {
                        self.error = Some(TokenError::new(*npos,
                            "Unacceptable character"));
                        return None;
                    }
                    _ => {
                        npos.line_start = false;
                    }
                };
                npos.line_offset += 1;
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
            error: None,
        }
    }

    fn skip_whitespace(&self) -> YamlIter<'b> {
        let mut iter = self.iter;
        loop {
            match iter.chars.peek() {
                Some(&(_, ch)) => match ch {
                    ' ' | '\n' | '\r' => {}
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
        let mut minindent = start.indent;
        if !start.line_start {
            minindent += 1;
        }
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
                    ' ' | '\n' | '\r' => {
                        // may end plainstring if next block is not indented
                        // as much
                        let niter = self.skip_whitespace();
                        self.iter = niter;
                        if niter.position.indent >= minindent {
                            match self.iter.chars.peek() {
                                Some(&(_, '#')) => {}
                                _ => { continue; }
                            }
                        }
                        self.add_token(PlainString, start, pos);
                        self.add_token(Whitespace, pos, niter.position);
                        return;
                    }
                    '\t' => {
                        self.error = Some(TokenError::new(pos,
                            "Tab character may appear only in quoted string"));
                        break;
                    }
                    _ => continue,
                },
                None => break,
            }
        }
        self.add_token(PlainString, start, self.iter.position);
    }

    fn read_block(&mut self, tok: TokenType, start: Pos) {
        // TODO(tailhook) we indent the same way as with plain scalars
        //                by PyYaml sets indent to the one of the first line
        //                of content, is it what's by spec?
        let mut minindent = start.indent;
        if !start.line_start {
            minindent += 1;
        }
        loop {
            match self.iter.next() {
                Some((pos, ch)) => match ch {
                    '\n' | '\r' => {
                        // may end folded if next block is not indented
                        // as much
                        let niter = self.skip_whitespace();
                        self.iter = niter;
                        if niter.position.indent >= minindent {
                            continue;
                        }
                        self.add_token(tok, start, pos);
                        self.add_token(Whitespace, pos, niter.position);
                        return;
                    }
                    _ => continue,
                },
                None => break,
            }
        }
        self.add_token(tok, start, self.iter.position);
    }

    fn add_token(&mut self, kind: TokenType, start: Pos, end: Pos) {
        self.result.push(Token {
            kind: kind,
            start: start,
            end: end,
            value: self.data.slice(start.offset, end.offset),
            });
    }


    fn tokenize(&mut self) -> Option<TokenError> {
        'tokenloop: loop {
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
                            break;
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
                Some((start, '%')) => {
                    if start.line_offset != 1 {
                        self.error = Some(TokenError::new(start,
                            "Directive must start at start of line"));
                        break;
                    }
                    for (_, ch) in self.iter {
                        if ch == '\r' || ch == '\n' {
                            break;
                        }
                    }
                    self.add_token(Directive, start, self.iter.position);
                }
                Some((start, '@')) | Some((start, '`')) => {
                    self.error = Some(TokenError::new(start,
                        "Characters '@' and '`' are not allowed"));
                    break;
                }
                Some((start, '\t')) => {
                    self.error = Some(TokenError::new(start,
                        "Tab character may appear only in quoted string"));
                    break;
                }
                Some((start, '"')) => {
                    let mut prev = '"';
                    for (_, ch) in self.iter {
                        if ch == '"' && prev != '\\' {
                            break;
                        }
                        prev = ch;
                    }
                    if self.iter.value.is_none() {
                        self.error = Some(TokenError::new(start,
                            "Unclosed double-quoted string"));
                        break;
                    }
                    self.add_token(DoubleString, start, self.iter.position);
                }
                Some((start, '\'')) => {
                    for (_, ch) in self.iter {
                        if ch == '\'' {
                            break;
                        }
                    }
                    if self.iter.value.is_none() {
                        self.error = Some(TokenError::new(start,
                            "Unclosed quoted string"));
                        break;
                    }
                    self.add_token(SingleString, start, self.iter.position);
                }
                Some((start, '#')) => {
                    for (_, ch) in self.iter {
                        if ch == '\r' || ch == '\n' {
                            break;
                        }
                    }
                    self.add_token(Comment, start, self.iter.position);
                }
                Some((start, '!')) => {
                    loop {
                        match self.iter.chars.peek() {
                            Some(&(_, ch)) if is_whitespace(ch) => break,
                            None => break,
                            _ => {}
                        }
                        let (pos, ch) = self.iter.next().unwrap();
                        if !is_tag_char(ch) {
                            self.error = Some(TokenError::new(pos,
                                "Bad char in tag name"));
                            break 'tokenloop;
                        }
                    }
                    self.add_token(Tag, start, self.iter.position);
                }
                Some((start, '&')) => {
                    loop {
                        match self.iter.chars.peek() {
                            Some(&(_, ch)) if is_whitespace(ch) => break,
                            None => break,
                            _ => {}
                        }
                        let (pos, ch) = self.iter.next().unwrap();
                        if is_flow_indicator(ch) {
                            self.error = Some(TokenError::new(pos,
                                "Bad char in anchor name"));
                            break 'tokenloop;
                        }
                    }
                    if self.iter.position.offset - start.offset < 2 {
                        self.error = Some(TokenError::new(start,
                           "Anchor name requires at least one character"));
                        break;
                    }
                    self.add_token(Anchor, start, self.iter.position);
                }
                Some((start, '*')) => {
                    loop {
                        match self.iter.chars.peek() {
                            Some(&(_, ch)) if is_whitespace(ch) => break,
                            None => break,
                            _ => {}
                        }
                        let (pos, ch) = self.iter.next().unwrap();
                        if is_flow_indicator(ch) {
                            self.error = Some(TokenError::new(pos,
                                "Bad char in alias name"));
                            break 'tokenloop;
                        }
                    }
                    if self.iter.position.offset - start.offset < 2 {
                        self.error = Some(TokenError::new(start,
                            "Alias name requires at least one character"));
                        break;
                    }
                    self.add_token(Alias, start, self.iter.position);
                }
                Some((start, ',')) => {
                    self.add_token(FlowEntry, start, self.iter.position);
                }
                Some((start, '[')) => {
                    self.add_token(FlowSeqStart, start, self.iter.position);
                }
                Some((start, ']')) => {
                    self.add_token(FlowSeqEnd, start, self.iter.position);
                }
                Some((start, '{')) => {
                    self.add_token(FlowMapStart, start, self.iter.position);
                }
                Some((start, '}')) => {
                    self.add_token(FlowMapEnd, start, self.iter.position);
                }
                Some((start, '|')) => {
                    self.read_block(Literal, start);
                }
                Some((start, '>')) => {
                    self.read_block(Folded, start);
                }
                Some((start, ' '))
                | Some((start, '\r')) | Some((start, '\n')) => {
                    self.iter = self.skip_whitespace();
                    self.add_token(Whitespace, start, self.iter.position);
                }
                Some((start, _)) => { self.read_plain(start); }
                None => break,
            }
        }
        return self.error.or(self.iter.error);
    }
}

fn tokenize<'x, 'y>(data: &'x str) -> Result<Vec<Token<'x>>, TokenError> {
    let mut result: Vec<Token<'x>> = Vec::new();
    //let iter = data.char_indices();
    return match Tokenizer::new(&mut result, data).tokenize() {
        Some(err) => Err(err),
        None => Ok(result),
    };
}

#[cfg(test)]
fn simple_tokens<'x>(res: Result<Vec<Token<'x>>, TokenError>)
    -> ~[(TokenType, &'x str)]
{
    match res {
        Ok(vec) => {
            return FromVec::<(TokenType, &str)>::from_vec(vec.iter().map(
            |&tok| {
                return (tok.kind, tok.value);
            }).collect());
        }
        Err(value) => {
            fail!("Error: {}", value);
        }
    }
}

#[test]
fn test_tokenize() {
    let tokens = tokenize("a:  b");
    let strings = simple_tokens(tokens);
    assert_eq!(strings, ~[
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, "  "),
        (PlainString, "b")]);
}

#[test]
fn test_list() {
    let tokens = tokenize("-");
    assert_eq!(simple_tokens(tokens),
        ~[(SequenceEntry, "-")]);
    let tokens = tokenize("---");
    assert_eq!(simple_tokens(tokens),
        ~[(DocumentStart, "---")]);
    let tokens = tokenize("- something");
    assert_eq!(simple_tokens(tokens),
        ~[(SequenceEntry, "-"), (Whitespace, " "),
            (PlainString, "something")]);
}

#[test]
fn test_map_key() {
    let tokens = tokenize("?");
    assert_eq!(simple_tokens(tokens),
        ~[(MappingKey, "?")]);
    let tokens = tokenize("?something");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "?something")]);
    let tokens = tokenize("? something");
    assert_eq!(simple_tokens(tokens),
        ~[(MappingKey, "?"), (Whitespace, " "), (PlainString, "something")]);
}

#[test]
fn test_map_value() {
    let tokens = tokenize(":");
    assert_eq!(simple_tokens(tokens),
        ~[(MappingValue, ":")]);
    let tokens = tokenize(":something");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, ":something")]);
    let tokens = tokenize(": something");
    assert_eq!(simple_tokens(tokens),
        ~[(MappingValue, ":"), (Whitespace, " "), (PlainString, "something")]);
}

#[test]
fn test_plain() {
    let tokens = tokenize("a");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "a")]);
    let tokens = tokenize("abc");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "abc")]);
    let tokens = tokenize("abc\ndef");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "abc\ndef")]);
    let tokens = tokenize("a#bc");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "a#bc")]);
    let tokens = tokenize(" a\nbc");
    assert_eq!(simple_tokens(tokens),
        ~[(Whitespace, " "), (PlainString, "a"),
          (Whitespace, "\n"), (PlainString, "bc")]);
    let tokens = tokenize("a:\n a\n bc");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "a"), (MappingValue, ":"), (Whitespace, "\n "),
            (PlainString, "a\n bc")]);
    let tokens = tokenize("a: a\nbc");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "a"), (MappingValue, ":"), (Whitespace, " "),
            (PlainString, "a"), (Whitespace, "\n"), (PlainString, "bc")]);
}

#[test]
fn test_block() {
    let tokens = tokenize("|\n a");
    assert_eq!(simple_tokens(tokens),
        ~[(Literal, "|\n a")]);
    let tokens = tokenize("a: >\n b\nc");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "a"), (MappingValue, ":"), (Whitespace, " "),
          (Folded, ">\n b"), (Whitespace, "\n"), (PlainString, "c")]);
}

#[test]
fn test_directive() {
    let tokens = tokenize("%");
    assert_eq!(simple_tokens(tokens),
        ~[(Directive, "%")]);
    let tokens = tokenize("%something\n");
    assert_eq!(simple_tokens(tokens),
        ~[(Directive, "%something\n")]);
    let tokens = tokenize("%abc\ndef");
    assert_eq!(simple_tokens(tokens),
        ~[(Directive, "%abc\n"), (PlainString, "def")]);
    let tokens = tokenize("a%bc");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "a%bc")]);
    let err = tokenize(" %bc").err().unwrap();
    // TODO(pc) add testcase with percent sign at start of token
    assert_eq!(format!("{}", err), "1:2: "
        + "Directive must start at start of line");
}

#[test]
fn test_reserved() {
    let err = tokenize("@").err().unwrap();
    assert_eq!(format!("{}", err), "1:1: "
        + "Characters '@' and '`' are not allowed");
    let err = tokenize("a:\n  @").err().unwrap();
    assert_eq!(format!("{}", err), "2:3: "
        + "Characters '@' and '`' are not allowed");
    let tokens = tokenize("a@");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "a@")]);
    let tokens = tokenize("a\n@");
    assert_eq!(simple_tokens(tokens),
        ~[(PlainString, "a\n@")]);
}

#[test]
fn test_bad_char() {
    let err = tokenize("\x01").err().unwrap();
    assert_eq!(format!("{}", err), "1:1: "
        + "Unacceptable character");
    let err = tokenize("\t").err().unwrap();
    assert_eq!(format!("{}", err), "1:1: "
        + "Tab character may appear only in quoted string");
    let err = tokenize("a:\n  \tbc").err().unwrap();
    assert_eq!(format!("{}", err), "2:3: "
        + "Tab character may appear only in quoted string");
    let err = tokenize("a\n\tb").err().unwrap();
    assert_eq!(format!("{}", err), "2:1: "
        + "Tab character may appear only in quoted string");
    let err = tokenize("a\tb").err().unwrap();
    assert_eq!(format!("{}", err), "1:2: "
        + "Tab character may appear only in quoted string");
}

#[test]
fn test_double_quoted() {
    let tokens = tokenize("\"\"");
    assert_eq!(simple_tokens(tokens),
        ~[(DoubleString, "\"\"")]);
    let tokens = tokenize("\"a\nb\"");
    assert_eq!(simple_tokens(tokens),
        ~[(DoubleString, "\"a\nb\"")]);
    let tokens = tokenize("\"a\\\"\nb\"");
    assert_eq!(simple_tokens(tokens),
        ~[(DoubleString, "\"a\\\"\nb\"")]);
    let err = tokenize("val: \"value\nof").err().unwrap();
    assert_eq!(format!("{}", err), "1:6: "
        + "Unclosed double-quoted string");
}

#[test]
fn test_single_quoted() {
    let tokens = tokenize("''");
    assert_eq!(simple_tokens(tokens),
        ~[(SingleString, "''")]);
    let tokens = tokenize("'a\nb'");
    assert_eq!(simple_tokens(tokens),
        ~[(SingleString, "'a\nb'")]);
    let tokens = tokenize("'a\\': 'b'");
    assert_eq!(simple_tokens(tokens),
        ~[(SingleString, "'a\\'"), (MappingValue, ":"),
          (Whitespace, " "), (SingleString, "'b'")]);
    let err = tokenize("val: 'value\nof").err().unwrap();
    assert_eq!(format!("{}", err), "1:6: "
        + "Unclosed quoted string");
}

#[test]
fn test_comment() {
    assert_eq!(simple_tokens(tokenize("#")),
        ~[(Comment, "#")]);
    assert_eq!(simple_tokens(tokenize("#a")),
        ~[(Comment, "#a")]);
    assert_eq!(simple_tokens(tokenize("#a\nb")),
        ~[(Comment, "#a\n"), (PlainString, "b")]);
    assert_eq!(simple_tokens(tokenize("a #b\nc")),
        ~[(PlainString, "a"), (Whitespace, " "),
          (Comment, "#b\n"), (PlainString, "c")]);
    assert_eq!(simple_tokens(tokenize("  #a\nb")),
        ~[(Whitespace, "  "), (Comment, "#a\n"), (PlainString, "b")]);
}

#[test]
fn test_tag() {
    assert_eq!(simple_tokens(tokenize("!")),
        ~[(Tag, "!")]);
    assert_eq!(simple_tokens(tokenize("!a b")),
        ~[(Tag, "!a"), (Whitespace, " "), (PlainString, "b")]);
    let err = tokenize("!a[]").err().unwrap();
    assert_eq!(format!("{}", err), "1:3: "
        + "Bad char in tag name");
}

#[test]
fn test_anchor() {
    assert_eq!(simple_tokens(tokenize("&abc")),
        ~[(Anchor, "&abc")]);
    assert_eq!(simple_tokens(tokenize("&a b")),
        ~[(Anchor, "&a"), (Whitespace, " "), (PlainString, "b")]);
    let err = tokenize("&a[]").err().unwrap();
    assert_eq!(format!("{}", err), "1:3: "
        + "Bad char in anchor name");
    let err = tokenize("&").err().unwrap();
    assert_eq!(format!("{}", err), "1:1: "
        + "Anchor name requires at least one character");
}

#[test]
fn test_alias() {
    assert_eq!(simple_tokens(tokenize("*abc")),
        ~[(Alias, "*abc")]);
    assert_eq!(simple_tokens(tokenize("*a b")),
        ~[(Alias, "*a"), (Whitespace, " "), (PlainString, "b")]);
    let err = tokenize("*a[]").err().unwrap();
    assert_eq!(format!("{}", err), "1:3: "
        + "Bad char in alias name");
    let err = tokenize("*").err().unwrap();
    assert_eq!(format!("{}", err), "1:1: "
        + "Alias name requires at least one character");
}
