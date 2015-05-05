use std::vec::Vec;
use std::str::CharIndices;
use std::rc::Rc;
use std::fmt::Display;
use std::fmt::Error as FormatError;
use std::fmt::{Formatter};
use std::iter::Iterator;
use std::clone::Clone;

use super::chars::is_whitespace;
use super::chars::is_printable;
use super::chars::is_tag_char;
use super::chars::is_flow_indicator;
use super::errors::Error;
use self::TokenType::*;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenType {
    Eof,
    DocumentStart,
    DocumentEnd,
    Indent,
    Unindent,
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
}


#[derive(Clone)]
pub struct Pos {
    pub filename: Rc<String>,
    pub indent: usize,
    pub line: usize,
    pub line_start: bool,
    pub line_offset: usize,
    pub offset: usize,
}

impl Display for Pos {
    fn fmt(&self, fmt:&mut Formatter) -> Result<(), FormatError> {
        write!(fmt, "{filename}:{line}:{offset}:",
            filename=self.filename,
            line=self.line,
            offset=self.line_offset)
    }
}

pub struct Token<'tok> {
    pub kind: TokenType,
    pub start: Pos,
    pub end: Pos,
    pub value: &'tok str,
}

struct YamlIter<'a> {
    buf: &'a str,
    chars: CharIndices<'a>,
    next_value: Option<(usize, char)>,
    position: Pos,
    value: Option<char>,
    error: Option<Error>,
}

impl<'a> Clone for YamlIter<'a> {
    #[inline]
    fn clone(&self) -> YamlIter<'a> {
        return YamlIter {
            buf: self.buf,
            next_value: self.next_value,
            chars: self.chars.clone(),
            position: self.position.clone(),
            value: self.value,
            error: self.error.clone(),
        }
    }
}

struct Tokenizer<'a, 'b: 'a> {
    result: &'a mut Vec<Token<'b>>,
    data: &'b str,
    iter: YamlIter<'b>,
    error: Option<Error>,
    indent_levels: Vec<usize>,
    flow_level: usize,
    doc_start: bool,
}

impl<'a> YamlIter<'a> {
    fn new<'x>(filename: Rc<String>, buf: &'x str) -> YamlIter<'x> {
        let mut iter = buf.char_indices();
        return YamlIter {
            buf: buf,
            next_value: iter.next(),
            chars: iter,
            position: Pos {
                filename: filename,
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

impl<'a> Iterator for YamlIter<'a> {
    type Item = (Pos, char);
    fn next(&mut self) -> Option<(Pos, char)> {
        let pos = self.position.clone();  // Current position is returned one
        let npos = &mut self.position;  // Store new position in self
        match self.next_value {
            None => {
                self.value = None;
                return None;
            }
            Some((_, value)) => {
                self.value = Some(value);
                self.next_value = self.chars.next();
                npos.offset = match self.next_value {
                    Some((off, _)) => off,
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
                        self.error = Some(Error::tokenizer_error(npos,
                            "Unacceptable character".to_string()));
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

    fn new<'x, 'y>(result: &'x mut Vec<Token<'y>>,
        name: Rc<String>, data: &'y str)
        -> Tokenizer<'x, 'y>
    {
        return Tokenizer {
            result: result,
            data: data,
            iter: YamlIter::new(name, data),
            error: None,
            indent_levels: vec!(0),
            flow_level: 0,
            doc_start: true,
        }
    }

    fn skip_whitespace(&self) -> YamlIter<'b> {
        let mut iter = self.iter.clone();
        loop {
            match iter.next_value {
                Some((_, ch)) => match ch {
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
        let mut minindent = *self.indent_levels.last().unwrap();
        if !self.doc_start {
            minindent += 1;
        }
        loop {
            match self.iter.next_value {
                Some((_, ch)) => match ch {
                    '[' | ']' | '{' | '}' | ',' if self.flow_level > 0 => {
                        let pos = self.iter.position.clone();
                        self.add_token(PlainString, start, pos);
                        return;
                    }
                    ':' => {
                        // may end plainstring if followed by WS or flow marker
                        let pos = self.iter.position.clone();
                        let mut niter = self.iter.clone();
                        niter.next();
                        match niter.next_value.map(|(_, x)| x) {
                            None | Some(' ') | Some('\t')
                            | Some('\n') | Some('\r') => {
                                self.add_token(PlainString, start, pos);
                                return;
                            }
                            Some('{') | Some('[') | Some(',')
                            | Some('}') | Some(']')
                            if self.flow_level > 0 => {
                                self.add_token(PlainString, start, pos);
                                return;
                            }
                            _ => {}
                        }
                    }
                    ' ' | '\n' | '\r' => {
                        // may end plainstring if next block is not indented
                        // as much
                        let pos = self.iter.position.clone();
                        let niter = self.skip_whitespace();
                        self.iter = niter.clone();
                        if pos.line == niter.position.line ||
                                niter.position.indent >= minindent
                        {
                            match self.iter.next_value.map(|(_, x)| x) {
                                Some('[') | Some(']') | Some('{') | Some('}')
                                | Some(',') if self.flow_level > 0 => {
                                    self.add_token(PlainString,
                                        start, pos.clone());
                                    self.add_token(Whitespace, pos,
                                        niter.position);
                                    return;
                                }
                                Some('\t') => {
                                    self.error = Some(
                                        Error::tokenizer_error(
                                        &self.iter.position,
                                        "Tab character may appear only in \
                                            quoted string".to_string()));
                                    break;
                                }
                                Some('#') => {
                                    self.add_token(PlainString,
                                        start, pos.clone());
                                    self.add_token(Whitespace, pos,
                                        niter.position);
                                    return;
                                }
                                _ => {}
                            }
                        } else {
                            self.add_token(PlainString, start, pos.clone());
                            self.add_token(Whitespace, pos,
                                niter.position);
                            return;
                        }
                    }
                    '\t' => {
                        self.error = Some(Error::tokenizer_error(
                            &self.iter.position,
                            "Tab character may appear only in quoted string"
                            .to_string()));
                        break;
                    }
                    _ => {},
                },
                None => break,
            }
            self.iter.next();
        }
        let end = self.iter.position.clone();
        self.add_token(PlainString, start, end);
    }

    fn read_block(&mut self, tok: TokenType, start: Pos) {
        // TODO(tailhook) we indent the same way as with plain scalars
        //                by PyYaml sets indent to the one of the first line
        //                of content, is it what's by spec?
        let mut minindent = *self.indent_levels.last().unwrap();
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
                        self.iter = niter.clone();
                        if niter.position.indent >= minindent {
                            continue;
                        }
                        self.add_token(tok, start, pos.clone());
                        self.add_token(Whitespace, pos, niter.position.clone());
                        return;
                    }
                    _ => continue,
                },
                None => break,
            }
        }
        let end = self.iter.position.clone();
        self.add_token(tok, start, end);
    }

    fn add_token(&mut self, kind: TokenType, start: Pos, end: Pos) {
        if kind != Whitespace && kind != Comment && kind != DocumentStart &&
            self.flow_level == 0
        {
            // always have "0" at bottom of the stack so just unwrap it
            self.doc_start = false;
            let cur = *self.indent_levels.last().unwrap();
            if start.indent > cur {
                self.result.push(Token {
                    kind: Indent,
                    start: start.clone(),
                    end: start.clone(),
                    value: &self.data[start.offset..start.offset],
                    });
                self.indent_levels.push(start.indent);
            } else if start.indent < cur {
                while *self.indent_levels.last().unwrap() > start.indent {
                    self.result.push(Token {
                        kind: Unindent,
                        start: start.clone(),
                        end: start.clone(),
                        value: &self.data[start.offset..start.offset],
                        });
                    self.indent_levels.pop();
                }
                if *self.indent_levels.last().unwrap() != start.indent {
                    self.error = Some(Error::tokenizer_error(&start,
                        "Unindent doesn't match any outer indentation level"
                        .to_string()));
                }
            }
        }
        self.result.push(Token {
            kind: kind,
            start: start.clone(),
            end: end.clone(),
            value: &self.data[start.offset..end.offset],
            });
    }


    fn tokenize(&mut self) -> Option<Error> {
        'tokenloop: loop  {
            if !self.error.is_none() {
                break;
            }
            match self.iter.next() {
                Some((start, '-')) => { // list element, doc start, plainstring
                    match self.iter.next() {
                        Some((_, '-')) => { // maybe document end
                            match self.iter.next() {
                                Some((_, '-')) => {
                                    let end = self.iter.position.clone();
                                    self.add_token(DocumentStart, start, end);
                                }
                                _ => self.read_plain(start),
                            }
                        }
                        Some((cur, ' ')) | Some((cur, '\t'))
                        | Some((cur, '\r')) | Some((cur, '\n')) => {
                            //  For handling nested maps and lists
                            //  indentation must be adjusted to the level
                            //  of the end of whitespace
                            if self.iter.position.line == start.line {
                                //  Line offset is human-readable so 1-based
                                //  as opposed to indentation
                                self.iter.position.indent =
                                    self.iter.position.line_offset - 1;
                            }
                            self.add_token(SequenceEntry, start, cur.clone());
                            self.iter = self.skip_whitespace();
                            let end = self.iter.position.clone();
                            self.add_token(Whitespace, cur, end);
                        }
                        Some(_) => {
                            self.read_plain(start);
                        }
                        None => {
                            let end = self.iter.position.clone();
                            self.add_token(SequenceEntry, start, end);
                            break;
                            }
                        };
                    }
                Some((start, '?')) => { // key, plainstring
                    // TODO(tailhook) in flow context space is not required
                    match self.iter.next() {
                        Some((cur, ' ')) | Some((cur, '\t'))
                        | Some((cur, '\r')) | Some((cur, '\n')) => {
                            self.add_token(MappingKey, start, cur.clone());
                            self.iter = self.skip_whitespace();
                            let end = self.iter.position.clone();
                            self.add_token(Whitespace, cur, end);
                        }
                        None => {
                            let end = self.iter.position.clone();
                            self.add_token(MappingKey, start, end);
                            break;
                        }
                        Some(_) =>  {
                            self.read_plain(start);
                        }
                    }
                }
                Some((start, ':')) => { // key, plainstring
                    // TODO(tailhook) in flow context space is not required
                    if self.flow_level > 0 {
                        match self.iter.next_value {
                            Some((_, ' ')) | Some((_, '\t'))
                            | Some((_, '\r')) | Some((_, '\n'))
                            | Some((_, '{')) | Some((_, '}'))
                            | Some((_, '[')) | Some((_, ']'))
                            | Some((_, ',')) => {
                                let end = self.iter.position.clone();
                                self.add_token(MappingValue, start, end);
                            }
                            _ => {
                                self.error = Some(Error::tokenizer_error(
                                    &start,
                                    "Either add a space or quote the colon"
                                    .to_string()));
                                break;
                            }
                        }
                    } else {
                        match self.iter.next() {
                            Some((cur, ' ')) | Some((cur, '\t'))
                            | Some((cur, '\r')) | Some((cur, '\n')) => {
                                self.add_token(MappingValue,
                                    start, cur.clone());
                                self.iter = self.skip_whitespace();
                                let end = self.iter.position.clone();
                                self.add_token(Whitespace, cur, end);
                            }
                            None => {
                                let end = self.iter.position.clone();
                                self.add_token(MappingValue, start, end);
                                break;
                            }
                            Some(_) =>  {
                                self.read_plain(start);
                            }
                        }
                    }
                }
                Some((start, '%')) => {
                    if start.line_offset != 1 {
                        self.error = Some(Error::tokenizer_error(&start,
                            "Directive must start at start of line"
                            .to_string()));
                        break;
                    }
                    for (_, ch) in &mut self.iter {
                        if ch == '\r' || ch == '\n' {
                            break;
                        }
                    }
                    let end = self.iter.position.clone();
                    self.add_token(Directive, start, end);
                }
                Some((start, '@')) | Some((start, '`')) => {
                    self.error = Some(Error::tokenizer_error(&start,
                        "Characters '@' and '`' are not allowed".to_string()));
                    break;
                }
                Some((start, '\t')) => {
                    self.error = Some(Error::tokenizer_error(&start,
                        "Tab character may appear only in quoted string"
                        .to_string()));
                    break;
                }
                Some((start, '"')) => {
                    let mut prev = '"';
                    for (_, ch) in &mut self.iter {
                        if ch == '"' && prev != '\\' {
                            break;
                        }
                        prev = ch;
                    }
                    if self.iter.value.is_none() {
                        self.error = Some(Error::tokenizer_error(&start,
                            "Unclosed double-quoted string".to_string()));
                        break;
                    }
                    let end = self.iter.position.clone();
                    self.add_token(DoubleString, start, end.clone());
                    // This is YAML 1.2 hack to be superset of json
                    match self.iter.next_value {
                        Some((_, ':')) => {
                            self.iter.next();
                            let mvalend = self.iter.position.clone();
                            self.add_token(MappingValue, end, mvalend);
                        }
                        _ => {}
                    }
                }
                Some((start, '\'')) => {
                    for (_, ch) in &mut self.iter {
                        if ch == '\'' {
                            break;
                        }
                    }
                    if self.iter.value.is_none() {
                        self.error = Some(Error::tokenizer_error(&start,
                            "Unclosed quoted string".to_string()));
                        break;
                    }
                    let end = self.iter.position.clone();
                    self.add_token(SingleString, start, end);
                }
                Some((start, '#')) => {
                    for (_, ch) in &mut self.iter {
                        if ch == '\r' || ch == '\n' {
                            break;
                        }
                    }
                    let end = self.iter.position.clone();
                    self.add_token(Comment, start, end);
                }
                Some((start, '!')) => {
                    loop {
                        match self.iter.next_value {
                            Some((_, ch)) if is_whitespace(ch) => break,
                            None => break,
                            _ => {}
                        }
                        let (pos, ch) = self.iter.next().unwrap();
                        if !is_tag_char(ch) {
                            self.error = Some(Error::tokenizer_error(&pos,
                                "Bad char in tag name".to_string()));
                            break 'tokenloop;
                        }
                    }
                    let end = self.iter.position.clone();
                    self.add_token(Tag, start, end);
                }
                Some((start, '&')) => {
                    loop {
                        match self.iter.next_value {
                            Some((_, ch)) if is_whitespace(ch) => break,
                            None => break,
                            _ => {}
                        }
                        let (pos, ch) = self.iter.next().unwrap();
                        if is_flow_indicator(ch) {
                            self.error = Some(Error::tokenizer_error(&pos,
                                "Bad char in anchor name".to_string()));
                            break 'tokenloop;
                        }
                    }
                    if self.iter.position.offset - start.offset < 2 {
                        self.error = Some(Error::tokenizer_error(&start,
                           "Anchor name requires at least one character"
                           .to_string()));
                        break;
                    }
                    let end = self.iter.position.clone();
                    self.add_token(Anchor, start, end);
                }
                Some((start, '*')) => {
                    loop {
                        match self.iter.next_value {
                            Some((_, ch)) if is_whitespace(ch) => break,
                            None => break,
                            _ => {}
                        }
                        let (pos, ch) = self.iter.next().unwrap();
                        if is_flow_indicator(ch) {
                            self.error = Some(Error::tokenizer_error(&pos,
                                "Bad char in alias name".to_string()));
                            break 'tokenloop;
                        }
                    }
                    if self.iter.position.offset - start.offset < 2 {
                        self.error = Some(Error::tokenizer_error(&start,
                            "Alias name requires at least one character"
                            .to_string()));
                        break;
                    }
                    let end = self.iter.position.clone();
                    self.add_token(Alias, start, end);
                }
                Some((start, ',')) => {
                    let end = self.iter.position.clone();
                    self.add_token(FlowEntry, start, end);
                }
                Some((start, '[')) => {
                    let end = self.iter.position.clone();
                    self.add_token(FlowSeqStart, start, end);
                    self.flow_level += 1;
                }
                Some((start, ']')) => {
                    let end = self.iter.position.clone();
                    self.add_token(FlowSeqEnd, start, end);
                    if self.flow_level > 0 {
                        self.flow_level -= 1;
                    }
                }
                Some((start, '{')) => {
                    let end = self.iter.position.clone();
                    self.add_token(FlowMapStart, start, end);
                    self.flow_level += 1;
                }
                Some((start, '}')) => {
                    let end = self.iter.position.clone();
                    self.add_token(FlowMapEnd, start, end);
                    if self.flow_level > 0 {
                        self.flow_level -= 1;
                    }
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
                    let end = self.iter.position.clone();
                    self.add_token(Whitespace, start, end);
                }
                Some((start, _)) => { self.read_plain(start); }
                None => break,
            }
        }
        let pos = self.iter.position.clone();
        if self.indent_levels.len() > 1 {
            for _ in 0..(self.indent_levels.len() - 1) {
                self.result.push(Token {
                    kind: Unindent,
                    start: pos.clone(),
                    end: pos.clone(),
                    value: &self.data[pos.offset..pos.offset],
                    });
            }
        }
        self.result.push(Token {
            kind: Eof,
            start: pos.clone(),
            end: pos.clone(),
            value: &self.data[pos.offset..pos.offset],
            });
        return self.error.clone().or(self.iter.error.clone());
    }
}

pub fn tokenize<'x>(name: Rc<String>, data: &'x str)
    -> Result<Vec<Token<'x>>, Error>
{
    let mut result: Vec<Token<'x>> = Vec::new();
    //let iter = data.char_indices();
    return match Tokenizer::new(&mut result, name, data).tokenize() {
        Some(err) => Err(err),
        None => Ok(result),
    };
}

#[cfg(test)]
pub fn test_tokenize<'x>(data: &'x str)
    -> Result<Vec<Token<'x>>, Error> {
    return tokenize(Rc::new("<inline_test>".to_string()), data);
}

#[cfg(test)]
fn simple_tokens<'x>(res: Result<Vec<Token<'x>>, Error>)
    -> Vec<(TokenType, &'x str)>
{
    match res {
        Ok(vec) => {
            assert_eq!(vec.last().unwrap().kind, Eof);
            return (&vec[0..(vec.len()-1)]).iter().map(
            |ref tok| {
                return (tok.kind, tok.value);
            }).collect();
        }
        Err(value) => {
            panic!("Error: {}", value);
        }
    }
}

#[test]
fn test_tokenize_map() {
    let tokens = test_tokenize("a:  b");
    let strings = simple_tokens(tokens);
    assert_eq!(strings, vec!(
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, "  "),
        (PlainString, "b")));
}

#[test]
fn test_tokenize_map_two_nulls() {
    let tokens = test_tokenize("a:\nb:");
    let strings = simple_tokens(tokens);
    assert_eq!(strings, vec!(
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, "\n"),
        (PlainString, "b"),
        (MappingValue, ":"),
        ));
}

#[test]
fn test_tag_map() {
    let tokens = test_tokenize("a: !Tag\n a:  b");
    let strings = simple_tokens(tokens);
    assert_eq!(strings, vec!(
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, " "),
        (Tag, "!Tag"),
        (Whitespace, "\n "),
        (Indent, ""),
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, "  "),
        (PlainString, "b"),
        (Unindent, ""),
        ));
}

#[test]
fn test_tag_map_flow() {
    let tokens = test_tokenize("a: !Tag { a:  b }");
    let strings = simple_tokens(tokens);
    assert_eq!(strings, vec!(
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, " "),
        (Tag, "!Tag"),
        (Whitespace, " "),
        (FlowMapStart, "{"),
        (Whitespace, " "),
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, "  "),
        (PlainString, "b"),
        (Whitespace, " "),
        (FlowMapEnd, "}"),
        ));
}

#[test]
fn test_map_map() {
    let tokens = test_tokenize("a:\n a:  b");
    let strings = simple_tokens(tokens);
    assert_eq!(strings, vec!(
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, "\n "),
        (Indent, ""),
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, "  "),
        (PlainString, "b"),
        (Unindent, ""),
        ));
}

#[test]
fn test_list() {
    let tokens = test_tokenize("-");
    assert_eq!(simple_tokens(tokens),
        vec!((SequenceEntry, "-")));
    let tokens = test_tokenize("---");
    assert_eq!(simple_tokens(tokens),
        vec!((DocumentStart, "---")));
    let tokens = test_tokenize("- something");
    assert_eq!(simple_tokens(tokens),
        vec!((SequenceEntry, "-"), (Whitespace, " "),
             (Indent, ""), (PlainString, "something"), (Unindent, "")));
    let tokens = test_tokenize("- -");
    assert_eq!(simple_tokens(tokens),
        vec!((SequenceEntry, "-"), (Whitespace, " "),
             (Indent, ""), (SequenceEntry, "-"), (Unindent, "")));
}

#[test]
fn test_list_map() {
    let tokens = test_tokenize("- a: 1\n  b: 2");
    assert_eq!(simple_tokens(tokens),
        vec!((SequenceEntry, "-"), (Whitespace, " "),
             (Indent, ""),
             (PlainString, "a"), (MappingValue, ":"),
             (Whitespace, " "), (PlainString, "1"),
             (Whitespace, "\n  "),
             (PlainString, "b"), (MappingValue, ":"),
             (Whitespace, " "), (PlainString, "2"),
             (Unindent, "")));
}

#[test]
fn test_list_tag() {
    let tokens = test_tokenize("- !Tag a");
    assert_eq!(simple_tokens(tokens),
        vec!((SequenceEntry, "-"), (Whitespace, " "),
             (Indent, ""),
             (Tag, "!Tag"), (Whitespace, " "), (PlainString, "a"),
             (Unindent, "")));
}

#[test]
fn test_map_key() {
    let tokens = test_tokenize("?");
    assert_eq!(simple_tokens(tokens),
        vec!((MappingKey, "?")));
    let tokens = test_tokenize("?something");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "?something")));
    let tokens = test_tokenize("? something");
    assert_eq!(simple_tokens(tokens),
        vec!((MappingKey, "?"), (Whitespace, " "), (PlainString, "something")));
}

#[test]
fn test_map_value() {
    let tokens = test_tokenize(":");
    assert_eq!(simple_tokens(tokens),
        vec!((MappingValue, ":")));
    let tokens = test_tokenize(":something");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, ":something")));
    let tokens = test_tokenize(": something");
    assert_eq!(simple_tokens(tokens),
        vec!((MappingValue, ":"), (Whitespace, " "),
             (PlainString, "something")));
}

#[test]
fn test_plain1() {
    let tokens = test_tokenize("a");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "a")));
}

#[test]
fn test_plain2() {
    let tokens = test_tokenize("abc");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "abc")));
}

#[test]
fn test_plain3() {
    let tokens = test_tokenize("abc\ndef");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "abc\ndef")));
}

#[test]
fn test_plain4() {
    let tokens = test_tokenize("a#bc");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "a#bc")));
}

#[test]
fn test_plain5() {
    let tokens = test_tokenize(" a\nbc");
    assert_eq!(simple_tokens(tokens),
        vec!((Whitespace, " "), (Indent, ""), (PlainString, "a\nbc"),
             (Unindent, "")));
}

#[test]
fn test_plain6() {
    let tokens = test_tokenize("a:\n a\n bc");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "a"), (MappingValue, ":"), (Whitespace, "\n "),
             (Indent, ""), (PlainString, "a\n bc"), (Unindent, "")));
}

#[test]
fn test_plain7() {
    let tokens = test_tokenize("a: a\nbc");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "a"), (MappingValue, ":"), (Whitespace, " "),
             (PlainString, "a"), (Whitespace, "\n"), (PlainString, "bc")));
}

#[test]
fn test_plain8() {
    let tokens = test_tokenize("b:\n 1\n 2\nc: end");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "b"), (MappingValue, ":"), (Whitespace, "\n "),
             (Indent, ""), (PlainString, "1\n 2"), (Whitespace, "\n"),
             (Unindent, ""), (PlainString, "c"), (MappingValue, ":"),
             (Whitespace, " "), (PlainString, "end"),
             ));
}

#[test]
fn test_plain_words() {
    let tokens = test_tokenize("a: a b");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "a"), (MappingValue, ":"),
             (Whitespace, " "), (PlainString, "a b")));
}

#[test]
fn test_words_in_list() {
    let tokens = test_tokenize("- a\n b");
    assert_eq!(simple_tokens(tokens),
        vec!((SequenceEntry, "-"), (Whitespace, " "),
             (Indent, ""), (PlainString, "a\n b"), (Unindent, "")));
}

#[test]
fn test_block() {
    let tokens = test_tokenize("|\n a");
    assert_eq!(simple_tokens(tokens),
        vec!((Literal, "|\n a")));
}

#[test]
fn test_block_flow() {
    let tokens = test_tokenize("a: >\n b\nc");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "a"), (MappingValue, ":"), (Whitespace, " "),
             (Folded, ">\n b"), (Whitespace, "\n"), (PlainString, "c")));
}

#[test]
fn test_block_in_list() {
    let tokens = test_tokenize("- |\n  a");
    assert_eq!(simple_tokens(tokens),
        vec!((SequenceEntry, "-"), (Whitespace, " "), (Indent, ""),
             (Literal, "|\n  a"), (Unindent, "")));
}

#[test]
fn test_directive() {
    let tokens = test_tokenize("%");
    assert_eq!(simple_tokens(tokens),
        vec!((Directive, "%")));
    let tokens = test_tokenize("%something\n");
    assert_eq!(simple_tokens(tokens),
        vec!((Directive, "%something\n")));
    let tokens = test_tokenize("%abc\ndef");
    assert_eq!(simple_tokens(tokens),
        vec!((Directive, "%abc\n"), (PlainString, "def")));
    let tokens = test_tokenize("a%bc");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "a%bc")));
    let err = test_tokenize(" %bc").err().unwrap();
    // TODO(pc) add testcase with percent sign at start of token
    assert_eq!(&format!("{}", err)[..], "<inline_test>:1:2: \
        Tokenizer Error: Directive must start at start of line");
}

#[test]
fn test_reserved() {
    let err = test_tokenize("@").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:1: \
        Tokenizer Error: Characters '@' and '`' are not allowed");
    let err = test_tokenize("a:\n  @").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:2:3: \
        Tokenizer Error: Characters '@' and '`' are not allowed");
    let tokens = test_tokenize("a@");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "a@")));
    let tokens = test_tokenize("a\n@");
    assert_eq!(simple_tokens(tokens),
        vec!((PlainString, "a\n@")));
}

#[test]
fn test_bad_char_ctl() {
    let err = test_tokenize("\x01").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:1: \
        Tokenizer Error: Unacceptable character");
}
#[test]
fn test_bad_char_tab() {
    let err = test_tokenize("\t").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:1: \
        Tokenizer Error: Tab character may appear only in quoted string");
}
#[test]
fn test_bad_char_tab2() {
    let err = test_tokenize("a:\n  \tbc").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:2:3: \
        Tokenizer Error: Tab character may appear only in quoted string");
}
#[test]
fn test_bad_char_tab3() {
    let err = test_tokenize("a\n\tb").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:2:1: \
        Tokenizer Error: Tab character may appear only in quoted string");
}
#[test]
fn test_bad_char_tab4() {
    let err = test_tokenize("a\tb").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:2: \
        Tokenizer Error: Tab character may appear only in quoted string");
}

#[test]
fn test_double_quoted() {
    let tokens = test_tokenize("\"\"");
    assert_eq!(simple_tokens(tokens),
        vec!((DoubleString, "\"\"")));
    let tokens = test_tokenize("\"a\nb\"");
    assert_eq!(simple_tokens(tokens),
        vec!((DoubleString, "\"a\nb\"")));
    let tokens = test_tokenize("\"a\\\"\nb\"");
    assert_eq!(simple_tokens(tokens),
        vec!((DoubleString, "\"a\\\"\nb\"")));
    let err = test_tokenize("val: \"value\nof").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:6: \
        Tokenizer Error: Unclosed double-quoted string");
}

#[test]
fn test_single_quoted() {
    let tokens = test_tokenize("''");
    assert_eq!(simple_tokens(tokens),
        vec!((SingleString, "''")));
    let tokens = test_tokenize("'a\nb'");
    assert_eq!(simple_tokens(tokens),
        vec!((SingleString, "'a\nb'")));
    let tokens = test_tokenize("'a\\': 'b'");
    assert_eq!(simple_tokens(tokens),
        vec!((SingleString, "'a\\'"), (MappingValue, ":"),
             (Whitespace, " "), (SingleString, "'b'")));
    let err = test_tokenize("val: 'value\nof").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:6: \
        Tokenizer Error: Unclosed quoted string");
}

#[test]
fn test_comment() {
    assert_eq!(simple_tokens(test_tokenize("#")),
        vec!((Comment, "#")));
    assert_eq!(simple_tokens(test_tokenize("#a")),
        vec!((Comment, "#a")));
    assert_eq!(simple_tokens(test_tokenize("#a\nb")),
        vec!((Comment, "#a\n"), (PlainString, "b")));
    assert_eq!(simple_tokens(test_tokenize("a #b\nc")),
        vec!((PlainString, "a"), (Whitespace, " "),
          (Comment, "#b\n"), (PlainString, "c")));
    assert_eq!(simple_tokens(test_tokenize("  #a\nb")),
        vec!((Whitespace, "  "), (Comment, "#a\n"), (PlainString, "b")));
}

#[test]
fn test_tag() {
    assert_eq!(simple_tokens(test_tokenize("!")),
        vec!((Tag, "!")));
    assert_eq!(simple_tokens(test_tokenize("!a b")),
        vec!((Tag, "!a"), (Whitespace, " "), (PlainString, "b")));
    let err = test_tokenize("!a[]").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:3: \
        Tokenizer Error: Bad char in tag name");
}

#[test]
fn test_anchor() {
    assert_eq!(simple_tokens(test_tokenize("&abc")),
        vec!((Anchor, "&abc")));
    assert_eq!(simple_tokens(test_tokenize("&a b")),
        vec!((Anchor, "&a"), (Whitespace, " "), (PlainString, "b")));
    let err = test_tokenize("&a[]").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:3: \
        Tokenizer Error: Bad char in anchor name");
    let err = test_tokenize("&").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:1: \
        Tokenizer Error: Anchor name requires at least one character");
}

#[test]
fn test_alias() {
    assert_eq!(simple_tokens(test_tokenize("*abc")),
        vec!((Alias, "*abc")));
    assert_eq!(simple_tokens(test_tokenize("*a b")),
        vec!((Alias, "*a"), (Whitespace, " "), (PlainString, "b")));
    let err = test_tokenize("*a[]").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:3: \
        Tokenizer Error: Bad char in alias name");
    let err = test_tokenize("*").err().unwrap();
    assert_eq!(&format!("{}", err), "<inline_test>:1:1: \
        Tokenizer Error: Alias name requires at least one character");
}

#[test]
fn test_nested() {
    assert_eq!(simple_tokens(test_tokenize("a:\n b:\n  c:\nd:")),
        vec!((PlainString, "a"), (MappingValue, ":"), (Whitespace, "\n "),
             (Indent, ""),
             (PlainString, "b"), (MappingValue, ":"), (Whitespace, "\n  "),
             (Indent, ""),
             (PlainString, "c"), (MappingValue, ":"), (Whitespace, "\n"),
             (Unindent, ""), (Unindent, ""),
             (PlainString, "d"), (MappingValue, ":")));
}

#[test]
fn test_flow_list() {
    assert_eq!(simple_tokens(test_tokenize("[a, b]")),
        vec!((FlowSeqStart, "["),
             (PlainString, "a"),
             (FlowEntry, ","),
             (Whitespace, " "),
             (PlainString, "b"),
             (FlowSeqEnd, "]")));
}

#[test]
fn test_flow_map_json() {
    assert_eq!(simple_tokens(test_tokenize(r#"{"a":1}"#)),
        vec!((FlowMapStart, "{"),
             (DoubleString, r#""a""#),
             (MappingValue, ":"),
             (PlainString, "1"),
             (FlowMapEnd, "}")));
}

#[test]
fn test_flow_map_map() {
    assert_eq!(simple_tokens(test_tokenize(r#"{a:{}}"#)),
        vec!((FlowMapStart, "{"),
             (PlainString, "a"),
             (MappingValue, ":"),
             (FlowMapStart, "{"),
             (FlowMapEnd, "}"),
             (FlowMapEnd, "}")));
}

#[test]
fn test_plain_scalar_braces() {
    assert_eq!(simple_tokens(test_tokenize(r#"a:{}"#)),
        vec!((PlainString, "a:{}")));
}
