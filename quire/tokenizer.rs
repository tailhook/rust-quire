use std::vec::Vec;
use std::str::CharOffsets;

use chars::is_indicator;
use chars::is_whitespace;

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
    line_offset: uint,
    offset: uint,
}

struct Token<'tok> {
    kind: TokenType,
    start: Pos,
    end: Pos,
    value: &'tok str,
}

struct Tokenizer<'a, 'b> {
    position: Pos,
    result: &'a mut Vec<Token<'b>>,
    data: &'b str,
    iter: CharOffsets<'b>,
    cur: Option<char>,
}

impl<'a, 'b> Tokenizer<'a, 'b> {

    fn new<'x, 'y>(result: &'x mut Vec<Token<'y>>, data: &'y str)
        -> Tokenizer<'x, 'y>
    {
        return Tokenizer {
            result: result,
            data: data,
            iter: data.char_indices(),
            cur: None,
            position: Pos {
                indent: 0,
                offset: 0,
                line: 1,
                line_offset: 0,
                },
        }
    }

    fn read_plain(&mut self, start: Pos) {
        loop {
            let prev = self.cur;
            match self.next() {
                Some(':') => {
                    // may end plainstring if followed by WS
                    }
                Some('\n') | Some('\r') => {
                    // may end plainstring if next block is not indented
                    // as much
                    if start.indent == 0 {
                        continue;
                    }
                    let possible_end = self.position;
                    loop {
                        match self.next() {
                            Some(' ') => continue,
                            Some(_) | None => break,
                        }
                    }
                    if self.position.indent <= start.indent {
                        self.add_token(PlainString, start, possible_end);
                        self.add_token(Indent, possible_end, self.position);
                        return;
                    }
                }
                Some('#') => {
                    // ends plainstring only if follows space
                    match prev {
                        Some(' ') | Some('\t') => break,
                        _ => continue,
                    }
                }
                Some(_) => continue,
                None => break,
            }
        }
        self.add_token(PlainString, start, self.position);
    }

    fn add_token(&mut self, kind: TokenType, start: Pos, end: Pos) {
        self.result.push(Token {
            kind: kind,
            start: start,
            end: end,
            value: self.data.slice(start.offset, end.offset),
            });
    }

    #[inline]
    fn next(&mut self) -> Option<char> {
        let next = match self.iter.next() {
            None => {
                return None;
            }
            Some((index, next)) => {
                self.position.offset = self.data.len();
                next
            }
        };
        let pos = &mut self.position;
        match self.cur {
            Some('\n') => {
                pos.line += 1;
                pos.line_offset = 0;
                pos.indent = 0;
            }
            Some(' ') if pos.indent+1 == pos.line_offset => {
                pos.indent += 1;
            }
            Some('\t') if pos.indent+1 == pos.line_offset => {
                // TODO: Emit error somehow
            }
            Some(_) | None => {}
        }
        pos.line_offset += 1;
        self.cur = Some(next);
        return self.cur;
    }

    fn tokenize(&mut self) {
        loop {
            let start = self.position;
            match self.next() {
                Some('-') => { // list element, doc start, plainstring
                    match self.next() {
                        Some('-') => { // maybe document end
                            match self.next() {
                                Some('-') => self.add_token(DocumentStart,
                                    start, self.position),
                                _ => self.read_plain(start),
                            }
                        }
                        Some(_) => {
                            self.read_plain(start);
                        }
                        None => { // list element at end of stream
                            self.add_token(SequenceEntry,
                                start, self.position);
                            return;
                            }
                        };
                    }
                Some(' ') | Some('\t') => {
                    // Just skipping it for now
                }
                Some(_) => { self.read_plain(start); }
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
    let tokens = tokenize("a: b");
    let strings = simple_tokens(&tokens);
    assert_eq!(strings, ~[
        (PlainString, "a"),
        (MappingValue, ":"),
        (Whitespace, " "),
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
        ~[(PlainString, "a"), (PlainString, "bc")]);
}
