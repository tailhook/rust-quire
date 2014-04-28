use std::vec::Vec;
use std::str::CharOffsets;

use chars::is_indicator;

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
    //filename: &'pos str,
    line: uint,
    line_offset: uint,
    offset: uint,
}

struct Token<'tok> {
    kind: TokenType,
    //filename: &'tok str,
    indent: uint,
    start: Pos,
    end: Pos,
    value: &'tok str,
}

struct Tokenizer<'a, 'b> {
    position: Pos,
    result: &'a mut Vec<Token<'b>>,
    data: &'b str,
    iter: CharOffsets<'b>,
    cur: char,
}

impl<'a, 'b> Tokenizer<'a, 'b> {

    fn new<'x, 'y>(result: &'x mut Vec<Token<'y>>, data: &'y str)
        -> Tokenizer<'x, 'y>
    {
        return Tokenizer {
            result: result,
            data: data,
            iter: data.char_indices(),
            cur: '\0',
            position: Pos {
                offset: 0,
                line: 0,
                line_offset: 0,
                },
        }
    }

    fn read_plain(&mut self, start: Pos) {

    }

    fn add_token(&mut self, kind: TokenType, start: Pos, end: Pos) {
        self.result.push(Token {
            kind: kind,
            indent: 0,
            start: start,
            end: end,
            value: self.data.slice(start.offset, end.offset),
            });
    }

    #[inline]
    fn next(&mut self) -> Option<char> {
        let ch: char;
        match self.iter.next() {
            None => {
                return None;
            }
            Some((index1, char1)) => {
                self.position.offset += 1;
                self.cur = char1;
                return Some(char1);
            }
        }
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
