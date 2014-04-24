use std::vec::Vec;

use chars::is_indicator;

mod chars;

enum TokenType {
    DocumentStart,
    DocumentEnd,
    Indent,
    WhiteSpace,
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

struct Pos<'pos> {
    filename: &'pos str,
    start: uint,
    end: uint,
}

struct Token<'tok> {
    kind: TokenType,
    filename: &'tok str,
    indent: uint,
    start: Pos<'tok>,
    end: Pos<'tok>,
    value: &'tok str,
}

fn tokenize(data: ~str) -> Vec<Token> {
    let result: Vec<Token> = Vec::new();
    for ch in data.chars() {
        if is_indicator(ch) {
        }
    };
    return result;
}

#[cfg(test)]
fn tokens_to_stringlist<'a>(vec: &'a Vec<Token>) -> ~[&'a str] {
    return vec.iter().map(|&tok| {
        return tok.value;
    }).collect();
}

#[test]
fn test_tokenize() {
    let tokens = tokenize(~"a: b");
    let strings = tokens_to_stringlist(&tokens);
    assert_eq!(strings, ~["a", ":", " ", "b"]);
}
