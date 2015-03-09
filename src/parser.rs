use std::iter::Peekable;
use std::str::Chars;
use std::rc::Rc;
use std::fmt::Debug;
use std::fmt::Error as FormatError;
use std::fmt::{Formatter};
use std::cmp::{Ordering};
use std::cmp::Ordering::{Less, Equal, Greater};
use std::collections::BTreeMap;

use super::tokenizer::{Token, Pos};
use super::errors::Error;
use super::tokenizer::tokenize;
use super::tokenizer::TokenType as T;
use self::Node::*;

type Aliases<'x> = BTreeMap<&'x str, &'x Node<'x>>;

pub struct TokenIter<'a> {
    index: usize,
    tokens: &'a[Token<'a>],
    eof_token: &'a Token<'a>,
}

impl<'a> TokenIter<'a> {
    fn new<'x>(src: &'x Vec<Token<'x>>) -> TokenIter<'x> {
        let last = src.last().expect("Non-empty document expected");
        assert!(last.kind == T::Eof);
        return TokenIter {
            index: 0,
            tokens: src.as_slice(),
            eof_token: last,
        };
    }
    fn peek(&mut self, index: usize) -> &'a Token<'a> {
        let mut num = index;
        for idx in self.index..self.tokens.len() {
            let tok = self.tokens.get(idx).unwrap();
            match tok.kind {
                T::Whitespace | T::Comment => continue,
                T::Eof => return self.eof_token,
                _ => {
                    if num == 0 {
                        return tok;
                    }
                    num -= 1;
                }
            }
        }
        return self.eof_token;
    }

    fn next(&mut self) -> Option<&'a Token<'a>> {
        loop {
            let tok = match self.tokens.get(self.index) {
                None => return None,
                Some(tok) => tok,
            };
            self.index += 1;
            match tok.kind {
                T::Whitespace | T::Comment => continue,
                T::Eof => return None,
                _ => return Some(tok),
            }
        }
    }
}

fn process_newline<'x>(iter: &mut Peekable<Chars<'x>>, res: &mut String,
    cut_limit: usize)
{
    while res.len() > cut_limit {
        match res.pop() {
            None => break,
            Some(' ') | Some('\t') => continue,
            Some(x) => {
                res.push(x);
                break;
            }
        }
    }
    let mut need_space = res.len() > cut_limit;
    loop {
        match iter.peek() {
            Some(&' ') => { }
            Some(&'\n') => {
                need_space = false;
                res.push('\n');
            }
            Some(_) => break,
            None => break,
        };
        iter.next();
    }
    if need_space {
        res.push(' ');
    }
}

fn plain_value<'a>(tok: &Token<'a>) -> String {
    let mut res = String::with_capacity(tok.value.len());
    match tok.kind {
        T::PlainString => {
            let mut iter = tok.value.chars().peekable();
            loop {
                let ch = match iter.next() {
                    None => break,
                    Some(ch) => ch,
                };
                if ch == '\n' {
                    process_newline(&mut iter, &mut res, 0);
                } else {
                    res.push(ch);
                }
            }
        }
        T::DoubleString => {
            let mut escaped_space = 0;
            let mut iter = tok.value.chars().peekable();
            assert_eq!(iter.next(), Some('"'));
            loop {
                match iter.next() {
                    None => break,
                    Some('\\') => {
                        match iter.next() {
                            None => res.push('\\'),  // fixme
                            Some('0') => res.push('\0'),
                            Some('a') => res.push('\x07'),
                            Some('b') => res.push('\x08'),
                            Some('t') | Some('\t') => res.push('\t'),
                            Some('n') => res.push('\n'),
                            Some('r') => res.push('\r'),
                            Some('v') => res.push('\x0b'),
                            Some('f') => res.push('\x0c'),
                            Some('e') => res.push('\x1b'),
                            Some(' ') => {
                                res.push(' ');
                            }
                            Some('"') => res.push('"'),
                            Some('/') => res.push('/'),
                            Some('\\') => res.push('\\'),
                            Some('N') => res.push('\u{85}'),
                            Some('_') => res.push('\u{a0}'),
                            Some('L') => res.push('\u{2028}'),
                            Some('P') => res.push('\u{2029}'),
                            Some('x') => {
                                unimplemented!();
                            },
                            Some('u') => {
                                unimplemented!();
                            },
                            Some('U') => {
                                unimplemented!();
                            },
                            Some('\n') => {
                                escaped_space = res.len();
                                process_newline(&mut iter, &mut res,
                                    escaped_space);
                            }
                            Some(x) => {
                                res.push('\\');
                                res.push(x);
                            }
                        }
                        escaped_space = res.len();
                    }
                    Some('"') => break,
                    Some('\n') => {
                        process_newline(&mut iter, &mut res,
                            escaped_space);
                    }
                    Some(x) => res.push(x),
                }
            }
        },
        T::SingleString => {
            let mut iter = tok.value.chars().peekable();
            assert_eq!(iter.next(), Some('\''));
            loop {
                match iter.next() {
                    None => break,
                    Some('\'') => {
                        match iter.next() {
                            None => break,
                            Some('\'') => res.push('\''),
                            Some(_) => unreachable!(),
                        }
                    }
                    Some('\n') => {
                        process_newline(&mut iter, &mut res, 0);
                    }
                    Some(x) => res.push(x),
                }
            }
        }
        T::Literal => {
            let mut lines = tok.value.split('\n');
            let fline = lines.next().unwrap();
            if fline == "|" {
                let mut indent = 0;
                for line in lines {
                    if indent == 0 {
                        let trimmed = line.trim_left_matches(' ');
                        indent = line.len() - trimmed.len();
                    }
                    res.push_str(line.slice(indent, line.len()));
                    res.push('\n');
                }
            } else {
                unimplemented!();
            }
        }
        T::Folded => {
            unimplemented!();
        }
        _ => unreachable!(),
    }
    return res;
}

pub struct Directive<'a>(&'a Token<'a>);

pub struct Document<'a> {
    pub directives: Vec<Directive<'a>>,
    pub root: Node<'a>,
    pub aliases: BTreeMap<&'a str, &'a Node<'a>>,
}

pub enum Node<'a> {
    Map(Option<&'a str>, Option<&'a str>,
        BTreeMap<Node<'a>, Node<'a>>, &'a[Token<'a>]),
    List(Option<&'a str>, Option<&'a str>, Vec<Node<'a>>, &'a[Token<'a>]),
    Scalar(Option<&'a str>, Option<&'a str>, String, &'a Token<'a>),
    // Explicit null is a Scalar at this state of parsing
    ImplicitNull(Option<&'a str>, Option<&'a str>, Pos),
    Alias(&'a str, &'a Token<'a>),
}

fn _compare(a: &Node, b: &Node) -> Ordering {
    match (a, b) {
        (&Scalar(_, _, ref a, _), &Scalar(_, _, ref b, _))
        => a.cmp(b),
        (&ImplicitNull(_, _, _), &Scalar(_, _, _, _)) => Less,
        (&Scalar(_, _, _, _), &ImplicitNull(_, _, _)) => Greater,
        (&ImplicitNull(_, _, _), &ImplicitNull(_, _, _)) => Equal,
        _ => unimplemented!(),
    }
}

impl<'a> Ord for Node<'a> {
    fn cmp(&self, other: &Node) -> Ordering {
        return _compare(self, other);
    }
}

impl<'a> PartialOrd for Node<'a> {
    fn partial_cmp<'x>(&self, other: &Node<'x>) -> Option<Ordering> {
        return Some(_compare(self, other));
    }
}

impl<'a> Eq for Node<'a> {}
impl<'a> PartialEq for Node<'a> {
    fn eq(&self, other: &Node) -> bool {
        return match (self, other) {
            (&Scalar(_, _, ref a, _), &Scalar(_, _, ref b, _)) => a == b,
            (&ImplicitNull(_, _, _), &Scalar(_, _, _, _)) => false,
            (&Scalar(_, _, _, _), &ImplicitNull(_, _, _)) => false,
            (&ImplicitNull(_, _, _), &ImplicitNull(_, _, _)) => true,
            _ => unimplemented!(),
        }
    }
}

impl<'a> Debug for Node<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FormatError> {
        match self {
            &Scalar(_, _, ref a, _) => write!(fmt, "<Scalar {}>", a),
            &ImplicitNull(_, _, _) => write!(fmt, "<Null>"),
            _ => unimplemented!(),
        }
    }
}


fn parse_list<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases,
    tag: Option<&'x str>)
    -> Result<Node<'x>, Error>
{
    let begin = tokiter.index;
    let mut children = Vec::new();
    loop {
        let marker = tokiter.peek(0);
        match marker.kind {
            T::SequenceEntry => {}
            _ => break,
        };
        tokiter.next().unwrap();
        let tok = tokiter.peek(0);
        match tok.kind {
            T::SequenceEntry if tok.start.indent == marker.start.indent => {
                children.push(ImplicitNull(None, None, marker.end.clone()));
            }
            T::Eof | T::Unindent => {
                children.push(ImplicitNull(None, None, tok.start.clone()));
                break;
            }
            _ => {
                let value = match parse_node(tokiter, aliases) {
                    Ok(value) => value,
                    Err(err) => return Err(err),
                };
                children.push(value);
            }
        }
    }
    return Ok(List(tag, None, children,
        tokiter.tokens.slice(begin, tokiter.index)));
}

fn parse_map<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases,
    tag: Option<&'x str>)
    -> Result<Node<'x>, Error>
{
    let begin = tokiter.index;
    let mut children = BTreeMap::new();
    loop {
        // TODO(tailhook) implement complex keys
        // TODO(tailhook) implement aliases and anchors
        let ktoken = tokiter.peek(0);
        let key = match ktoken.kind {
            T::Eof => break,
            T::Unindent => break,
            T::PlainString | T::SingleString | T::DoubleString
            => Scalar(None, None, plain_value(ktoken), ktoken),
            _ => return Err(Error::parse_error(&ktoken.start,
                format!("Expected mapping key or unindent, got {:?}",
                        ktoken.kind))),
        };
        tokiter.next().unwrap();
        let delim = tokiter.peek(0);
        match delim.kind {
            T::MappingValue => {}
            T::Eof => return Err(Error::parse_error(&delim.start,
                "Unexpected end of file, expected mapping value".to_string())),
            _ => return Err(Error::parse_error(&delim.start,
                "Expected colon `:` which denotes mapping value".to_string())),
        }
        tokiter.next().unwrap();
        let tok = tokiter.peek(0);
        match tok.kind {
            T::SequenceEntry if tok.start.line > delim.end.line => {
                // Allow sequences on the same indentation level as a key in
                // mapping
                let tag = maybe_parse_tag(tokiter);
                let value = match parse_list(tokiter, aliases, tag) {
                    Ok(value) => value,
                    Err(err) => return Err(err),
                };
                if children.insert(key, value).is_some() {
                    return Err(Error::parse_error(&ktoken.start,
                        "Duplicate key".to_string()));
                }
            }
            T::Eof | T::Unindent => {
                children.insert(key, ImplicitNull(
                    None, None, tok.start.clone()));
                break;
            }
            _ if tok.start.line == delim.end.line ||
                 tok.start.indent > delim.end.indent => {
                let value = match parse_node(tokiter, aliases) {
                    Ok(value) => value,
                    Err(err) => return Err(err),
                };
                if children.insert(key, value).is_some() {
                    return Err(Error::parse_error(&ktoken.start,
                        "Duplicate key".to_string()));
                }
            }
            _ => {
                let value = ImplicitNull(None, None, delim.end.clone());
                if children.insert(key, value).is_some() {
                    return Err(Error::parse_error(&ktoken.start,
                        "Duplicate key".to_string()));
                }
                continue;
            }
        }
    }
    return Ok(Map(tag, None, children,
        tokiter.tokens.slice(begin, tokiter.index)));
}

fn parse_flow_list<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases,
    tag: Option<&'x str>)
    -> Result<Node<'x>, Error>
{
    let begin = tokiter.index;
    let mut children = Vec::new();
    tokiter.next();
    loop {
        let tok = tokiter.peek(0);
        match tok.kind {
            T::FlowSeqEnd => {
                tokiter.next();
                break;
            }
            _ => {
                let value = match parse_flow_node(tokiter, aliases) {
                    Ok(value) => value,
                    Err(err) => return Err(err),
                };
                children.push(value);
            }
        }
        let tok = tokiter.next().unwrap();
        match tok.kind {
            T::FlowSeqEnd => break,
            T::FlowEntry => continue,
            _ => return Err(Error::parse_error(&tok.start,
                "Expected comma `,` or colon `:`".to_string())),
        }
    }
    return Ok(List(tag, None, children,
        tokiter.tokens.slice(begin, tokiter.index)));
}

fn parse_flow_map<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases,
    tag: Option<&'x str>)
    -> Result<Node<'x>, Error>
{
    let begin = tokiter.index;
    let mut children = BTreeMap::new();
    tokiter.next();
    loop {
        // TODO(tailhook) implement complex keys
        // TODO(tailhook) implement aliases and anchors
        let ktoken = tokiter.next().unwrap();
        let key = match ktoken.kind {
            T::FlowMapEnd => break,
            T::PlainString | T::SingleString | T::DoubleString
            => Scalar(None, None, plain_value(ktoken), ktoken),
            _ => return Err(Error::parse_error(&ktoken.start,
                "Expected next mapping key or or closing bracket `}`"
                .to_string())),
        };

        let tok = tokiter.next().unwrap();
        match tok.kind {
            T::FlowMapEnd => {
                // Value is null
                if children.insert(key,
                    ImplicitNull(None, None, tok.start.clone())).is_some() {
                    return Err(Error::parse_error(&ktoken.start,
                        "Duplicate key".to_string()));
                }
                break;
            }
            T::FlowEntry => {
                // Value is null
                if children.insert(key,
                    ImplicitNull(None, None, tok.start.clone())).is_some() {
                    return Err(Error::parse_error(&ktoken.start,
                        "Duplicate key".to_string()));
                }
                continue;
            }
            T::MappingValue => {}
            _ => return Err(Error::parse_error(&tok.start,
                "Expected comma `,`, colon `:` or closing bracket `}`"
                .to_string())),
        }
        let value = match parse_flow_node(tokiter, aliases) {
            Ok(value) => value,
            Err(err) => return Err(err),
        };
        if children.insert(key, value).is_some() {
            return Err(Error::parse_error(&ktoken.start,
                "Duplicate key".to_string()));
        }
        let tok = tokiter.next().unwrap();
        match tok.kind {
            T::FlowMapEnd => break,
            T::FlowEntry => continue,
            _ => return Err(Error::parse_error(&tok.start,
                "Expected comma `,` or closing bracket `}`".to_string())),
        }
    }
    return Ok(Map(tag, None, children,
        tokiter.tokens.slice(begin, tokiter.index)));
}

fn parse_flow_node<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<Node<'x>, Error>
{
    let tag = maybe_parse_tag(tokiter);
    let tok = tokiter.peek(0);
    match tok.kind {
        T::PlainString | T::SingleString | T::DoubleString => {
            tokiter.next();
            return Ok(Scalar(None, None, plain_value(tok), tok));
        }
        T::FlowSeqStart => {
            return parse_flow_list(tokiter, aliases, tag);
        }
        T::FlowMapStart => {
            return parse_flow_map(tokiter, aliases, tag);
        }
        _ => return Err(Error::parse_error(&tok.start,
            "Expected plain string, sequence or mapping".to_string())),
    };
}

fn maybe_parse_tag<'x>(tokiter: &mut TokenIter<'x>) -> Option<&'x str> {
    let tok = tokiter.peek(0);
    let mut tag = None;
    match tok.kind {
        T::Tag => {
            tag = Some(tok.value);
            tokiter.next();
        }
        _ => {}
    }
    return tag;
}

fn parse_node<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<Node<'x>, Error>
{
    let mut indent = false;
    let mut tok = tokiter.peek(0);

    if tok.kind == T::Indent {  // Indent in list is before tag
        tokiter.next();
        tok = tokiter.peek(0);
        indent = true;
    }
    let mut tag = maybe_parse_tag(tokiter);
    tok = tokiter.peek(0);
    if !indent && tok.kind == T::Indent {  // Otherwise indent is after tag
        tokiter.next();
        tok = tokiter.peek(0);
        indent = true;
    }
    let result = match tok.kind {
        T::PlainString | T::SingleString | T::DoubleString
        | T::Literal | T::Folded => {
            if tok.start.line == tok.end.line {
                // Only one-line scalars are allowed to be mapping keys
                let val = tokiter.peek(1);
                if val.kind == T::MappingValue &&
                        val.start.line == tok.end.line
                {
                    parse_map(tokiter, aliases, tag)
                } else {
                    tokiter.next();
                    Ok(Scalar(tag, None, plain_value(tok), tok))
                }
            } else {
                tokiter.next();
                Ok(Scalar(tag, None, plain_value(tok), tok))
            }
        }
        T::Eof | T::Unindent => {
            Ok(ImplicitNull(tag, None, tok.start.clone()))
        }
        T::SequenceEntry => {
            parse_list(tokiter, aliases, tag)
        }
        T::FlowSeqStart => {
            parse_flow_list(tokiter, aliases, tag)
        }
        T::FlowMapStart => {
            parse_flow_map(tokiter, aliases, tag)
        }
        _ => Err(Error::parse_error(&tok.start,
            format!("Expected scalar, sequence or mapping, got {:?}",
                    tok.kind))),
    };
    if result.is_ok() && indent {
        let end = tokiter.peek(0);
        if end.kind != T::Unindent {
            return Err(Error::parse_error(&end.start,
                format!("Expected unindent, got {:?}", end.kind)));
        } else {
            tokiter.next();
        }
    }
    return result;
}

fn parse_root<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<(Vec<Directive<'x>>, Node<'x>), Error>
{
    let mut directives = Vec::new();
    loop {
        let tok = tokiter.peek(0);
        match tok.kind {
            T::Eof => return Ok((directives,
                                 ImplicitNull(None, None, tok.start.clone()))),
            T::Directive => directives.push(Directive(tok)),
            T::DocumentStart => break,
            _ => break,  // Start reading node
        }
        tokiter.next();
    }
    let res =  match parse_node(tokiter, aliases) {
        Ok(node) => node,
        Err(e) => return Err(e),
    };
    loop {
        let tok = match tokiter.next() {
            Some(tok) => tok,
            None => break,
        };
        match tok.kind {
            T::DocumentEnd => {}
            _ => {
                return Err(Error::parse_error(&tok.start,
                    format!("Expected document end, got {:?}", tok.kind)));
            }
        }
    }
    return Ok((directives, res));
}


pub fn parse_tokens<'x>(tokens: &'x Vec<Token<'x>>)
    -> Result<Document<'x>, Error>
{
    let mut aliases = BTreeMap::new();
    let mut iter = TokenIter::new(tokens);
    let (directives, root) = match parse_root(&mut iter, &mut aliases) {
        Ok((directives, root)) => (directives, root),
        Err(e) => return Err(e),
    };
    return Ok(Document {
        directives: directives,
        root: root,
        aliases: aliases,
        });
}

pub fn parse<'x, T, F>(name: Rc<String>, data: &str, process: F)
    -> Result<T, Error>
    where F: FnOnce(Document) -> T
{
    let tokens = match tokenize(name, data) {
        Ok(lst) => lst,
        Err(e) => return Err(e),
    };
    let doc = match parse_tokens(&tokens) {
        Ok(doc) => doc,
        Err(e) => return Err(e),
    };
    return Ok(process(doc));
}

