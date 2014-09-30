use std::slice::Items;
use std::iter::Peekable;
use std::str::Chars;
use std::rc::Rc;
use std::fmt::{Show, Formatter, FormatError};

use collections::treemap::TreeMap;
use collections::ringbuf::RingBuf;
use collections::Deque;

use super::tokenizer::{Token, Pos};
use super::errors::Error;
use super::errors::TokenError;
use super::errors::ParserError;
use super::tokenizer::tokenize;
use T = super::tokenizer;

type Aliases<'x> = TreeMap<&'x str, &'x Node<'x>>;

pub struct TokenIter<'a> {
    index: uint,
    peeked: RingBuf<&'a Token<'a>>,
    tokens: &'a[Token<'a>],
    eof_token: &'a Token<'a>,
}

impl<'a> TokenIter<'a> {
    fn new<'x>(src: &'x Vec<Token<'x>>) -> TokenIter<'x> {
        let last = src.last().expect("Non-empty document expected");
        assert!(last.kind == T::Eof);
        return TokenIter {
            index: 0,
            peeked: RingBuf::new(),
            tokens: src.as_slice(),
            eof_token: last,
        };
    }
    fn peek(&mut self, index: uint) -> &'a Token<'a> {
        let mut num = index;
        for idx in range(self.index, self.tokens.len()) {
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

fn process_newline<'x>(iter: &mut Peekable<char, Chars<'x>>, res: &mut String,
    cut_limit: uint)
{
    while res.len() > cut_limit {
        match res.pop_char() {
            None => break,
            Some(' ') | Some('\t') => continue,
            Some(x) => {
                res.push_char(x);
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
                res.push_char('\n');
            }
            Some(_) => break,
            None => break,
        };
        iter.next();
    }
    if need_space {
        res.push_char(' ');
    }
}

impl<'a> T::Token<'a> {
    fn plain_value(&self) -> String {
        let mut res = String::with_capacity(self.value.len());
        match self.kind {
            T::PlainString => {
                let mut iter = self.value.chars().peekable();
                loop {
                    let ch = match iter.next() {
                        None => break,
                        Some(ch) => ch,
                    };
                    if ch == '\n' {
                        process_newline(&mut iter, &mut res, 0);
                    } else {
                        res.push_char(ch);
                    }
                }
            }
            T::DoubleString => {
                let mut escaped_space = 0;
                let mut iter = self.value.chars().peekable();
                assert_eq!(iter.next(), Some('"'));
                loop {
                    match iter.next() {
                        None => break,
                        Some('\\') => {
                            match iter.next() {
                                None => res.push_char('\\'),  // fixme
                                Some('0') => res.push_char('\0'),
                                Some('a') => res.push_char('\x07'),
                                Some('b') => res.push_char('\x08'),
                                Some('t') | Some('\t') => res.push_char('\t'),
                                Some('n') => res.push_char('\n'),
                                Some('r') => res.push_char('\r'),
                                Some('v') => res.push_char('\x0b'),
                                Some('f') => res.push_char('\x0c'),
                                Some('e') => res.push_char('\x1b'),
                                Some(' ') => {
                                    res.push_char(' ');
                                    escaped_space = res.len();
                                }
                                Some('"') => res.push_char('"'),
                                Some('/') => res.push_char('/'),
                                Some('\\') => res.push_char('\\'),
                                Some('N') => res.push_char('\x85'),
                                Some('_') => res.push_char('\xa0'),
                                Some('L') => res.push_char('\u2028'),
                                Some('P') => res.push_char('\u2029'),
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
                                    res.push_char('\\');
                                    res.push_char(x);
                                }
                            }
                            escaped_space = res.len();
                        }
                        Some('"') => break,
                        Some('\n') => {
                            process_newline(&mut iter, &mut res,
                                escaped_space);
                        }
                        Some(x) => res.push_char(x),
                    }
                }
            },
            T::SingleString => {
                let mut iter = self.value.chars().peekable();
                assert_eq!(iter.next(), Some('\''));
                loop {
                    match iter.next() {
                        None => break,
                        Some('\'') => {
                            match iter.next() {
                                None => break,
                                Some('\'') => res.push_char('\''),
                                Some(x) => unreachable!(),
                            }
                        }
                        Some('\n') => {
                            process_newline(&mut iter, &mut res, 0);
                        }
                        Some(x) => res.push_char(x),
                    }
                }
            }
            T::Literal => {
                let mut lines = self.value.split('\n');
                let fline = lines.next().unwrap();
                if fline == "|" {
                    let mut indent = 0;
                    for line in lines {
                        if indent == 0 {
                            let trimmed = line.trim_left_chars(' ');
                            indent = line.len() - trimmed.len();
                        }
                        res.push_str(line.slice(indent, line.len()));
                        res.push_char('\n');
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
}

pub struct Directive<'a>(&'a Token<'a>);

pub struct Document<'a> {
    pub directives: Vec<Directive<'a>>,
    pub root: Node<'a>,
    pub aliases: TreeMap<&'a str, &'a Node<'a>>,
}

pub enum Node<'a> {
    Map(Option<&'a str>, Option<&'a str>,
        TreeMap<Node<'a>, Node<'a>>, &'a[Token<'a>]),
    List(Option<&'a str>, Option<&'a str>, Vec<Node<'a>>, &'a[Token<'a>]),
    Scalar(Option<&'a str>, Option<&'a str>, String, &'a Token<'a>),
    // Explicit null is a Scalar at this state of parsing
    ImplicitNull(Option<&'a str>, Option<&'a str>, Pos),
    Alias(&'a str, &'a Token<'a>),
}


impl<'a> PartialOrd for Node<'a> {
    fn lt(&self, other: &Node) -> bool {
        return self.cmp(other) == Less;
    }
}
impl<'a> Ord for Node<'a> {
    fn cmp(&self, other: &Node) -> Ordering {
        return match (self, other) {
            (&Scalar(_, _, ref a, _), &Scalar(_, _, ref b, _))
            => a.cmp(b),
            (&ImplicitNull(_, _, _), &Scalar(_, _, _, _)) => Less,
            (&Scalar(_, _, _, _), &ImplicitNull(_, _, _)) => Greater,
            (&ImplicitNull(_, _, _), &ImplicitNull(_, _, _)) => Equal,
            _ => unimplemented!(),
        }
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

impl<'a> Show for Node<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FormatError> {
        match self {
            &Scalar(_, _, ref a, _) => format!("<Scalar {}>", a).fmt(fmt),
            &ImplicitNull(_, _, _) => "<Null>".fmt(fmt),
            _ => unimplemented!(),
        }
    }
}


fn parse_list<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<Node<'x>, ParserError>
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
            T::Indent => {
                tokiter.next().unwrap();
                let value = match parse_node(tokiter, aliases) {
                    Ok(value) => value,
                    Err(err) => return Err(err),
                };
                children.push(value);
                let etok = tokiter.peek(0);
                match etok.kind {
                    T::Unindent => {}
                    _ => return Err(ParserError::new(
                        etok.start.clone(), etok.end.clone(),
                        "Unexpected token")),
                }
                tokiter.next().unwrap();
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
    return Ok(List(None, None, children,
        tokiter.tokens.slice(begin, tokiter.index)));
}

fn parse_map<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<Node<'x>, ParserError>
{
    let begin = tokiter.index;
    let mut children = TreeMap::new();
    loop {
        // TODO(tailhook) implement complex keys
        // TODO(tailhook) implement aliases and anchors
        let ktoken = tokiter.peek(0);
        let key = match ktoken.kind {
            T::Eof => break,
            T::Unindent => break,
            T::PlainString | T::SingleString | T::DoubleString
            => Scalar(None, None, ktoken.plain_value(), ktoken),
            _ => return Err(ParserError::new(
                ktoken.start.clone(), ktoken.end.clone(), "Unexpected token")),
        };
        tokiter.next().unwrap();
        let delim = tokiter.peek(0);
        match delim.kind {
            T::MappingValue => {}
            T::Eof => return Err(ParserError::new(
                delim.start.clone(), delim.end.clone(),
                "Unexpected end of file")),
            _ => return Err(ParserError::new(
                delim.start.clone(), delim.end.clone(),
                "Unexpected token")),
        }
        tokiter.next().unwrap();
        let tok = tokiter.peek(0);
        match tok.kind {
            T::Indent => {
                tokiter.next().unwrap();
                let value = match parse_node(tokiter, aliases) {
                    Ok(value) => value,
                    Err(err) => return Err(err),
                };
                if !children.insert(key, value) {
                    return Err(ParserError::new(
                        ktoken.start.clone(), ktoken.end.clone(),
                        "Duplicate key"));
                }
                let etok = tokiter.peek(0);
                match etok.kind {
                    T::Unindent => {}
                    _ => return Err(ParserError::new(
                        etok.start.clone(), etok.end.clone(),
                        "Unexpected token")),
                }
                tokiter.next();
            }
            T::SequenceEntry if tok.start.line > delim.end.line => {
                // Allow sequences on the same indentation level as a key in
                // mapping
                let value = match parse_list(tokiter, aliases) {
                    Ok(value) => value,
                    Err(err) => return Err(err),
                };
                if !children.insert(key, value) {
                    return Err(ParserError::new(
                        ktoken.start.clone(), ktoken.end.clone(),
                        "Duplicate key"));
                }
            }
            T::Eof | T::Unindent => {
                children.insert(key, ImplicitNull(
                    None, None, tok.start.clone()));
                break;
            }
            _ => {
                let value = match parse_node(tokiter, aliases) {
                    Ok(value) => value,
                    Err(err) => return Err(err),
                };
                if !children.insert(key, value) {
                    return Err(ParserError::new(
                        ktoken.start.clone(), ktoken.end.clone(),
                        "Duplicate key"));
                }
            }
        }
    }
    return Ok(Map(None, None, children,
        tokiter.tokens.slice(begin, tokiter.index)));
}

fn parse_flow_list<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<Node<'x>, ParserError>
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
            _ => return Err(ParserError::new(
                tok.start.clone(), tok.end.clone(), "Unexpected token")),
        }
    }
    return Ok(List(None, None, children,
        tokiter.tokens.slice(begin, tokiter.index)));
}

fn parse_flow_map<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<Node<'x>, ParserError>
{
    let begin = tokiter.index;
    let mut children = TreeMap::new();
    tokiter.next();
    loop {
        // TODO(tailhook) implement complex keys
        // TODO(tailhook) implement aliases and anchors
        let ktoken = tokiter.next().unwrap();
        let key = match ktoken.kind {
            T::FlowMapEnd => break,
            T::PlainString | T::SingleString | T::DoubleString
            | T::Literal | T::Folded
            => Scalar(None, None, ktoken.plain_value(), ktoken),
            _ => return Err(ParserError::new(
                ktoken.start.clone(), ktoken.end.clone(), "Unexpected token")),
        };

        let tok = tokiter.next().unwrap();
        match tok.kind {
            T::FlowMapEnd => {
                // Value is null
                if !children.insert(key,
                    ImplicitNull(None, None, tok.start.clone())) {
                    return Err(ParserError::new(
                        ktoken.start.clone(), ktoken.end.clone(),
                        "Duplicate key"));
                }
                break;
            }
            T::FlowEntry => {
                // Value is null
                if !children.insert(key,
                    ImplicitNull(None, None, tok.start.clone())) {
                    return Err(ParserError::new(
                        ktoken.start.clone(), ktoken.end.clone(),
                        "Duplicate key"));
                }
                continue;
            }
            T::MappingValue => {}
            _ => return Err(ParserError::new(
                tok.start.clone(), tok.end.clone(), "Unexpected token")),
        }
        let value = match parse_flow_node(tokiter, aliases) {
            Ok(value) => value,
            Err(err) => return Err(err),
        };
        if !children.insert(key, value) {
            return Err(ParserError::new(
                ktoken.start.clone(), ktoken.end.clone(), "Duplicate key"));
        }
        let tok = tokiter.next().unwrap();
        match tok.kind {
            T::FlowMapEnd => break,
            T::FlowEntry => continue,
            _ => return Err(ParserError::new(
                tok.start.clone(), tok.end.clone(), "Unexpected token")),
        }
    }
    return Ok(Map(None, None, children,
        tokiter.tokens.slice(begin, tokiter.index)));
}

fn parse_flow_node<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<Node<'x>, ParserError>
{
    let tok = tokiter.peek(0);
    match tok.kind {
        T::PlainString | T::SingleString | T::DoubleString => {
            if tok.start.line == tok.end.line {
                // Only one-line scalars are allowed to be mapping keys
                let val = tokiter.peek(1);
                if (val.kind == T::MappingValue &&
                    val.start.line == tok.end.line) {
                    return parse_map(tokiter, aliases);
                }
            }
            tokiter.next();
            return Ok(Scalar(None, None, tok.plain_value(), tok));
        }
        T::FlowSeqStart => {
            return parse_flow_list(tokiter, aliases);
        }
        T::FlowMapStart => {
            return parse_flow_map(tokiter, aliases);
        }
        _ => return Err(ParserError::new(
            tok.start.clone(), tok.end.clone(), "Unexpected token")),
    };
}

fn parse_node<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<Node<'x>, ParserError>
{
    let mut tok = tokiter.peek(0);
    let mut tag = None;
    match tok.kind {
        T::Tag => {
            tag = Some(tok.value);
            tokiter.next();
            tok = tokiter.peek(0);
        }
        _ => {}
    }
    match tok.kind {
        T::PlainString | T::SingleString | T::DoubleString
        | T::Literal | T::Folded => {
            if tok.start.line == tok.end.line {
                // Only one-line scalars are allowed to be mapping keys
                let val = tokiter.peek(1);
                if (val.kind == T::MappingValue &&
                    val.start.line == tok.end.line) {
                    return parse_map(tokiter, aliases);
                }
            }
            tokiter.next();
            return Ok(Scalar(tag, None, tok.plain_value(), tok));
        }
        T::Eof => {
            return Ok(ImplicitNull(tag, None, tok.start.clone()));
        }
        T::SequenceEntry => {
            return parse_list(tokiter, aliases);
        }
        T::FlowSeqStart => {
            return parse_flow_list(tokiter, aliases);
        }
        T::FlowMapStart => {
            return parse_flow_map(tokiter, aliases);
        }
        _ => return Err(ParserError::new(
            tok.start.clone(), tok.end.clone(), "Unexpected token")),
    };
}

fn parse_root<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<(Vec<Directive<'x>>, Node<'x>), ParserError>
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
        println!("Tok kind {}", tok.kind);
        match tok.kind {
            T::DocumentEnd => {}
            _ => {
                return Err(ParserError::new(
                    tok.start.clone(), tok.end.clone(), "Unexpected token"));
            }
        }
    }
    return Ok((directives, res));
}


pub fn parse_tokens<'x>(tokens: &'x Vec<Token<'x>>)
    -> Result<Document<'x>, ParserError>
{
    let mut aliases = TreeMap::new();
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

pub fn parse<'x, T>(name: Rc<String>, data: &str, process: |Document| -> T)
    -> Result<T, Error>
{
    let tokens = match tokenize(name, data) {
        Ok(lst) => lst,
        Err(e) => return Err(TokenError(e)),
    };
    let doc = match parse_tokens(&tokens) {
        Ok(doc) => doc,
        Err(e) => return Err(ParserError(e)),
    };
    return Ok(process(doc));
}

