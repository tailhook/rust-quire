use std::slice::Items;
use std::iter::Peekable;

use collections::treemap::TreeMap;
use collections::ringbuf::RingBuf;
use collections::Deque;

use super::tokenizer::Token;
use super::errors::Error;
use super::errors::TokenError;
use super::errors::ParserError;
use super::tokenizer::tokenize;
use T = super::tokenizer;

type Aliases<'x> = TreeMap<&'x str, &'x Node<'x>>;

pub struct TokenIter<'a> {
    peeked: RingBuf<&'a Token<'a>>,
    iter: Items<'a, Token<'a>>,
    eof_token: &'a Token<'a>,
}

impl<'a> TokenIter<'a> {
    fn new<'x>(src: &'x Vec<Token<'x>>) -> TokenIter<'x> {
        let last = src.last().expect("Non-empty document expected");
        assert!(last.kind == T::Eof);
        return TokenIter {
            peeked: RingBuf::new(),
            iter: src.iter(),
            eof_token: last,
        };
    }
    fn peek(&mut self, num: uint) -> &'a Token<'a> {
        'peeking: loop {
            if self.peeked.len() > num {
                break;
            }
            for tok in self.iter {
                if tok.kind == T::Whitespace || tok.kind == T::Comment {
                    continue;
                }
                self.peeked.push_back(tok);
                continue 'peeking;
            }
            self.peeked.push_back(self.eof_token);
        }
        return *self.peeked.get(num);
    }

    fn next(&mut self) -> Option<&'a Token<'a>> {
        if self.peeked.len() > 0 {
            let res = self.peeked.pop_front().unwrap();
            if res.kind == T::Eof {
                return None;
            }
            return Some(res);
        } else {
            for tok in self.iter {
                if tok.kind == T::Whitespace || tok.kind == T::Comment {
                    continue;
                }
                if tok.kind == T::Eof  {
                    return None;
                }
                return Some(tok);
            }
            return None;
        }
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
    Scalar(Option<&'a str>, Option<&'a str>, &'a Token<'a>),
    Null(Option<&'a str>, Option<&'a str>),
    Alias(&'a str),
}

fn parse_node<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<Node<'x>, ParserError>
{
    loop {
        let tok = tokiter.peek(0);
        let res = match tok.kind {
            T::PlainString | T::SingleString | T::DoubleString
              => Scalar(None, None, tok),
            _ => return Err(ParserError::new(
                tok.start, tok.end, "Unexpected token")),
        };
        tokiter.next();
        return Ok(res);
    }
}

fn parse_root<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<(Vec<Directive<'x>>, Node<'x>), ParserError>
{
    let mut directives = Vec::new();
    loop {
        let tok = tokiter.peek(0);
        match tok.kind {
            T::Eof => return Ok((directives, Null(None, None))),
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
                    tok.start, tok.end, "Unexpected token"));
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

pub fn parse<'x, T>(data: &str, process: |Document| -> T)
    -> Result<T, Error>
{
    let tokens = match tokenize(data) {
        Ok(lst) => lst,
        Err(e) => return Err(TokenError(e)),
    };
    let doc = match parse_tokens(&tokens) {
        Ok(doc) => doc,
        Err(e) => return Err(ParserError(e)),
    };
    return Ok(process(doc));
}

