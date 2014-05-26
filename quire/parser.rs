use std::slice::Items;
use std::iter::Peekable;
use std::vec::FromVec;

use collections::treemap::TreeMap;

use super::tokenizer::Token;
use super::errors::Error;
use super::errors::TokenError;
use super::errors::ParserError;
use super::tokenizer::tokenize;
use T = super::tokenizer;

type TokenIter<'x> = Peekable<&'x Token<'x>, Items<'x, Token<'x>>>;
type Aliases<'x> = TreeMap<&'x str, &'x Node<'x>>;

pub struct Directive<'a>(&'a Token<'a>);

pub struct Document<'a> {
    pub directives: ~[Directive<'a>],
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
        let tok = match tokiter.peek() {
            None => return Ok(Null(None, None)),
            Some(&tok) => tok,
        };
        let res = match tok.kind {
            T::PlainString => Scalar(None, None, tok),
            _ => return Err(ParserError::new(
                tok.start, tok.end, "Unexpected token")),
        };
        tokiter.next();
        return Ok(res);
    }
}

fn parse_root<'x>(tokiter: &mut TokenIter<'x>, aliases: &mut Aliases)
    -> Result<(~[Directive], Node<'x>), ParserError>
{
    let mut directives = Vec::new();
    loop {
        match tokiter.peek() {
            None => return Ok(
                (FromVec::<Directive>::from_vec(directives),
                 Null(None, None))),
            Some(tok) => {
                match tok.kind {
                    T::Directive => directives.push(Directive(*tok)),
                    T::DocumentStart => break,
                    T::Whitespace => {},
                    _ => break,  // Start reading node
                }
            }
        }
        tokiter.next();
    }
    let res =  match parse_node(tokiter, aliases) {
        Ok(node) => node,
        Err(e) => return Err(e),
    };
    match tokiter.peek() {
        None => {}
        Some(tok) => {
            match tok.kind {
                T::DocumentEnd => {}
                _ => {
                    return Err(ParserError::new(
                        tok.start, tok.end, "Unexpected token"));
                }
            }
        }
    }
    return Ok((FromVec::<Directive>::from_vec(directives), res));
}


pub fn parse_tokens<'x>(tokens: &'x Vec<Token<'x>>)
    -> Result<Document<'x>, ParserError>
{
    let mut aliases = TreeMap::new();
    let mut iter = tokens.iter().peekable();
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

