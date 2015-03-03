use std::fmt::Display;
use std::fmt::Error as FormatError;
use std::fmt::{Formatter};
use std::default::Default;
use std::collections::BTreeMap;

use super::tokenizer::Pos;
use super::errors::Error;
use super::parser::Node as P;
use super::parser::{Directive, Node, Document};
use super::tokenizer::TokenType as T;
use self::Ast::*;
use self::NullKind::*;
use self::ScalarKind::*;
use self::Tag::*;


#[derive(Clone, Copy)]
pub struct Options {
    pub merges: bool,
    pub aliases: bool,
    pub print_warnings: bool,
}

impl Default for Options {
    fn default() -> Options {
        return Options {
            merges: true,
            aliases: true,
            print_warnings: true,
        }
    }
}

pub enum ScalarKind {
    Plain,
    Quoted,
}

pub enum NullKind {
    Implicit,
    Explicit,
}

#[derive(Debug)]
pub enum Tag {
    NonSpecific,
    LocalTag(String),
    GlobalTag(String),
}

impl Tag {
    pub fn is_specific(&self) -> bool {
        match *self {
            NonSpecific => false,
            _ => true,
        }
    }
}

pub enum Ast {
    Map(Pos, Tag, BTreeMap<String, Ast>),
    List(Pos, Tag, Vec<Ast>),
    Scalar(Pos, Tag, ScalarKind, String),
    Null(Pos, Tag, NullKind),
}

impl Display for Ast {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FormatError> {
        match *self {
            Map(_, _, _) => write!(fmt, "Map"),
            List(_, _, _) => write!(fmt, "List"),
            Scalar(_, _, _, _) => write!(fmt, "Scalar"),
            Null(_, _, _) => write!(fmt, "Null"),
        }
    }
}

impl Ast {
    pub fn pos(&self) -> Pos {
        match *self {
            Map(ref pos, _, _) => pos.clone(),
            List(ref pos, _, _) => pos.clone(),
            Scalar(ref pos, _, _, _) => pos.clone(),
            Null(ref pos, _, _) => pos.clone(),
        }
    }
    pub fn tag<'x>(&'x self) -> &'x Tag {
        match *self {
            Map(_, ref tag, _) => tag,
            List(_, ref tag, _) => tag,
            Scalar(_, ref tag, _, _) => tag,
            Null(_, ref tag, _) => tag,
        }
    }
    pub fn with_tag(self, tag: Tag) -> Ast {
        match self {
            Map(pos, _, children) => Map(pos, tag, children),
            List(pos,  _, children) => List(pos, tag, children),
            Scalar(pos, _, style, value) => Scalar(pos, tag, style, value),
            Null(pos, _, kind) => Null(pos, tag, kind),
        }
    }
}

struct Context<'a> {
    options: Options,
    warnings: Vec<Error>,
    directives: Vec<Directive<'a>>,
    aliases: BTreeMap<&'a str, &'a Node<'a>>,
}


fn pos_for_node<'x>(node: &Node<'x>) -> Pos {
    match *node {
        P::Map(_, _, _, ref tokens) => tokens[0].start.clone(),
        P::List(_, _, _, ref tokens) => tokens[0].start.clone(),
        P::Scalar(_, _, _, ref token) => token.start.clone(),
        P::ImplicitNull(_, _, ref pos) => pos.clone(),
        P::Alias(_, ref token) => token.start.clone(),
    }
}

impl<'a> Context<'a> {
    fn process(&mut self, node: &Node<'a>) -> Ast {
        match *node {
            P::Map(ref origtag, _, _, ref tokens) => {
                let pos = tokens[0].start.clone();
                let tag = self.string_to_tag(&pos, origtag);
                let mut mapping = BTreeMap::new();
                self.merge_mapping(&mut mapping, node);

                return Map(pos, tag, mapping);
            }
            P::List(ref origtag, _, _, ref tokens) => {
                let pos = tokens[0].start.clone();
                let tag = self.string_to_tag(&pos, origtag);
                let mut seq = Vec::new();
                self.merge_sequence(&mut seq, node);

                return List(pos, tag, seq);
            }
            P::Scalar(ref tag, _, ref val, ref tok) => {
                let pos = tok.start.clone();
                let tag = self.string_to_tag(&pos, tag);
                if tok.kind == T::PlainString {
                    if val.as_slice() == "~" || val.as_slice() == "null" {
                        return Ast::Null(tok.start.clone(), tag, Explicit);
                    } else {
                        return Ast::Scalar(pos, tag, Plain, val.clone());
                    }
                } else {
                    return Ast::Scalar(pos, tag, Quoted, val.clone());
                }
            }
            P::ImplicitNull(ref tag, _, ref pos) => {
                let tag = self.string_to_tag(pos, tag);
                return Ast::Null(pos.clone(), tag, Implicit);
            }
            P::Alias(_, _) => {
                unimplemented!();
            }
        }
    }

    fn merge_mapping(&mut self, target: &mut BTreeMap<String, Ast>,
        node: &Node<'a>)
    {
        match *node {
            P::Map(_, _, ref children, _) => {
                let mut merge = None;
                for (k, v) in children.iter() {
                    let string_key = match *k {
                        P::Scalar(_, _, ref key, ref tok) => {
                            if tok.kind == T::PlainString && key.as_slice() == "<<" {
                                merge = Some(v);
                                continue;
                            }
                            key.clone()
                        }
                        P::ImplicitNull(_, _, _) => "".to_string(),
                        P::Alias(_, _) => {
                            unimplemented!();
                        }
                        ref node => {
                            self.warnings.push(
                                Error::preprocess_error(&pos_for_node(node),
                                    "Non scalar keys are not supported yet"
                                    .to_string()));
                            continue;
                        }
                    };
                    let value = self.process(v);
                    if !target.contains_key(&string_key) {
                        target.insert(string_key, value);
                    }
                }
                match merge {
                    Some(node) => self.merge_mapping(target, node),
                    _ => {}
                }
            }
            P::List(_, _, ref lst, _) => {
                // TODO(tailhook) check and assert on tags?
                for item in lst.iter() {
                    self.merge_mapping(target, item);
                }
            }
            P::Alias(_, _) => {
                unimplemented!();
            }
            _ => {
                self.warnings.push(Error::preprocess_error(&pos_for_node(node),
                    "Value of merge key must be either mapping or \
                     list of mappings".to_string()));
            }
        }
    }

    fn merge_sequence(&mut self, target: &mut Vec<Ast>,
        node: &Node<'a>)
    {
        match *node {
            P::List(_, _, ref children, _) => {
                for item in children.iter() {
                    match *item {
                        P::List(Some("!Unpack"), _, ref children, _) => {
                            for child in children.iter() {
                                self.merge_sequence(target, child);
                            }
                        }
                        _ => {
                            let value = self.process(item);
                            target.push(value);
                        }
                    }
                }
            }
            P::Alias(_, _) => {
                unimplemented!();
            }
            _ => {
                self.warnings.push(Error::preprocess_error(&pos_for_node(node),
                    "The of !Unpack node must be sequence".to_string()));
            }
        }
    }

    fn string_to_tag(&mut self, pos: &Pos, src: &Option<&'a str>)
        -> Tag
    {
        match *src {
            Some(val) => {
                let mut pieces = val.splitn(2, '!');
                assert!(pieces.next().unwrap() == "");
                match (pieces.next().unwrap(), pieces.next()) {
                    ("", None) => {
                        self.warnings.push(Error::preprocess_error(pos,
                            "Unexpected empty tag".to_string()));
                        NonSpecific
                    }
                    (val, None) => {
                        LocalTag(val.to_string())
                    }
                    ("", Some(_)) => {
                        self.warnings.push(Error::preprocess_error(pos,
                            "Global tags are unsupported yet".to_string()));
                        NonSpecific
                    }
                    (_, Some(_)) => {
                        self.warnings.push(Error::preprocess_error(pos,
                            "Tag prefixes are unsupported yet".to_string()));
                        NonSpecific
                    }
                }
            }
            None => NonSpecific,
        }
    }
}


pub fn process(opt: Options, doc: Document)
    -> (Ast, Vec<Error>)
{
    let mut ctx = Context {
        options: opt,
        directives: doc.directives,
        aliases: doc.aliases,
        warnings: Vec::new(),
    };
    let ast = ctx.process(&doc.root);
    return (ast, ctx.warnings);
}

