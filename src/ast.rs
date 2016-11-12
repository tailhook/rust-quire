//! Abstract syntax tree

use std::fmt::Display;
use std::fmt::Error as FormatError;
use std::fmt::{Formatter};
use std::collections::BTreeMap;

use super::tokenizer::Pos;
use super::errors::{Error, ErrorCollector};
use super::parser::Node as P;
use super::parser::{Directive, Node, Document};
use super::tokenizer::TokenType as T;
use self::Ast::*;
use self::NullKind::*;
use self::ScalarKind::*;
use self::Tag::*;
use options::{Options, Include, DoInclude};


/// Kind of scalar value
///
/// This is usually needed to distinguish numeric value `123` from
/// string value `"123"` (usually quire doesn't care on decode stage,
/// but it might be useful for some cases)
#[derive(Debug)]
pub enum ScalarKind {
    /// Scalar value
    Plain,
    Quoted,
}

/// Kind of null value
#[derive(Debug)]
pub enum NullKind {
    /// Implicit null value, like in `a:`, there is implicit null value of a
    Implicit,
    /// Explicit null, specified as `null`, or `~` in yaml
    Explicit,
}

/// Yaml tag
#[derive(Debug)]
pub enum Tag {
    /// Value without any tag, it's derived from the value kind
    NonSpecific,
    /// Local tag `!tag` (single exclamation mark)
    LocalTag(String),
    /// Global tag, i.e. either a prefix defined in directives or a full
    /// url tag
    ///
    /// Largely unsupported in the current code
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

/// Yaml node
#[derive(Debug)]
pub enum Ast {
    /// Mapping node
    Map(Pos, Tag, BTreeMap<String, Ast>),
    /// Sequence node
    List(Pos, Tag, Vec<Ast>),
    /// Scalar node (except null)
    Scalar(Pos, Tag, ScalarKind, String),
    /// Null node
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
    /// A special null value that is used when node inside is errorneous
    pub fn void(pos: &Pos) -> Ast {
        Ast::Null(pos.clone(), Tag::NonSpecific, NullKind::Implicit)
    }
}

struct Context<'a, 'b: 'a> {
    options: &'a Options<'b>,
    directives: Vec<Directive<'a>>,
    err: &'a ErrorCollector,
}


fn pos_for_node<'x>(node: &Node<'x>) -> Pos {
    match *node {
        P::Map(_, _, _, ref tokens) => tokens[0].start.clone(),
        P::List(_, _, _, ref tokens) => tokens[0].start.clone(),
        P::Scalar(_, _, _, ref token) => token.start.clone(),
        P::ImplicitNull(_, _, ref pos) => pos.clone(),
        P::Alias(_, ref token, _) => token.start.clone(),
    }
}

fn parse_key<'x>(node: &Node, value: &'x Node<'x>, merge: &mut Option<&Node<'x>>)
    -> Result<String, Option<Error>>
{
     match *node {
        P::Scalar(_, _, ref key, ref tok) => {
            if tok.kind == T::PlainString && &key[..] == "<<" {
                *merge = Some(value);
                return Err(None);
            }
            Ok(key.clone())
        }
        P::ImplicitNull(_, _, _) => Ok("".to_string()),
        P::Alias(_, _, ref node) => {
            parse_key(node, value, merge)
        }
        ref node => {
            Err(Some(Error::preprocess_error(&pos_for_node(node),
                "Non scalar keys are not supported yet"
                .to_string())))
        }
    }
}

impl<'a, 'b: 'a> Context<'a, 'b> {
    fn process(&mut self, node: &'a Node<'a>) -> Ast {
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
            P::Scalar(Some("!*Include"), _anch, ref val, ref tok) => {
                return self.options.include(&tok.start,
                    &Include::File { filename: val }, self.err);
            }
            P::Scalar(ref tag, _, ref val, ref tok) => {
                let pos = tok.start.clone();
                let tag = self.string_to_tag(&pos, tag);
                if tok.kind == T::PlainString {
                    if &val[..] == "~" || &val[..] == "null" {
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
            P::Alias(_, _, ref node) => {
                return self.process(node);
            }
        }
    }

    fn merge_mapping(&mut self, target: &mut BTreeMap<String, Ast>,
        node: &'a Node<'a>)
    {
        match *node {
            P::Map(_, _, ref children, _) => {
                let mut merge = None;
                for (k, v) in children.iter() {
                    let string_key = match parse_key(k, v, &mut merge) {
                        Ok(k) => k,
                        Err(None) => continue,  // merge key
                        Err(Some(e)) => {
                            self.err.add_error(e);
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
            P::Scalar(Some("!*Include"), _anch, ref val, ref tok) => {
                self.merge_mapping_ast(target,
                    self.options.include(&tok.start,
                        &Include::File { filename: val }, self.err));
            }
            P::Alias(_, _, ref node) => {
                self.merge_mapping(target, node);
            }
            _ => {
                self.err.add_error(Error::preprocess_error(&pos_for_node(node),
                    "Value of merge key must be either mapping or \
                     list of mappings".to_string()));
            }
        }
    }
    /// This is same as merge_mapping but for asts
    ///
    /// Used for includes, may be this function can be used for everything
    /// but we don't use it perhaps for efficiency
    fn merge_mapping_ast(&mut self, target: &mut BTreeMap<String, Ast>,
        ast: Ast)
    {
        match ast {
            Map(_, _, children) => {
                for (k, v) in children.into_iter() {
                    if !target.contains_key(&k) {
                        // We don't make deep merging here, because other map
                        // is already merged
                        target.insert(k, v);
                    }
                }
            }
            List(_, _, lst) => {
                // TODO(tailhook) check and assert on tags?
                for item in lst.into_iter() {
                    self.merge_mapping_ast(target, item);
                }
            }
            node => {
                self.err.add_error(Error::preprocess_error(&node.pos(),
                    "Value of merge key must be either mapping or \
                     list of mappings".to_string()));
            }
        }
    }

    fn merge_sequence(&mut self, target: &mut Vec<Ast>,
        node: &'a Node<'a>)
    {
        match *node {
            P::List(_, _, ref children, _) => {
                for item in children.iter() {
                    match *item {
                        P::List(Some("!*Unpack"), _, ref children, _) => {
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
            P::Alias(_, _, ref node) => {
                self.merge_sequence(target, node);
            }
            P::Scalar(Some("!*Include"), _anch, ref val, ref tok) => {
                let ast = self.options.include(&tok.start,
                    &Include::File { filename: val }, self.err);
                // We don't make deep unpacking here, because other map
                // is already unpacked
                match ast {
                    List(_, _, vec) => {
                        target.extend(vec)
                    }
                    other => {
                        self.err.add_error(
                            Error::preprocess_error(&pos_for_node(node),
                                "The of !*Unpack node must be sequence"
                                .to_string()));
                        target.push(other);
                    }
                }
            }
            _ => {
                self.err.add_error(Error::preprocess_error(&pos_for_node(node),
                    "The of !*Unpack node must be sequence".to_string()));
            }
        }
    }

    fn string_to_tag(&mut self, pos: &Pos, src: &Option<&'a str>)
        -> Tag
    {
        match *src {
            Some(val) => {
                let mut pieces = val.splitn(3, '!');
                assert!(pieces.next().unwrap() == "");
                match (pieces.next().unwrap(), pieces.next()) {
                    ("", None) => {
                        self.err.add_error(Error::preprocess_error(pos,
                            "Unexpected empty tag".to_string()));
                        NonSpecific
                    }
                    (val, None) => {
                        LocalTag(val.to_string())
                    }
                    ("", Some(_)) => {
                        self.err.add_error(Error::preprocess_error(pos,
                            "Global tags are unsupported yet".to_string()));
                        NonSpecific
                    }
                    (_, Some(_)) => {
                        self.err.add_error(Error::preprocess_error(pos,
                            "Tag prefixes are unsupported yet".to_string()));
                        NonSpecific
                    }
                }
            }
            None => NonSpecific,
        }
    }
}


///  Preprocess AST
///
///  This includes:
///
///  * anchor substitution
///  * resolving merge keys `<<` and `!*Unpack`
///  * resolving includes `!*Include` and similar
///
pub fn process(opt: &Options, doc: Document, err: &ErrorCollector) -> Ast {
    let mut ctx = Context {
        options: opt,
        directives: doc.directives,
        err: err,
    };
    return ctx.process(&doc.root);
}

