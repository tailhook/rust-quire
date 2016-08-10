use super::tokenizer::{self, Pos};

#[derive(Clone, Debug)]
pub struct ErrorPos(String, usize, usize);

quick_error! {
    #[derive(Clone, Debug)]
    pub enum Error {
        TokenizerError(pos: ErrorPos, err: tokenizer::Error) {
            display("{filename}:{line}:{offset}: Tokenizer Error: {err}",
                    filename=pos.0, line=pos.1, offset=pos.2, err=err)
        }
        ParseError(pos: ErrorPos, msg: String) {
            display("{filename}:{line}:{offset}: Parse Error: {text}",
                    filename=pos.0, line=pos.1, offset=pos.2, text=msg)
        }
        ValidationError(pos: ErrorPos, msg: String) {
            display("{filename}:{line}:{offset}: Validation Error: {text}",
                    filename=pos.0, line=pos.1, offset=pos.2, text=msg)
        }
        PreprocessError(pos: ErrorPos, msg: String) {
            display("{filename}:{line}:{offset}: Preprocess Error: {text}",
                    filename=pos.0, line=pos.1, offset=pos.2, text=msg)
        }
        DecodeError(pos: ErrorPos, path: String, msg: String) {
            display("{filename}:{line}:{offset}: \
                Decode error at {path}: {text}",
                    filename=pos.0, line=pos.1, offset=pos.2,
                    path=path, text=msg)
        }
    }
}

unsafe impl Send for Error {}

impl Error {
    pub fn parse_error(pos: &Pos, message: String) -> Error {
        return Error::ParseError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message);
    }
    pub fn tokenizer_error((pos, err): (Pos, tokenizer::Error)) -> Error {
        return Error::TokenizerError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            err);
    }
    pub fn validation_error(pos: &Pos, message: String) -> Error {
        return Error::ValidationError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message);
    }
    pub fn decode_error(pos: &Pos, path: &String, message: String) -> Error {
        return Error::DecodeError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            path.clone(),
            message);
    }
    pub fn preprocess_error(pos: &Pos, message: String) -> Error {
        return Error::PreprocessError(
            ErrorPos((*pos.filename).clone(), pos.line, pos.line_offset),
            message);
    }
}
