pub fn is_printable(character: char) -> bool {
    let ch: u32 = character as u32;
    return ch == 0x09 || ch == 0x0A || ch == 0x0D
        || 0x20 <= ch && ch <= 0x7E
        || ch == 0x85
        || 0xA0 <= ch && ch <= 0xD7FF
        || 0xE000 <= ch && ch <= 0xFFFD
        || 0x10000 <= ch && ch <= 0x10FFFF;
}

pub fn is_flow_indicator(ch: char) -> bool {
    return ch == ',' || ch == '[' || ch == ']' || ch == '{' || ch == '}';
}

pub fn is_whitespace(ch: char) -> bool {
    return ch == '\x20' || ch == '\x09' || ch == '\r' || ch == '\n';
}

pub fn is_dec_digit(ch: char) -> bool {
    return '0' <= ch && ch <= '9';
}

pub fn is_word_char(ch: char) -> bool {
    return is_dec_digit(ch) || ch == '-'
        || 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z';
}

pub fn is_tag_char(ch: char) -> bool {
    return ch == '%' || is_word_char(ch)
        || ch == '#'
        || ch == ';' || ch == '/' || ch == '?' || ch == ':' || ch == '@'
        || ch == '&' || ch == '=' || ch == '+' || ch == '$'
        || ch == '_' || ch == '.' || ch == '~' || ch == '*'
        || ch == '\'' || ch == '(' || ch == ')';
}

#[test]
fn test_printable() {
    assert!(is_printable('a'));
    assert!(is_printable('я'));
}
