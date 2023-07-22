pub fn is_space(ch: char) -> bool {
    matches!(ch, '\t' | '\r' | ' ')
}

pub fn is_text(ch: char) -> bool {
    !(is_space(ch) || ch == '\n' || ch == '#')
}

pub fn is_logic(ch: char) -> bool {
    matches!(ch, '0' | '1' | '-')
}
