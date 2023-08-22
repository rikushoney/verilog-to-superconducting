pub fn is_ident(c: char) -> bool {
    c.is_ascii_graphic()
}

pub fn is_space(c: char) -> bool {
    matches!(c, '\t' | '\r' | ' ')
}
