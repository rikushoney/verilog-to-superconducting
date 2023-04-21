use std::str::Chars;

#[derive(Clone)]
pub struct ParseState<'a> {
    cursor: Chars<'a>,
}

impl<'a> ParseState<'a> {
    pub fn new<Input: Into<&'a str>>(input: Input) -> Self {
        Self {
            cursor: input.into().chars(),
        }
    }
}

impl Iterator for ParseState<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.cursor.next()
    }
}
