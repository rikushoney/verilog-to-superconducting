#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StrSpan<'a> {
    text: &'a str,
    start: usize,
}

impl<'a> StrSpan<'a> {
    pub fn new(text: &'a str, start: usize) -> Self {
        Self { text, start }
    }

    pub fn as_str(&self) -> &'a str {
        self.text
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn empty() -> Self {
        Self { text: "", start: 0 }
    }
}
