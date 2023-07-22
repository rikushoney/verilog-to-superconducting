/// A string slice and its position in the string slice containing it
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StrSpan<'a> {
    text: &'a str,
    start: usize,
}

impl<'a> StrSpan<'a> {
    /// Create a new `StrSpan`
    pub fn new(text: &'a str, start: usize) -> Self {
        Self { text, start }
    }

    /// Create an empty `StrSpan`
    pub fn empty() -> Self {
        Self { text: "", start: 0 }
    }

    /// Get the underlying string slice
    pub fn as_str(&self) -> &'a str {
        self.text
    }

    /// Get the position in the containing string slice
    pub fn start(&self) -> usize {
        self.start
    }
}
