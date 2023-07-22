use crate::strspan::StrSpan;

/// A cursor into a string slice that can move around and peek characters
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Cursor<'a> {
    text: &'a str,
    pos: usize,
}

impl<'a> Cursor<'a> {
    /// Create a new cursor
    pub fn new(text: &'a str) -> Self {
        Self { text, pos: 0 }
    }

    /// Get the current position of the cursor
    pub fn pos(&self) -> usize {
        self.pos
    }

    /// Get the position of one byte past the last character
    pub fn end(&self) -> usize {
        self.text.len()
    }

    /// Set the cursor position
    pub fn set_pos(&mut self, new_pos: usize) {
        debug_assert!(self.text.is_char_boundary(new_pos));
        self.pos = new_pos;
    }

    /// Set the cursor's position to the end
    pub fn jump_end(&mut self) {
        self.set_pos(self.end());
    }

    /// Peek the character at the current position
    pub fn peek(&self) -> Option<char> {
        self.chars().next()
    }

    /// Advance by `count` bytes
    pub fn advance(&mut self, count: usize) {
        self.pos += count;
        debug_assert!(self.pos <= self.text.len());
        debug_assert!(self.text.is_char_boundary(self.pos));
    }

    /// Advance by a single byte
    pub fn bump(&mut self) {
        self.advance(1);
    }

    /// Check if the position is at the end
    pub fn is_eof(&self) -> bool {
        self.pos == self.text.len()
    }

    /// Get a `StrSpan` of the text from `start` up to the current position (exclusive)
    pub fn slice_from(&self, start: usize) -> StrSpan<'a> {
        debug_assert!(start <= self.pos);
        debug_assert!(self.text.is_char_boundary(start));
        StrSpan::new(&self.text[start..self.pos], start)
    }

    /// Get a `StrSpan` of the text from the current position up to `end` (exclusive)
    pub fn slice_until(&self, end: usize) -> StrSpan<'a> {
        debug_assert!(self.pos <= end);
        debug_assert!(self.text.is_char_boundary(end));
        StrSpan::new(&self.text[self.pos..end], self.pos)
    }

    /// Get a `StrSpan` of the text from the current position up to the end of the cursor
    pub fn slice_end(&self) -> StrSpan<'a> {
        self.slice_until(self.text.len())
    }

    /// Get an empty `StrSpan` that contains the current position
    pub fn slice_pos(&self) -> StrSpan<'a> {
        self.slice_from(self.pos())
    }

    /// Check if the current position starts with `needle`
    pub fn starts_with(&self, needle: &str) -> bool {
        self.slice_end().as_str().starts_with(needle)
    }

    /// Get an iterator over the characters from the current position
    pub fn chars(&self) -> std::str::Chars<'a> {
        self.slice_end().as_str().chars()
    }

    /// Skip characters while it matches `pred`
    pub fn skip_while<F>(&mut self, pred: F)
    where
        F: Fn(char) -> bool,
    {
        let skip_count = self
            .chars()
            .take_while(|&ch| pred(ch))
            .fold(0, |count, ch| count + ch.len_utf8());
        self.advance(skip_count);
    }

    /// Get the line and column number of `needle`
    pub fn find(&self, needle: &StrSpan<'a>) -> Option<(usize, usize)> {
        let src_ptr = self.text.as_ptr() as usize + self.pos;
        let needle_ptr = needle.as_str().as_ptr() as usize;
        if needle_ptr != src_ptr {
            return None;
        }
        let mut line = 1;
        let mut column = 1;
        for ch in self.text[0..self.pos].chars() {
            if ch == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        Some((line, column))
    }
}
