#![allow(dead_code)]

use crate::strspan::*;

pub(crate) struct Cursor<'a> {
    text: &'a str,
    pos: usize,
    end: usize,
}

impl<'a> Cursor<'a> {
    pub(crate) fn from_str(text: &'a str) -> Self {
        let pos = 0;
        let end = text.len();
        Self { text, pos, end }
    }

    pub(crate) fn pos(&self) -> usize {
        self.pos
    }

    pub(crate) fn end(&self) -> usize {
        self.end
    }

    pub(crate) fn slice_head(&self, head: usize) -> &'a str {
        &self.text[head..self.pos]
    }

    pub(crate) fn slice_tail(&self, tail: usize) -> &'a str {
        &self.text[self.pos..tail]
    }

    pub(crate) fn slice_end(&self) -> &'a str {
        self.slice_tail(self.end)
    }

    pub(crate) fn peek(&self) -> Option<char> {
        if self.pos < self.end {
            self.slice_end().chars().next()
        } else {
            None
        }
    }

    pub(crate) fn bump(&mut self) {
        self.advance(1)
    }

    pub(crate) fn advance(&mut self, count: usize) {
        debug_assert!(self.pos + count <= self.end);
        self.pos += count;
        debug_assert!(self.text.is_char_boundary(self.pos));
    }

    pub(crate) fn is_eof(&self) -> bool {
        // if `pos` is greater than `end`, there is something very wrong!
        debug_assert!(self.pos <= self.end);
        self.pos == self.end
    }

    pub(crate) fn starts_with(&self, needle: &str) -> bool {
        self.slice_end().starts_with(needle)
    }

    pub(crate) fn chars(&self) -> std::str::Chars<'a> {
        self.slice_end().chars()
    }

    pub(crate) fn find(&self, needle: &StrSpan<'a>) -> (usize, usize) {
        let needle_ptr = needle.span().as_ptr() as usize;
        let cursor_ptr = self.text.as_ptr() as usize + needle.start();
        if needle_ptr != cursor_ptr {
            return (0, 0);
        }
        let mut line = 1;
        let mut column = 1;

        for ch in self.text[0..needle.start()].chars() {
            column += 1;
            if ch == '\n' {
                line += 1;
                column = 1;
            }
        }

        (line, column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_advance() {
        let mut cursor = Cursor::from_str("abc");
        assert_eq!(cursor.peek().unwrap(), 'a');
        cursor.bump();
        assert_eq!(cursor.slice_end(), "bc");
        cursor.advance(2);
        assert_eq!(cursor.peek(), None);
        assert!(cursor.is_eof());
    }

    #[cfg(debug_assertions)]
    #[test]
    #[should_panic]
    fn test_advance_past_end() {
        let mut cursor = Cursor::from_str("");
        cursor.bump();
    }
}
