use crate::strspan::StrSpan;

pub struct Cursor<'a> {
    text: &'a str,
    pos: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(text: &'a str) -> Self {
        Self { text, pos: 0 }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn set_pos(&mut self, new_pos: usize) {
        debug_assert!(self.text.is_char_boundary(new_pos));
        self.pos = new_pos;
    }

    pub fn peek(&self) -> Option<char> {
        self.chars().next()
    }

    pub fn advance(&mut self, count: usize) {
        debug_assert!(self.pos + count <= self.text.len());
        self.pos += count;
        debug_assert!(self.text.is_char_boundary(self.pos));
    }

    pub fn bump(&mut self) {
        self.advance(1);
    }

    pub fn is_eof(&self) -> bool {
        self.pos == self.text.len()
    }

    pub fn slice_from(&self, start: usize) -> StrSpan<'a> {
        debug_assert!(start <= self.pos);
        debug_assert!(self.text.is_char_boundary(start));
        StrSpan::new(&self.text[start..self.pos], start)
    }

    pub fn slice_until(&self, end: usize) -> StrSpan<'a> {
        debug_assert!(self.pos <= end);
        debug_assert!(self.text.is_char_boundary(end));
        StrSpan::new(&self.text[self.pos..end], self.pos)
    }

    pub fn slice_end(&self) -> StrSpan<'a> {
        self.slice_until(self.text.len())
    }

    pub fn starts_with(&self, needle: &str) -> bool {
        self.slice_end().as_str().starts_with(needle)
    }

    pub fn chars(&self) -> std::str::Chars<'a> {
        self.slice_end().as_str().chars()
    }

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

    pub fn skip_whitespace_and_line_continue(&mut self) {
        self.skip_while(|ch| matches!(ch, '\t' | '\r' | ' '));
        if self.starts_with("\\\n") {
            self.advance(2);
        } else if self.starts_with("\\\r\n") {
            self.advance(3);
        }
    }

    #[allow(dead_code)]
    pub fn find(&self, needle: &StrSpan<'a>) -> Option<(usize, usize)> {
        if needle.as_str() == "" {
            return None;
        }
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
