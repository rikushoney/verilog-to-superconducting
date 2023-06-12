#![allow(dead_code)]

use crate::*;

use std::str;

#[derive(Clone, Debug, PartialEq)]
enum Error {
    Expected(String),
    InvalidIdent,
    InvalidSignal,
    UnexpectedEof,
    Unknown,
}

macro_rules! expect_err {
    ($fmt:expr, $($args:tt)*) => {
        Error::Expected(format!($fmt, $($args)*))
    };
    ($what:expr) => {
        Error::Expected($what.to_string())
    };
}

struct Cursor<'a> {
    buffer: &'a [u8],
    pos: usize,
    end: usize,
}

impl<'a> Cursor<'a> {
    fn from_str(text: &'a str) -> Self {
        let buffer = text.as_bytes();
        let end = buffer.len();
        Self {
            buffer,
            pos: 0,
            end,
        }
    }

    fn pos(&self) -> usize {
        self.pos
    }

    fn set_pos(&mut self, pos: usize) {
        debug_assert!(pos <= self.end);
        self.pos = pos;
    }

    fn peek(&self) -> Option<u8> {
        if self.pos < self.end {
            Some(self.buffer[self.pos])
        } else {
            None
        }
    }

    fn bump(&mut self) {
        self.advance(1)
    }

    fn advance(&mut self, count: usize) {
        debug_assert!(self.pos + count <= self.end);
        self.pos += count;
    }

    fn is_eof(&self) -> bool {
        debug_assert!(self.pos <= self.end);
        self.buffer[self.pos..self.end].is_empty()
    }

    fn slice_head(&self, head: usize) -> &'a [u8] {
        debug_assert!(head <= self.pos);
        &self.buffer[head..self.pos]
    }

    fn slice_tail(&self, tail: usize) -> &'a [u8] {
        debug_assert!(self.pos <= tail);
        debug_assert!(tail <= self.end);
        &self.buffer[self.pos..tail]
    }

    fn slice_end(&self) -> &'a [u8] {
        self.slice_tail(self.end)
    }

    fn starts_with(&self, needle: &[u8]) -> bool {
        self.buffer.starts_with(needle)
    }

    fn skip_while<F>(&mut self, pred: F) -> Result<(), ()>
    where
        F: Fn(u8) -> bool,
    {
        if self.take_while(|&ch| pred(ch)).count() > 0 {
            Ok(())
        } else {
            Err(())
        }
    }

    fn skip_whitespace(&mut self) -> Result<(), Error> {
        self.skip_while(|ch| matches!(ch, 0x09 | 0x0D | 0x20))
            .map_err(|_| expect_err!("whitespace"))
    }

    fn skip_newline(&mut self) -> Result<(), Error> {
        match self.peek() {
            Some(b'\n') => self.bump(),
            Some(b'\r') => {
                self.bump();
                if self.peek() != Some(b'\n') {
                    return Err(expect_err!("newline"));
                } else {
                    self.bump();
                }
            }
            _ => {
                return Err(expect_err!("newline"));
            }
        };
        Ok(())
    }

    fn skip_comment(&mut self) {
        debug_assert!(self.peek().unwrap() == b'#');
        self.skip_while(|ch| ch != b'\n').unwrap();
    }

    fn chars(&self) -> std::str::Chars<'a> {
        // input is assumed to always be valid UTF-8
        str::from_utf8(self.slice_end()).unwrap().chars()
    }
}

impl Iterator for Cursor<'_> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.peek() {
            self.bump();
            Some(ch)
        } else {
            None
        }
    }
}

fn is_ident_start(ch: char) -> bool {
    unicode_ident::is_xid_start(ch) || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    unicode_ident::is_xid_continue(ch) || ch == '_'
}

// Ident ::= (XID_Start | '_') (XID_Continue | '_')*
fn parse_ident<'a>(cursor: &mut Cursor<'a>) -> Result<&'a str, Error> {
    let mut iter = cursor.chars();
    let first_len = match iter.next() {
        Some(ch) if is_ident_start(ch) => ch.len_utf8(),
        _ => return Err(expect_err!("identifier")),
    };
    let count = iter
        .take_while(|&ch| is_ident_continue(ch))
        .fold(first_len, |len, ch| len + ch.len_utf8());
    let start = cursor.pos();
    cursor.advance(count);
    Ok(str::from_utf8(cursor.slice_head(start)).expect("invalid UTF-8 in parse_ident"))
}

fn is_signal(ch: char) -> bool {
    const BANNED_CHARS: &str = "#=";
    !(ch.is_ascii_whitespace() || BANNED_CHARS.contains(ch))
}

// Signal ::= ([^#=] - S)+
fn parse_signal<'a>(cursor: &mut Cursor<'a>) -> Result<Signal<'a>, Error> {
    let count = cursor
        .chars()
        .take_while(|&ch| is_signal(ch))
        .fold(0, |len, ch| len + ch.len_utf8());
    if count == 0 {
        return Err(expect_err!("signal"));
    }
    let start = cursor.pos();
    cursor.advance(count);
    Ok(str::from_utf8(cursor.slice_head(start)).unwrap())
}

// SignalList ::= Signal (S+ Signal)*
fn parse_signal_list<'a>(cursor: &mut Cursor<'a>) -> Result<Vec<Signal<'a>>, Error> {
    let mut signals = vec![parse_signal(cursor)?];
    while cursor.skip_whitespace().is_ok() {
        match parse_signal(cursor) {
            Ok(signal) => signals.push(signal),
            _ => break,
        };
    }
    Ok(signals)
}

fn parse_command_name(cursor: &mut Cursor<'_>, name: &'static str) -> Result<(), Error> {
    debug_assert!(name.starts_with('.'));
    if !cursor.starts_with(name.as_bytes()) {
        return Err(expect_err!(name));
    }
    cursor.advance(name.len());
    Ok(())
}

// ModelField  ::= ('.inputs' | '.outputs' | '.clock') S+ SignalList
fn parse_model_field<'a>(
    cursor: &mut Cursor<'a>,
    name: &'static str,
) -> Result<Vec<Signal<'a>>, Error> {
    parse_command_name(cursor, name)
        .and_then(|_| cursor.skip_whitespace())
        .and_then(|_| parse_signal_list(cursor))
}

// ModelHeader ::= '.model' S+ Ident (EOL ModelFields)?
// ModelFields ::= ModelField (EOL ModelField)*
fn parse_model_header<'a>(cursor: &mut Cursor<'a>) -> Result<Model<'a>, Error> {
    let name = parse_command_name(cursor, ".model")
        .and_then(|_| cursor.skip_whitespace())
        .and_then(|_| parse_ident(cursor))
        .ok();
    let mut inputs = vec![];
    let mut outputs = vec![];
    let mut clocks = vec![];
    loop {
        if cursor.skip_newline().is_err() {
            break;
        }
        if let Ok(signals) = parse_model_field(cursor, ".inputs") {
            inputs.extend(signals);
        } else if let Ok(signals) = parse_model_field(cursor, ".outputs") {
            outputs.extend(signals);
        } else if let Ok(signals) = parse_model_field(cursor, ".clock") {
            clocks.extend(signals);
        } else {
            break;
        }
    }
    Ok(Model {
        name,
        inputs,
        outputs,
        clocks,
        commands: vec![],
    })
}

// Model ::= ModelHeader? (EOL Command)* (EOL '.end')?
fn parse_model<'a>(cursor: &mut Cursor<'a>) -> Result<Model<'a>, Error> {
    let _model_header = parse_model_header(cursor);
    let _ = parse_command_name(cursor, ".end");
    todo!()
}

// Circuit ::= Model (EOL Model)*
fn parse_circuit<'a>(_cursor: &mut Cursor<'a>) -> Result<Vec<Model<'a>>, Error> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ident() {
        let mut cursor = Cursor::from_str("nand2 123");
        assert_eq!(parse_ident(&mut cursor).unwrap(), "nand2");
        assert_eq!(cursor.slice_end(), " 123".as_bytes());

        let mut cursor = Cursor::from_str("lut4_mux aaa");
        assert_eq!(parse_ident(&mut cursor).unwrap(), "lut4_mux");
        assert_eq!(cursor.slice_end(), " aaa".as_bytes());
    }
}
