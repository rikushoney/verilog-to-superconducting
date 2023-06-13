#![allow(dead_code)]

use crate::cursor::*;
use crate::strspan::*;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Command {
    Model,
    Inputs,
    Outputs,
    Clock,
    End,
    Names,
    Latch,
    Gate,
    MLatch,
    Subckt,
}

const COMMAND_MAP: &[(&str, Command)] = &[
    ("model", Command::Model),
    ("inputs", Command::Inputs),
    ("outputs", Command::Outputs),
    ("clock", Command::Clock),
    ("end", Command::End),
    ("names", Command::Names),
    ("latch", Command::Latch),
    ("gate", Command::Gate),
    ("mlatch", Command::MLatch),
    ("subckt", Command::Subckt),
];

#[derive(Clone, Debug, PartialEq)]
enum Token<'a> {
    Command(Command, StrSpan<'a>),
    Eof,
    Equal(StrSpan<'a>),
    Newline(StrSpan<'a>),
    Text(StrSpan<'a>),
    Whitespace(StrSpan<'a>),
}

fn is_blif_whitespace(ch: char) -> bool {
    matches!(ch, '\t' | '\r' | '\n' | ' ')
}

fn is_blif_text(ch: char) -> bool {
    const BANNED_CHARS: &str = "#=";
    !(is_blif_whitespace(ch) || BANNED_CHARS.contains(ch))
}

struct Lexer<'a> {
    cursor: Cursor<'a>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            cursor: Cursor::from_str(input),
        }
    }

    fn skip_while<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool,
    {
        // since a single `char` could be up to 4 bytes in length,
        // we need to count it's UTF-8 "length"
        let count = self
            .cursor
            .chars()
            .take_while(|&ch| predicate(ch))
            .fold(0, |count, ch| count + ch.len_utf8());
        self.cursor.advance(count);
    }

    fn parse_text(&mut self) -> Token {
        debug_assert!(is_blif_text(self.cursor.peek().unwrap()));
        self.cursor.bump();
        let start = self.cursor.pos();
        self.skip_while(is_blif_text);
        return Token::Text(StrSpan::new(self.cursor.slice_head(start), start));
    }

    fn parse_next(&mut self) -> Token {
        loop {
            if let Some(ch) = self.cursor.peek() {
                let start = self.cursor.pos();
                match ch {
                    '\n' => {
                        self.cursor.bump();
                        let span = self.cursor.slice_head(start);
                        return Token::Newline(StrSpan::new(span, start));
                    }
                    '\\' => {
                        const UNIX_NEWLINE: &str = "\\\n";
                        const DOS_NEWLINE: &str = "\\\r\n";
                        if self.cursor.starts_with(UNIX_NEWLINE) {
                            self.cursor.advance(UNIX_NEWLINE.len());
                        } else if self.cursor.starts_with(DOS_NEWLINE) {
                            self.cursor.advance(DOS_NEWLINE.len());
                        } else {
                            return self.parse_text();
                        }
                    }
                    '#' => {
                        self.skip_while(|ch| ch != '\n');
                        // it is not an error for a comment to be at the EOF,
                        // so check if there is a newline
                        if !self.cursor.is_eof() {
                            let start = self.cursor.pos();
                            self.cursor.bump();
                            let span = self.cursor.slice_head(start);
                            return Token::Newline(StrSpan::new(span, start));
                        } else {
                            return Token::Eof;
                        }
                    }
                    '.' => {
                        self.cursor.bump();
                        // TODO: is this faster than a PHF or `match`ing each command?
                        for (name, command) in COMMAND_MAP.iter() {
                            if self.cursor.starts_with(name) {
                                self.cursor.advance(name.len());
                                let span = self.cursor.slice_head(start);
                                return Token::Command(*command, StrSpan::new(span, start));
                            }
                        }
                        // unknown command
                        return self.parse_text();
                    }
                    _ => {
                        if is_blif_whitespace(ch) {
                            self.cursor.bump();
                            self.skip_while(is_blif_whitespace);
                            let span = self.cursor.slice_head(start);
                            return Token::Whitespace(StrSpan::new(span, start));
                        } else if is_blif_text(ch) {
                            return self.parse_text();
                        }
                        eprintln!("unhandled character encountered in input \'{ch}\'");
                        self.cursor.bump();
                        continue;
                    }
                }
            } else {
                return Token::Eof;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip_comment() {
        let mut lexer = Lexer::new("# this is a comment");
        assert_eq!(lexer.parse_next(), Token::Eof);
        let mut lexer = Lexer::new("# also a comment\n");
        assert_eq!(lexer.parse_next(), Token::Eof);
    }
}
