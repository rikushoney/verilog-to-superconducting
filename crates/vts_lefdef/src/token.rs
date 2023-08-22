use crate::defchar::{is_ident, is_space};
use crate::error::Error;

use vts_shared::{cursor::Cursor, strspan::StrSpan};

macro_rules! keyword {
    (VERSION) => {
        "VERSION"
    };
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Ident {
        span: StrSpan<'a>,
    },
    Version {
        span: StrSpan<'a>,
        major: StrSpan<'a>,
        minor: StrSpan<'a>,
        sub_minor: Option<StrSpan<'a>>,
    },
}

pub struct Tokenizer<'a> {
    cursor: Cursor<'a>,
}

pub type ParseResult<T> = Result<T, Error>;

fn skip_whitespace(cur: &mut Cursor) {
    cur.skip_while(is_space);
}

fn parse_ident<'a>(cur: &mut Cursor<'a>) -> ParseResult<StrSpan<'a>> {
    debug_assert!(is_ident(cur.peek().unwrap()));
    let start = cur.pos();
    cur.skip_while(is_ident);
    Ok(cur.slice_from(start))
}

fn parse_int<'a>(cur: &mut Cursor<'a>) -> ParseResult<StrSpan<'a>> {
    let start = cur.pos();
    cur.skip_while(|c| c.is_ascii_digit());
    let slice = cur.slice_from(start);
    if slice.as_str().len() > 0 {
        Ok(slice)
    } else {
        Err(expected!("digit"))
    }
}

fn parse_version<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    debug_assert!(cur.starts_with_ignore_case(keyword!(VERSION)));
    let start = cur.pos();
    cur.advance(keyword!(VERSION).len());
    skip_whitespace(cur);
    let major = parse_int(cur)?;
    cur.consume('.').ok_or(expected!("."))?;
    let minor = parse_int(cur)?;
    let mut sub_minor = None;
    if cur.consume('.').is_some() {
        sub_minor = Some(parse_int(cur)?);
    }
    Ok(Token::Version {
        span: cur.slice_from(start),
        major,
        minor,
        sub_minor,
    })
}

fn parse_quoted_string<'a>(cur: &mut Cursor<'a>) -> ParseResult<StrSpan<'a>> {
    debug_assert_eq!(cur.peek().unwrap(), '\"');
    cur.bump();
    let start = cur.pos();
    while let Some(c) = cur.peek() {
        match c {
            '\\' => {
                cur.bump();
                if matches!(cur.peek(), Some('\"')) {
                    cur.bump();
                }
            }
            '\"' => {
                let span = cur.slice_from(start);
                cur.bump();
                return Ok(span);
            }
            _ if is_space(c) || is_ident(c) => {
                cur.bump();
            }
            _ => {
                break;
            }
        }
    }
    Err(expected!("a printable character"))
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            cursor: Cursor::new(input),
        }
    }

    fn parse_next(&mut self) -> Option<ParseResult<Token<'a>>> {
        let cur = &mut self.cursor;
        let ch = cur.peek()?;
        let start = cur.pos();
        match ch {
            '\\' => {
                // TODO: parse escaped characters
                todo!()
            }
            '\"' => {
                let inside_quotes = parse_quoted_string(cur);
                todo!()
            }
            ch if is_ident(ch) => {
                if cur.starts_with_ignore_case(keyword!(VERSION)) {
                    Some(parse_version(cur))
                } else {
                    let ident = parse_ident(cur);
                    todo!()
                }
            }
            _ => Some(Err(unexpected!("{ch}"))),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = ParseResult<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token = None;
        while !self.cursor.is_eof() && token.is_none() {
            token = self.parse_next();
            match token {
                Some(Ok(_)) => {
                    break;
                }
                Some(Err(_)) => {
                    self.cursor.jump_end();
                    break;
                }
                None => {
                    continue;
                }
            }
        }
        token
    }
}
