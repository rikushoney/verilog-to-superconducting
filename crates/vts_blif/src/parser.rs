#![allow(dead_code)]

use crate::*;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::satisfy,
    combinator::{map, opt, recognize},
    multi::{many0_count, many1_count, separated_list0},
    sequence::{pair, preceded},
};

#[derive(Clone, Debug, PartialEq)]
enum Error {
    Expected(String),
    InvalidIdent,
    InvalidSignal,
    UnexpectedEof,
    Unknown,
}

impl<I> nom::error::ParseError<I> for Error {
    fn from_error_kind(_input: I, _kind: nom::error::ErrorKind) -> Self {
        Error::Unknown
    }

    fn append(_input: I, _kind: nom::error::ErrorKind, _other: Self) -> Self {
        Error::Unknown
    }
}

impl From<Error> for nom::Err<Error> {
    fn from(error: Error) -> nom::Err<Error> {
        nom::Err::Error(error)
    }
}

macro_rules! expect_err {
    ($what:literal) => {
        Error::Expected($what.to_string())
    };
    ($fmt:expr, $($args:tt)*) => {
        Error::Expected(format!($fmt, $($args)*))
    };
}

macro_rules! make_err {
    ($error:expr) => {
        nom::Err::Error($error)
    };
}

macro_rules! wrap_err {
    ($error:expr) => {
        |_: nom::Err<()>| make_err!($error)
    };
}

type ParseResult<'a, T = &'a [u8]> = Result<(&'a [u8], T), nom::Err<Error>>;

fn is_xid_start(ch: char) -> bool {
    unicode_ident::is_xid_start(ch) || ch == '_'
}

fn is_xid_continue(ch: char) -> bool {
    unicode_ident::is_xid_continue(ch) || ch == '_'
}

fn is_signal(ch: u8) -> bool {
    const BANNED_CHARS: &[u8] = b"#=";
    !(ch.is_ascii_whitespace() || BANNED_CHARS.contains(&ch))
}

// Ident ::= XID_Start (XID_Continue)*
fn parse_ident(input: &[u8]) -> ParseResult<&str> {
    // input is always assumed to be valid UTF-8
    let input = std::str::from_utf8(input).unwrap();
    recognize(pair(
        satisfy(is_xid_start),
        many0_count(satisfy(is_xid_continue)),
    ))(input)
    .map(|(input, ident)| (input.as_bytes(), ident))
    .map_err(wrap_err!(Error::InvalidIdent))
}

// Signal ::= [^#=]+
fn parse_signal(input: &[u8]) -> ParseResult<Signal<'_>> {
    map(take_while1(is_signal), |signal| {
        std::str::from_utf8(signal).expect("weird symbol in signal!")
    })(input)
    .map_err(wrap_err!(Error::InvalidSignal))
}

fn dot_command(command: &'static str) -> impl FnMut(&[u8]) -> ParseResult {
    move |input| {
        recognize(pair(tag(b"."), tag(command.as_bytes())))(input)
            .map_err(wrap_err!(expect_err!(".{}", command)))
    }
}

// EOL ::= '\r'? '\n'
fn line_end(input: &[u8]) -> ParseResult {
    recognize(pair(opt(tag(b"\r")), tag(b"\n")))(input).map_err(wrap_err!(expect_err!("line end")))
}

// LineContinue ::= EOL '\\'
fn line_continue(input: &[u8]) -> ParseResult {
    recognize(pair(line_end, tag(b"\\")))(input)
}

// https://infra.spec.whatwg.org/#ascii-whitespace
// S ::= '\t' | '\n' | #x0C | '\r' | ' ' | LineContinue
fn space(input: &[u8]) -> ParseResult {
    alt((
        tag(b"\t"),
        tag(b"\n"),
        tag([0x0C]),
        tag(b"\r"),
        tag(b" "),
        line_continue,
    ))(input)
    .map_err(|_| make_err!(expect_err!("whitespace")))
}

// S*
fn space0(input: &[u8]) -> ParseResult {
    recognize(many0_count(space))(input)
}

// S+
fn space1(input: &[u8]) -> ParseResult {
    recognize(many1_count(space))(input)
}

// SignalList ::= Signal (S+ Signal)*
fn parse_signal_list(input: &[u8]) -> ParseResult<Vec<Signal<'_>>> {
    separated_list0(space1, parse_signal)(input)
}

fn parse_model_field(command: &'static str) -> impl FnMut(&[u8]) -> ParseResult<Vec<Signal<'_>>> {
    move |input| {
        map(
            separated_list0(
                line_end,
                preceded(pair(dot_command(command), space1), parse_signal_list),
            ),
            |signals| signals.into_iter().flatten().collect(),
        )(input)
    }
}

// Model       ::= '.model' S+ Ident? ModelFields (EOL Commands)? (EOL '.end')?
// ModelFields ::= Inputs? (EOL Inputs)* (EOL Outputs)* (EOL Clocks)*
// Inputs      ::= '.inputs' S+ SignalList
// Outputs     ::= '.outputs' S+ SignalList
// Clocks      ::= '.clock' S+ SignalList
fn parse_model(input: &[u8]) -> ParseResult<Model<'_>> {
    let (_input, _) = dot_command("model")(input)?;
    todo!()
}

fn parse_circuit(_input: &[u8]) -> Result<Vec<Model<'_>>, Error> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ident() {
        let input = b"my_ident 123";
        let (rest, ident) = parse_ident(input).unwrap();
        assert_eq!(ident, "my_ident");
        assert_eq!(rest, b" 123");

        let input = rest;
        assert_eq!(parse_ident(input).unwrap_err(), Error::InvalidIdent.into());
    }
}
