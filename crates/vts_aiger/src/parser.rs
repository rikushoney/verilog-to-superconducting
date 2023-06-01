#![allow(dead_code)]

use thiserror::Error;

#[derive(Debug, PartialEq)]
pub enum FileType {
    ASCII,
    Binary,
}

#[derive(Debug, PartialEq)]
pub struct Header {
    pub max_idx: usize,
    pub num_inputs: usize,
    pub num_latches: usize,
    pub num_outputs: usize,
    pub num_gates: usize,
}

#[derive(Debug, Error, PartialEq)]
pub enum ParseError {
    #[error("unexpected end of file")]
    Eof,
    #[error("expected space")]
    ExpectedSpace,
    #[error("expected newline")]
    ExpectedNewline,
    #[error("invalid integer")]
    InvalidInteger,
    #[error("invalid file magic")]
    InvalidMagic,
    #[error("unknown error")]
    Unknown,
}

type Result<'a, T> = std::result::Result<(&'a [u8], T), ParseError>;

fn take(count: usize) -> impl FnMut(&[u8]) -> Result<'_, &[u8]> {
    move |input| {
        let taken = input.get(0..count).ok_or(ParseError::Eof)?;
        Ok((&input[count..], taken))
    }
}

fn take_while<P>(pred: P) -> impl FnMut(&[u8]) -> Result<'_, &[u8]>
where
    P: Fn(u8) -> bool,
{
    move |input| match input.iter().position(|&x| !pred(x)) {
        Some(count) => take(count)(input),
        None => Ok((&[], input)),
    }
}

fn skip_space(input: &[u8]) -> Result<'_, ()> {
    let mut take_1 = take(1);
    let (input, space) = take_1(input)?;
    if space != b" " {
        return Err(ParseError::ExpectedSpace);
    }
    Ok((input, ()))
}

fn skip_newline(input: &[u8]) -> Result<'_, ()> {
    let mut take_1 = take(1);
    let (mut input, mut newline) = take_1(input)?;
    if newline == b"\r" {
        (input, newline) = take_1(input)?;
    }
    if newline != b"\n" {
        return Err(ParseError::ExpectedNewline);
    }
    Ok((input, ()))
}

fn parse_magic(input: &[u8]) -> Result<'_, FileType> {
    let (input, magic) = take(3)(input)?;
    let file_type = match magic {
        b"aag" => FileType::ASCII,
        b"aig" => FileType::Binary,
        _ => return Err(ParseError::InvalidMagic),
    };
    Ok((input, file_type))
}

fn parse_unsigned(input: &[u8]) -> Result<'_, usize> {
    let (input, x) = take_while(|x| x.is_ascii_digit())(input)?;
    // SAFETY: ascii is always valid utf8
    let x = unsafe { std::str::from_utf8_unchecked(x) };
    Ok((input, x.parse().map_err(|_| ParseError::InvalidInteger)?))
}

fn parse_header(input: &[u8]) -> Result<'_, Header> {
    let (input, magic) = parse_magic(input)?;
    match magic {
        FileType::ASCII => {
            let (input, _) = skip_space(input)?;
            let (input, max_idx) = parse_unsigned(input)?;
            let (input, _) = skip_space(input)?;
            let (input, num_inputs) = parse_unsigned(input)?;
            let (input, _) = skip_space(input)?;
            let (input, num_latches) = parse_unsigned(input)?;
            let (input, _) = skip_space(input)?;
            let (input, num_outputs) = parse_unsigned(input)?;
            let (input, _) = skip_space(input)?;
            let (input, num_gates) = parse_unsigned(input)?;
            Ok((
                input,
                Header {
                    max_idx,
                    num_inputs,
                    num_latches,
                    num_outputs,
                    num_gates,
                },
            ))
        }
        FileType::Binary => todo!(),
    }
}

fn parse_body<'a>(input: &'a [u8], header: &Header) -> Result<'a, ()> {
    let mut input = input;
    let mut inputs = Vec::new();
    if header.num_inputs > 0 {
        let mut literal;
        (input, literal) = parse_unsigned(input)?;
        inputs.push(literal);
        if header.num_inputs > 1 {
            for _ in 1..header.num_inputs {
                (input, _) = skip_newline(input)?;
                (input, literal) = parse_unsigned(input)?;
                inputs.push(literal);
            }
        }
    }
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_take() {
        let input = b"abcd";
        let (input, taken) = take(1)(input).unwrap();
        assert_eq!(taken, b"a");
        assert_eq!(input, b"bcd");
        let (input, taken) = take(2)(input).unwrap();
        assert_eq!(taken, b"bc");
        assert_eq!(input, b"d");
        let (input, taken) = take(1)(input).unwrap();
        assert_eq!(taken, b"d");
        assert_eq!(input, b"");
        assert_eq!(take(1)(input), Err(ParseError::Eof));
    }

    #[test]
    fn test_take_while() {
        let input = b"0a12b";
        let mut take_digits = take_while(|x| x.is_ascii_digit());
        let mut take_alphas = take_while(|x| x.is_ascii_alphabetic());
        let (input, taken) = take_digits(input).unwrap();
        assert_eq!(taken, b"0");
        assert_eq!(input, b"a12b");
        let (input, taken) = take_alphas(input).unwrap();
        assert_eq!(taken, b"a");
        assert_eq!(input, b"12b");
        let (input, taken) = take_digits(input).unwrap();
        assert_eq!(taken, b"12");
        assert_eq!(input, b"b");
        let (input, taken) = take_alphas(input).unwrap();
        assert_eq!(taken, b"b");
        assert_eq!(input, b"");
    }

    #[test]
    fn test_parse_magic() {
        let input = b"aag";
        let (input, file_type) = parse_magic(input).unwrap();
        assert_eq!(file_type, FileType::ASCII);
        assert_eq!(input, &[]);

        let input = b"aig";
        let (input, file_type) = parse_magic(input).unwrap();
        assert_eq!(file_type, FileType::Binary);
        assert_eq!(input, &[]);
    }

    #[test]
    fn test_parse_unsigned() {
        let input = b"123 456";
        let (input, int) = parse_unsigned(input).unwrap();
        assert_eq!(int, 123);
        assert_eq!(input, b" 456");
        let (input, _) = skip_space(input).unwrap();
        let (input, int) = parse_unsigned(input).unwrap();
        assert_eq!(int, 456);
        assert_eq!(input, b"");
    }

    #[test]
    fn test_parse_empty() {
        let input = b"aag 0 0 0 0 0";
        let (input, header) = parse_header(input).unwrap();
        assert_eq!(
            header,
            Header {
                max_idx: 0,
                num_inputs: 0,
                num_latches: 0,
                num_outputs: 0,
                num_gates: 0
            }
        );
        assert_eq!(input, b"");
    }
}
