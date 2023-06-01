#![allow(dead_code)]

struct Buffer<'a> {
    start: *const u8,
    cursor: *const u8,
    end: *const u8,
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> Buffer<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        let start = bytes.as_ptr();
        // SAFETY: `end` will point to one byte past `bytes`
        let end = unsafe { start.add(bytes.len()) };
        Self {
            start,
            cursor: start,
            end,
            _marker: std::marker::PhantomData,
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.cursor < self.end {
            // SAFETY: `cursor` is bounds checked
            Some(unsafe { *self.cursor })
        } else {
            None
        }
    }

    fn peek_n(&self, count: usize) -> Option<&[u8]> {
        if self.cursor as usize + count <= self.end as usize {
            // SAFETY: `cursor + count` is bounds checked
            Some(unsafe { std::slice::from_raw_parts(self.cursor, count) })
        } else {
            None
        }
    }

    // SAFETY: same as `advance`
    unsafe fn bump(&mut self) {
        self.advance(1)
    }

    // SAFETY: this might increment `cursor` beyond `end`
    unsafe fn advance(&mut self, count: usize) {
        self.cursor = self.cursor.add(count);
        debug_assert!(self.cursor <= self.end);
    }
}

const ASCII_MAGIC: &[u8] = b"aag";
const BINARY_MAGIC: &[u8] = b"aig";

#[derive(Debug, PartialEq)]
enum FileType {
    Ascii,
    Binary,
}

#[derive(Debug, PartialEq)]
enum Error {
    ExpectedNewline,
    ExpectedSpace,
    InvalidMagic,
    InvalidUnsigned,
    Eof,
    Overflow,
}

#[derive(Debug, PartialEq)]
struct Header {
    max_index: usize,
    num_inputs: usize,
    num_latches: usize,
    num_outputs: usize,
    num_gates: usize,
}

type Result<T> = std::result::Result<T, Error>;

struct Literal(usize);

struct Latch(Literal, Literal);

struct Gate(Literal, Literal, Literal);

fn skip_space(buffer: &mut Buffer) -> Result<()> {
    if buffer.peek().ok_or(Error::Eof)? != b' ' {
        return Err(Error::ExpectedSpace);
    }
    // SAFETY: peek (above) was successful
    unsafe { buffer.bump() };
    Ok(())
}

fn skip_newline(buffer: &mut Buffer) -> Result<()> {
    loop {
        match buffer.peek().ok_or(Error::Eof)? {
            // SAFETY: peek (above) was successful
            b'\r' => unsafe { buffer.bump() },
            b'\n' => break,
            _ => return Err(Error::ExpectedNewline),
        }
    }
    // SAFETY: peek (above) was successful
    unsafe { buffer.bump() };
    Ok(())
}

fn parse_magic(buffer: &mut Buffer) -> Result<FileType> {
    const MAGIC_LEN: usize = 3;
    let magic = buffer.peek_n(MAGIC_LEN).ok_or(Error::Eof)?;
    let magic = match magic {
        ASCII_MAGIC => FileType::Ascii,
        BINARY_MAGIC => FileType::Binary,
        _ => return Err(Error::InvalidMagic),
    };
    // SAFETY: peeking `MAGIC_LEN` bytes (above) was successful
    unsafe { buffer.advance(MAGIC_LEN) };
    Ok(magic)
}

fn parse_unsigned(buffer: &mut Buffer) -> Result<usize> {
    const RADIX: usize = 10;
    if buffer.cursor == buffer.end {
        return Err(Error::Eof);
    }
    let mut cursor = buffer.cursor;
    while cursor < buffer.end {
        // SAFETY: `cursor` is bounds checked
        if !unsafe { *cursor }.is_ascii_digit() {
            break;
        }
        // SAFETY: same as above
        cursor = unsafe { cursor.add(1) };
    }
    let len = cursor as usize - buffer.cursor as usize;
    debug_assert!(buffer.start as usize + len <= buffer.end as usize);
    // SAFETY: `start + len` is always less than or equal to `end`
    let digits = unsafe { buffer.peek_n(len).unwrap_unchecked() };
    if digits.is_empty() {
        return Err(Error::InvalidUnsigned);
    }
    let mut number = 0usize;
    // TODO: checking overflow twice is redundant - `number >= digit` is sufficient
    for &digit in digits {
        number = number
            .checked_mul(RADIX)
            .ok_or(Error::Overflow)?
            .checked_add(digit as usize - b'0' as usize)
            .ok_or(Error::Overflow)?;
        // if number < digit as usize {
        //     return Err(Error::Overflow);
        // }
    }
    // SAFETY: peeking `len` bytes (above) was successfull
    unsafe { buffer.advance(len) };
    Ok(number)
}

fn parse_header(buffer: &mut Buffer) -> Result<(FileType, Header)> {
    let magic = parse_magic(buffer)?;
    skip_space(buffer)?;
    let max_index = parse_unsigned(buffer)?;
    skip_space(buffer)?;
    let num_inputs = parse_unsigned(buffer)?;
    skip_space(buffer)?;
    let num_latches = parse_unsigned(buffer)?;
    skip_space(buffer)?;
    let num_outputs = parse_unsigned(buffer)?;
    skip_space(buffer)?;
    let num_gates = parse_unsigned(buffer)?;
    Ok((
        magic,
        Header {
            max_index,
            num_inputs,
            num_latches,
            num_outputs,
            num_gates,
        },
    ))
}

fn parse_literals(buffer: &mut Buffer, num_literals: usize) -> Result<Vec<Literal>> {
    let mut literals = Vec::with_capacity(num_literals);
    for _ in 0..num_literals {
        literals.push(Literal(parse_unsigned(buffer)?));
        skip_newline(buffer)?
    }
    Ok(literals)
}

fn parse_latches(buffer: &mut Buffer, num_latches: usize) -> Result<Vec<Latch>> {
    let mut latches = Vec::with_capacity(num_latches);
    for _ in 0..num_latches {
        let current_state = Literal(parse_unsigned(buffer)?);
        skip_space(buffer)?;
        let next_state = Literal(parse_unsigned(buffer)?);
        latches.push(Latch(current_state, next_state));
        skip_newline(buffer)?
    }
    Ok(latches)
}

fn parse_gates(buffer: &mut Buffer, num_gates: usize) -> Result<Vec<Gate>> {
    let mut gates = Vec::with_capacity(num_gates);
    for _ in 0..num_gates {
        let lhs = Literal(parse_unsigned(buffer)?);
        skip_space(buffer)?;
        let rhs1 = Literal(parse_unsigned(buffer)?);
        skip_space(buffer)?;
        let rhs2 = Literal(parse_unsigned(buffer)?);
        gates.push(Gate(lhs, rhs1, rhs2));
        skip_newline(buffer)?
    }
    Ok(gates)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_peek_past_end() {
        let mut buffer = Buffer::new(b"a");
        assert_eq!(buffer.peek().unwrap(), b'a');
        unsafe { buffer.bump() };
        assert_eq!(buffer.peek(), None);
        let mut buffer = Buffer::new(b"a");
        assert_eq!(buffer.peek_n(1).unwrap(), b"a");
        unsafe { buffer.advance(1) };
        assert_eq!(buffer.peek_n(1), None);
    }

    #[test]
    fn test_parse_magic() {
        let mut buffer = Buffer::new(b"aig");
        assert_eq!(parse_magic(&mut buffer).unwrap(), FileType::Binary);
        let mut buffer = Buffer::new(b"aag");
        assert_eq!(parse_magic(&mut buffer).unwrap(), FileType::Ascii);
        let mut buffer = Buffer::new(b"baa");
        assert_eq!(parse_magic(&mut buffer).unwrap_err(), Error::InvalidMagic);
    }

    #[test]
    fn test_parse_unsigned() {
        let mut buffer = Buffer::new(b"123");
        assert_eq!(parse_unsigned(&mut buffer).unwrap(), 123);
        let mut buffer = Buffer::new(b"12a3");
        assert_eq!(parse_unsigned(&mut buffer).unwrap(), 12);
        let mut buffer = Buffer::new(b"a123");
        assert_eq!(
            parse_unsigned(&mut buffer).unwrap_err(),
            Error::InvalidUnsigned
        );
        let mut input = format!("{}", usize::MAX);
        let max_tens_plus_one = format!("{}", (usize::MAX % 10) + 1);
        assert_eq!(max_tens_plus_one.len(), 1);
        input.replace_range(input.len() - 1.., &max_tens_plus_one);
        let mut buffer = Buffer::new(input.as_bytes());
        assert_eq!(parse_unsigned(&mut buffer).unwrap_err(), Error::Overflow);
        let mut input = format!("{}", usize::MAX);
        input.push('0');
        let mut buffer = Buffer::new(input.as_bytes());
        assert_eq!(parse_unsigned(&mut buffer).unwrap_err(), Error::Overflow);
    }

    #[test]
    fn test_parse_header() {
        let mut buffer = Buffer::new(b"aag 0 0 0 0 0");
        assert_eq!(
            parse_header(&mut buffer).unwrap(),
            (
                FileType::Ascii,
                Header {
                    max_index: 0,
                    num_inputs: 0,
                    num_latches: 0,
                    num_outputs: 0,
                    num_gates: 0
                }
            )
        );
        let mut buffer = Buffer::new(b"aag 10 4 3 2 1");
        assert_eq!(
            parse_header(&mut buffer).unwrap(),
            (
                FileType::Ascii,
                Header {
                    max_index: 10,
                    num_inputs: 4,
                    num_latches: 3,
                    num_outputs: 2,
                    num_gates: 1
                }
            )
        );
        let mut buffer = Buffer::new(b"aag 4 1 2 1a");
        assert_eq!(parse_header(&mut buffer).unwrap_err(), Error::ExpectedSpace);
        let mut buffer = Buffer::new(b"aag 4 1 2 1 ");
        assert_eq!(parse_header(&mut buffer).unwrap_err(), Error::Eof);
        let mut buffer = Buffer::new(b"aag 4 1 2 1 a");
        assert_eq!(
            parse_header(&mut buffer).unwrap_err(),
            Error::InvalidUnsigned
        );
    }
}
