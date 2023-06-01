#![allow(dead_code)]
#![allow(clippy::missing_safety_doc)]

pub struct Buffer<'a> {
    start: *const u8,
    cursor: *const u8,
    end: *const u8,
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> Buffer<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        let start = bytes.as_ptr();
        // SAFETY: `bytes` is a valid slice
        let end = unsafe { start.add(bytes.len()) };
        Self {
            start,
            cursor: start,
            end,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn peek(&self) -> Option<u8> {
        if self.cursor < self.end {
            // SAFETY: `cursor` is bounds checked
            Some(unsafe { *self.cursor })
        } else {
            None
        }
    }

    pub fn peek_n(&self, count: usize) -> Option<&[u8]> {
        if self.cursor as usize + count <= self.end as usize {
            // SAFETY: `cursor + count` is bounds checked
            Some(unsafe { std::slice::from_raw_parts(self.cursor, count) })
        } else {
            None
        }
    }

    // SAFETY: same as `advance`
    pub unsafe fn bump(&mut self) {
        self.advance(1)
    }

    // SAFETY: this might increment `cursor` beyond `end`
    pub unsafe fn advance(&mut self, count: usize) {
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

struct Header {
    max_index: usize,
    num_inputs: usize,
    num_latches: usize,
    num_outputs: usize,
    num_gates: usize,
}

type Result<T> = std::result::Result<T, Error>;

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
    }
    // SAFETY: peeking `len` bytes (above) was successfull
    unsafe { buffer.advance(len) };
    Ok(number)
}

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

fn parse_header(buffer: &mut Buffer) -> Result<(FileType, Header)> {
    let magic = parse_magic(buffer)?;
    let max_index = parse_unsigned(buffer)?;
    skip_space(buffer)?;
    let num_inputs = parse_unsigned(buffer)?;
    skip_space(buffer)?;
    let num_latches = parse_unsigned(buffer)?;
    skip_space(buffer)?;
    let num_outputs = parse_unsigned(buffer)?;
    skip_space(buffer)?;
    let num_gates = parse_unsigned(buffer)?;
    skip_newline(buffer)?;
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
}
