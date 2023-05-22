use crate::Span;

pub enum BlifParseError<'a> {
    Expected(&'a str),
    Unknown,
}

impl<'a> BlifParseError<'a> {
    pub fn to_string(&self) -> String {
        match *self {
            Self::Expected(what) => format!("expected {}", what),
            _ => "unexpected error".to_string(),
        }
    }
}

pub enum BlifErrorKind<'a> {
    ParseError(BlifParseError<'a>),
}

pub struct BlifError<'a> {
    pub error: BlifErrorKind<'a>,
    pub span: Span<'a>,
}
