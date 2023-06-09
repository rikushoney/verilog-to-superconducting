#[derive(Debug)]
pub enum Error {
    Eof,
    Expected(String),
    InvalidLatchTrigger,
    InvalidLogicValue,
    IoError(std::io::Error),
    MultipleInitValues,
    MultipleOutputs,
    Unexpected(String),
}

macro_rules! expected {
    ($fmt:expr, $($args:tt)*) => {
        Error::Expected(format!($fmt, $($args)*))
    };
    ($what:expr) => {
        Error::Expected($what.to_string())
    }
}

macro_rules! unexpected {
    ($fmt:expr, $($args:tt)*) => {
        Error::Unexpected(format!($fmt, $($args)*))
    };
    ($what:expr) => {
        Error::Unexpected($what.to_string())
    }
}
