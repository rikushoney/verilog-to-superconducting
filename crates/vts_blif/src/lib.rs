pub mod ast;
pub mod emitter;
pub mod error;
pub mod parser;

use nom_locate::LocatedSpan;
pub type Span<'a> = LocatedSpan<&'a str>;
