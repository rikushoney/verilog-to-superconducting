#[derive(Clone, Debug, PartialEq)]
pub(crate) struct StrSpan<'a> {
    span: &'a str,
    start: usize,
}

impl<'a> StrSpan<'a> {
    pub(crate) fn new(span: &'a str, start: usize) -> Self {
        Self { span, start }
    }

    pub(crate) fn span(&self) -> &'a str {
        self.span
    }

    pub(crate) fn start(&self) -> usize {
        self.start
    }
}
