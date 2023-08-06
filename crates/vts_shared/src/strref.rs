use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StrRef<'a> {
    Owned(Arc<str>),
    Ref(&'a str),
}

impl From<&Arc<str>> for StrRef<'_> {
    fn from(s: &Arc<str>) -> Self {
        Self::Owned(s.clone())
    }
}

impl<'a> From<&'a str> for StrRef<'a> {
    fn from(s: &'a str) -> Self {
        Self::Ref(s)
    }
}
