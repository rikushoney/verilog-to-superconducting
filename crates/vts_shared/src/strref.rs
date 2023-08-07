use std::{cmp::Ordering, ops::Deref, sync::Arc};

#[derive(Clone, Debug)]
pub enum StrRef<'a> {
    Counted(Arc<str>),
    Ref(&'a str),
}

impl<'a> StrRef<'a> {
    pub fn new_counted<S: Into<Arc<str>>>(s: S) -> Self {
        Self::Counted(s.into())
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::Counted(s) => s,
            Self::Ref(s) => s,
        }
    }
}

impl From<&Arc<str>> for StrRef<'_> {
    fn from(s: &Arc<str>) -> Self {
        Self::Counted(s.clone())
    }
}

impl<'a> From<&'a str> for StrRef<'a> {
    fn from(s: &'a str) -> Self {
        Self::Ref(s)
    }
}

impl From<String> for StrRef<'_> {
    fn from(s: String) -> Self {
        Self::Counted(s.into())
    }
}

impl<'a> Deref for StrRef<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl<'a, T> AsRef<T> for StrRef<'a>
where
    T: ?Sized,
    <StrRef<'a> as Deref>::Target: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        self.deref().as_ref()
    }
}

impl PartialEq for StrRef<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for StrRef<'_> {}

impl Ord for StrRef<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl PartialOrd for StrRef<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from() {
        let s = "test1";
        let s_ref = StrRef::from(s);
        assert_eq!(*s, *s_ref);
        assert_eq!(s.as_ptr(), s_ref.as_ptr());

        let s: Arc<str> = "test2".into();
        let s_ref = StrRef::from(&s);
        assert_eq!(*s, *s_ref);
        assert_eq!(s.as_ptr(), s_ref.as_ptr());
    }

    #[test]
    fn test_new_counted() {
        let s = "test1";
        let s_ref = StrRef::new_counted(s);
        assert_eq!(*s, *s_ref);
        assert!(matches!(s_ref, StrRef::Counted(..)));
        // `s` is _copied_ to `s_ref` => pointer comparison fails
        // assert_eq!(s.as_ptr(), s_ref.as_ptr());

        let s: Arc<str> = "test2".into();
        let s_ptr = s.as_ptr();
        let s_ref = StrRef::new_counted(s);
        assert!(matches!(s_ref, StrRef::Counted(..)));
        assert_eq!(s_ptr, s_ref.as_ptr());
    }
}
