use core::fmt;
use std::{cmp::Ordering, ops::Deref, sync::Arc};

/// A reference to a string that is either borrowed or reference counted
#[derive(Clone, Debug)]
pub enum StrRef<'a> {
    Borrowed(&'a str),
    Counted(Arc<str>),
}

impl<'a> StrRef<'a> {
    /// Create a new, reference counted, empty string
    pub fn new() -> Self {
        Self::new_counted("")
    }

    /// Create a new, reference counted, string from `s`
    pub fn new_counted<S: Into<Arc<str>>>(s: S) -> Self {
        Self::Counted(s.into())
    }

    /// Create a new, borrowed, string from `s`
    pub fn new_borrowed<S: AsRef<str> + ?Sized>(s: &'a S) -> Self {
        Self::Borrowed(s.as_ref())
    }

    /// Get a reference to the underlying string
    pub fn as_str(&self) -> &str {
        match self {
            Self::Counted(s) => s,
            Self::Borrowed(s) => s,
        }
    }
}

impl Default for StrRef<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for StrRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

impl<'a> From<&'a StrRef<'a>> for StrRef<'a> {
    fn from(s: &'a StrRef<'a>) -> Self {
        match s {
            Self::Borrowed(s) => Self::new_borrowed(s),
            Self::Counted(s) => Self::new_counted(s.clone()),
        }
    }
}

macro_rules! impl_from {
    ($from:ty, $to:ty, $conv:ident) => {
        impl<'a> From<$from> for $to {
            fn from(s: $from) -> Self {
                Self::$conv(s)
            }
        }
    };
}

macro_rules! impl_from_counted {
    ($from:ty, $to:ty) => {
        impl_from!($from, $to, new_counted);
    };
}

macro_rules! impl_from_borrowed {
    ($from:ty, $to:ty) => {
        impl_from!($from, $to, new_borrowed);
    };
}

impl_from_counted!(String, StrRef<'a>);
impl_from_counted!(Arc<str>, StrRef<'a>);

impl From<&Arc<str>> for StrRef<'_> {
    fn from(s: &Arc<str>) -> Self {
        Self::new_counted(s.clone())
    }
}

impl_from_borrowed!(&'a str, StrRef<'a>);
impl_from_borrowed!(&'a String, StrRef<'a>);

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

macro_rules! impl_eq {
    ($other:ty) => {
        impl<'a, 'b> PartialEq<$other> for StrRef<'a> {
            fn eq(&self, other: &$other) -> bool {
                PartialEq::eq(&self[..], &other[..])
            }
        }

        impl<'a, 'b> PartialEq<StrRef<'a>> for $other {
            fn eq(&self, other: &StrRef<'a>) -> bool {
                PartialEq::eq(&self[..], &other[..])
            }
        }
    };
}

impl_eq!(str);
impl_eq!(&'b str);
impl_eq!(String);
impl_eq!(&'b String);
impl_eq!(Arc<str>);
impl_eq!(&Arc<str>);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from() {
        let s = "test1";
        let s_ref = StrRef::from(s);
        assert_eq!(s, s_ref);
        assert_eq!(s.as_ptr(), s_ref.as_ptr());

        let s: Arc<str> = "test2".into();
        let s_ref = StrRef::from(&s);
        assert_eq!(s, s_ref);
        assert_eq!(s.as_ptr(), s_ref.as_ptr());
    }

    #[test]
    fn test_new_counted() {
        let s = "test1";
        let s_ref = StrRef::new_counted(s);
        assert_eq!(s, s_ref);
        assert!(matches!(s_ref, StrRef::Counted(..)));
        // `s` is _copied_ to `s_ref` => pointer comparison fails
        // assert_eq!(s.as_ptr(), s_ref.as_ptr());

        let s: Arc<str> = "test2".into();
        let s_ptr = s.as_ptr();
        let s_ref = StrRef::new_counted(s);
        assert!(matches!(s_ref, StrRef::Counted(..)));
        assert_eq!(s_ptr, s_ref.as_ptr());
    }

    #[test]
    fn test_new_borrowed() {
        let s = "test1";
        let s_ref = StrRef::new_borrowed(&s);
        assert!(matches!(s_ref, StrRef::Borrowed(..)));
        assert_eq!(s.as_ptr(), s_ref.as_ptr());

        let s = "test2".to_string();
        let s_ref = StrRef::new_borrowed(&s);
        assert!(matches!(s_ref, StrRef::Borrowed(..)));
        assert_eq!(s.as_ptr(), s_ref.as_ptr());
    }

    #[test]
    fn test_eqs() {
        let lhs: &str = "test1";
        let rhs = StrRef::from(lhs);
        assert!(PartialEq::eq(lhs, &rhs));
        // unsized locals are unstable
        // let lhs = *lhs;
        // assert!(PartialEq::eq(lhs, &rhs));
        assert!(PartialEq::eq("test1", &rhs));

        let lhs: String = "test2".to_string();
        let rhs = StrRef::from(&lhs);
        assert!(PartialEq::eq(&lhs, &rhs));
        let lhs: &String = &lhs;
        assert!(PartialEq::eq(&lhs, &rhs));

        let lhs: Arc<str> = Arc::from("test3");
        let rhs = StrRef::from(&lhs);
        assert!(PartialEq::eq(&lhs, &rhs));
        let lhs: &Arc<str> = &lhs;
        assert!(PartialEq::eq(&lhs, &rhs));
    }
}
