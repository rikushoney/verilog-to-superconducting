use fnv::FnvHashMap;

use std::num::TryFromIntError;
use std::ops::{Index, IndexMut};

/// The supported number formats
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Number {
    /// 64-bit signed integer
    Int(i64),
    /// 64-bit IEEE 754 double precision float
    Float(f64),
}

impl From<i64> for Number {
    fn from(x: i64) -> Self {
        Self::Int(x)
    }
}

impl From<f64> for Number {
    fn from(x: f64) -> Self {
        Self::Float(x)
    }
}

macro_rules! impl_number_from {
    ($base:ty, $($prim:ty),*) => {
        $(
            impl From<$prim> for Number {
                fn from(x: $prim) -> Self {
                    Self::from(x as $base)
                }
            }
        )*
    };
}

impl_number_from!(i64, i8, u8, i16, u16, i32, u32);
impl_number_from!(f64, f32);

impl TryFrom<u64> for Number {
    type Error = TryFromIntError;

    fn try_from(x: u64) -> Result<Self, Self::Error> {
        Ok(Number::Int(x.try_into()?))
    }
}

macro_rules! impl_number_eq {
    ($variant:ident, $base:ty, $($prim:ty),*) => {
        $(
            impl PartialEq<$prim> for Number {
                fn eq(&self, x: &$prim) -> bool {
                    match self {
                        Number::$variant(y) => (*x as $base).eq(y),
                        _ => false,
                    }
                }
            }

            impl PartialEq<Number> for $prim {
                fn eq(&self, x: &Number) -> bool {
                    x.eq(self)
                }
            }
        )*
    };
}

impl_number_eq!(Int, i64, i8, u8, i16, u16, i32, u32, i64);
impl_number_eq!(Float, f64, f32, f64);

impl PartialEq<u64> for Number {
    fn eq(&self, x: &u64) -> bool {
        match self {
            Number::Int(y) => i64::try_from(*x).map_or(false, |x| x.eq(y)),
            _ => false,
        }
    }
}

impl PartialEq<Number> for u64 {
    fn eq(&self, x: &Number) -> bool {
        x.eq(self)
    }
}

pub type Dict = FnvHashMap<String, Value>;

/// The supported value types of component parameters
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Number(Number),
    String(String),
    List(Vec<Value>),
    Dict(Dict),
}

macro_rules! impl_value_as {
    ($as_fn:ident, $variant:ident, &$prim:ty) => {
        pub fn $as_fn(&self) -> Option<&$prim> {
            match self {
                Self::$variant(x) => Some(x),
                _ => None,
            }
        }
    };
    ($as_fn:ident, $variant:ident, $prim:ty) => {
        pub fn $as_fn(&self) -> Option<$prim> {
            match self {
                Self::$variant(x) => Some(*x),
                _ => None,
            }
        }
    };
}

impl Value {
    impl_value_as!(as_bool, Bool, bool);
    impl_value_as!(as_number, Number, Number);
    impl_value_as!(as_string, String, &str);
    impl_value_as!(as_list, List, &Vec<Value>);
    impl_value_as!(as_dict, Dict, &Dict);

    pub fn as_unit(&self) -> Option<()> {
        match self {
            Self::Unit => Some(()),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Self::Number(Number::Int(i)) => Some(*i),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Self::Number(Number::Float(f)) => Some(*f),
            _ => None,
        }
    }
}

macro_rules! impl_value_from {
    ($variant:ident, $base:ty) => {
        impl From<$base> for Value {
            fn from(v: $base) -> Self {
                Value::$variant(v)
            }
        }
    };
}

impl_value_from!(Bool, bool);
impl_value_from!(String, String);
impl_value_from!(List, Vec<Value>);
impl_value_from!(Dict, Dict);

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl<X: Into<Number>> From<X> for Value {
    fn from(x: X) -> Self {
        Self::Number(x.into())
    }
}

impl TryFrom<u64> for Value {
    type Error = TryFromIntError;

    fn try_from(x: u64) -> Result<Self, Self::Error> {
        Ok(Self::Number(x.try_into()?))
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Self::from(s.to_string())
    }
}

impl<const N: usize> From<[(String, Value); N]> for Value {
    fn from(dict: [(String, Value); N]) -> Self {
        let mut d = FnvHashMap::with_capacity_and_hasher(dict.len(), Default::default());
        for (k, v) in dict.into_iter() {
            d.insert(k, v);
        }
        Self::Dict(d)
    }
}

impl<const N: usize> From<[Value; N]> for Value {
    fn from(list: [Value; N]) -> Self {
        Self::List(list.into())
    }
}

impl<T: Into<Value>> FromIterator<T> for Value {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self::List(iter.into_iter().map(|x| x.into()).collect())
    }
}

impl<S: Into<String>, V: Into<Value>> FromIterator<(S, V)> for Value {
    fn from_iter<I: IntoIterator<Item = (S, V)>>(iter: I) -> Self {
        Self::Dict(Dict::from_iter(
            iter.into_iter().map(|(s, v)| (s.into(), v.into())),
        ))
    }
}

impl Index<usize> for Value {
    type Output = Value;

    fn index(&self, idx: usize) -> &Self::Output {
        self.as_list().expect("should be a list").index(idx)
    }
}

impl IndexMut<usize> for Value {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        let a = match self {
            Self::List(a) => Some(a),
            _ => None,
        };
        a.expect("should be a list").index_mut(idx)
    }
}

impl Index<&str> for Value {
    type Output = Value;

    fn index(&self, key: &str) -> &Self::Output {
        self.as_dict().expect("should be a dictionary").index(key)
    }
}

impl IndexMut<&str> for Value {
    fn index_mut(&mut self, key: &str) -> &mut Self::Output {
        let o = match self {
            Self::Dict(o) => Some(o),
            _ => None,
        };
        o.expect("should be a dictionary")
            .get_mut(key)
            .expect("key not in map")
    }
}

#[macro_export]
macro_rules! param {
    ($value:expr) => {
        Value::from($value)
    };
    ($($value:expr),*) => {
        Value::from([$(param!($value)),*])
    };
    ($($key:literal : $value:expr),*) => {
        Value::from([$((String::from($key), param!($value))),*])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_param_macro() {
        let value = param!(23);
        assert_eq!(value.as_number().unwrap(), 23);

        let value = param!("test");
        assert_eq!(value.as_string().unwrap(), "test");

        let value = param!["a", 1, true];
        let mut iter = value.as_list().unwrap().iter();
        assert_eq!(iter.next(), Some(&param!("a")));
        assert_eq!(iter.next(), Some(&param!(1)));
        assert_eq!(iter.next(), Some(&param!(true)));

        let value = param! {
            "width": 12.0,
            "height": 15.0,
            "test": true
        };
        let dict = value.as_dict().unwrap();
        assert_eq!(dict.get("width"), Some(&param!(12.0)));
        assert_eq!(dict.get("height"), Some(&param!(15.0)));
        assert_eq!(dict.get("test"), Some(&param!(true)));
    }
}
