use std::collections::HashMap;
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

pub type Map = HashMap<String, Value>;

/// The supported value types of component parameters
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(Map),
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
    impl_value_as!(as_array, Array, &Vec<Value>);
    impl_value_as!(as_object, Object, &Map);

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
impl_value_from!(Array, Vec<Value>);
impl_value_from!(Object, Map);

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
    fn from(a: [(String, Value); N]) -> Self {
        Self::Object(a.into())
    }
}

impl<T: Into<Value>> FromIterator<T> for Value {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self::Array(iter.into_iter().map(|x| x.into()).collect())
    }
}

impl<S: Into<String>, V: Into<Value>> FromIterator<(S, V)> for Value {
    fn from_iter<I: IntoIterator<Item = (S, V)>>(iter: I) -> Self {
        Self::Object(Map::from_iter(
            iter.into_iter().map(|(s, v)| (s.into(), v.into())),
        ))
    }
}

impl Index<usize> for Value {
    type Output = Value;

    fn index(&self, idx: usize) -> &Self::Output {
        self.as_array().expect("should be an array").index(idx)
    }
}

impl IndexMut<usize> for Value {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        let a = match self {
            Self::Array(a) => Some(a),
            _ => None,
        };
        a.expect("should be an array").index_mut(idx)
    }
}

impl Index<&str> for Value {
    type Output = Value;

    fn index(&self, key: &str) -> &Self::Output {
        self.as_object().expect("should be an object").index(key)
    }
}

impl IndexMut<&str> for Value {
    fn index_mut(&mut self, key: &str) -> &mut Self::Output {
        let o = match self {
            Self::Object(o) => Some(o),
            _ => None,
        };
        o.expect("should be an object")
            .get_mut(key)
            .expect("key not in map")
    }
}
