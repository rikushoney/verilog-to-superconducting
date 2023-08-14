use std::collections::HashMap;

/// The supported number formats
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Number {
    /// 64-bit signed integer
    Int(i64),
    /// 64-bit IEEE 754 double precision float
    Float(f64),
}

impl From<i64> for Number {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<f64> for Number {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

pub type Map = HashMap<String, Value>;

/// The supported value types of component properties
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(Map),
}

macro_rules! impl_as {
    ($as_fn:ident, $variant:ident, $primitive:ty) => {
        pub fn $as_fn(&self) -> Option<$primitive> {
            match self {
                Self::$variant(value) => Some(value),
                _ => None,
            }
        }
    };
}

impl Value {
    pub fn as_unit(&self) -> Option<()> {
        match self {
            Self::Unit => Some(()),
            _ => None,
        }
    }

    impl_as!(as_bool, Bool, &bool);
    impl_as!(as_number, Number, &Number);
    impl_as!(as_string, String, &str);
    impl_as!(as_array, Array, &Vec<Value>);
    impl_as!(as_object, Object, &Map);
}

macro_rules! impl_from_value {
    ($variant:ident, $primitive:ty) => {
        impl From<$primitive> for Value {
            fn from(value: $primitive) -> Self {
                Value::$variant(value)
            }
        }
    };
}

impl_from_value!(Bool, bool);
impl_from_value!(Number, Number);
impl_from_value!(String, String);
impl_from_value!(Array, Vec<Value>);
impl_from_value!(Object, Map);

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

macro_rules! impl_from_number {
    ($primitive:ty) => {
        impl From<$primitive> for Value {
            fn from(value: $primitive) -> Self {
                Value::Number(Number::from(value))
            }
        }
    };
}

impl_from_number!(i64);
impl_from_number!(f64);
