use crate::param::*;

pub enum Error {
    IntegerOverflow,
    InvalidKey,
    InvalidNumber,
    UnsupportedDataType,
}

mod json {
    use super::*;

    use serde_json::{Map as JsonMap, Number as JsonNumber, Value as JsonValue};

    impl TryFrom<JsonNumber> for Number {
        type Error = Error;

        fn try_from(value: JsonNumber) -> Result<Self, Self::Error> {
            Ok(if value.is_i64() {
                Self::Int(value.as_i64().expect("should be i64"))
            } else if value.is_u64() {
                Self::Int(
                    i64::try_from(value.as_u64().expect("should be u64"))
                        .map_err(|_| Error::IntegerOverflow)?,
                )
            } else if value.is_f64() {
                Self::Float(value.as_f64().expect("should be f64"))
            } else {
                return Err(Error::InvalidNumber);
            })
        }
    }

    impl TryFrom<Number> for JsonNumber {
        type Error = Error;

        fn try_from(value: Number) -> Result<Self, Self::Error> {
            Ok(match value {
                Number::Int(i) => Self::from(i),
                Number::Float(f) => Self::from_f64(f).ok_or(Error::InvalidNumber)?,
            })
        }
    }

    impl TryFrom<JsonValue> for Value {
        type Error = Error;

        fn try_from(value: JsonValue) -> Result<Self, Self::Error> {
            Ok(match value {
                JsonValue::Null => Self::Unit,
                JsonValue::Bool(b) => Self::Bool(b),
                JsonValue::Number(n) => Self::Number(n.try_into()?),
                JsonValue::String(s) => Self::String(s),
                JsonValue::Array(a) => {
                    let mut v = Vec::with_capacity(a.len());
                    for i in a.into_iter() {
                        v.push(i.try_into()?);
                    }
                    Self::Array(v)
                }
                JsonValue::Object(o) => {
                    let mut m = Map::with_capacity(o.len());
                    for (k, v) in o.into_iter() {
                        m.insert(k, v.try_into()?);
                    }
                    Self::Object(m)
                }
            })
        }
    }

    impl TryFrom<Value> for JsonValue {
        type Error = Error;

        fn try_from(value: Value) -> Result<Self, Self::Error> {
            Ok(match value {
                Value::Unit => Self::Null,
                Value::Bool(b) => Self::Bool(b),
                Value::Number(n) => Self::Number(n.try_into()?),
                Value::String(s) => Self::String(s),
                Value::Array(a) => {
                    let mut v = Vec::with_capacity(a.len());
                    for i in a.into_iter() {
                        v.push(i.try_into()?);
                    }
                    Self::Array(v)
                }
                Value::Object(o) => {
                    let mut m = JsonMap::with_capacity(o.len());
                    for (k, v) in o.into_iter() {
                        m.insert(k, v.try_into()?);
                    }
                    Self::Object(m)
                }
            })
        }
    }
}

mod yaml {
    use super::*;

    use serde_yaml::{Mapping as YamlMap, Number as YamlNumber, Value as YamlValue};

    impl TryFrom<YamlNumber> for Number {
        type Error = Error;

        fn try_from(value: YamlNumber) -> Result<Self, Self::Error> {
            Ok(if value.is_i64() {
                Self::Int(value.as_i64().expect("should be i64"))
            } else if value.is_u64() {
                Self::Int(
                    i64::try_from(value.as_u64().expect("should be u64"))
                        .map_err(|_| Error::IntegerOverflow)?,
                )
            } else if value.is_f64() {
                Self::Float(value.as_f64().expect("should be f64"))
            } else {
                return Err(Error::InvalidNumber);
            })
        }
    }

    impl TryFrom<Number> for YamlNumber {
        type Error = Error;

        fn try_from(value: Number) -> Result<Self, Self::Error> {
            Ok(match value {
                Number::Int(i) => Self::from(i),
                Number::Float(f) => Self::from(f),
            })
        }
    }

    impl TryFrom<YamlValue> for Value {
        type Error = Error;

        fn try_from(value: YamlValue) -> Result<Self, Self::Error> {
            Ok(match value {
                YamlValue::Null => Self::Unit,
                YamlValue::Bool(b) => Self::Bool(b),
                YamlValue::Number(n) => Self::Number(n.try_into()?),
                YamlValue::String(s) => Self::String(s),
                YamlValue::Sequence(s) => {
                    let mut v = Vec::with_capacity(s.len());
                    for i in s.into_iter() {
                        v.push(i.try_into()?);
                    }
                    Self::Array(v)
                }
                YamlValue::Mapping(m) => {
                    let mut o = Map::with_capacity(m.len());
                    for (k, v) in m.into_iter() {
                        o.insert(
                            k.as_str().ok_or(Error::InvalidKey)?.to_string(),
                            v.try_into()?,
                        );
                    }
                    Self::Object(o)
                }
                YamlValue::Tagged(_) => {
                    return Err(Error::UnsupportedDataType);
                }
            })
        }
    }

    impl TryFrom<Value> for YamlValue {
        type Error = Error;

        fn try_from(value: Value) -> Result<Self, Self::Error> {
            Ok(match value {
                Value::Unit => Self::Null,
                Value::Bool(b) => Self::Bool(b),
                Value::Number(n) => Self::Number(n.try_into()?),
                Value::String(s) => Self::String(s),
                Value::Array(a) => {
                    let mut v = Vec::with_capacity(a.len());
                    for i in a.into_iter() {
                        v.push(i.try_into()?);
                    }
                    Self::Sequence(v)
                }
                Value::Object(o) => {
                    let mut m = YamlMap::with_capacity(o.len());
                    for (k, v) in o.into_iter() {
                        m.insert(Self::String(k), v.try_into()?);
                    }
                    Self::Mapping(m)
                }
            })
        }
    }
}

mod toml {
    use super::*;

    use ::toml::{Table as TomlTable, Value as TomlValue};

    impl TryFrom<TomlValue> for Number {
        type Error = Error;

        fn try_from(value: TomlValue) -> Result<Self, Self::Error> {
            Ok(match value {
                TomlValue::Integer(i) => Self::Int(i),
                TomlValue::Float(f) => Self::Float(f),
                _ => {
                    return Err(Error::InvalidNumber);
                }
            })
        }
    }

    impl TryFrom<Number> for TomlValue {
        type Error = Error;

        fn try_from(value: Number) -> Result<Self, Self::Error> {
            Ok(match value {
                Number::Int(i) => Self::Integer(i),
                Number::Float(f) => Self::Float(f),
            })
        }
    }

    impl TryFrom<TomlValue> for Value {
        type Error = Error;

        fn try_from(value: TomlValue) -> Result<Self, Self::Error> {
            Ok(match value {
                TomlValue::String(s) => Self::String(s),
                TomlValue::Integer(i) => Self::Number(Number::Int(i)),
                TomlValue::Float(f) => Self::Number(Number::Float(f)),
                TomlValue::Boolean(b) => Self::Bool(b),
                TomlValue::Datetime(_) => {
                    return Err(Error::UnsupportedDataType);
                }
                TomlValue::Array(a) => {
                    let mut v = Vec::with_capacity(a.len());
                    for i in a.into_iter() {
                        v.push(Self::try_from(i)?);
                    }
                    Self::Array(v)
                }
                TomlValue::Table(t) => {
                    let mut o = Map::with_capacity(t.len());
                    for (k, v) in t.into_iter() {
                        o.insert(k, Self::try_from(v)?);
                    }
                    Self::Object(o)
                }
            })
        }
    }

    impl TryFrom<Value> for TomlValue {
        type Error = Error;

        fn try_from(value: Value) -> Result<Self, Self::Error> {
            Ok(match value {
                Value::Unit => Self::Table(TomlTable::new()),
                Value::Bool(b) => Self::Boolean(b),
                Value::Number(n) => n.try_into()?,
                Value::String(s) => Self::String(s),
                Value::Array(a) => {
                    let mut v = Vec::with_capacity(a.len());
                    for i in a.into_iter() {
                        v.push(i.try_into()?);
                    }
                    Self::Array(v)
                }
                Value::Object(o) => {
                    let mut t = TomlTable::with_capacity(o.len());
                    for (k, v) in o.into_iter() {
                        t.insert(k, v.try_into()?);
                    }
                    Self::Table(t)
                }
            })
        }
    }
}

pub use self::toml::*;
pub use json::*;
pub use yaml::*;
