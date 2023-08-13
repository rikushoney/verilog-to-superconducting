use crate::*;

mod json {
    use super::*;

    use serde_json::{Map as JsonMap, Number as JsonNumber, Value as JsonValue};

    impl From<JsonNumber> for Number {
        fn from(value: JsonNumber) -> Self {
            if value.is_i64() {
                Self::Int(value.as_i64().expect("should be i64"))
            } else if value.is_u64() {
                Self::Int(value.as_u64().expect("should be u64") as i64)
            } else if value.is_f64() {
                Self::Float(value.as_f64().expect("should be f64"))
            } else {
                unreachable!("should be either i64, u64 or f64")
            }
        }
    }

    impl From<Number> for JsonNumber {
        fn from(value: Number) -> Self {
            match value {
                Number::Int(i) => Self::from(i),
                Number::Float(f) => Self::from_f64(f).expect("should be valid f64"),
            }
        }
    }

    impl From<JsonValue> for Value {
        fn from(value: JsonValue) -> Self {
            match value {
                JsonValue::Null => Self::Unit,
                JsonValue::Bool(b) => Self::Bool(b),
                JsonValue::Number(n) => Self::Number(Number::from(n)),
                JsonValue::String(s) => Self::String(s),
                JsonValue::Array(a) => Self::Array(a.into_iter().map(|i| i.into()).collect()),
                JsonValue::Object(o) => {
                    Self::Object(Map::from_iter(o.into_iter().map(|(k, v)| (k, v.into()))))
                }
            }
        }
    }

    impl From<Value> for JsonValue {
        fn from(value: Value) -> Self {
            match value {
                Value::Unit => Self::Null,
                Value::Bool(b) => Self::Bool(b),
                Value::Number(n) => Self::Number(n.into()),
                Value::String(s) => Self::String(s),
                Value::Array(a) => Self::Array(a.into_iter().map(|i| i.into()).collect()),
                Value::Object(o) => Self::Object(JsonMap::from_iter(
                    o.into_iter().map(|(k, v)| (k, v.into())),
                )),
            }
        }
    }
}

mod yaml {
    use super::*;

    use serde_yaml::{Mapping as YamlMap, Number as YamlNumber, Value as YamlValue};

    impl From<YamlNumber> for Number {
        fn from(value: YamlNumber) -> Self {
            if value.is_i64() {
                Number::from(value.as_i64().expect("should be i64"))
            } else if value.is_u64() {
                Number::from(value.as_u64().expect("should be u64") as i64)
            } else if value.is_f64() {
                Number::from(value.as_f64().expect("should be f64"))
            } else {
                unreachable!("should be i64, u64 or f64")
            }
        }
    }

    impl From<Number> for YamlNumber {
        fn from(value: Number) -> Self {
            match value {
                Number::Int(i) => Self::from(i),
                Number::Float(f) => Self::from(f),
            }
        }
    }

    impl From<YamlValue> for Value {
        fn from(value: YamlValue) -> Self {
            match value {
                YamlValue::Null => Self::Unit,
                YamlValue::Bool(b) => Self::Bool(b),
                YamlValue::Number(n) => Self::Number(n.into()),
                YamlValue::String(s) => Self::String(s),
                YamlValue::Sequence(s) => Self::Array(s.into_iter().map(|i| i.into()).collect()),
                YamlValue::Mapping(m) => {
                    Self::Object(Map::from_iter(m.into_iter().map(|(k, v)| {
                        (k.as_str().expect("should be string").to_string(), v.into())
                    })))
                }
                YamlValue::Tagged(_) => {
                    unreachable!("tagged values are not supported")
                }
            }
        }
    }

    impl From<Value> for YamlValue {
        fn from(value: Value) -> Self {
            match value {
                Value::Unit => Self::Null,
                Value::Bool(b) => Self::Bool(b),
                Value::Number(n) => Self::Number(n.into()),
                Value::String(s) => Self::String(s),
                Value::Array(a) => Self::Sequence(a.into_iter().map(|i| i.into()).collect()),
                Value::Object(o) => Self::Mapping(YamlMap::from_iter(
                    o.into_iter().map(|(k, v)| (k.into(), v.into())),
                )),
            }
        }
    }
}

mod toml {
    use super::*;

    use ::toml::{map::Map as TomlMap, Value as TomlValue};

    impl From<TomlValue> for Number {
        fn from(value: TomlValue) -> Self {
            match value {
                TomlValue::Integer(i) => Self::Int(i),
                TomlValue::Float(f) => Self::Float(f),
                _ => {
                    unreachable!("should be i64 or f64")
                }
            }
        }
    }

    impl From<Number> for TomlValue {
        fn from(value: Number) -> Self {
            match value {
                Number::Int(i) => Self::Integer(i),
                Number::Float(f) => Self::Float(f),
            }
        }
    }

    impl From<TomlValue> for Value {
        fn from(value: TomlValue) -> Self {
            match value {
                TomlValue::String(s) => Self::String(s),
                TomlValue::Integer(i) => Self::Number(Number::Int(i)),
                TomlValue::Float(f) => Self::Number(Number::Float(f)),
                TomlValue::Boolean(b) => Self::Bool(b),
                TomlValue::Datetime(_) => {
                    unreachable!("datetime is not supported")
                }
                TomlValue::Array(a) => Self::Array(a.into_iter().map(|i| i.into()).collect()),
                TomlValue::Table(t) => {
                    Self::Object(Map::from_iter(t.into_iter().map(|(k, v)| (k, v.into()))))
                }
            }
        }
    }

    impl From<Value> for TomlValue {
        fn from(value: Value) -> Self {
            match value {
                Value::Unit => Self::Array(Vec::new()),
                Value::Bool(b) => Self::Boolean(b),
                Value::Number(n) => Self::from(n),
                Value::String(s) => Self::String(s),
                Value::Array(a) => Self::Array(a.into_iter().map(|i| i.into()).collect()),
                Value::Object(o) => Self::Table(TomlMap::from_iter(
                    o.into_iter().map(|(k, v)| (k, v.into())),
                )),
            }
        }
    }
}

pub use self::toml::*;
pub use json::*;
pub use yaml::*;
