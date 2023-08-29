use crate::*;

use serde::de::{Deserialize, Deserializer, Error, MapAccess, SeqAccess, Unexpected, Visitor};

use std::fmt;

pub struct ValueVisitor;

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(ValueVisitor)
    }
}

impl<'de> Visitor<'de> for ValueVisitor {
    type Value = Value;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "a parameter value")
    }

    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Value::Unit)
    }

    fn visit_bool<E>(self, b: bool) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Value::Bool(b))
    }

    fn visit_i64<E>(self, x: i64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Value::Number(x.into()))
    }

    fn visit_u64<E>(self, x: u64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        if x <= i64::MAX as u64 {
            Ok((x as i64).into())
        } else {
            Err(E::invalid_value(
                Unexpected::Unsigned(x),
                &format!("a value between {} and {}", i64::MIN, i64::MAX).as_str(),
            ))
        }
    }

    fn visit_f64<E>(self, f: f64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Value::Number(f.into()))
    }

    fn visit_string<E>(self, s: String) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Value::String(s))
    }

    fn visit_str<E>(self, s: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Ok(Value::String(s.to_string()))
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut list = match seq.size_hint() {
            Some(n) => Vec::with_capacity(n),
            None => Vec::new(),
        };
        while let Some(i) = seq.next_element()? {
            list.push(i);
        }
        Ok(Value::List(list))
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut dict = match map.size_hint() {
            Some(n) => Dict::with_capacity_and_hasher(n, Default::default()),
            None => Dict::default(),
        };
        while let Some((k, v)) = map.next_entry()? {
            dict.insert(k, v);
        }
        Ok(Value::Dict(dict))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deserialize_json() {
        let value = param! {"test": 123};
        let output = serde_json::to_value(value).unwrap();
        assert_eq!(output, serde_json::json!({"test": 123}));
    }

    #[test]
    fn test_deserialize_yaml() {
        let value = param! {"test": 123};
        let output = serde_yaml::to_value(value).unwrap();
        assert_eq!(
            output,
            serde_yaml::from_str::<serde_yaml::Value>("test: 123").unwrap()
        );
    }

    #[test]
    fn test_deserialize_toml() {
        let value = param! {"test": 123};
        let output = toml::Table::try_from(value).unwrap();
        assert_eq!(output, toml::toml!(test = 123));
    }
}
