use crate::*;

use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Unit => serializer.serialize_unit(),
            Self::Bool(b) => serializer.serialize_bool(*b),
            Self::Number(Number::Int(i)) => serializer.serialize_i64(*i),
            Self::Number(Number::Float(f)) => serializer.serialize_f64(*f),
            Self::String(s) => serializer.serialize_str(&s),
            Self::Array(a) => {
                let mut seq = serializer.serialize_seq(Some(a.len()))?;
                for i in a.into_iter() {
                    seq.serialize_element(i)?;
                }
                seq.end()
            }
            Self::Object(o) => {
                let mut map = serializer.serialize_map(Some(o.len()))?;
                for (k, v) in o.into_iter() {
                    map.serialize_entry(k, v)?;
                }
                map.end()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_serialize_json() {
        let input = serde_json::json!({"test": 123});
        let value: Value = serde_json::from_value(input).unwrap();
        assert_eq!(
            value,
            Value::Object(Map::from([("test".to_string(), 123.into())]))
        );
    }

    #[test]
    fn test_serialize_yaml() {
        let input = "test: 123";
        let value: Value = serde_yaml::from_str(input).unwrap();
        assert_eq!(
            value,
            Value::Object(Map::from([("test".to_string(), 123.into())]))
        );
    }

    #[test]
    fn test_serialize_toml() {
        let input = toml::toml!(test = 123);
        let value: Value = input.try_into().unwrap();
        assert_eq!(
            value,
            Value::Object(Map::from([("test".to_string(), 123.into())]))
        );
    }
}
