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
            Self::Number(n) => match n {
                Number::Int(i) => serializer.serialize_i64(*i),
                Number::Float(f) => serializer.serialize_f64(*f),
            },
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
