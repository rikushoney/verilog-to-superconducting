use std::{collections::HashMap as Map, ops::Deref, slice, sync::Arc};

use uuid::Uuid;

use vts_shared::strref::StrRef;

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

/// The supported value types of component properties
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(Map<String, Value>),
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
    impl_as!(as_object, Object, &Map<String, Value>);
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
impl_from_value!(Object, Map<String, Value>);

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

fn unnamed_uuid() -> String {
    let uuid = Uuid::new_v4();
    format!(
        "Unnamed_{}",
        uuid.simple().encode_lower(&mut Uuid::encode_buffer())
    )
}

fn check_unnamed_and_length(name: &StrRef) -> (bool, bool) {
    (
        name.starts_with("Unnamed_"),
        name.len() == "Unnamed_".len() + 32,
    )
}

fn extract_uuid(name: &StrRef) -> Option<Uuid> {
    let (unnamed, len_good) = check_unnamed_and_length(name);
    if unnamed && len_good {
        Uuid::try_parse(&name["Unnamed_".len()..]).ok()
    } else {
        None
    }
}

/// A port for a component
#[derive(Clone, Debug, PartialEq)]
pub struct Port<'a> {
    pub name: StrRef<'a>,
    /// The port's width in terms of pin count
    pub width: usize,
}

impl<'a> Port<'a> {
    /// Create a new port with `name` and `width`
    pub fn new<S: Into<StrRef<'a>>>(name: S, width: usize) -> Self {
        Self {
            name: name.into(),
            width,
        }
    }

    /// Create a new, reference counted, port with `name` and `width`
    pub fn new_rc<S: Into<StrRef<'a>>>(name: S, width: usize) -> PortRef<'a> {
        PortRef::new(Self::new(name, width))
    }

    /// Create a new unnamed port with `width` and a unique identifier
    pub fn with_uuid(width: usize) -> Self {
        Self {
            name: unnamed_uuid().into(),
            width,
        }
    }

    /// Create a new, unnamed, reference counted port with `width` and a unique identifier
    pub fn with_uuid_rc(width: usize) -> PortRef<'a> {
        PortRef::new(Self::with_uuid(width))
    }

    /// Check if the port was created unnamed
    pub fn is_unnamed(&self) -> bool {
        check_unnamed_and_length(&self.name).0
    }

    /// Extract the unique identifier from the port name
    pub fn uuid(&self) -> Option<Uuid> {
        extract_uuid(&self.name)
    }

    /// Turn this port into a reference counted port
    pub fn make_ref(self) -> PortRef<'a> {
        PortRef::new(self)
    }
}

/// A counted reference to a port
#[derive(Clone, Debug, PartialEq)]
pub struct PortRef<'a>(Arc<Port<'a>>);

impl<'a> PortRef<'a> {
    /// Create a new reference counted port by consuming the original port
    pub fn new(port: Port<'a>) -> Self {
        Self(Arc::new(port))
    }
}

impl<'a> Deref for PortRef<'a> {
    type Target = Port<'a>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

/// An owned component
#[derive(Clone, Debug, PartialEq)]
pub struct Component<'a> {
    pub name: StrRef<'a>,
    pub children: Vec<ComponentRef<'a>>,
    pub ports: Vec<PortRef<'a>>,
    pub parameters: Map<String, Value>,
}

impl<'a> Component<'a> {
    /// Create a new empty component with `name`
    pub fn new<S: Into<StrRef<'a>>>(name: S) -> Self {
        Self {
            name: name.into(),
            children: Vec::new(),
            ports: Vec::new(),
            parameters: Map::new(),
        }
    }

    /// Create a new, reference counted, empty component with `name`
    pub fn new_rc<S: Into<StrRef<'a>>>(name: S) -> ComponentRef<'a> {
        ComponentRef::new(Self::new(name))
    }

    /// Create a new, unnamed, empty component with a unique identifier
    pub fn with_uuid() -> Self {
        Self::new(unnamed_uuid())
    }

    /// Create a new, unnamed, reference counted, empty component with a unique identifier
    pub fn with_uuid_rc() -> ComponentRef<'a> {
        ComponentRef::new(Self::with_uuid())
    }

    /// Check if the component was created unnamed
    pub fn is_unnamed(&self) -> bool {
        check_unnamed_and_length(&self.name).0
    }

    /// Extract the unique identifier from the component name
    pub fn uuid(&self) -> Option<Uuid> {
        extract_uuid(&self.name)
    }

    /// Get an iterator over the child components of this component
    pub fn children(&self) -> slice::Iter<'_, ComponentRef<'_>> {
        self.children.iter()
    }

    /// Get an iterator over the ports of this component
    pub fn ports(&self) -> slice::Iter<'_, PortRef<'_>> {
        self.ports.iter()
    }

    pub fn parameter(&self, key: &str) -> Option<&Value> {
        self.parameters.get(key)
    }

    /// Turn this component into a reference counted component
    pub fn make_ref(self) -> ComponentRef<'a> {
        ComponentRef::new(self)
    }
}

/// A counted reference to a component
#[derive(Clone, Debug, PartialEq)]
pub struct ComponentRef<'a>(Arc<Component<'a>>);

impl<'a> ComponentRef<'a> {
    /// Create a reference counted component consuming the original component
    pub fn new(c: Component<'a>) -> Self {
        Self(Arc::new(c))
    }
}

impl<'a> Deref for ComponentRef<'a> {
    type Target = Component<'a>;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<'a> Extend<ComponentRef<'a>> for Component<'a> {
    fn extend<T: IntoIterator<Item = ComponentRef<'a>>>(&mut self, iter: T) {
        self.children.extend(iter);
    }
}

impl<'a> Extend<PortRef<'a>> for Component<'a> {
    fn extend<T: IntoIterator<Item = PortRef<'a>>>(&mut self, iter: T) {
        self.ports.extend(iter);
    }
}

impl<'a> Extend<(String, Value)> for Component<'a> {
    fn extend<T: IntoIterator<Item = (String, Value)>>(&mut self, iter: T) {
        self.parameters.extend(iter);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComponentBuilder<'a> {
    name: Option<StrRef<'a>>,
    children: Vec<ComponentRef<'a>>,
    ports: Vec<Port<'a>>,
    parameters: Map<String, Value>,
}

impl<'a> ComponentBuilder<'a> {
    pub fn new() -> Self {
        Self {
            name: None,
            children: Vec::new(),
            ports: Vec::new(),
            parameters: Map::new(),
        }
    }

    pub fn name<S: Into<StrRef<'a>>>(mut self, name: S) -> Self {
        self.name = Some(name.into());
        self
    }

    pub fn child(mut self, c: ComponentRef<'a>) -> Self {
        self.children.push(c);
        self
    }

    pub fn children<I: IntoIterator<Item = ComponentRef<'a>>>(mut self, iter: I) -> Self {
        self.children.extend(iter);
        self
    }

    pub fn port(mut self, port: Port<'a>) -> Self {
        self.ports.push(port);
        self
    }

    pub fn ports<I: IntoIterator<Item = Port<'a>>>(mut self, iter: I) -> Self {
        self.ports.extend(iter);
        self
    }

    pub fn parameter<S: Into<String>>(mut self, key: S, val: Value) -> Self {
        self.parameters.insert(key.into(), val);
        self
    }

    pub fn parameters<S, I>(mut self, iter: I) -> Self
    where
        S: Into<String>,
        I: IntoIterator<Item = (S, Value)>,
    {
        self.parameters
            .extend(iter.into_iter().map(|(key, value)| (key.into(), value)));
        self
    }

    pub fn build(self) -> Component<'a> {
        let mut c = match self.name {
            Some(name) => Component::new(name),
            None => Component::with_uuid(),
        };
        c.extend(self.children.into_iter());
        c.extend(self.ports.into_iter().map(|p| p.make_ref()));
        c.extend(self.parameters.into_iter());
        c
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_componentref() {
        let test1 = ComponentBuilder::new().name("test1").build();
        assert_eq!(test1.name, "test1");

        let test2 = ComponentBuilder::new()
            .name("test2")
            .ports([Port::new("a", 1), Port::new("b", 2)])
            .child(test1.make_ref())
            .build()
            .make_ref();
        assert_eq!(test2.children().count(), 1);
        assert_eq!(test2.ports().count(), 2);
    }

    #[test]
    fn test_uuid() {
        let c = Component::new("test");
        assert!(!c.is_unnamed());
        assert!(c.uuid().is_none());

        // don't actually do this... use `Component::with_uuid` instead
        let uuid = Uuid::new_v4();
        let c = Component::new(format!(
            "Unnamed_{}",
            uuid.simple().encode_lower(&mut Uuid::encode_buffer())
        ));
        assert_eq!(check_unnamed_and_length(&c.name), (true, true));
        assert_eq!(c.uuid().unwrap(), uuid);

        let c = Component::new("Unnamed_aaa");
        assert_eq!(check_unnamed_and_length(&c.name), (true, false));
        assert!(c.uuid().is_none());
    }

    #[test]
    fn test_component_properties() {
        let c = ComponentBuilder::new()
            .name("test1")
            .parameter("param".to_string(), Value::Unit)
            .build();
        assert_eq!(c.parameter("param").unwrap(), &Value::Unit);

        let c = ComponentBuilder::new()
            .name("test2")
            .parameters([
                ("param", Value::Unit),
                ("other", Value::from(Number::from(1))),
            ])
            .build();
        assert_eq!(c.parameter("param").unwrap(), &Value::Unit);
        assert_eq!(c.parameter("other").unwrap(), &Value::from(1));
    }
}
