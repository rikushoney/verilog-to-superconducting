use crate::param;

use std::{ops::Deref, slice, sync::Arc};

use uuid::Uuid;

use vts_shared::strref::StrRef;

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
    pub parameters: param::Dict,
}

impl<'a> Component<'a> {
    /// Create a new empty component with `name`
    pub fn new<S: Into<StrRef<'a>>>(name: S) -> Self {
        Self {
            name: name.into(),
            children: Vec::new(),
            ports: Vec::new(),
            parameters: param::Dict::default(),
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

    /// Get the parameter value for `key`
    pub fn parameter(&self, key: &str) -> Option<&param::Value> {
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

impl<'a> Extend<(String, param::Value)> for Component<'a> {
    fn extend<T: IntoIterator<Item = (String, param::Value)>>(&mut self, iter: T) {
        self.parameters.extend(iter);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ComponentBuilder<'a> {
    name: Option<StrRef<'a>>,
    children: Vec<ComponentRef<'a>>,
    ports: Vec<Port<'a>>,
    parameters: param::Dict,
}

impl<'a> ComponentBuilder<'a> {
    pub fn new() -> Self {
        Self {
            name: None,
            children: Vec::new(),
            ports: Vec::new(),
            parameters: param::Dict::default(),
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

    pub fn parameter<S: Into<String>>(mut self, key: S, val: param::Value) -> Self {
        self.parameters.insert(key.into(), val);
        self
    }

    pub fn parameters<S, I>(mut self, iter: I) -> Self
    where
        S: Into<String>,
        I: IntoIterator<Item = (S, param::Value)>,
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
    use crate::Value;

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
        assert_eq!(c.uuid(), Some(uuid));

        let c = Component::new("Unnamed_aaa");
        assert_eq!(check_unnamed_and_length(&c.name), (true, false));
        assert!(c.uuid().is_none());
    }

    #[test]
    fn test_component_parameters() {
        let c = ComponentBuilder::new()
            .name("test1")
            .parameter("param", param!(()))
            .build();
        assert_eq!(c.parameter("param"), Some(&param!(())));

        let c = ComponentBuilder::new()
            .name("test2")
            .parameters([("param", param!(())), ("other", param!(1))])
            .build();
        assert_eq!(c.parameter("param"), Some(&param!(())));
        assert_eq!(c.parameter("other"), Some(&param!(1)));
    }
}
