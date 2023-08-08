use std::{ops::Deref, slice, sync::Arc};

use uuid::Uuid;

use vts_shared::strref::StrRef;

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
}

/// An owned component
#[derive(Clone, Debug, PartialEq)]
pub struct Component<'a> {
    pub name: StrRef<'a>,
    pub children: Vec<ComponentRef<'a>>,
    pub ports: Vec<Port<'a>>,
}

impl<'a> Component<'a> {
    /// Create a new empty component with `name`
    pub fn new<S: Into<StrRef<'a>>>(name: S) -> Self {
        Self {
            name: name.into(),
            children: Vec::new(),
            ports: Vec::new(),
        }
    }

    /// Create a new, reference counted, empty component with `name`
    pub fn new_rc<S: Into<StrRef<'a>>>(name: S) -> ComponentRef<'a> {
        ComponentRef::new(Self::new(name))
    }

    /// Create a new, unnamed, empty component with a unique identifier
    pub fn with_uuid() -> Self {
        let uuid = Uuid::new_v4();
        let name = format!(
            "Unnamed_{}",
            uuid.simple().encode_lower(&mut Uuid::encode_buffer())
        );
        Self::new(name)
    }

    /// Create a new, unnamed, reference counted, empty component with a unique identifier
    pub fn with_uuid_rc() -> ComponentRef<'a> {
        ComponentRef::new(Self::with_uuid())
    }

    /// Check if the component starts with "Unnamed_" and long enough to hold a unique identifier
    fn check_unnamed_and_length(&self) -> (bool, bool) {
        (
            self.name.starts_with("Unnamed_"),
            self.name.len() == "Unnamed_".len() + 32,
        )
    }

    /// Check if the component was created unnamed
    pub fn is_unnamed(&self) -> bool {
        self.check_unnamed_and_length().0
    }

    /// Extract the unique identifier from the component name
    pub fn uuid(&self) -> Option<Uuid> {
        let (unnamed, len_good) = self.check_unnamed_and_length();
        if unnamed && len_good {
            Uuid::try_parse(&self.name["Unnamed_".len()..]).ok()
        } else {
            None
        }
    }

    /// Get an iterator over the child components of this component
    pub fn children(&self) -> slice::Iter<'_, ComponentRef<'_>> {
        self.children.iter()
    }

    /// Get an iterator over the ports of this component
    pub fn ports(&self) -> slice::Iter<'_, Port<'_>> {
        self.ports.iter()
    }

    /// Turn this component into a reference counted component
    pub fn make_ref(self) -> ComponentRef<'a> {
        ComponentRef::new(self)
    }
}

/// A reference to a component
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

impl<'a> Extend<Port<'a>> for Component<'a> {
    fn extend<T: IntoIterator<Item = Port<'a>>>(&mut self, iter: T) {
        self.ports.extend(iter);
    }
}

pub struct ComponentBuilder<'a> {
    name: Option<StrRef<'a>>,
    children: Vec<ComponentRef<'a>>,
    ports: Vec<Port<'a>>,
}

impl<'a> ComponentBuilder<'a> {
    pub fn new() -> Self {
        Self {
            name: None,
            children: Vec::new(),
            ports: Vec::new(),
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

    pub fn build(self) -> Component<'a> {
        let mut c = match self.name {
            Some(name) => Component::new(name),
            None => Component::with_uuid(),
        };
        c.extend(self.children.into_iter());
        c.extend(self.ports.into_iter());
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
        assert_eq!(c.check_unnamed_and_length(), (true, true));
        assert_eq!(c.uuid().unwrap(), uuid);

        let c = Component::new("Unnamed_aaa");
        assert_eq!(c.check_unnamed_and_length(), (true, false));
        assert!(c.uuid().is_none());
    }
}
