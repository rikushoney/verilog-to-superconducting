use std::{ops::Deref, sync::Arc};

use uuid::Uuid;

use vts_shared::strref::StrRef;

/// A port for a component
#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub struct Component<'a> {
    pub name: StrRef<'a>,
    pub ports: Vec<Port<'a>>,
}

impl<'a> Component<'a> {
    /// Create a new empty component with `name`
    pub fn new<S: Into<StrRef<'a>>>(name: S) -> Self {
        Self {
            name: name.into(),
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

    /// Turn this component into a reference counted component
    pub fn make_ref(self) -> ComponentRef<'a> {
        ComponentRef::new(self)
    }
}

/// A reference to a component
pub struct ComponentRef<'a>(Arc<Component<'a>>);

impl<'a> ComponentRef<'a> {
    /// Turn `c` into a reference counted component
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

#[cfg(test)]
mod tests {
    use super::*;

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
