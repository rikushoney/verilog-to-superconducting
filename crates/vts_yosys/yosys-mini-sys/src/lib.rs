pub mod yosys {
    include!("bindings.rs");

    pub struct Design {
        inner: YosysDesignPtr,
    }

    impl Design {
        pub fn new() -> Self {
            Self {
                inner: unsafe { yosys_design_new() },
            }
        }
    }

    impl Drop for Design {
        fn drop(&mut self) {
            unsafe { yosys_design_delete(self.inner) };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::yosys::*;

    #[test]
    fn smoke_test() {
        let design = Design::new();
        drop(design);
    }
}
