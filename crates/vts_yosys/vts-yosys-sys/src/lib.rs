pub mod yosys {
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
    pub struct Design {
        inner: YosysDesignPtr,
    }

    impl Design {
        pub fn new() -> Self {
            Self {
                inner: unsafe { yosys_new_design() },
            }
        }
    }

    impl Drop for Design {
        fn drop(&mut self) {
            unsafe { yosys_delete_design(self.inner) };
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
