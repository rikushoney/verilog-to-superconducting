pub trait Builder<T> {
    fn build(self) -> T;
}
