pub fn add(left: usize, right: usize, unused: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2, 2);
        assert_eq!(result, 4);
    }
}
