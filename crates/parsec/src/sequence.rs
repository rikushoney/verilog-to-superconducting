use super::*;

use std::iter;
use std::marker::PhantomData;

pub struct Many<P, TOut>(P, PhantomData<TOut>);

impl<P: Parsec, TOut: Default + Extend<P::Output>> Parsec for Many<P, TOut> {
    type Output = TOut;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        let mut output = TOut::default();
        output.extend((&mut self.0).iter(state));
        Some(output)
    }
}

pub fn many<P: Parsec, TOut: Default + Extend<P::Output>>(parser: P) -> Many<P, TOut> {
    Many(parser, PhantomData)
}

pub struct ManyOne<P, TOut>(P, PhantomData<TOut>);

impl<P: Parsec, TOut: Default + Extend<P::Output>> Parsec for ManyOne<P, TOut> {
    type Output = TOut;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        self.0.parse(state).and_then(|first| {
            let mut output = TOut::default();
            output.extend(iter::once(first).chain((&mut self.0).iter(state)));
            Some(output)
        })
    }
}

pub fn many_one<P: Parsec, TOut: Default + Extend<P::Output>>(parser: P) -> ManyOne<P, TOut> {
    ManyOne(parser, PhantomData)
}

pub struct SkipMany<P>(P);

impl<P: Parsec> Parsec for SkipMany<P> {
    type Output = ();

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        while let Some(_) = attempt(&mut self.0).parse(state) {
            // do nothing
        }
        Some(())
    }
}

pub fn skip_many<P: Parsec>(parser: P) -> SkipMany<P> {
    SkipMany(parser)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_many() {
        let mut state = ParseState::new("aaab");
        assert_eq!(many('a').parse(&mut state), Some("aaa".to_string()));
        assert_eq!(state.next(), Some('b'));

        let mut state = ParseState::new("baaa");
        assert_eq!(many('a').parse(&mut state), Some("".to_string()));
        assert_eq!(state.next(), Some('b'));
    }

    #[test]
    fn test_many_one() {
        let mut state = ParseState::new("aaab");
        assert_eq!(many_one('a').parse(&mut state), Some("aaa".to_string()));
        assert_eq!(state.next(), Some('b'));

        let mut state = ParseState::new("baaa");
        assert_eq!(attempt(many_one::<_, String>('a')).parse(&mut state), None);
        assert_eq!(state.next(), Some('b'));
    }

    #[test]
    fn test_skip_many() {
        let mut state = ParseState::new("aaab");
        assert_eq!(skip_many('a').parse(&mut state), Some(()));
        assert_eq!(state.next(), Some('b'));

        let mut state = ParseState::new("baaaa");
        assert_eq!(skip_many('a').parse(&mut state), Some(()));
        assert_eq!(state.next(), Some('b'));
    }
}
