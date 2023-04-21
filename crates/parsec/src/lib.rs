pub mod combinators;
pub mod sequence;
pub mod state;

use combinators::*;
use state::*;

pub trait Parsec {
    type Output;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output>;

    fn attempt(self) -> Attempt<Self>
    where
        Self: Sized,
    {
        Attempt(self)
    }

    fn then<F: FnMut(Self::Output) -> Option<TOut>, TOut>(self, mapper: F) -> Then<Self, F>
    where
        Self: Sized,
    {
        Then(self, mapper)
    }

    fn map<F: FnMut(Option<Self::Output>) -> Option<TOut>, TOut>(self, mapper: F) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map(self, mapper)
    }

    fn and<P: Parsec>(self, other: P) -> And<Self, P>
    where
        Self: Sized,
    {
        combinators::and(self, other)
    }

    fn left<P: Parsec>(self, other: P) -> Left<Self, P>
    where
        Self: Sized,
    {
        combinators::left(self, other)
    }

    fn right<P: Parsec>(self, other: P) -> Right<Self, P>
    where
        Self: Sized,
    {
        combinators::right(self, other)
    }

    fn between<P1: Parsec, P2: Parsec>(self, left: P1, right: P2) -> Between<Self, P1, P2>
    where
        Self: Sized,
    {
        combinators::between(self, left, right)
    }

    fn iter<'a, 'b>(self, state: &'a mut ParseState<'b>) -> Iter<'a, 'b, Self>
    where
        Self: Sized,
    {
        Iter::new(self, state)
    }
}

impl<P: Parsec> Parsec for &mut P {
    type Output = P::Output;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        (**self).parse(state)
    }
}

impl Parsec for char {
    type Output = char;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        state
            .next()
            .and_then(|ch| if *self == ch { Some(ch) } else { None })
    }
}

pub struct Attempt<P>(P);

impl<P: Parsec> Parsec for Attempt<P> {
    type Output = P::Output;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        let backtrack = state.clone();
        self.0.parse(state).or_else(|| {
            *state = backtrack;
            None
        })
    }
}

pub fn attempt<P: Parsec>(parser: P) -> Attempt<P> {
    Attempt(parser)
}

pub struct Then<P, F>(P, F);

impl<P: Parsec, F: FnMut(P::Output) -> Option<TOut>, TOut> Parsec for Then<P, F> {
    type Output = TOut;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        self.0.parse(state).and_then(|output| (self.1)(output))
    }
}

pub fn then<P: Parsec, F: FnMut(P::Output) -> Option<TOut>, TOut>(
    parser: P,
    mapper: F,
) -> Then<P, F> {
    Then(parser, mapper)
}

pub struct Map<P, F>(P, F);

impl<P: Parsec, F: FnMut(Option<P::Output>) -> Option<TOut>, TOut> Parsec for Map<P, F> {
    type Output = TOut;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        (self.1)(self.0.parse(state))
    }
}

pub struct Iter<'a, 'b, P>(Attempt<P>, &'a mut ParseState<'b>);

impl<'a, 'b, P: Parsec> Iter<'a, 'b, P> {
    pub fn new(parser: P, state: &'a mut ParseState<'b>) -> Self {
        Iter(attempt(parser), state)
    }
}

impl<'a, 'b, P: Parsec> Iterator for Iter<'a, 'b, P> {
    type Item = P::Output;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.parse(self.1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char() {
        let mut state = ParseState::new("a");
        assert_eq!('a'.parse(&mut state), Some('a'));
        assert_eq!(state.next(), None);
    }

    #[test]
    fn test_attempt() {
        let mut state = ParseState::new("a");
        assert_eq!(attempt('a').parse(&mut state), Some('a'));
        assert_eq!(state.next(), None);

        let mut state = ParseState::new("b");
        assert_eq!(attempt('a').parse(&mut state), None);
        assert_eq!(state.next(), Some('b'));
    }

    #[test]
    fn test_then() {
        let mut state = ParseState::new("a");
        assert_eq!('a'.then(|ch| { Some(ch) }).parse(&mut state), Some('a'));
    }

    #[test]
    fn test_map() {
        let mut state = ParseState::new("a");
        assert_eq!('a'.map(|ch| { ch }).parse(&mut state), Some('a'));
    }

    #[test]
    fn test_iter() {
        let mut state = ParseState::new("aaab");
        let mut iter = 'a'.iter(&mut state);
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.next(), Some('a'));
        assert_eq!(iter.next(), None);
        assert_eq!(state.next(), Some('b'));
    }
}
