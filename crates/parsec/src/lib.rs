pub mod combinators;
pub mod sequence;
pub mod state;

pub use combinators::*;
pub use sequence::*;
pub use state::*;

use unicode_ident::{is_xid_continue, is_xid_start};

pub trait Parsec {
    type Output;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output>;

    fn attempt(self) -> Attempt<Self>
    where
        Self: Sized,
    {
        Attempt(self)
    }

    fn then<F>(self, mapper: F) -> Then<Self, F>
    where
        Self: Sized,
    {
        Then(self, mapper)
    }

    fn map<F>(self, mapper: F) -> Map<Self, F>
    where
        Self: Sized,
    {
        Map(self, mapper)
    }

    fn and<P>(self, other: P) -> And<Self, P>
    where
        Self: Sized,
    {
        combinators::and(self, other)
    }

    fn or<P>(self, other: P) -> Or<Self, P>
    where
        Self: Sized,
    {
        combinators::or(self, other)
    }

    fn left<P>(self, other: P) -> Left<Self, P>
    where
        Self: Sized,
    {
        combinators::left(self, other)
    }

    fn right<P>(self, other: P) -> Right<Self, P>
    where
        Self: Sized,
    {
        combinators::right(self, other)
    }

    fn between<P1, P2>(self, left: P1, right: P2) -> Between<Self, P1, P2>
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
        satisfy(|ch| ch == *self).parse(state)
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

pub fn attempt<P>(parser: P) -> Attempt<P> {
    Attempt(parser)
}

pub struct Satisfy<F>(F);

impl<F: Fn(char) -> bool> Parsec for Satisfy<F> {
    type Output = char;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        let output = state.next()?;
        if (self.0)(output) {
            Some(output)
        } else {
            None
        }
    }
}

pub fn satisfy<F>(predicate: F) -> Satisfy<F> {
    Satisfy(predicate)
}

pub struct Then<P, F>(P, F);

impl<P: Parsec, F: Fn(P::Output) -> POut, POut: Parsec> Parsec for Then<P, F> {
    type Output = POut::Output;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        (self.1)(self.0.parse(state)?).parse(state)
    }
}

pub fn then<P, F>(parser: P, mapper: F) -> Then<P, F> {
    Then(parser, mapper)
}

pub struct Map<P, F>(P, F);

impl<P: Parsec, F: Fn(P::Output) -> Option<TOut>, TOut> Parsec for Map<P, F> {
    type Output = TOut;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        (self.1)(self.0.parse(state)?)
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

pub fn unicode_ident() -> impl Parsec<Output = String> {
    satisfy(is_xid_start).then(|first| {
        many(satisfy(is_xid_continue)).map(move |mut rest: String| {
            rest.insert(0, first);
            Some(rest)
        })
    })
}

pub fn identifier() -> impl Parsec<Output = String> {
    let underscore = |ch| ch == '_';
    (satisfy(is_xid_start).or(satisfy(underscore))).then(move |first| {
        many(satisfy(is_xid_continue).or(satisfy(underscore))).map(move |mut rest: String| {
            rest.insert(0, first);
            Some(rest)
        })
    })
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
    fn test_satisfy() {
        let mut state = ParseState::new("ab");
        assert_eq!(satisfy(|ch| ch == 'a').parse(&mut state), Some('a'));
        assert_eq!(satisfy(|ch| ch == 'b').parse(&mut state), Some('b'));
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
        let mut state = ParseState::new("ab");
        assert_eq!('a'.then(|_| { 'b' }).parse(&mut state), Some('b'));
    }

    #[test]
    fn test_map() {
        let mut state = ParseState::new("a");
        assert_eq!('a'.map(|ch| { Some(ch) }).parse(&mut state), Some('a'));
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

    #[test]
    fn test_unicode_ident() {
        let mut state = ParseState::new("someVariable");
        assert_eq!(
            unicode_ident().parse(&mut state),
            Some("someVariable".to_string())
        );
        // please excuse this German speakers :)
        let mut state = ParseState::new("etwasVeränderlich");
        assert_eq!(
            unicode_ident().parse(&mut state),
            Some("etwasVeränderlich".to_string())
        );
        // emoji's are not identifiers :(
        let mut state = ParseState::new("😊");
        assert_eq!(unicode_ident().parse(&mut state), None);
    }
}
