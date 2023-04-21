use super::*;

pub struct And<P1, P2>(P1, P2);

impl<P1: Parsec, P2: Parsec> Parsec for And<P1, P2> {
    type Output = (P1::Output, P2::Output);

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        self.0
            .parse(state)
            .and_then(|first| self.1.parse(state).and_then(|second| Some((first, second))))
    }
}

pub fn and<P1: Parsec, P2: Parsec>(first: P1, second: P2) -> And<P1, P2> {
    And(first, second)
}

pub struct Or<P1, P2>(P1, P2);

impl<P1: Parsec<Output = TOut>, P2: Parsec<Output = TOut>, TOut> Parsec for Or<P1, P2> {
    type Output = TOut;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        attempt(&mut self.0).parse(state).or(self.1.parse(state))
    }
}

pub struct Left<P1, P2>(P1, P2);

impl<P1: Parsec, P2: Parsec> Parsec for Left<P1, P2> {
    type Output = P1::Output;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        and(&mut self.0, &mut self.1)
            .then(|(left, _)| Some(left))
            .parse(state)
    }
}

pub fn left<P1: Parsec, P2: Parsec>(left: P1, right: P2) -> Left<P1, P2> {
    Left(left, right)
}

pub struct Right<P1, P2>(P1, P2);

impl<P1: Parsec, P2: Parsec> Parsec for Right<P1, P2> {
    type Output = P2::Output;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        and(&mut self.0, &mut self.1)
            .then(|(_, right)| Some(right))
            .parse(state)
    }
}

pub fn right<P1: Parsec, P2: Parsec>(left: P1, right: P2) -> Right<P1, P2> {
    Right(left, right)
}

pub struct Between<P, P1, P2>(P, P1, P2);

impl<P: Parsec, P1: Parsec, P2: Parsec> Parsec for Between<P, P1, P2> {
    type Output = P::Output;

    fn parse(&mut self, state: &mut ParseState) -> Option<Self::Output> {
        (&mut self.1)
            .right(&mut self.0)
            .left(&mut self.2)
            .parse(state)
    }
}

pub fn between<P: Parsec, P1: Parsec, P2: Parsec>(
    parser: P,
    left: P1,
    right: P2,
) -> Between<P, P1, P2> {
    Between(parser, left, right)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_and() {
        let mut state = ParseState::new("ab");
        assert_eq!('a'.and('b').parse(&mut state), Some(('a', 'b')));
    }

    #[test]
    fn test_left() {
        let mut state = ParseState::new("ab");
        assert_eq!('a'.left('b').parse(&mut state), Some('a'));
    }

    #[test]
    fn test_right() {
        let mut state = ParseState::new("ab");
        assert_eq!('a'.right('b').parse(&mut state), Some('b'));
    }

    #[test]
    fn test_between() {
        let mut state = ParseState::new("[a]");
        assert_eq!('a'.between('[', ']').parse(&mut state), Some('a'));
    }
}
