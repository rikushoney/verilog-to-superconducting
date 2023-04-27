use std::iter;
use std::marker::PhantomData;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Position {
    pub column: isize,
    pub line: isize,
}

impl Position {
    pub fn new(column: isize, line: isize) -> Self {
        Self { column, line }
    }
}

pub trait ParseError<State> {
    fn position(&self) -> Position;
    fn empty(position: Position) -> Self;
    fn with_state(position: Position, state: State) -> Self;
}

pub type ParseResult<Output, State, Error> = Result<(Output, State), Error>;

pub trait ParseState {
    type Tok;

    fn next_token(&mut self) -> Option<Self::Tok>;
    fn position(&self) -> Position;
    fn empty() -> Self;
}

pub trait Parser<State: ParseState, Error: ParseError<State>> {
    type Output;

    fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error>;

    fn and_then<F>(self, mapper: F) -> combinator::AndThen<Self, F>
    where
        Self: Sized,
    {
        combinator::and_then(self, mapper)
    }

    fn optional(self) -> combinator::Optional<Self>
    where
        Self: Sized,
    {
        combinator::optional(self)
    }

    fn left<R>(self) -> combinator::Either<Self, R>
    where
        Self: Sized,
    {
        combinator::left(self)
    }

    fn right<L>(self) -> combinator::Either<L, Self>
    where
        Self: Sized,
    {
        combinator::right(self)
    }

    fn then<F>(self, mapper: F) -> sequence::Then<Self, F>
    where
        Self: Sized,
    {
        sequence::then(self, mapper)
    }

    fn and<P>(self, other: P) -> sequence::And<Self, P>
    where
        Self: Sized,
    {
        sequence::and(self, other)
    }

    fn skip<P>(self, parser: P) -> sequence::Skip<Self, P>
    where
        Self: Sized,
    {
        sequence::skip(self, parser)
    }

    fn with<P>(self, parser: P) -> sequence::With<Self, P>
    where
        Self: Sized,
    {
        sequence::with(self, parser)
    }

    fn iter(self, state: &'_ mut State) -> repeat::Iter<'_, State, Error, Self>
    where
        Self: Sized,
    {
        repeat::iter(self, state)
    }

    fn many<Output>(self) -> repeat::Many<Self, Output>
    where
        Self: Sized,
    {
        repeat::many(self)
    }

    fn many1<Output>(self) -> repeat::Many1<Self, Output>
    where
        Self: Sized,
    {
        repeat::many1(self)
    }

    fn skip_many(self) -> repeat::SkipMany<Self>
    where
        Self: Sized,
    {
        repeat::skip_many(self)
    }
}

impl<State, Error, P> Parser<State, Error> for &mut P
where
    State: ParseState,
    Error: ParseError<State>,
    P: Parser<State, Error>,
{
    type Output = P::Output;

    fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
        (**self).parse(state)
    }
}

pub mod combinator {
    use super::*;

    #[derive(Clone, Copy)]
    pub struct Attempt<P>(P);

    #[derive(Clone, Copy)]
    pub struct AttemptError<State, Error>(Error, State);

    impl<State, Error> ParseError<State> for AttemptError<State, Error>
    where
        State: ParseState,
        Error: ParseError<State>,
    {
        fn position(&self) -> Position {
            self.0.position()
        }

        fn empty(position: Position) -> Self {
            AttemptError(Error::empty(position), State::empty())
        }

        fn with_state(position: Position, state: State) -> Self {
            AttemptError(Error::empty(position), state)
        }
    }

    impl<State, Error, P> Parser<State, AttemptError<State, Error>> for Attempt<P>
    where
        State: ParseState + Clone,
        Error: ParseError<State>,
        P: Parser<State, Error>,
    {
        type Output = P::Output;

        fn parse(
            &mut self,
            state: State,
        ) -> ParseResult<Self::Output, State, AttemptError<State, Error>> {
            self.0
                .parse(state.clone())
                .map_err(|err| AttemptError(err, state))
        }
    }

    #[derive(Clone, Copy)]
    pub struct AndThen<P, F>(P, F);

    impl<State, Error, P, F, Output> Parser<State, Error> for AndThen<P, F>
    where
        State: ParseState,
        Error: ParseError<State>,
        P: Parser<State, Error>,
        F: FnMut(P::Output, State) -> ParseResult<Output, State, Error>,
    {
        type Output = Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let (output, state) = self.0.parse(state)?;
            (self.1)(output, state)
        }
    }

    pub fn and_then<P, F>(parser: P, mapper: F) -> AndThen<P, F> {
        AndThen(parser, mapper)
    }

    #[derive(Clone, Copy)]
    pub struct Optional<P>(P);

    impl<State, Error, P> Parser<State, Error> for Optional<P>
    where
        State: ParseState + Clone,
        Error: ParseError<State>,
        P: Parser<State, Error>,
    {
        type Output = Option<P::Output>;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let backtrack = state.clone();
            if let Ok((output, state)) = self.0.parse(state) {
                Ok((Some(output), state))
            } else {
                Ok((None, backtrack))
            }
        }
    }

    pub fn optional<P>(parser: P) -> Optional<P> {
        Optional(parser)
    }

    #[derive(Clone, Copy)]
    pub enum Either<L, R> {
        Left(L),
        Right(R),
    }

    impl<State, Error, L, R, Output> Parser<State, Error> for Either<L, R>
    where
        State: ParseState,
        Error: ParseError<State>,
        L: Parser<State, Error, Output = Output>,
        R: Parser<State, Error, Output = Output>,
    {
        type Output = Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            match *self {
                Either::Left(ref mut left) => left.parse(state),
                Either::Right(ref mut right) => right.parse(state),
            }
        }
    }

    pub fn left<L, R>(left: L) -> Either<L, R> {
        Either::Left(left)
    }

    pub fn right<L, R>(right: R) -> Either<L, R> {
        Either::Right(right)
    }
}

pub mod sequence {
    use super::*;

    #[derive(Clone, Copy)]
    pub struct Then<P, F>(P, F);

    impl<State, Error, P1, P2, F> Parser<State, Error> for Then<P1, F>
    where
        State: ParseState,
        Error: ParseError<State>,
        P1: Parser<State, Error>,
        P2: Parser<State, Error>,
        F: FnMut(P1::Output) -> P2,
    {
        type Output = P2::Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let (output, state) = self.0.parse(state)?;
            (self.1)(output).parse(state)
        }
    }

    pub fn then<P, F>(parser: P, mapper: F) -> Then<P, F> {
        Then(parser, mapper)
    }

    #[derive(Clone, Copy)]
    pub struct And<P1, P2>(P1, P2);

    impl<State, Error, P1, P2> Parser<State, Error> for And<P1, P2>
    where
        State: ParseState,
        Error: ParseError<State>,
        P1: Parser<State, Error>,
        P2: Parser<State, Error>,
    {
        type Output = (P1::Output, P2::Output);

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let (first, state) = self.0.parse(state)?;
            let (second, state) = self.1.parse(state)?;
            Ok(((first, second), state))
        }
    }

    pub fn and<P1, P2>(first: P1, second: P2) -> And<P1, P2> {
        And(first, second)
    }

    #[derive(Clone, Copy)]
    pub struct Skip<P1, P2>(P1, P2);

    impl<State, Error, P1, P2> Parser<State, Error> for Skip<P1, P2>
    where
        State: ParseState,
        Error: ParseError<State>,
        P1: Parser<State, Error>,
        P2: Parser<State, Error>,
    {
        type Output = P1::Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let (first, state) = self.0.parse(state)?;
            let (_, state) = self.1.parse(state)?;
            Ok((first, state))
        }
    }

    pub fn skip<P1, P2>(first: P1, second: P2) -> Skip<P1, P2> {
        Skip(first, second)
    }

    #[derive(Clone, Copy)]
    pub struct With<P1, P2>(P1, P2);

    impl<State, Error, P1, P2> Parser<State, Error> for With<P1, P2>
    where
        State: ParseState,
        Error: ParseError<State>,
        P1: Parser<State, Error>,
        P2: Parser<State, Error>,
    {
        type Output = P2::Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let (_, state) = self.0.parse(state)?;
            let (second, state) = self.1.parse(state)?;
            Ok((second, state))
        }
    }

    pub fn with<P1, P2>(first: P1, second: P2) -> With<P1, P2> {
        With(first, second)
    }
}

pub mod repeat {
    use super::*;

    pub struct Iter<'a, State, Error, P>(P, &'a mut State, PhantomData<Error>);

    impl<State, Error, P> Iterator for Iter<'_, State, Error, P>
    where
        State: ParseState + Clone,
        Error: ParseError<State>,
        P: Parser<State, Error>,
    {
        type Item = P::Output;

        fn next(&mut self) -> Option<Self::Item> {
            let state = self.1.clone();
            if let Ok((output, state)) = self.0.parse(state) {
                *self.1 = state;
                Some(output)
            } else {
                None
            }
        }
    }

    pub fn iter<State, Error, P>(parser: P, state: &'_ mut State) -> Iter<'_, State, Error, P> {
        Iter(parser, state, PhantomData)
    }

    #[derive(Clone, Copy)]
    pub struct Many<P, Output>(P, PhantomData<Output>);

    impl<State, Error, P, Output> Parser<State, Error> for Many<P, Output>
    where
        State: ParseState + Clone,
        Error: ParseError<State>,
        P: Parser<State, Error>,
        Output: Default + Extend<P::Output>,
    {
        type Output = Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let mut container = Output::default();
            let mut state = state;
            container.extend((&mut self.0).iter(&mut state));
            Ok((container, state))
        }
    }

    pub fn many<P, Output>(parser: P) -> Many<P, Output> {
        Many(parser, PhantomData)
    }

    #[derive(Clone, Copy)]
    pub struct Many1<P, Output>(P, PhantomData<Output>);

    impl<State, Error, P, Output> Parser<State, Error> for Many1<P, Output>
    where
        State: ParseState + Clone,
        Error: ParseError<State>,
        P: Parser<State, Error>,
        Output: Default + Extend<P::Output>,
    {
        type Output = Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let (first, state) = self.0.parse(state)?;
            let mut container = Output::default();
            let mut state = state;
            container.extend(iter::once(first).chain((&mut self.0).iter(&mut state)));
            Ok((container, state))
        }
    }

    pub fn many1<P, Output>(parser: P) -> Many1<P, Output> {
        Many1(parser, PhantomData)
    }

    #[derive(Clone, Copy)]
    pub struct SkipMany<P>(P);

    impl<State, Error, P> Parser<State, Error> for SkipMany<P>
    where
        State: ParseState + Clone,
        Error: ParseError<State>,
        P: Parser<State, Error>,
    {
        type Output = ();

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let mut state = state;
            (&mut self.0)
                .iter(&mut state)
                .for_each(|_| { /* do nothing */ });
            Ok(((), state))
        }
    }

    pub fn skip_many<P>(parser: P) -> SkipMany<P> {
        SkipMany(parser)
    }

    #[derive(Clone, Copy)]
    pub struct SkipMany1<P>(P);

    impl<State, Error, P> Parser<State, Error> for SkipMany1<P>
    where
        State: ParseState + Clone,
        Error: ParseError<State>,
        P: Parser<State, Error>,
    {
        type Output = ();

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let (_, mut state) = self.0.parse(state)?;
            (&mut self.0)
                .iter(&mut state)
                .for_each(|_| { /* do nothing */ });
            Ok(((), state))
        }
    }
}

pub mod token {
    use super::*;

    pub struct Any<State>(PhantomData<State>);

    impl<State: ParseState, Error: ParseError<State>> Parser<State, Error> for Any<State> {
        type Output = State::Tok;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let mut state = state;
            let position = state.position();
            state
                .next_token()
                .map(|tok| (tok, state))
                .ok_or(ParseError::empty(position))
        }
    }

    pub fn any<State>() -> Any<State> {
        Any(PhantomData)
    }

    pub struct OneOf<Tokens, State>(Tokens, PhantomData<State>);

    impl<State, Error, Tokens> Parser<State, Error> for OneOf<Tokens, State>
    where
        State: ParseState,
        Error: ParseError<State>,
        Tokens: Clone + IntoIterator<Item = State::Tok>,
        State::Tok: Clone + PartialEq,
    {
        type Output = State::Tok;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            satisfy(|tok| self.0.clone().into_iter().any(|test| tok == test)).parse(state)
        }
    }

    pub fn one_of<Tokens, State>(tokens: Tokens) -> OneOf<Tokens, State> {
        OneOf(tokens, PhantomData)
    }

    pub struct Satisfy<F>(F);

    impl<State, Error, F> Parser<State, Error> for Satisfy<F>
    where
        State: ParseState,
        Error: ParseError<State>,
        F: FnMut(State::Tok) -> bool,
        State::Tok: Clone,
    {
        type Output = State::Tok;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State, Error> {
            let mut state = state;
            let position = state.position();
            let (token, state) = state
                .next_token()
                .map(|token| (token, state))
                .ok_or(ParseError::empty(position))?;
            if (self.0)(token.clone()) {
                Ok((token, state))
            } else {
                Err(ParseError::empty(position))
            }
        }
    }

    pub fn satisfy<F>(predicate: F) -> Satisfy<F> {
        Satisfy(predicate)
    }
}

#[cfg(test)]
mod tests {
    use super::combinator::*;
    use super::repeat::*;
    use super::sequence::*;
    use super::*;

    #[test]
    fn test_and_then() {}
}
