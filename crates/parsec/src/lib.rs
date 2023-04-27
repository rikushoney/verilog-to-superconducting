use std::iter;
use std::marker::PhantomData;

pub struct ParseError {
    pub messages: Vec<()>,
}

impl ParseError {
    pub fn empty() -> Self {
        Self {
            messages: Vec::new(),
        }
    }
}

pub type ParseResult<Output, State> = Result<(Output, State), ParseError>;

pub trait ParseState {
    type Tok;

    fn next_token(&mut self) -> Option<Self::Tok>;
}

pub trait Parser<State: ParseState> {
    type Output;

    fn parse(&mut self, state: State) -> ParseResult<Self::Output, State>;

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

    fn iter<'a>(self, state: &'a mut State) -> repeat::Iter<'a, State, Self>
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

impl<State: ParseState, P: Parser<State>> Parser<State> for &mut P {
    type Output = P::Output;

    fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
        (**self).parse(state)
    }
}

pub mod combinator {
    use super::*;

    pub struct AndThen<P, F>(P, F);

    impl<State, P, F, Output> Parser<State> for AndThen<P, F>
    where
        State: ParseState,
        P: Parser<State>,
        F: FnMut(P::Output, State) -> ParseResult<Output, State>,
    {
        type Output = Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
            let (output, state) = self.0.parse(state)?;
            (self.1)(output, state)
        }
    }

    pub fn and_then<P, F>(parser: P, mapper: F) -> AndThen<P, F> {
        AndThen(parser, mapper)
    }

    pub struct Optional<P>(P);

    impl<State: ParseState + Clone, P: Parser<State>> Parser<State> for Optional<P> {
        type Output = Option<P::Output>;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
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

    pub enum Either<L, R> {
        Left(L),
        Right(R),
    }

    impl<State, L, R, Output> Parser<State> for Either<L, R>
    where
        State: ParseState,
        L: Parser<State, Output = Output>,
        R: Parser<State, Output = Output>,
    {
        type Output = Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
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

    pub struct Then<P, F>(P, F);

    impl<State: ParseState, P1: Parser<State>, P2: Parser<State>, F: FnMut(P1::Output) -> P2>
        Parser<State> for Then<P1, F>
    {
        type Output = P2::Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
            let (output, state) = self.0.parse(state)?;
            (self.1)(output).parse(state)
        }
    }

    pub fn then<P, F>(parser: P, mapper: F) -> Then<P, F> {
        Then(parser, mapper)
    }

    pub struct And<P1, P2>(P1, P2);

    impl<State: ParseState, P1: Parser<State>, P2: Parser<State>> Parser<State> for And<P1, P2> {
        type Output = (P1::Output, P2::Output);

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
            let (first, state) = self.0.parse(state)?;
            let (second, state) = self.1.parse(state)?;
            Ok(((first, second), state))
        }
    }

    pub fn and<P1, P2>(first: P1, second: P2) -> And<P1, P2> {
        And(first, second)
    }

    pub struct Skip<P1, P2>(P1, P2);

    impl<State: ParseState, P1: Parser<State>, P2: Parser<State>> Parser<State> for Skip<P1, P2> {
        type Output = P1::Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
            let (first, state) = self.0.parse(state)?;
            let (_, state) = self.1.parse(state)?;
            Ok((first, state))
        }
    }

    pub fn skip<P1, P2>(first: P1, second: P2) -> Skip<P1, P2> {
        Skip(first, second)
    }

    pub struct With<P1, P2>(P1, P2);

    impl<State: ParseState, P1: Parser<State>, P2: Parser<State>> Parser<State> for With<P1, P2> {
        type Output = P2::Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
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

    pub struct Iter<'a, State, P>(P, &'a mut State);

    impl<'a, State: ParseState + Clone, P: Parser<State>> Iterator for Iter<'a, State, P> {
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

    pub fn iter<'a, State, P>(parser: P, state: &'a mut State) -> Iter<'a, State, P> {
        Iter(parser, state)
    }

    pub struct Many<P, Output>(P, PhantomData<Output>);

    impl<State, P, Output> Parser<State> for Many<P, Output>
    where
        State: ParseState + Clone,
        P: Parser<State>,
        Output: Default + Extend<P::Output>,
    {
        type Output = Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
            let mut container = Output::default();
            let mut state = state;
            container.extend((&mut self.0).iter(&mut state));
            Ok((container, state))
        }
    }

    pub fn many<P, Output>(parser: P) -> Many<P, Output> {
        Many(parser, PhantomData)
    }

    pub struct Many1<P, Output>(P, PhantomData<Output>);

    impl<State, P, Output> Parser<State> for Many1<P, Output>
    where
        State: ParseState + Clone,
        P: Parser<State>,
        Output: Default + Extend<P::Output>,
    {
        type Output = Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
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

    pub struct SkipMany<P>(P);

    impl<State: ParseState + Clone, P: Parser<State>> Parser<State> for SkipMany<P> {
        type Output = ();

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
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
}

pub mod token {
    use super::*;

    pub struct Any<State>(PhantomData<State>);

    impl<State: ParseState> Parser<State> for Any<State> {
        type Output = State::Tok;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
            let mut state = state;
            state
                .next_token()
                .map(|tok| (tok, state))
                .ok_or(ParseError::empty())
        }
    }

    pub struct OneOf<Tokens, State>(Tokens, PhantomData<State>);

    impl<State, Tokens> Parser<State> for OneOf<Tokens, State>
    where
        State: ParseState,
        Tokens: Clone + IntoIterator<Item = State::Tok>,
        State::Tok: Clone + PartialEq,
    {
        type Output = State::Tok;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
            satisfy(|tok| self.0.clone().into_iter().any(|test| tok == test)).parse(state)
        }
    }

    pub struct Satisfy<F>(F);

    impl<State, F> Parser<State> for Satisfy<F>
    where
        State: ParseState,
        F: FnMut(State::Tok) -> bool,
        State::Tok: Clone,
    {
        type Output = State::Tok;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
            let mut state = state;
            let (token, state) = state
                .next_token()
                .map(|token| (token, state))
                .ok_or(ParseError::empty())?;
            if (self.0)(token.clone()) {
                Ok((token, state))
            } else {
                Err(ParseError::empty())
            }
        }
    }

    pub fn satisfy<F>(predicate: F) -> Satisfy<F> {
        Satisfy(predicate)
    }
}
