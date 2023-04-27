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

    pub struct With<P1, P2>(P1, P2);

    impl<State: ParseState, P1: Parser<State>, P2: Parser<State>> Parser<State> for With<P1, P2> {
        type Output = P2::Output;

        fn parse(&mut self, state: State) -> ParseResult<Self::Output, State> {
            let (_, state) = self.0.parse(state)?;
            let (second, state) = self.1.parse(state)?;
            Ok((second, state))
        }
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
            container.extend(iter(&mut self.0, &mut state));
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
            container.extend(iter::once(first).chain(iter(&mut self.0, &mut state)));
            Ok((container, state))
        }
    }

    pub fn many1<P, Output>(parser: P) -> Many1<P, Output> {
        Many1(parser, PhantomData)
    }
}
