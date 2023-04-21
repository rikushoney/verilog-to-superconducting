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
