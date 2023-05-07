#![allow(dead_code)]

use std::path;

#[derive(Clone, Debug, PartialEq)]
pub struct SingleOutput<'a> {
    pub inputs: &'a str,
    pub output: char,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LogicGate<'a> {
    pub exdc: bool,
    pub inputs: Vec<&'a str>,
    pub output: &'a str,
    pub pla_description: Vec<SingleOutput<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LatchKind {
    FallingEdge,
    RisingEdge,
    ActiveHigh,
    ActiveLow,
    Asynchronous,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub enum LogicValue {
    Zero,
    One,
    DontCare,
    #[default]
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LatchControl<'a> {
    Clock(&'a str),
    GlobalClock,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericLatch<'a> {
    pub input: &'a str,
    pub output: &'a str,
    pub kind: LatchKind,
    pub control: LatchControl<'a>,
    pub init: LogicValue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LibraryLatch<'a> {
    pub control: LatchControl<'a>,
    pub init: LogicValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LibraryTechnology<'a> {
    Gate,
    Latch(LibraryLatch<'a>),
}

pub type FormalActual<'a> = Vec<(&'a str, &'a str)>;

#[derive(Clone, Debug, PartialEq)]
pub struct LibraryGate<'a> {
    pub name: &'a str,
    pub formal_actual: FormalActual<'a>,
    pub technology: LibraryTechnology<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModelReference<'a> {
    pub name: &'a str,
    pub formal_actual: FormalActual<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SubfileReference<'a> {
    pub filename: &'a path::Path,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FsmDescription {}

#[derive(Clone, Debug, PartialEq)]
pub struct BeforeAfter {
    pub before: f64,
    pub after: f64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RiseFall {
    Rise,
    Fall,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClockEvent<'a> {
    pub event_percent: f64,
    pub events: Vec<(RiseFall, &'a str, Option<BeforeAfter>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClockConstraint<'a> {
    pub cycle_time: f64,
    pub clock_events: Vec<ClockEvent<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DelayConstraint {}

#[derive(Clone, Debug, PartialEq)]
pub enum Command<'a> {
    LogicGate(LogicGate<'a>),
    GenericLatch(GenericLatch<'a>),
    LibraryGate(LibraryGate<'a>),
    ModelReference(ModelReference<'a>),
    SubfileReference(SubfileReference<'a>),
    FsmDescription(FsmDescription),
    ClockConstraint(ClockConstraint<'a>),
    DelayConstraint(DelayConstraint),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Model<'a> {
    pub name: Option<&'a str>,
    pub inputs: Vec<&'a str>,
    pub outputs: Vec<&'a str>,
    pub clocks: Vec<&'a str>,
    pub commands: Vec<Command<'a>>,
}
