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
pub enum BeforeAfter {
    Before,
    After,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RiseFall {
    Rise,
    Fall,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Event<'a> {
    pub rise_fall: RiseFall,
    pub clock: &'a str,
    pub before: f64,
    pub after: f64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClockEvent<'a> {
    pub event_percent: f64,
    pub events: Vec<Event<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClockConstraint<'a> {
    pub cycle_time: f64,
    pub clock_events: Vec<ClockEvent<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DelayPhase {
    Inverting,
    NonInverting,
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Delay<'a> {
    pub in_name: &'a str,
    pub phase: DelayPhase,
    pub load: f64,
    pub max_load: f64,
    pub block_rise: f64,
    pub drive_rise: f64,
    pub block_fall: f64,
    pub drive_fall: f64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InputArrival<'a> {
    pub in_name: &'a str,
    pub rise: f64,
    pub fall: f64,
    pub event: Option<(BeforeAfter, RiseFall, &'a str)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OutputRequired<'a> {
    pub out_name: &'a str,
    pub rise: f64,
    pub fall: f64,
    pub event: Option<(BeforeAfter, RiseFall, &'a str)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DelayConstraintKind<'a> {
    Area(f64),
    Delay(Delay<'a>),
    WireLoadSlope(f64),
    Wire(Vec<f64>),
    InputArrival(InputArrival<'a>),
    DefaultInputArrival((f64, f64)),
    OutputRequired(OutputRequired<'a>),
    DefaultOutputRequired((f64, f64)),
    InputDrive((&'a str, f64, f64)),
    DefaultInputDrive((f64, f64)),
    MaxInputLoad(f64),
    DefaultMaxInputLoad(f64),
    OutputLoad((&'a str, f64)),
    DefaultOutputLoad(f64),
}

#[derive(Clone, Debug, PartialEq)]
pub struct DelayConstraint<'a> {
    pub constraints: Vec<DelayConstraintKind<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command<'a> {
    LogicGate(LogicGate<'a>),
    GenericLatch(GenericLatch<'a>),
    LibraryGate(LibraryGate<'a>),
    ModelReference(ModelReference<'a>),
    SubfileReference(SubfileReference<'a>),
    FsmDescription(FsmDescription),
    ClockConstraint(ClockConstraint<'a>),
    DelayConstraint(DelayConstraint<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Model<'a> {
    pub name: Option<&'a str>,
    pub inputs: Vec<&'a str>,
    pub outputs: Vec<&'a str>,
    pub clocks: Vec<&'a str>,
    pub commands: Vec<Command<'a>>,
}
