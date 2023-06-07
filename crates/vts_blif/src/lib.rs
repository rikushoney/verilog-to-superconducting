pub type Signal<'a> = &'a str;

#[derive(Clone, Debug, PartialEq)]
pub enum LogicValue {
    Zero,
    One,
    DontCare,
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SingleOutput {
    pub inputs: Vec<LogicValue>,
    pub output: LogicValue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LogicGate<'a> {
    pub inputs: Vec<Signal<'a>>,
    pub output: Signal<'a>,
    pub pla_description: Vec<SingleOutput>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LatchTrigger {
    FallingEdge,
    RisingEdge,
    ActiveHigh,
    ActiveLow,
    Asynchronous,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LatchControl<'a> {
    Global,
    Clock(Signal<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericLatch<'a> {
    pub input: Signal<'a>,
    pub output: Signal<'a>,
    pub trigger: LatchTrigger,
    pub control: LatchControl<'a>,
    pub init: LogicValue,
}

pub type FormalActual<'a> = (&'a str, &'a str);

#[derive(Clone, Debug, PartialEq)]
pub enum Technology<'a> {
    Gate,
    Latch {
        control: LatchControl<'a>,
        init: LogicValue,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct LibraryGate<'a> {
    pub name: &'a str,
    pub formal_actual: Vec<FormalActual<'a>>,
    pub technology: Technology<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModelReference<'a> {
    pub name: &'a str,
    pub formal_actual: Vec<FormalActual<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command<'a> {
    LogicGate(LogicGate<'a>),
    GenericLatch(GenericLatch<'a>),
    LibraryGate(LibraryGate<'a>),
    ModelReference(ModelReference<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Model<'a> {
    pub name: Option<&'a str>,
    pub inputs: Vec<Signal<'a>>,
    pub outputs: Vec<Signal<'a>>,
    pub clocks: Vec<Signal<'a>>,
    pub commands: Vec<Command<'a>>,
}
