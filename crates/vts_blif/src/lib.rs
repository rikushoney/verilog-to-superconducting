mod blifchar;
pub mod emitter;
#[macro_use]
mod error;
mod token;

use crate::error::Error;
use crate::token::{Token, Tokenizer};

use vts_shared::strspan::StrSpan;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LogicValue {
    Zero,
    One,
    DontCare,
    Unknown,
}

impl LogicValue {
    fn from_unchecked(ch: char) -> Self {
        match ch {
            '0' => LogicValue::Zero,
            '1' => LogicValue::One,
            '2' | '-' => LogicValue::DontCare,
            '3' => LogicValue::Unknown,
            _ => {
                unreachable!("should be checked by tokenizer")
            }
        }
    }
}

impl TryFrom<char> for LogicValue {
    type Error = Error;

    fn try_from(ch: char) -> Result<Self, Self::Error> {
        Ok(match ch {
            '0' => LogicValue::Zero,
            '1' => LogicValue::One,
            '2' | '-' => LogicValue::DontCare,
            '3' => LogicValue::Unknown,
            _ => {
                return Err(Error::InvalidLogicValue);
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SingleOutput {
    pub inputs: Vec<LogicValue>,
    pub output: LogicValue,
}

impl SingleOutput {
    pub fn new<I>(inputs: I, output: LogicValue) -> Self
    where
        I: IntoIterator<Item = LogicValue>,
    {
        Self {
            inputs: inputs.into_iter().collect(),
            output,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LogicGate<'a> {
    pub inputs: Vec<&'a str>,
    pub output: &'a str,
    pub pla_description: Vec<SingleOutput>,
}

impl<'a> LogicGate<'a> {
    pub fn new<I1, I2>(inputs: I1, output: &'a str, pla_description: I2) -> Self
    where
        I1: IntoIterator<Item = &'a str>,
        I2: IntoIterator<Item = SingleOutput>,
    {
        Self {
            inputs: inputs.into_iter().collect(),
            output,
            pla_description: pla_description.into_iter().collect(),
        }
    }

    fn empty() -> Self {
        Self {
            inputs: vec![],
            output: "",
            pla_description: vec![],
        }
    }

    fn is_empty(&self) -> bool {
        self.inputs.is_empty() && self.output.is_empty() && self.pla_description.is_empty()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LatchTrigger {
    FallingEdge,
    RisingEdge,
    ActiveHigh,
    ActiveLow,
    Asynchronous,
}

impl TryFrom<&str> for LatchTrigger {
    type Error = Error;

    fn try_from(text: &str) -> Result<Self, Self::Error> {
        Ok(match text {
            "fe" => Self::FallingEdge,
            "re" => Self::RisingEdge,
            "ah" => Self::ActiveHigh,
            "al" => Self::ActiveLow,
            "as" => Self::Asynchronous,
            _ => {
                return Err(Error::InvalidLatchTrigger);
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LatchControlKind<'a> {
    Global,
    Clock(&'a str),
}

#[derive(Clone, Debug, PartialEq)]
pub struct LatchControl<'a> {
    pub clock: LatchControlKind<'a>,
    pub trigger: LatchTrigger,
}

impl<'a> From<&'a str> for LatchControlKind<'a> {
    fn from(signal: &'a str) -> Self {
        if signal == "NIL" {
            LatchControlKind::Global
        } else {
            LatchControlKind::Clock(signal)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericLatch<'a> {
    pub input: &'a str,
    pub output: &'a str,
    pub control: Option<LatchControl<'a>>,
    pub init_val: LogicValue,
}

impl<'a> GenericLatch<'a> {
    pub fn new(
        input: &'a str,
        output: &'a str,
        control: Option<LatchControl<'a>>,
        init_val: LogicValue,
    ) -> Self {
        Self {
            input,
            output,
            control,
            init_val,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalActual<'a> {
    pub formal: &'a str,
    pub actual: &'a str,
}

impl<'a> From<(&'a str, &'a str)> for FormalActual<'a> {
    fn from(formal_actual: (&'a str, &'a str)) -> Self {
        Self {
            formal: formal_actual.0,
            actual: formal_actual.1,
        }
    }
}

impl<'a> TryFrom<&'a str> for FormalActual<'a> {
    type Error = crate::error::Error;

    fn try_from(text: &'a str) -> Result<Self, Self::Error> {
        text.split_once('=')
            .map(|formal_actual| formal_actual.into())
            .ok_or(expected!("formal=actual"))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LibraryGate<'a> {
    pub name: &'a str,
    pub formal_actual_list: Vec<FormalActual<'a>>,
}

impl<'a> LibraryGate<'a> {
    pub fn new<I>(name: &'a str, formal_actual_list: I) -> Self
    where
        I: IntoIterator<Item = FormalActual<'a>>,
    {
        Self {
            name,
            formal_actual_list: formal_actual_list.into_iter().collect(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModelReference<'a> {
    pub name: &'a str,
    pub formal_actual_list: Vec<FormalActual<'a>>,
}

impl<'a> ModelReference<'a> {
    pub fn new<I>(name: &'a str, formal_actual_list: I) -> Self
    where
        I: IntoIterator<Item = FormalActual<'a>>,
    {
        Self {
            name,
            formal_actual_list: formal_actual_list.into_iter().collect(),
        }
    }
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
    pub inputs: Vec<&'a str>,
    pub outputs: Vec<&'a str>,
    pub commands: Vec<Command<'a>>,
}

fn process_model_header<'a>(model: &mut Model<'a>, model_name: Option<StrSpan<'a>>) {
    model.name = model_name.map(|name| name.as_str());
}

fn process_inputs<'a>(model: &mut Model<'a>, input_list: Vec<StrSpan<'a>>) {
    model
        .inputs
        .extend(input_list.into_iter().map(|input| input.as_str()));
}

fn process_outputs<'a>(model: &mut Model<'a>, output_list: Vec<StrSpan<'a>>) {
    model
        .outputs
        .extend(output_list.into_iter().map(|output| output.as_str()));
}

fn process_names<'a>(
    logic_gate: &mut LogicGate<'a>,
    inputs: Vec<StrSpan<'a>>,
    output: StrSpan<'a>,
) {
    debug_assert!(logic_gate.is_empty());
    logic_gate
        .inputs
        .extend(inputs.into_iter().map(|input| input.as_str()));
    logic_gate.output = output.as_str();
}

fn process_single_output<'a>(
    logic_gate: &mut LogicGate<'a>,
    input_plane: StrSpan<'a>,
    output_plane: StrSpan<'a>,
) {
    debug_assert!(!logic_gate.is_empty());
    let inputs = input_plane
        .as_str()
        .chars()
        // SAFETY: checked by tokenizer
        .map(LogicValue::from_unchecked)
        .collect();
    // SAFETY: checked by tokenizer
    let output = LogicValue::from_unchecked(
        output_plane
            .as_str()
            .chars()
            .next()
            .expect("should be checked by tokenizer"),
    );
    logic_gate
        .pla_description
        .push(SingleOutput { inputs, output });
}

fn process_pla_description_end<'a>(model: &mut Model<'a>, logic_gate: &mut LogicGate<'a>) {
    let prev_logic_gate = std::mem::replace(logic_gate, LogicGate::empty());
    model.commands.push(Command::LogicGate(prev_logic_gate));
}

fn process_latch<'a>(
    model: &mut Model<'a>,
    input: StrSpan<'a>,
    output: StrSpan<'a>,
    trigger: Option<StrSpan<'a>>,
    control: Option<StrSpan<'a>>,
    init_val: Option<StrSpan<'a>>,
) -> Result<(), Error> {
    let input = input.as_str();
    let output = output.as_str();
    let control = match Option::zip(trigger, control) {
        Some((trigger, control)) => {
            let trigger = LatchTrigger::try_from(trigger.as_str())?;
            let clock = LatchControlKind::from(control.as_str());
            Some(LatchControl { clock, trigger })
        }
        None => None,
    };
    let init_val = init_val
        .map(|init_val| {
            LogicValue::try_from(
                init_val
                    .as_str()
                    .chars()
                    .next()
                    .expect("should be checked by tokenizer"),
            )
        })
        .transpose()?
        .unwrap_or(LogicValue::Unknown);
    let latch = GenericLatch {
        input,
        output,
        control,
        init_val,
    };
    model.commands.push(Command::GenericLatch(latch));
    Ok(())
}

fn process_gate<'a>(
    model: &mut Model<'a>,
    name: StrSpan<'a>,
    formal_actual_list: Vec<StrSpan<'a>>,
) {
    let name = name.as_str();
    let formal_actual_list = formal_actual_list
        .into_iter()
        .map(|formal_actual| {
            FormalActual::try_from(formal_actual.as_str()).expect("should be checked by tokenizer")
        })
        .collect();
    let gate = LibraryGate {
        name,
        formal_actual_list,
    };
    model.commands.push(Command::LibraryGate(gate));
}

fn process_subckt<'a>(
    model: &mut Model<'a>,
    name: StrSpan<'a>,
    formal_actual_list: Vec<StrSpan<'a>>,
) {
    let name = name.as_str();
    let formal_actual_list = formal_actual_list
        .into_iter()
        .map(|formal_actual| {
            FormalActual::try_from(formal_actual.as_str()).expect("should be checked by tokenizer")
        })
        .collect();
    let model_reference = ModelReference {
        name,
        formal_actual_list,
    };
    model
        .commands
        .push(Command::ModelReference(model_reference));
}

impl<'a> Model<'a> {
    pub fn new<I1, I2>(name: Option<&'a str>, inputs: I1, outputs: I1, commands: I2) -> Self
    where
        I1: IntoIterator<Item = &'a str>,
        I2: IntoIterator<Item = Command<'a>>,
    {
        Self {
            name,
            inputs: inputs.into_iter().collect(),
            outputs: outputs.into_iter().collect(),
            commands: commands.into_iter().collect(),
        }
    }

    fn empty() -> Self {
        Self {
            name: None,
            inputs: vec![],
            outputs: vec![],
            commands: vec![],
        }
    }

    fn parse(tokens: &mut Tokenizer<'a>) -> Result<Self, Error> {
        let mut model = Self::empty();
        let mut logic_gate = LogicGate::empty();
        for token in tokens.by_ref() {
            let token = token?;
            match token {
                Token::ModelHeader { model_name, .. } => {
                    process_model_header(&mut model, model_name);
                }
                Token::Inputs { input_list, .. } => {
                    process_inputs(&mut model, input_list);
                }
                Token::Outputs { output_list, .. } => {
                    process_outputs(&mut model, output_list);
                }
                Token::End { .. } => {
                    break;
                }
                Token::Names { inputs, output, .. } => {
                    process_names(&mut logic_gate, inputs, output);
                }
                Token::SingleOutput {
                    input_plane,
                    output_plane,
                    ..
                } => {
                    process_single_output(&mut logic_gate, input_plane, output_plane);
                }
                Token::PlaDescriptionEnd { .. } => {
                    process_pla_description_end(&mut model, &mut logic_gate);
                }
                Token::Latch {
                    input,
                    output,
                    trigger,
                    control,
                    init_val,
                    ..
                } => {
                    process_latch(&mut model, input, output, trigger, control, init_val)?;
                }
                Token::Gate {
                    name,
                    formal_actual_list,
                    ..
                } => {
                    process_gate(&mut model, name, formal_actual_list);
                }
                Token::Subckt {
                    name,
                    formal_actual_list,
                    ..
                } => {
                    process_subckt(&mut model, name, formal_actual_list);
                }
                Token::Text { span } => {
                    return Err(unexpected!(span.as_str()));
                }
                Token::Newline { .. } | Token::Whitespace { .. } => {
                    unreachable!("should be handled by tokenizer");
                }
            }
        }
        Ok(model)
    }
}

pub fn parse_model(input: &str) -> Result<Model<'_>, Error> {
    let mut tokens = Tokenizer::new(input);
    Model::parse(&mut tokens)
}

pub fn parse_circuit(input: &str) -> Result<Vec<Model<'_>>, Error> {
    let mut tokens = Tokenizer::new(input);
    let first = Model::parse(&mut tokens)?;
    let mut models = vec![first];
    loop {
        match Model::parse(&mut tokens) {
            Ok(parsed) => {
                models.push(parsed);
            }
            Err(Error::Eof) => {
                break;
            }
            Err(error) => {
                return Err(error);
            }
        }
    }
    Ok(models)
}
