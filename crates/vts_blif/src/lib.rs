mod blifchar;
mod cursor;
#[macro_use]
mod error;
mod strspan;
mod token;

use crate::error::Error;
use crate::token::{Token, Tokenizer};

use std::path;

pub type Signal<'a> = &'a str;

#[derive(Clone, Debug, PartialEq)]
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
            '-' => LogicValue::DontCare,
            _ => {
                unreachable!("should be checked by tokenizer")
            }
        }
    }
}

impl TryFrom<char> for LogicValue {
    type Error = crate::error::Error;

    fn try_from(ch: char) -> Result<Self, Self::Error> {
        Ok(match ch {
            '0' => LogicValue::Zero,
            '1' => LogicValue::One,
            '-' => LogicValue::DontCare,
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

#[derive(Clone, Debug, PartialEq)]
pub struct LogicGate<'a> {
    pub inputs: Vec<Signal<'a>>,
    pub output: Signal<'a>,
    pub pla_description: Vec<SingleOutput>,
}

impl<'a> LogicGate<'a> {
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
    type Error = crate::error::Error;

    fn try_from(text: &str) -> std::result::Result<Self, Self::Error> {
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
pub enum LatchControl<'a> {
    Global,
    Clock(Signal<'a>),
}

impl<'a> From<&'a str> for LatchControl<'a> {
    fn from(signal: &'a str) -> Self {
        if signal == "NIL" {
            LatchControl::Global
        } else {
            LatchControl::Clock(signal)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericLatch<'a> {
    pub input: Signal<'a>,
    pub output: Signal<'a>,
    pub trigger: Option<LatchTrigger>,
    pub control: LatchControl<'a>,
    pub init_val: LogicValue,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FormalActual<'a>(&'a str, &'a str);

impl<'a> From<(&'a str, &'a str)> for FormalActual<'a> {
    fn from(formal_actual: (&'a str, &'a str)) -> Self {
        Self(formal_actual.0, formal_actual.1)
    }
}

impl<'a> TryFrom<&'a str> for FormalActual<'a> {
    type Error = ();

    fn try_from(text: &'a str) -> Result<Self, Self::Error> {
        text.split_once('=')
            .map(|formal_actual| formal_actual.into())
            .ok_or(())
    }
}

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
    pub formal_actual_list: Vec<FormalActual<'a>>,
    pub technology: Technology<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModelReference<'a> {
    pub name: &'a str,
    pub formal_actual_list: Vec<FormalActual<'a>>,
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

impl<'a> Model<'a> {
    fn empty() -> Self {
        Self {
            name: None,
            inputs: vec![],
            outputs: vec![],
            clocks: vec![],
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
                    model.name = model_name.map(|name| name.as_str());
                }
                Token::Inputs { input_list, .. } => {
                    model
                        .inputs
                        .extend(input_list.into_iter().map(|input| input.as_str()));
                }
                Token::Outputs { output_list, .. } => {
                    model
                        .outputs
                        .extend(output_list.into_iter().map(|output| output.as_str()));
                }
                Token::Clock { clock_list, .. } => {
                    model
                        .clocks
                        .extend(clock_list.into_iter().map(|clock| clock.as_str()));
                }
                Token::End(..) => {
                    break;
                }
                Token::Names { inputs, output, .. } => {
                    debug_assert!(logic_gate.is_empty());
                    logic_gate
                        .inputs
                        .extend(inputs.into_iter().map(|input| input.as_str()));
                    logic_gate.output = output.as_str();
                }
                Token::SingleOutput {
                    input_plane,
                    output_plane,
                    ..
                } => {
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
                Token::PlaDescriptionEnd(..) => {
                    let prev_logic_gate = std::mem::replace(&mut logic_gate, LogicGate::empty());
                    model.commands.push(Command::LogicGate(prev_logic_gate));
                }
                Token::Latch {
                    input,
                    output,
                    trigger,
                    control,
                    init_val,
                    ..
                } => {
                    let input = input.as_str();
                    let output = output.as_str();
                    let trigger = trigger
                        .map(|trigger| LatchTrigger::try_from(trigger.as_str()))
                        .transpose()?;
                    let control = control.map_or(LatchControl::Global, |control| {
                        LatchControl::from(control.as_str())
                    });
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
                        trigger,
                        control,
                        init_val,
                    };
                    model.commands.push(Command::GenericLatch(latch));
                }
                Token::Search { .. } => {
                    // TODO: open file and read model... OR
                    // do not support ".search" since this would require managing
                    // source files and add a lot of complexity
                }
                Token::Newline(..) | Token::Whitespace(..) => {
                    unreachable!("should be handled by tokenizer");
                }
                _ => todo!(),
            }
        }
        Ok(model)
    }
}

pub fn parse_circuit(input: &str) -> Result<Vec<Model<'_>>, Error> {
    let mut tokens = Tokenizer::new(input);
    let first = Model::parse(&mut tokens)?;
    let mut models = vec![first];
    loop {
        let parsed = Model::parse(&mut tokens);
        if let Err(Error::Eof) = parsed {
            break;
        }
        models.push(parsed?);
    }
    Ok(models)
}
