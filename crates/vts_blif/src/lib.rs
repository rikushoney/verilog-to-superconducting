mod blifchar;
mod cursor;
#[macro_use]
mod error;
mod strspan;
mod token;

use crate::error::Error;
use crate::token::{Token, Tokenizer};

pub type Signal<'a> = &'a str;

#[derive(Clone, Debug, PartialEq)]
pub enum LogicValue {
    Zero,
    One,
    DontCare,
    Unknown,
}

impl TryFrom<char> for LogicValue {
    type Error = ();

    fn try_from(ch: char) -> Result<Self, Self::Error> {
        match ch {
            '0' => Ok(LogicValue::Zero),
            '1' => Ok(LogicValue::One),
            '-' => Ok(Self::DontCare),
            _ => Err(()),
        }
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

#[derive(Clone, Debug, PartialEq)]
pub enum LatchTrigger {
    FallingEdge,
    RisingEdge,
    ActiveHigh,
    ActiveLow,
    Asynchronous,
}

impl TryFrom<&str> for LatchTrigger {
    type Error = ();

    fn try_from(text: &str) -> std::result::Result<Self, Self::Error> {
        Ok(match text {
            "fe" => Self::FallingEdge,
            "re" => Self::RisingEdge,
            "ah" => Self::ActiveHigh,
            "al" => Self::ActiveLow,
            "as" => Self::Asynchronous,
            _ => {
                return Err(());
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
    pub control: Option<LatchControl<'a>>,
    pub init: LogicValue,
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
        let mut logic_gate = None;
        for token in tokens.by_ref() {
            let token = token?;
            if logic_gate.is_some() && !matches!(token, Token::SingleOutput { .. }) {
                model.commands.push(Command::LogicGate(logic_gate.unwrap()));
                logic_gate = None;
            }
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
                Token::Names { inputs, output, .. } => {
                    logic_gate = Some(LogicGate {
                        inputs: inputs.into_iter().map(|input| input.as_str()).collect(),
                        output: output.as_str(),
                        pla_description: vec![],
                    });
                }
                Token::SingleOutput {
                    input_plane,
                    output_plane,
                    ..
                } => {
                    if let Some(logic_gate) = logic_gate.as_mut() {
                        let inputs = input_plane
                            .as_str()
                            .chars()
                            .map(|ch| LogicValue::try_from(ch).unwrap())
                            .collect();
                        let output =
                            LogicValue::try_from(output_plane.as_str().chars().next().unwrap())
                                .unwrap();
                        logic_gate
                            .pla_description
                            .push(SingleOutput { inputs, output });
                    } else {
                        return Err(unexpected!("single output"));
                    }
                }
                _ => todo!(),
            }
        }
        Ok(model)
    }
}

pub fn parse_circuit<'a>(input: &'a str) -> Result<Vec<Model<'a>>, Error> {
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
