#![allow(dead_code)]

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace1, satisfy},
    combinator::{fail, map, map_res, opt, recognize},
    multi::{many0, many0_count, many1, many1_count, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

use unicode_ident::{is_xid_continue, is_xid_start};

fn unicode_ident(input: &str) -> IResult<&str, &str> {
    let ident_start = alt((satisfy(is_xid_start), char('_')));
    let ident_continue = alt((satisfy(is_xid_continue), char('_')));
    recognize(pair(ident_start, many0_count(ident_continue)))(input)
}

fn dot_command<'a>(command: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    preceded(char('.'), tag(command))
}

macro_rules! unimplemented_command {
    ($input:expr) => {
        map_res(fail::<_, &str, _>, |_| Err("unimplemented command"))($input)
    };
}

#[derive(Clone, Debug, PartialEq)]
pub struct SingleOutput<'a> {
    inputs: &'a str,
    output: char,
}

impl<'a> SingleOutput<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        let input_plane = alt((char('0'), char('1'), char('-')));
        let (input, inputs) = terminated(recognize(many1_count(input_plane)), multispace1)(input)?;
        let (input, output) = terminated(alt((char('0'), char('1'))), multispace1)(input)?;
        Ok((input, Self { inputs, output }))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LogicGate<'a> {
    inputs: Vec<&'a str>,
    output: &'a str,
    pla_description: Vec<SingleOutput<'a>>,
}

impl<'a> LogicGate<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        // .names <in-1> <in-2> ... <in-n> <output>
        let mut names = delimited(
            pair(dot_command("names"), multispace1),
            separated_list1(multispace1, unicode_ident),
            multispace1,
        );
        let (input, mut inputs) = names(input)?;
        // it is guarenteed that names contains at least a single element due to separated_list1
        let output = inputs.pop().unwrap();
        let (input, pla_description) = many1(SingleOutput::parse)(input)?;
        Ok((
            input,
            LogicGate {
                inputs,
                output,
                pla_description,
            },
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericLatch {}

impl GenericLatch {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct LibraryGate {}

impl LibraryGate {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ModelReference {}

impl ModelReference {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SubfileReference {}

impl SubfileReference {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FsmDescription {}

impl FsmDescription {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClockConstraint {}

impl ClockConstraint {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DelayConstraint {}

impl DelayConstraint {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command<'a> {
    LogicGate(LogicGate<'a>),
    GenericLatch(GenericLatch),
    LibraryGate(LibraryGate),
    ModelReference(ModelReference),
    SubfileReference(SubfileReference),
    FsmDescription(FsmDescription),
    ClockConstraint(ClockConstraint),
    DelayConstraint(DelayConstraint),
}

macro_rules! command_parser {
    (let $name:ident = $struct:ident) => {
        let $name = map($struct::parse, |$name| Command::$struct($name));
    };
}

impl<'a> Command<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        command_parser!(let logic_gate = LogicGate);
        command_parser!(let generic_latch = GenericLatch);
        command_parser!(let library_gate = LibraryGate);
        command_parser!(let model_reference = ModelReference);
        command_parser!(let subfile_reference = SubfileReference);
        command_parser!(let fsm_description = FsmDescription);
        command_parser!(let clock_constraint = ClockConstraint);
        command_parser!(let delay_constraint = DelayConstraint);
        alt((
            logic_gate,
            generic_latch,
            library_gate,
            model_reference,
            subfile_reference,
            fsm_description,
            clock_constraint,
            delay_constraint,
        ))(input)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Model<'a> {
    pub name: Option<&'a str>,
    pub inputs: Vec<&'a str>,
    pub outputs: Vec<&'a str>,
    pub clocks: Vec<&'a str>,
    pub commands: Vec<Command<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
enum ModelField<'a> {
    Inputs(Vec<&'a str>),
    Outputs(Vec<&'a str>),
    Clock(Vec<&'a str>),
}

impl<'a> ModelField<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        let field_decl = |name| {
            terminated(
                pair(
                    terminated(dot_command(name), multispace1),
                    separated_list1(multispace1, unicode_ident),
                ),
                multispace1,
            )
        };
        // .inputs <decl-input-list>
        let inputs_decl = field_decl("inputs");
        // .outputs <decl-output-list>
        let outputs_decl = field_decl("outputs");
        // .clock <decl-clock-list>
        let clock_decl = field_decl("clock");
        map_res(
            alt((inputs_decl, outputs_decl, clock_decl)),
            |(name, items)| match name {
                "inputs" => Ok(ModelField::Inputs(items)),
                "outputs" => Ok(ModelField::Outputs(items)),
                "clock" => Ok(ModelField::Clock(items)),
                _ => Err(format!("unexpected declaration \"{}\"", name)),
            },
        )(input)
    }
}

impl<'a> Model<'a> {
    fn parse(input: &'a str) -> IResult<&str, Self> {
        // .model <decl-model-name>
        let mut model_decl = delimited(
            pair(dot_command("model"), multispace1),
            opt(unicode_ident),
            multispace1,
        );
        let (input, model_name) = model_decl(input)?;
        // .inputs <decl-input-list>
        // .outputs <decl-outputs-list>
        // .clock <decl-clock-list>
        let (input, fields) = many0(ModelField::parse)(input)?;
        let mut inputs = Vec::new();
        let mut outputs = Vec::new();
        let mut clocks = Vec::new();
        for field in fields {
            match field {
                ModelField::Inputs(mut items) => inputs.append(&mut items),
                ModelField::Outputs(mut items) => outputs.append(&mut items),
                ModelField::Clock(mut items) => clocks.append(&mut items),
            }
        }
        // <command>
        //     .
        //     .
        //     .
        // <command>
        let (input, commands) = many1(Command::parse)(input)?;
        // .end
        let (input, _) = opt(dot_command("end"))(input)?;
        Ok((
            input,
            Self {
                name: model_name,
                inputs,
                outputs,
                clocks,
                commands,
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unicode_ident() {
        let tests = [
            ("myVariable#", "myVariable", "#"),
            ("veränderlich", "veränderlich", ""),
        ];

        for (input, expected, rest) in tests {
            assert_eq!(unicode_ident(input), Ok((rest, expected)));
        }
    }

    #[test]
    fn test_single_output() {
        let tests = [
            (
                "10-1 0\n",
                SingleOutput {
                    inputs: "10-1",
                    output: '0',
                },
                "",
            ),
            (
                "-111 0\n",
                SingleOutput {
                    inputs: "-111",
                    output: '0',
                },
                "",
            ),
            (
                "0-11 1\n",
                SingleOutput {
                    inputs: "0-11",
                    output: '1',
                },
                "",
            ),
        ];

        for (input, expected, rest) in tests {
            assert_eq!(SingleOutput::parse(input), Ok((rest, expected)));
        }
    }

    #[test]
    fn test_logic_gate() {
        let tests = [
            (
                r#".names a b c d
00- 0
1-0 1
--1 1
"#,
                LogicGate {
                    inputs: vec!["a", "b", "c"],
                    output: "d",
                    pla_description: vec![
                        SingleOutput {
                            inputs: "00-",
                            output: '0',
                        },
                        SingleOutput {
                            inputs: "1-0",
                            output: '1',
                        },
                        SingleOutput {
                            inputs: "--1",
                            output: '1',
                        },
                    ],
                },
                "",
            ),
            (
                r#".names a b c e
0-1 1
--0 1
-01 1
"#,
                LogicGate {
                    inputs: vec!["a", "b", "c"],
                    output: "e",
                    pla_description: vec![
                        SingleOutput {
                            inputs: "0-1",
                            output: '1',
                        },
                        SingleOutput {
                            inputs: "--0",
                            output: '1',
                        },
                        SingleOutput {
                            inputs: "-01",
                            output: '1',
                        },
                    ],
                },
                "",
            ),
        ];

        for (input, expected, rest) in tests {
            assert_eq!(LogicGate::parse(input), Ok((rest, expected)));
        }
    }

    #[test]
    fn test_model() {
        let tests = [
            (
                r#".model myModel
.inputs a b c
.outputs d e f
.clock clk1 clk2
.names a b c d
000 0
-1- 1
0-0 0
.end"#,
                Model {
                    name: Some("myModel"),
                    inputs: vec!["a", "b", "c"],
                    outputs: vec!["d", "e", "f"],
                    clocks: vec!["clk1", "clk2"],
                    commands: vec![Command::LogicGate(LogicGate {
                        inputs: vec!["a", "b", "c"],
                        output: "d",
                        pla_description: vec![
                            SingleOutput {
                                inputs: "000",
                                output: '0',
                            },
                            SingleOutput {
                                inputs: "-1-",
                                output: '1',
                            },
                            SingleOutput {
                                inputs: "0-0",
                                output: '0',
                            },
                        ],
                    })],
                },
                "",
            ),
            (
                r#".model myModel
.outputs d e f
.clock clk1
.inputs a b
.inputs c
.clock clk2
.names a b c d
1-0 1
-01 0
-1- 0
.end"#,
                Model {
                    name: Some("myModel"),
                    inputs: vec!["a", "b", "c"],
                    outputs: vec!["d", "e", "f"],
                    clocks: vec!["clk1", "clk2"],
                    commands: vec![Command::LogicGate(LogicGate {
                        inputs: vec!["a", "b", "c"],
                        output: "d",
                        pla_description: vec![
                            SingleOutput {
                                inputs: "1-0",
                                output: '1',
                            },
                            SingleOutput {
                                inputs: "-01",
                                output: '0',
                            },
                            SingleOutput {
                                inputs: "-1-",
                                output: '0',
                            },
                        ],
                    })],
                },
                "",
            ),
        ];

        for (input, expected, rest) in tests {
            assert_eq!(Model::parse(input), Ok((rest, expected)));
        }
    }
}
