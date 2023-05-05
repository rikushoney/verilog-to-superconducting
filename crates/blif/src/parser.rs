#![allow(dead_code)]

use crate::ast::*;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace1, satisfy, space1},
    combinator::{fail, map, map_res, opt, recognize, success, verify},
    error::context,
    multi::{many0, many1, many1_count, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

fn node_name(input: &str) -> IResult<&str, &str> {
    let non_whitespace = many1_count(satisfy(|ch| !ch.is_whitespace()));
    recognize(non_whitespace)(input)
}

fn dot_command<'a>(command: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    preceded(char('.'), tag(command))
}

fn ensure_newline(input: &str) -> IResult<&str, &str> {
    context(
        "expected newline",
        verify(multispace1, |spaces: &str| spaces.contains('\n')),
    )(input)
}

macro_rules! unimplemented_command {
    ($input:expr) => {{
        context("unimplemented_command", fail)($input)
    }};
}

impl<'a> SingleOutput<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        // {0, 1, -}
        let input_plane = alt((char('0'), char('1'), char('-')));
        // {0, 1}
        let output_plane = alt((char('0'), char('1')));
        map(
            tuple((
                terminated(recognize(many1_count(input_plane)), space1),
                terminated(output_plane, ensure_newline),
            )),
            |(inputs, output)| Self { inputs, output },
        )(input)
    }
}

impl<'a> LogicGate<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            tuple((
                // .exdc
                map(
                    opt(terminated(dot_command("exdc"), ensure_newline)),
                    |exdc| exdc.is_some(),
                ),
                // .names <in-1> <in-2> ... <in-n> <output>
                delimited(
                    pair(dot_command("names"), space1),
                    separated_list1(space1, node_name),
                    ensure_newline,
                ),
                // <single-output-cover>
                many1(SingleOutput::parse),
            )),
            |(exdc, mut inputs, pla_description)| {
                // it is guarenteed that `inputs` contains at least a single element due to
                // separated_list1
                let output = inputs.pop().unwrap();
                LogicGate {
                    exdc,
                    inputs,
                    output,
                    pla_description,
                }
            },
        )(input)
    }
}

impl LatchType {
    fn parse(input: &str) -> IResult<&str, Self> {
        alt((
            map(tag("fe"), |_| LatchType::FallingEdge),
            map(tag("re"), |_| LatchType::RisingEdge),
            map(tag("ah"), |_| LatchType::ActiveHigh),
            map(tag("al"), |_| LatchType::ActiveLow),
            map(tag("as"), |_| LatchType::Asynchronous),
        ))(input)
    }
}

impl InitValue {
    fn parse(input: &str) -> IResult<&str, Self> {
        alt((
            map(char('0'), |_| InitValue::Zero),
            map(char('1'), |_| InitValue::One),
            map(char('2'), |_| InitValue::DontCare),
            map(char('3'), |_| InitValue::Unknown),
        ))(input)
    }
}

impl<'a> GenericLatch<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            delimited(
                // .latch
                terminated(dot_command("latch"), space1),
                tuple((
                    // input
                    terminated(node_name, space1),
                    // output
                    terminated(node_name, space1),
                    // type
                    terminated(LatchType::parse, space1),
                    // control
                    terminated(node_name, space1),
                    // init-val
                    alt((InitValue::parse, success(InitValue::default()))),
                )),
                ensure_newline,
            ),
            |(input, output, ty, control, init)| {
                let control = match control {
                    "NIL" => LatchControl::GlobalClock,
                    clock => LatchControl::Clock(clock),
                };
                GenericLatch {
                    input,
                    output,
                    ty,
                    control,
                    init,
                }
            },
        )(input)
    }
}

impl LibraryGate {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

impl ModelReference {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

impl SubfileReference {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

impl FsmDescription {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

impl ClockConstraint {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

impl DelayConstraint {
    fn parse(input: &str) -> IResult<&str, Self> {
        unimplemented_command!(input)
    }
}

macro_rules! command_parser {
    ($name:ident => $struct:ident) => {
        map($struct::parse, |$name| Command::$struct($name))
    };
}

impl<'a> Command<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        alt((
            command_parser!(logic_gate => LogicGate),
            command_parser!(generic_latch => GenericLatch),
            command_parser!(library_gate => LibraryGate),
            command_parser!(model_reference => ModelReference),
            command_parser!(subfile_reference => SubfileReference),
            command_parser!(fsm_description => FsmDescription),
            command_parser!(clock_constraint => ClockConstraint),
            command_parser!(delay_constraint => DelayConstraint),
        ))(input)
    }
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
                    terminated(dot_command(name), space1),
                    separated_list1(space1, node_name),
                ),
                ensure_newline,
            )
        };
        map_res(
            alt((
                // .inputs <decl-input-list>
                field_decl("inputs"),
                // .outputs <decl-output-list>
                field_decl("outputs"),
                // .clock <decl-clock-list>
                field_decl("clock"),
            )),
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
        map(
            terminated(
                tuple((
                    // .model <decl-model-name>
                    delimited(
                        pair(dot_command("model"), space1),
                        opt(node_name),
                        ensure_newline,
                    ),
                    // .inputs <decl-input-list>
                    // .outputs <decl-outputs-list>
                    // .clock <decl-clock-list>
                    many0(ModelField::parse),
                    // <command>
                    //     .
                    //     .
                    //     .
                    // <command>
                    many1(Command::parse),
                )),
                // .end
                opt(dot_command("end")),
            ),
            |(name, fields, commands)| {
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
                Self {
                    name,
                    inputs,
                    outputs,
                    clocks,
                    commands,
                }
            },
        )(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_parser {
        ($test_name:ident, $test_fn:expr, $test_cases:expr) => {
            #[test]
            fn $test_name() {
                let tests = $test_cases;
                for (input, expected, rest) in tests {
                    assert_eq!($test_fn(input), Ok((rest, expected)));
                }
            }
        };
    }

    test_parser!(
        test_node_name,
        node_name,
        [
            ("node_a node_b", "node_a", " node_b"),
            ("a[0] a[1]", "a[0]", " a[1]"),
            ("A.B.C\n", "A.B.C", "\n"),
        ]
    );

    test_parser!(
        test_single_output,
        SingleOutput::parse,
        [
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
        ]
    );

    test_parser!(
        test_logic_gate,
        LogicGate::parse,
        [
            (
                r#".names a b c d
00- 0
1-0 1
--1 1
"#,
                LogicGate {
                    exdc: false,
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
                r#".exdc
.names a b c e
0-1 1
--0 1
-01 1
"#,
                LogicGate {
                    exdc: true,
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
        ]
    );

    test_parser!(
        test_model,
        Model::parse,
        [
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
                        exdc: false,
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
.exdc
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
                        exdc: true,
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
        ]
    );
}
