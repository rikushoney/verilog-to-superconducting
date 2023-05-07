#![allow(dead_code)]

use crate::ast::*;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, line_ending, not_line_ending, one_of, satisfy, space0, space1},
    combinator::{eof, fail, map, map_res, opt, recognize, success, value},
    error::context,
    multi::{many0, many1, many1_count, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

fn signal_name(input: &str) -> IResult<&str, &str> {
    // signal names can be anything except whitespace, '#' or '='
    recognize(many1_count(satisfy(|ch| {
        !ch.is_whitespace() && "#=".chars().all(|invalid| ch != invalid)
    })))(input)
}

fn dot_command<'a>(command: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    // .<command>
    preceded(char('.'), tag(command))
}

fn comment(input: &str) -> IResult<&str, &str> {
    // # ...
    delimited(char('#'), not_line_ending, line_ending)(input)
}

fn ensure_newline(input: &str) -> IResult<&str, &str> {
    preceded(
        space0,
        alt((
            // skip whitespace and comments and ensure that there is at least a single newline
            recognize(many1_count(delimited(
                space0,
                recognize(alt((line_ending, comment))),
                space0,
            ))),
            // treat end-of-file as newline in case a newline is missing
            eof,
        )),
    )(input)
}

fn space1_escape(input: &str) -> IResult<&str, &str> {
    // treat '\' at the end of line as regular whitespace
    recognize(many1_count(alt((tag("\\\n"), space1))))(input)
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
            // <input-plane> <output-plane>
            tuple((
                terminated(recognize(many1_count(input_plane)), space1_escape),
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
                    pair(dot_command("names"), space1_escape),
                    separated_list1(space1_escape, signal_name),
                    ensure_newline,
                ),
                // <single-output-cover>
                many1(SingleOutput::parse),
            )),
            |(exdc, mut inputs, pla_description)| {
                // it is guaranteed that `inputs` contains at least a single element due to
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

impl LatchKind {
    fn parse(input: &str) -> IResult<&str, Self> {
        alt((
            value(LatchKind::FallingEdge, tag("fe")),
            value(LatchKind::RisingEdge, tag("re")),
            value(LatchKind::ActiveHigh, tag("ah")),
            value(LatchKind::ActiveLow, tag("al")),
            value(LatchKind::Asynchronous, tag("as")),
        ))(input)
    }
}

impl LogicValue {
    fn parse(input: &str) -> IResult<&str, Self> {
        alt((
            value(LogicValue::Zero, char('0')),
            value(LogicValue::One, char('1')),
            value(LogicValue::DontCare, one_of("2-xX")),
            value(LogicValue::Unknown, char('3')),
        ))(input)
    }
}

impl<'a> LatchControl<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(signal_name, |control| match control {
            "NIL" => LatchControl::GlobalClock,
            clock => LatchControl::Clock(clock),
        })(input)
    }
}

impl<'a> GenericLatch<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            delimited(
                // .latch
                terminated(dot_command("latch"), space1_escape),
                tuple((
                    // input
                    terminated(signal_name, space1_escape),
                    // output
                    terminated(signal_name, space1_escape),
                    // type
                    terminated(LatchKind::parse, space1_escape),
                    // control
                    terminated(LatchControl::parse, opt(space1_escape)),
                    // init-val (default: Unknown)
                    alt((LogicValue::parse, success(LogicValue::default()))),
                )),
                ensure_newline,
            ),
            |(input, output, kind, control, init)| GenericLatch {
                input,
                output,
                kind,
                control,
                init,
            },
        )(input)
    }
}

fn formal_actual<'a>(input: &'a str) -> IResult<&'a str, FormalActual<'a>> {
    // formal1=actual1 formal2=actual2 ...
    separated_list1(
        space1_escape,
        separated_pair(signal_name, char('='), signal_name),
    )(input)
}

impl<'a> LibraryGate<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        // <control> [<init-val>]
        let library_latch = |input| {
            map(
                tuple((
                    terminated(LatchControl::parse, opt(space1_escape)),
                    alt((LogicValue::parse, success(LogicValue::default()))),
                )),
                |(control, init)| LibraryTechnology::Latch(LibraryLatch { control, init }),
            )(input)
        };
        map(
            alt((
                tuple((
                    delimited(
                        // .gate
                        terminated(dot_command("gate"), space1_escape),
                        // <name>
                        signal_name,
                        space1_escape,
                    ),
                    // <formal-actual-list>
                    terminated(formal_actual, ensure_newline),
                    success(LibraryTechnology::Gate),
                )),
                tuple((
                    delimited(
                        // .mlatch
                        terminated(dot_command("mlatch"), space1_escape),
                        // <name>
                        signal_name,
                        space1_escape,
                    ),
                    // <formal-actual>
                    terminated(formal_actual, space1_escape),
                    library_latch,
                )),
            )),
            |(name, formal_actual, technology)| LibraryGate {
                name,
                formal_actual,
                technology,
            },
        )(input)
    }
}

impl<'a> ModelReference<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        map(
            // .subckt <model-name> <formal-actual-list>
            tuple((
                // .subckt <model-name>
                delimited(
                    // .subckt
                    terminated(dot_command("subckt"), space1_escape),
                    // <model-name>
                    signal_name,
                    space1_escape,
                ),
                // <formal-actual-list>
                terminated(formal_actual, ensure_newline),
            )),
            |(name, formal_actual)| Self {
                name,
                formal_actual,
            },
        )(input)
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
                    terminated(dot_command(name), space1_escape),
                    separated_list1(space1_escape, signal_name),
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
                        pair(dot_command("model"), space1_escape),
                        opt(signal_name),
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
        test_space1_escape,
        space1_escape,
        [("\t   \t\\\naaa", "\t   \t\\\n", "aaa")]
    );

    test_parser!(
        test_signal_name,
        signal_name,
        [
            ("signal_a signal_b", "signal_a", " signal_b"),
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
            (
                "0-11 1\n# a comment\n",
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
.names a b c \
e
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
        test_latch,
        GenericLatch::parse,
        [
            (
                ".latch a b ah clk 1 ",
                GenericLatch {
                    input: "a",
                    output: "b",
                    kind: LatchKind::ActiveHigh,
                    control: LatchControl::Clock("clk"),
                    init: LogicValue::One,
                },
                ""
            ),
            (
                ".latch a b re NIL",
                GenericLatch {
                    input: "a",
                    output: "b",
                    kind: LatchKind::RisingEdge,
                    control: LatchControl::GlobalClock,
                    init: LogicValue::Unknown,
                },
                ""
            )
        ]
    );

    test_parser!(
        test_library_gate,
        LibraryGate::parse,
        [
            (
                ".gate and2 A=v1 B=v2 O=x",
                LibraryGate {
                    name: "and2",
                    formal_actual: vec![("A", "v1"), ("B", "v2"), ("O", "x")],
                    technology: LibraryTechnology::Gate
                },
                ""
            ),
            (
                ".mlatch jk_rising_edge J=j K=k Q=q clk 1",
                LibraryGate {
                    name: "jk_rising_edge",
                    formal_actual: vec![("J", "j"), ("K", "k"), ("Q", "q")],
                    technology: LibraryTechnology::Latch(LibraryLatch {
                        control: LatchControl::Clock("clk"),
                        init: LogicValue::One
                    })
                },
                ""
            )
        ]
    );

    test_parser!(
        test_model_reference,
        ModelReference::parse,
        [(
            ".subckt submodel a=v1 b=v2 c=v3",
            ModelReference {
                name: "submodel",
                formal_actual: vec![("a", "v1"), ("b", "v2"), ("c", "v3")]
            },
            ""
        ),]
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
.names a b c d # this is a comment
1-0 1
-01 0
# a freestanding comment
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
            (
                r#".model myModel
.inputs a b c
.outputs d e f
.clock clk1 clk2
.latch a d fe clk1 2
.end"#,
                Model {
                    name: Some("myModel"),
                    inputs: vec!["a", "b", "c"],
                    outputs: vec!["d", "e", "f"],
                    clocks: vec!["clk1", "clk2"],
                    commands: vec![Command::GenericLatch(GenericLatch {
                        input: "a",
                        output: "d",
                        kind: LatchKind::FallingEdge,
                        control: LatchControl::Clock("clk1"),
                        init: LogicValue::DontCare
                    })],
                },
                "",
            ),
            (
                r#".model 4bitadder
.inputs A3 A2 A1 A0 B3 B2 B1 B0 CIN
.outputs COUT S3 S2 S1 S0
.subckt fulladder a=A0 b=B0 cin=CIN s=S0 cout=CARRY1
.subckt fulladder a=A3 b=B3 cin=CARRY3 s=S3 cout=COUT
.subckt fulladder b=B1 a=A1 cin=CARRY1 s=XX cout=CARRY2
.subckt fulladder a=JJ b=B2 cin=CARRY2 s=S2 cout=CARRY3
# for the sake of example,
.names XX S1 # formal output ‘s’ does not fanout to a primary output
1 1
.names A2 JJ # formal input ‘a’ does not fanin from a primary input
1 1
.end"#,
                Model {
                    name: Some("4bitadder"),
                    inputs: vec!["A3", "A2", "A1", "A0", "B3", "B2", "B1", "B0", "CIN"],
                    outputs: vec!["COUT", "S3", "S2", "S1", "S0"],
                    clocks: vec![],
                    commands: vec![
                        Command::ModelReference(ModelReference {
                            name: "fulladder",
                            formal_actual: vec![
                                ("a", "A0"),
                                ("b", "B0"),
                                ("cin", "CIN"),
                                ("s", "S0"),
                                ("cout", "CARRY1")
                            ]
                        }),
                        Command::ModelReference(ModelReference {
                            name: "fulladder",
                            formal_actual: vec![
                                ("a", "A3"),
                                ("b", "B3"),
                                ("cin", "CARRY3"),
                                ("s", "S3"),
                                ("cout", "COUT")
                            ]
                        }),
                        Command::ModelReference(ModelReference {
                            name: "fulladder",
                            formal_actual: vec![
                                ("b", "B1"),
                                ("a", "A1"),
                                ("cin", "CARRY1"),
                                ("s", "XX"),
                                ("cout", "CARRY2")
                            ]
                        }),
                        Command::ModelReference(ModelReference {
                            name: "fulladder",
                            formal_actual: vec![
                                ("a", "JJ"),
                                ("b", "B2"),
                                ("cin", "CARRY2"),
                                ("s", "S2"),
                                ("cout", "CARRY3")
                            ]
                        }),
                        Command::LogicGate(LogicGate {
                            exdc: false,
                            inputs: vec!["XX"],
                            output: "S1",
                            pla_description: vec![SingleOutput {
                                inputs: "1",
                                output: '1'
                            },]
                        }),
                        Command::LogicGate(LogicGate {
                            exdc: false,
                            inputs: vec!["A2"],
                            output: "JJ",
                            pla_description: vec![SingleOutput {
                                inputs: "1",
                                output: '1'
                            },]
                        })
                    ]
                },
                ""
            )
        ]
    );
}
