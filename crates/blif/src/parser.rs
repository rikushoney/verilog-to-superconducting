#![allow(dead_code)]

use crate::ast::*;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, line_ending, not_line_ending, one_of, satisfy},
    combinator::{fail, map, opt, recognize, success, value},
    error::{context, ContextError, ParseError},
    multi::{many0_count, many1, many1_count, separated_list1},
    number::complete::double,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

use std::path;

// Ident ::= ([^#=] - S)+
fn ident<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    // identifiers can be anything except whitespace, '#' or '='
    recognize(many1_count(satisfy(|ch| {
        !ch.is_whitespace() && "#=".chars().all(|illegal| ch != illegal)
    })))(input)
}

fn dot_command<'a, E: ParseError<&'a str>>(
    command: &'static str,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E> {
    // .<command>
    preceded(char('.'), tag(command))
}

// Comment ::= '#' [^\n]* \n
fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    // # ...
    delimited(char('#'), not_line_ending, line_ending)(input)
}

// EOL ::= S* (S* (\n | Comment) S*)+
fn end_of_line<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(tuple((
        many_space,
        many1_count(tuple((many_space, alt((line_ending, comment)), many_space))),
    )))(input)
}

fn space<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    alt((recognize(one_of(" \t")), tag("\\\n")))(input)
}

fn many_space<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(many0_count(space))(input)
}

fn some_space<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(many1_count(space))(input)
}

macro_rules! unimplemented_command {
    ($input:expr) => {{
        context("unimplemented command", fail)($input)
    }};
}

impl<'a> SingleOutput<'a> {
    fn parse<E: ParseError<&'a str> + ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
        let input_plane = alt((char('0'), char('1'), char('-')));
        let output_plane = alt((char('0'), char('1')));
        map(
            separated_pair(
                recognize(many1_count(input_plane)),
                some_space,
                output_plane,
            ),
            |(inputs, output)| Self { inputs, output },
        )(input)
    }
}

// SingleOutputCover ::= InputPlane+ OutputPlane (EOL InputPlane+ OutputPlane)*
fn single_output_cover<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<SingleOutput>, E> {
    separated_list1(end_of_line, SingleOutput::parse)(input)
}

// LogicGate ::= (".exdc" EOL)? ".names" S+ SignalList EOL SingleOutputCover
impl<'a> LogicGate<'a> {
    fn parse<E: ParseError<&'a str> + ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
        map(
            tuple((
                map(opt(terminated(dot_command("exdc"), end_of_line)), |exdc| {
                    exdc.is_some()
                }),
                delimited(
                    terminated(dot_command("names"), some_space),
                    signal_list,
                    end_of_line,
                ),
                single_output_cover,
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

// LatchKind ::= "fe" | "re" | "ah" | "al" | "as"
impl<'a> LatchKind {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        alt((
            value(LatchKind::FallingEdge, tag("fe")),
            value(LatchKind::RisingEdge, tag("re")),
            value(LatchKind::ActiveHigh, tag("ah")),
            value(LatchKind::ActiveLow, tag("al")),
            value(LatchKind::Asynchronous, tag("as")),
        ))(input)
    }
}

// LogicValue ::= [0-3] | DontCare
impl<'a> LogicValue {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        alt((
            value(LogicValue::Zero, char('0')),
            value(LogicValue::One, char('1')),
            value(LogicValue::DontCare, one_of("2-xX")),
            value(LogicValue::Unknown, char('3')),
        ))(input)
    }
}

// LatchControl ::= Ident | "NIL"
impl<'a> LatchControl<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(ident, |control| match control {
            "NIL" => LatchControl::GlobalClock,
            clock => LatchControl::Clock(clock),
        })(input)
    }
}

// GenericLatch ::= ".latch" S+ Ident S+ Ident S+ LatchKind S+ LatchControl (S+ LogicValue)?
impl<'a> GenericLatch<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            preceded(
                terminated(dot_command("latch"), some_space),
                tuple((
                    terminated(ident, some_space),
                    terminated(ident, some_space),
                    terminated(LatchKind::parse, some_space),
                    LatchControl::parse,
                    alt((
                        preceded(some_space, LogicValue::parse),
                        success(LogicValue::default()),
                    )),
                )),
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

// FormalActualList ::= Ident "=" Ident (S+ Ident "=" Ident)*
fn formal_actual_list<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, FormalActual<'a>, E> {
    separated_list1(some_space, separated_pair(ident, char('='), ident))(input)
}

// GateTechnology ::= ".gate" S+ Ident S+ FormalActualList
fn gate_technology<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, LibraryGate, E> {
    map(
        preceded(
            terminated(dot_command("gate"), some_space),
            pair(terminated(ident, some_space), formal_actual_list),
        ),
        |(name, formal_actual)| LibraryGate {
            name,
            formal_actual,
            technology: LibraryTechnology::Gate,
        },
    )(input)
}

// LatchTechnology ::= ".mlatch" S+ Ident S+ FormalActualList S+ LatchControl (S+ LogicValue)?
fn latch_technology<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, LibraryGate, E> {
    map(
        preceded(
            terminated(dot_command("mlatch"), some_space),
            tuple((
                terminated(ident, some_space),
                terminated(formal_actual_list, some_space),
                LatchControl::parse,
                opt(preceded(some_space, LogicValue::parse)),
            )),
        ),
        |(name, formal_actual, control, init)| LibraryGate {
            name,
            formal_actual,
            technology: LibraryTechnology::Latch(LibraryLatch {
                control,
                init: init.unwrap_or(LogicValue::default()),
            }),
        },
    )(input)
}

// LibraryGate ::= GateTechnology | LatchTechnology
impl<'a> LibraryGate<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        alt((gate_technology, latch_technology))(input)
    }
}

// ModelReference ::= ".subckt" S+ Ident S+ FormalActualList
impl<'a> ModelReference<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            preceded(
                terminated(dot_command("subckt"), some_space),
                pair(terminated(ident, some_space), formal_actual_list),
            ),
            |(name, formal_actual)| Self {
                name,
                formal_actual,
            },
        )(input)
    }
}

// SubfileReference ::= ".search" S+ Ident
impl<'a> SubfileReference<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            preceded(
                terminated(dot_command("search"), some_space),
                map(ident, path::Path::new),
            ),
            |filename| Self { filename },
        )(input)
    }
}

impl<'a> FsmDescription {
    fn parse<E: ParseError<&'a str> + ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
        unimplemented_command!(input)
    }
}

impl<'a> ClockEvent<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        // .clock_event <event-percent> <event-1> [<event-2> ... <event-n>]
        map(
            tuple((
                delimited(
                    // .clock_event
                    terminated(dot_command("clock_event"), some_space),
                    // <event-percent>
                    double,
                    some_space,
                ),
                // <event-1> [<event-2> ... <event-n>]
                terminated(
                    separated_list1(
                        some_space,
                        tuple((
                            terminated(
                                alt((
                                    value(RiseFall::Rise, char('r')),
                                    value(RiseFall::Fall, char('f')),
                                )),
                                char('\''),
                            ),
                            ident,
                            opt(preceded(
                                some_space,
                                map(
                                    separated_pair(double, some_space, double),
                                    |(before, after)| BeforeAfter { before, after },
                                ),
                            )),
                        )),
                    ),
                    end_of_line,
                ),
            )),
            |(event_percent, events)| Self {
                event_percent,
                events,
            },
        )(input)
    }
}

impl<'a> ClockConstraint<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            tuple((
                // .cycle <cycle-time>
                delimited(
                    terminated(dot_command("cycle"), some_space),
                    double,
                    end_of_line,
                ),
                // .clock_event <event-percent> <event-1> [<event-2> ... <event-n>]
                many1(terminated(ClockEvent::parse, end_of_line)),
            )),
            |(cycle_time, clock_events)| Self {
                cycle_time,
                clock_events,
            },
        )(input)
    }
}

impl<'a> DelayConstraint {
    fn parse<E: ParseError<&'a str> + ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
        unimplemented_command!(input)
    }
}

macro_rules! command_parser {
    ($name:ident => $struct:ident) => {
        map($struct::parse, |$name| Command::$struct($name))
    };
}

impl<'a> Command<'a> {
    fn parse<E: ParseError<&'a str> + ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
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

// SignalList ::= Ident (S+ Ident)*
fn signal_list<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Vec<&'a str>, E> {
    separated_list1(some_space, ident)(input)
}

// ModelField  ::= (InputsList | OutputsList | ClockList)
// InputsList  ::= ".inputs" S+ SignalList
// OutputsList ::= ".outputs" S+ SignalList
// ClockList   ::= ".clock" S+ SignalList
impl<'a> ModelField<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            alt((
                pair(terminated(dot_command("inputs"), some_space), signal_list),
                pair(terminated(dot_command("outputs"), some_space), signal_list),
                pair(terminated(dot_command("clock"), some_space), signal_list),
            )),
            |(name, items)| match name {
                "inputs" => ModelField::Inputs(items),
                "outputs" => ModelField::Outputs(items),
                "clock" => ModelField::Clock(items),
                _ => panic!("unsupported model field"),
            },
        )(input)
    }
}

// ModelFields ::= ModelField (EOL ModelField)*
fn model_fields<'a, E: ParseError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<ModelField>, E> {
    separated_list1(end_of_line, ModelField::parse)(input)
}

// Commands ::= Command (EOL Command)*
fn commands<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<Command>, E> {
    separated_list1(end_of_line, Command::parse)(input)
}

// Model ::= ".model" S+ Ident EOL ModelFields EOL Commands (EOL ".end")?
impl<'a> Model<'a> {
    fn parse<E: ParseError<&'a str> + ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&str, Self, E> {
        map(
            delimited(
                terminated(dot_command("model"), some_space),
                tuple((
                    opt(terminated(ident, end_of_line)),
                    terminated(model_fields, end_of_line),
                    commands,
                )),
                opt(pair(end_of_line, dot_command("end"))),
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

    use nom::{
        error::{convert_error, VerboseError},
        Finish,
    };

    macro_rules! test_parser {
        ($test_name:ident, $test_fn:expr, $test_cases:expr) => {
            #[test]
            fn $test_name() {
                let tests = $test_cases;
                for (input, expected, rest) in tests {
                    let result: IResult<_, _, VerboseError<&str>> = $test_fn(input);
                    if let Ok((remaining, actual)) = result {
                        assert_eq!(actual, expected);
                        assert_eq!(remaining, rest);
                    } else {
                        panic!(
                            "parser failed with errors:\n{}",
                            convert_error(input, result.finish().err().unwrap())
                        );
                    }
                }
            }
        };
    }

    test_parser!(
        test_some_space,
        some_space,
        [("\t   \t\\\naaa", "\t   \t\\\n", "aaa")]
    );

    test_parser!(
        test_ident,
        ident,
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
                "\n",
            ),
            (
                "-111 0\n",
                SingleOutput {
                    inputs: "-111",
                    output: '0',
                },
                "\n",
            ),
            (
                "0-11 1\n",
                SingleOutput {
                    inputs: "0-11",
                    output: '1',
                },
                "\n",
            ),
            (
                "0-11 1\n# a comment\n",
                SingleOutput {
                    inputs: "0-11",
                    output: '1',
                },
                "\n# a comment\n",
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
                "\n",
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
                "\n",
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
                " "
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
        test_subfile_reference,
        SubfileReference::parse,
        [(
            ".search myFunkyFile.blif",
            SubfileReference {
                filename: path::Path::new("myFunkyFile.blif")
            },
            ""
        )]
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
