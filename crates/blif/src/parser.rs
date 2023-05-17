#![allow(dead_code)]

use crate::ast::*;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit0, line_ending, not_line_ending, one_of, satisfy},
    combinator::{map, map_res, opt, recognize, success, value, verify},
    error::{ContextError, FromExternalError, ParseError},
    multi::{many0_count, many1, many1_count, separated_list1},
    number::complete::double,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

use std::num::ParseIntError;
use std::path;

// Ident ::= ([^#=] - S)+
fn ident<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(many1_count(satisfy(|ch| {
        !ch.is_whitespace() && "#=".chars().all(|illegal| ch != illegal)
    })))(input)
}

// .Ident
fn dot_command<'a, E: ParseError<&'a str>>(
    command: &'static str,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E> {
    preceded(char('.'), tag(command))
}

// Comment ::= '#' [^\n]* "\n"
fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    delimited(char('#'), not_line_ending, line_ending)(input)
}

// EOL ::= S* (S* ("\n" | Comment) S*)+
fn end_of_line<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(tuple((
        many_space,
        many1_count(tuple((many_space, alt((line_ending, comment)), many_space))),
    )))(input)
}

// S ::= [ \t] | "\\\n"
fn space<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    alt((recognize(one_of(" \t")), tag("\\\n")))(input)
}

// S*
fn many_space<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(many0_count(space))(input)
}

// S+
fn some_space<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    recognize(many1_count(space))(input)
}

// PosInt ::= [0-9]+ - "0"
fn positive_integer<'a, E: ParseError<&'a str> + FromExternalError<&'a str, ParseIntError>>(
    input: &'a str,
) -> IResult<&'a str, usize, E> {
    verify(map_res(digit0, |int: &'a str| int.parse()), |int| *int != 0)(input)
}

// SingleOutput ::= InputPlane+ S+ OutputPlane
impl<'a> SingleOutput {
    fn parse<E: ParseError<&'a str> + ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
        let input_plane = |input| {
            alt((
                value(LogicValue::Zero, char('0')),
                value(LogicValue::One, char('1')),
                value(LogicValue::DontCare, char('-')),
            ))(input)
        };
        let output_plane = |input| {
            alt((
                value(LogicValue::Zero, char('0')),
                value(LogicValue::One, char('1')),
            ))(input)
        };
        map(
            separated_pair(many1(input_plane), some_space, output_plane),
            |(inputs, output)| Self { inputs, output },
        )(input)
    }
}

// SingleOutputCover ::= SingleOutput (EOL SingleOutput)*
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

// LogicValue ::= [0-3]
impl<'a> LogicValue {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        alt((
            value(LogicValue::Zero, char('0')),
            value(LogicValue::One, char('1')),
            value(LogicValue::DontCare, char('2')),
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

// SubfileReference ::= ".search" S+ Filename
// Filename         ::= ("\"" [^"] "\"") | Ident
impl<'a> SubfileReference<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        let filename = |input| {
            map(
                alt((
                    ident,
                    delimited(
                        char('"'),
                        recognize(many0_count(satisfy(|ch| ch != '"'))),
                        char('"'),
                    ),
                )),
                path::Path::new,
            )(input)
        };
        map(
            preceded(terminated(dot_command("search"), some_space), filename),
            |filename| Self { filename },
        )(input)
    }
}

// StateTransition ::= LogicValue+ S+ Ident S+ Ident S+ LogicValue+
impl<'a> StateTransition<'a> {
    fn parse<
        E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
    >(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
        map(
            tuple((
                terminated(many1(LogicValue::parse), some_space),
                terminated(ident, some_space),
                terminated(ident, some_space),
                many1(LogicValue::parse),
            )),
            |(inputs, current_state, next_state, outputs)| StateTransition {
                inputs,
                current_state,
                next_state,
                outputs,
            },
        )(input)
    }
}

// FsmDescription  ::= ".start_kiss" EOL FsmFields EOL StateMapping EOL ".end_kiss" FsmEnd
// FsmFields       ::= NumInputs EOL NumOutputs (EOL NumTerms)? (EOL NumStates)? (EOL ResetState)?
// NumInputs       ::= ".i" S+ PosInt
// NumOutputs      ::= ".o" S+ PosInt
// NumTerms        ::= ".p" S+ PosInt
// NumStates       ::= ".s" S+ PosInt
// ResetState      ::= ".r" S+ Ident
// StateMapping    ::= StateTransition (EOL StateTransition)*
// FsmEnd          ::= (EOL LatchOrder)? (EOL CodeMapping)?
// LatchOrder      ::= ".latch_order" S+ LatchOrderList
// LatchOrderList  ::= Ident (S+ Ident)*
// CodeMapping     ::= CodeMap (EOL CodeMap)*
// CodeMap         ::= ".code" S+ Ident S+ Ident
impl<'a> FsmDescription<'a> {
    fn parse<
        E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
    >(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
        map(
            tuple((
                delimited(
                    terminated(dot_command("start_kiss"), end_of_line),
                    preceded(terminated(dot_command("i"), some_space), positive_integer),
                    end_of_line,
                ),
                preceded(terminated(dot_command("o"), some_space), positive_integer),
                opt(preceded(
                    delimited(end_of_line, dot_command("p"), some_space),
                    positive_integer,
                )),
                opt(preceded(
                    delimited(end_of_line, dot_command("s"), some_space),
                    positive_integer,
                )),
                opt(preceded(
                    delimited(end_of_line, dot_command("r"), some_space),
                    ident,
                )),
                terminated(
                    separated_list1(end_of_line, StateTransition::parse),
                    preceded(end_of_line, dot_command("end_kiss")),
                ),
                alt((
                    preceded(
                        delimited(end_of_line, dot_command("latch_order"), some_space),
                        separated_list1(some_space, ident),
                    ),
                    success(vec![]),
                )),
                alt((
                    preceded(
                        end_of_line,
                        separated_list1(
                            end_of_line,
                            preceded(
                                terminated(dot_command("code"), some_space),
                                pair(terminated(ident, some_space), ident),
                            ),
                        ),
                    ),
                    success(vec![]),
                )),
            )),
            |(
                num_inputs,
                num_outputs,
                num_terms,
                num_states,
                reset_state,
                state_mapping,
                latch_order,
                code_mapping,
            )| {
                Self {
                    num_inputs,
                    num_outputs,
                    num_terms,
                    num_states,
                    reset_state,
                    state_mapping,
                    latch_order,
                    code_mapping,
                }
            },
        )(input)
    }
}

// Event ::= [rf] "'" Ident (S+ Number S+ Number)?
impl<'a> Event<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        let clock_edge_skew = |input| {
            map(
                pair(delimited(some_space, double, some_space), double),
                |(before, after)| ClockEdgeSkew { before, after },
            )(input)
        };
        map(
            tuple((
                alt((
                    value(ClockEdgeKind::Rise, char('r')),
                    value(ClockEdgeKind::Fall, char('f')),
                )),
                preceded(char('\''), ident),
                alt((clock_edge_skew, success(ClockEdgeSkew::default()))),
            )),
            |(edge, clock, skew)| Event { edge, clock, skew },
        )(input)
    }
}

// ClockEvent ::= ".clock_event" S+ Number S+ EventsList
// EventsList ::= Event (S+ Event)*
impl<'a> ClockEvent<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            tuple((
                preceded(
                    terminated(dot_command("clock_event"), some_space),
                    terminated(double, some_space),
                ),
                separated_list1(some_space, Event::parse),
            )),
            |(event_percent, events)| Self {
                event_percent,
                events,
            },
        )(input)
    }
}

// ClockConstraint ::= ".cycle" S+ Number EOL ClockEvents EOL
// ClockEvents     ::= ClockEvent (EOL ClockEvent)*
impl<'a> ClockConstraint<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            tuple((
                preceded(
                    terminated(dot_command("cycle"), some_space),
                    terminated(double, end_of_line),
                ),
                terminated(separated_list1(end_of_line, ClockEvent::parse), end_of_line),
            )),
            |(cycle_time, clock_events)| Self {
                cycle_time,
                clock_events,
            },
        )(input)
    }
}

// Delay       ::= ".delay" S+ Ident S+ DelayFields
// DelayFields ::= Phase S+ Number S+ Number S+ Number S+ Number S+ Number S+ Number
// Phase       ::= "INV" | "NONINV" | "UNKNOWN"
impl<'a> Delay<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            tuple((
                delimited(
                    terminated(dot_command("delay"), some_space),
                    ident,
                    some_space,
                ),
                terminated(
                    alt((
                        value(DelayPhaseKind::Inverting, tag("INV")),
                        value(DelayPhaseKind::NonInverting, tag("NONINV")),
                        value(DelayPhaseKind::Unknown, tag("UNKNOWN")),
                    )),
                    some_space,
                ),
                terminated(double, some_space),
                terminated(double, some_space),
                terminated(double, some_space),
                terminated(double, some_space),
                terminated(double, some_space),
                double,
            )),
            |(in_name, phase, load, max_load, block_rise, drive_rise, block_fall, drive_fall)| {
                Self {
                    in_name,
                    phase,
                    load,
                    max_load,
                    block_rise,
                    drive_rise,
                    block_fall,
                    drive_fall,
                }
            },
        )(input)
    }
}

// RelativeEvent ::= [ba] S+ [rf] "'" Ident
impl<'a> RelativeEvent<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            tuple((
                terminated(
                    alt((
                        value(ClockEventPositionKind::Before, char('b')),
                        value(ClockEventPositionKind::After, char('a')),
                    )),
                    some_space,
                ),
                terminated(
                    alt((
                        value(ClockEdgeKind::Rise, char('r')),
                        value(ClockEdgeKind::Fall, char('f')),
                    )),
                    char('\''),
                ),
                ident,
            )),
            |(position, edge, clock)| RelativeEvent {
                position,
                edge,
                clock,
            },
        )(input)
    }
}

// InputArrival ::= ".input_arrival" S+ Ident S+ Number S+ Number (S+ RelativeEvent)?
impl<'a> InputArrival<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            tuple((
                delimited(
                    terminated(dot_command("input_arrival"), some_space),
                    ident,
                    some_space,
                ),
                terminated(double, some_space),
                double,
                opt(preceded(some_space, RelativeEvent::parse)),
            )),
            |(in_name, rise, fall, event)| Self {
                in_name,
                rise,
                fall,
                event,
            },
        )(input)
    }
}

// OutputRequired ::= ".output_required" S+ Ident S+ Number S+ Number (S+ RelativeEvent)?
impl<'a> OutputRequired<'a> {
    fn parse<E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Self, E> {
        map(
            tuple((
                delimited(
                    terminated(dot_command("output_required"), some_space),
                    ident,
                    some_space,
                ),
                terminated(double, some_space),
                double,
                opt(preceded(some_space, RelativeEvent::parse)),
            )),
            |(out_name, rise, fall, event)| Self {
                out_name,
                rise,
                fall,
                event,
            },
        )(input)
    }
}

// DelayConstraintKind ::= Area              |
//                         Delay             |
//                         WireLoadSlope     |
//                         Wire              |
//                         InputArrival      |
//                         DefInputArrival   |
//                         OutputRequired    |
//                         DefOutputRequired |
//                         InputDrive        |
//                         DefInputDrive     |
//                         MaxInputLoad      |
//                         DefMaxInputLoad   |
//                         OutputLoad        |
//                         DefOutputLoad
impl<'a> DelayConstraintKind<'a> {
    fn parse<E: ParseError<&'a str> + ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
        // Area ::= ".area" S+ Number
        let area = |input| {
            map(
                preceded(terminated(dot_command("area"), some_space), double),
                DelayConstraintKind::Area,
            )(input)
        };
        // WireLoadSlope ::= ".wire_load_slope" S+ Number
        let wire_load_slope = |input| {
            map(
                preceded(
                    terminated(dot_command("wire_load_slope"), some_space),
                    double,
                ),
                DelayConstraintKind::WireLoadSlope,
            )(input)
        };
        // Wire         ::= ".wire" S+ WireLoadList
        // WireLoadList ::= Number (S+ Number)*
        let wire = |input| {
            map(
                preceded(
                    terminated(dot_command("wire"), some_space),
                    separated_list1(some_space, double),
                ),
                DelayConstraintKind::Wire,
            )(input)
        };
        // DefInputArrival ::= ".default_input_arrival" S+ Number S+ Number
        let default_input_arrival = |input| {
            map(
                pair(
                    preceded(
                        terminated(dot_command("default_input_arrival"), some_space),
                        terminated(double, some_space),
                    ),
                    double,
                ),
                |(rise, fall)| DelayConstraintKind::DefaultInputArrival { rise, fall },
            )(input)
        };
        // DefOutputRequired ::= ".default_output_required" S+ Number S+ Number
        let default_output_required = |input| {
            map(
                pair(
                    preceded(
                        terminated(dot_command("default_output_required"), some_space),
                        terminated(double, some_space),
                    ),
                    double,
                ),
                |(rise, fall)| DelayConstraintKind::DefaultOutputRequired { rise, fall },
            )(input)
        };
        // InputDrive ::= ".input_drive" S+ Ident S+ Number S+ Number
        let input_drive = |input| {
            map(
                tuple((
                    preceded(
                        terminated(dot_command("input_drive"), some_space),
                        terminated(ident, some_space),
                    ),
                    terminated(double, some_space),
                    double,
                )),
                |(in_name, rise, fall)| DelayConstraintKind::InputDrive {
                    in_name,
                    rise,
                    fall,
                },
            )(input)
        };
        // DefInputDrive ::= ".default_input_drive" S+ Number S+ Number
        let default_input_drive = |input| {
            map(
                pair(
                    preceded(
                        terminated(dot_command("default_input_drive"), some_space),
                        terminated(double, some_space),
                    ),
                    double,
                ),
                |(rise, fall)| DelayConstraintKind::DefaultInputDrive { rise, fall },
            )(input)
        };
        // MaxInputLoad ::= ".max_input_load" S+ Ident S+ Number
        let max_input_load = |input| {
            map(
                pair(
                    preceded(
                        terminated(dot_command("max_input_load"), some_space),
                        terminated(ident, some_space),
                    ),
                    double,
                ),
                |(in_name, load)| DelayConstraintKind::MaxInputLoad { in_name, load },
            )(input)
        };
        // DefMaxInputLoad ::= ".default_max_input_load" S+ Number
        let default_max_input_load = |input| {
            map(
                preceded(
                    terminated(dot_command("default_max_input_load"), some_space),
                    double,
                ),
                DelayConstraintKind::DefaultMaxInputLoad,
            )(input)
        };
        // OutputLoad ::= ".output_load" S+ Ident S+ Number
        let output_load = |input| {
            map(
                pair(
                    preceded(
                        terminated(dot_command("output_load"), some_space),
                        terminated(ident, some_space),
                    ),
                    double,
                ),
                |(out_name, load)| DelayConstraintKind::OutputLoad { out_name, load },
            )(input)
        };
        // DefOutputLoad ::= ".default_output_load" S+ Number
        let default_output_load = |input| {
            map(
                preceded(
                    terminated(dot_command("default_output_load"), some_space),
                    double,
                ),
                DelayConstraintKind::DefaultOutputLoad,
            )(input)
        };
        alt((
            area,
            map(Delay::parse, DelayConstraintKind::Delay),
            wire_load_slope,
            wire,
            map(InputArrival::parse, DelayConstraintKind::InputArrival),
            default_input_arrival,
            map(OutputRequired::parse, DelayConstraintKind::OutputRequired),
            default_output_required,
            input_drive,
            default_input_drive,
            max_input_load,
            default_max_input_load,
            output_load,
            default_output_load,
        ))(input)
    }
}

// DelayConstraint ::= DelayConstraintKind (EOF DelayConstraintKind)*
impl<'a> DelayConstraint<'a> {
    fn parse<E: ParseError<&'a str> + ContextError<&'a str>>(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
        map(
            separated_list1(end_of_line, DelayConstraintKind::parse),
            |constraints| DelayConstraint { constraints },
        )(input)
    }
}

// Command ::= LogicGate        |
//             GenericLatch     |
//             LibraryGate      |
//             ModelReference   |
//             SubfileReference |
//             FsmDescription   |
//             ClockConstraint  |
//             DelayConstraint
impl<'a> Command<'a> {
    fn parse<
        E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
    >(
        input: &'a str,
    ) -> IResult<&'a str, Self, E> {
        alt((
            map(LogicGate::parse, Command::LogicGate),
            map(GenericLatch::parse, Command::GenericLatch),
            map(LibraryGate::parse, Command::LibraryGate),
            map(ModelReference::parse, Command::ModelReference),
            map(SubfileReference::parse, Command::SubfileReference),
            map(FsmDescription::parse, Command::FsmDescription),
            map(ClockConstraint::parse, Command::ClockConstraint),
            map(DelayConstraint::parse, Command::DelayConstraint),
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
fn commands<
    'a,
    E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
>(
    input: &'a str,
) -> IResult<&'a str, Vec<Command>, E> {
    separated_list1(end_of_line, Command::parse)(input)
}

// Model ::= ".model" S+ Ident EOL ModelFields EOL Commands (EOL ".end")?
impl<'a> Model<'a> {
    fn parse<
        E: ParseError<&'a str> + ContextError<&'a str> + FromExternalError<&'a str, ParseIntError>,
    >(
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
                        /*
                        if actual != expected {
                            println!("parser succeeded but did not get expected value");
                            println!("expected:\n{:#?}", expected);
                            println!("actual:\n{:#?}", actual);
                        }
                        if remaining != rest {
                            println!("parser succeeded but remaining is not as expected");
                            println!("expected remaining:\n{:#?}", rest);
                            println!("actual remaining:\n{:#?}", remaining);
                        }
                        if actual != expected || remaining != rest {
                            panic!();
                        }
                        */
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
        test_positive_integer,
        positive_integer,
        [("123", 123, ""), ("1024", 1024, ""), ("012", 12, "")]
    );

    test_parser!(
        test_single_output,
        SingleOutput::parse,
        [
            (
                "10-1 0\n",
                SingleOutput {
                    inputs: vec![
                        LogicValue::One,
                        LogicValue::Zero,
                        LogicValue::DontCare,
                        LogicValue::One
                    ],
                    output: LogicValue::Zero,
                },
                "\n",
            ),
            (
                "-111 0\n",
                SingleOutput {
                    inputs: vec![
                        LogicValue::DontCare,
                        LogicValue::One,
                        LogicValue::One,
                        LogicValue::One
                    ],
                    output: LogicValue::Zero
                },
                "\n",
            ),
            (
                "0-11 1\n",
                SingleOutput {
                    inputs: vec![
                        LogicValue::Zero,
                        LogicValue::DontCare,
                        LogicValue::One,
                        LogicValue::One
                    ],
                    output: LogicValue::One
                },
                "\n",
            ),
            (
                "0-11 1\n# a comment\n",
                SingleOutput {
                    inputs: vec![
                        LogicValue::Zero,
                        LogicValue::DontCare,
                        LogicValue::One,
                        LogicValue::One
                    ],
                    output: LogicValue::One
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
                            inputs: vec![LogicValue::Zero, LogicValue::Zero, LogicValue::DontCare],
                            output: LogicValue::Zero,
                        },
                        SingleOutput {
                            inputs: vec![LogicValue::One, LogicValue::DontCare, LogicValue::Zero],
                            output: LogicValue::One,
                        },
                        SingleOutput {
                            inputs: vec![
                                LogicValue::DontCare,
                                LogicValue::DontCare,
                                LogicValue::One,
                            ],
                            output: LogicValue::One,
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
                            inputs: vec![LogicValue::Zero, LogicValue::DontCare, LogicValue::One],
                            output: LogicValue::One,
                        },
                        SingleOutput {
                            inputs: vec![
                                LogicValue::DontCare,
                                LogicValue::DontCare,
                                LogicValue::Zero
                            ],
                            output: LogicValue::One,
                        },
                        SingleOutput {
                            inputs: vec![LogicValue::DontCare, LogicValue::Zero, LogicValue::One,],
                            output: LogicValue::One,
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
        test_clock_constraint,
        ClockConstraint::parse,
        [(
            r#".cycle 10
.clock_event 50 r'clk1 f'clk2
.clock_event 75 f'global 1 1
"#,
            ClockConstraint {
                cycle_time: 10.0,
                clock_events: vec![
                    ClockEvent {
                        event_percent: 50.0,
                        events: vec![
                            Event {
                                edge: ClockEdgeKind::Rise,
                                clock: "clk1",
                                skew: ClockEdgeSkew {
                                    before: 0.0,
                                    after: 0.0,
                                }
                            },
                            Event {
                                edge: ClockEdgeKind::Fall,
                                clock: "clk2",
                                skew: ClockEdgeSkew {
                                    before: 0.0,
                                    after: 0.0
                                }
                            }
                        ]
                    },
                    ClockEvent {
                        event_percent: 75.0,
                        events: vec![Event {
                            edge: ClockEdgeKind::Fall,
                            clock: "global",
                            skew: ClockEdgeSkew {
                                before: 1.0,
                                after: 1.0
                            }
                        },]
                    }
                ]
            },
            ""
        )]
    );

    test_parser!(
        test_delay_constraint,
        DelayConstraint::parse,
        [
            (
                ".area 10",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::Area(10.0)]
                },
                ""
            ),
            (
                ".delay input_a INV 5.0 10 2.5 3.1 2.7 2.43",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::Delay(Delay {
                        in_name: "input_a",
                        phase: DelayPhaseKind::Inverting,
                        load: 5.0,
                        max_load: 10.0,
                        block_rise: 2.5,
                        drive_rise: 3.1,
                        block_fall: 2.7,
                        drive_fall: 2.43,
                    })]
                },
                ""
            ),
            (
                ".wire_load_slope 3.141",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::WireLoadSlope(3.141)]
                },
                ""
            ),
            (
                ".wire 1.0 1.5 1.4",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::Wire(vec![1.0, 1.5, 1.4])]
                },
                ""
            ),
            (
                ".input_arrival node[1] 10.0 11.0",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::InputArrival(InputArrival {
                        in_name: "node[1]",
                        rise: 10.0,
                        fall: 11.0,
                        event: None,
                    })]
                },
                ""
            ),
            (
                ".input_arrival node[2] 13.0 12.0 b r'clk1",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::InputArrival(InputArrival {
                        in_name: "node[2]",
                        rise: 13.0,
                        fall: 12.0,
                        event: Some(RelativeEvent {
                            position: ClockEventPositionKind::Before,
                            edge: ClockEdgeKind::Rise,
                            clock: "clk1"
                        }),
                    })]
                },
                ""
            ),
            (
                ".default_input_arrival 3.2 4.3",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::DefaultInputArrival {
                        rise: 3.2,
                        fall: 4.3
                    }]
                },
                ""
            ),
            (
                ".output_required sum_out 9.2 9.54",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::OutputRequired(OutputRequired {
                        out_name: "sum_out",
                        rise: 9.2,
                        fall: 9.54,
                        event: None
                    })]
                },
                ""
            ),
            (
                ".output_required carry(out) 8.55 4.33 a f'clk2",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::OutputRequired(OutputRequired {
                        out_name: "carry(out)",
                        rise: 8.55,
                        fall: 4.33,
                        event: Some(RelativeEvent {
                            position: ClockEventPositionKind::After,
                            edge: ClockEdgeKind::Fall,
                            clock: "clk2"
                        })
                    })]
                },
                ""
            ),
            (
                ".default_output_required 5.67 9.45",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::DefaultOutputRequired {
                        rise: 5.67,
                        fall: 9.45
                    }]
                },
                ""
            ),
            (
                ".input_drive input_a 15 14",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::InputDrive {
                        in_name: "input_a",
                        rise: 15.0,
                        fall: 14.0
                    }]
                },
                ""
            ),
            (
                ".default_input_drive 22.5 14.25",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::DefaultInputDrive {
                        rise: 22.5,
                        fall: 14.25
                    }]
                },
                ""
            ),
            (
                ".max_input_load in.a 123.456",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::MaxInputLoad {
                        in_name: "in.a",
                        load: 123.456
                    }]
                },
                ""
            ),
            (
                ".default_max_input_load 99",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::DefaultMaxInputLoad(99.0)]
                },
                ""
            ),
            (
                ".output_load S 15.67",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::OutputLoad {
                        out_name: "S",
                        load: 15.67
                    }]
                },
                ""
            ),
            (
                ".default_output_load 33.33",
                DelayConstraint {
                    constraints: vec![DelayConstraintKind::DefaultOutputLoad(33.33)]
                },
                ""
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
                                inputs: vec![LogicValue::Zero, LogicValue::Zero, LogicValue::Zero,],
                                output: LogicValue::Zero,
                            },
                            SingleOutput {
                                inputs: vec![
                                    LogicValue::DontCare,
                                    LogicValue::One,
                                    LogicValue::DontCare,
                                ],
                                output: LogicValue::One,
                            },
                            SingleOutput {
                                inputs: vec![
                                    LogicValue::Zero,
                                    LogicValue::DontCare,
                                    LogicValue::Zero
                                ],
                                output: LogicValue::Zero,
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
                                inputs: vec![
                                    LogicValue::One,
                                    LogicValue::DontCare,
                                    LogicValue::Zero,
                                ],
                                output: LogicValue::One,
                            },
                            SingleOutput {
                                inputs: vec![
                                    LogicValue::DontCare,
                                    LogicValue::Zero,
                                    LogicValue::One,
                                ],
                                output: LogicValue::Zero,
                            },
                            SingleOutput {
                                inputs: vec![
                                    LogicValue::DontCare,
                                    LogicValue::One,
                                    LogicValue::DontCare
                                ],
                                output: LogicValue::Zero,
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
                                inputs: vec![LogicValue::One],
                                output: LogicValue::One
                            },]
                        }),
                        Command::LogicGate(LogicGate {
                            exdc: false,
                            inputs: vec!["A2"],
                            output: "JJ",
                            pla_description: vec![SingleOutput {
                                inputs: vec![LogicValue::One],
                                output: LogicValue::One
                            },]
                        })
                    ]
                },
                ""
            )
        ]
    );
}
