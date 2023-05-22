#![allow(dead_code, unused_variables)]

use crate::ast;

use std::io::{self, Write};

pub struct Emitter<W: Write> {
    sink: W,
}

impl<W: Write> Emitter<W> {
    pub fn new(sink: W) -> Self {
        Self { sink }
    }

    fn emit_single_output(&mut self, single_output: &ast::SingleOutput) -> io::Result<()> {
        for logic_value in &single_output.inputs {
            self.emit_logic_value(&logic_value, false)?;
        }
        write!(self.sink, " ")?;
        self.emit_logic_value(&single_output.output, false)?;
        write!(self.sink, "\n")
    }

    fn emit_logic_gate(&mut self, logic_gate: &ast::LogicGate) -> io::Result<()> {
        if logic_gate.exdc {
            writeln!(self.sink, ".exdc")?;
        }
        writeln!(
            self.sink,
            ".names {} {}",
            logic_gate.inputs.join(" "),
            logic_gate.output
        )?;
        for single_output in &logic_gate.pla_description {
            self.emit_single_output(&single_output)?;
        }
        Ok(())
    }

    fn emit_latch_kind(&mut self, latch_kind: &ast::LatchKind) -> io::Result<()> {
        match latch_kind {
            ast::LatchKind::FallingEdge => write!(self.sink, "fe"),
            ast::LatchKind::RisingEdge => write!(self.sink, "re"),
            ast::LatchKind::ActiveHigh => write!(self.sink, "ah"),
            ast::LatchKind::ActiveLow => write!(self.sink, "al"),
            ast::LatchKind::Asynchronous => write!(self.sink, "as"),
        }
    }

    fn emit_logic_value(&mut self, logic_value: &ast::LogicValue, numeric: bool) -> io::Result<()> {
        match logic_value {
            ast::LogicValue::Zero => write!(self.sink, "0"),
            ast::LogicValue::One => write!(self.sink, "1"),
            ast::LogicValue::DontCare => write!(self.sink, "{}", if numeric { "2" } else { "-" }),
            ast::LogicValue::Unknown => write!(self.sink, "3"),
        }
    }

    fn emit_latch_control(&mut self, latch_control: &ast::LatchControl) -> io::Result<()> {
        match latch_control {
            ast::LatchControl::Clock(clock) => write!(self.sink, "{}", clock),
            ast::LatchControl::GlobalClock => write!(self.sink, "NIL"),
        }
    }

    fn emit_generic_latch(&mut self, generic_latch: &ast::GenericLatch) -> io::Result<()> {
        write!(
            self.sink,
            ".latch {} {} ",
            generic_latch.input, generic_latch.output
        )?;
        self.emit_latch_kind(&generic_latch.kind)?;
        write!(self.sink, " ")?;
        self.emit_latch_control(&generic_latch.control)?;
        write!(self.sink, " ")?;
        self.emit_logic_value(&generic_latch.init, true)
    }

    fn emit_library_latch(&mut self, library_latch: &ast::LibraryLatch) -> io::Result<()> {
        write!(self.sink, " ")?;
        self.emit_latch_control(&library_latch.control)?;
        write!(self.sink, " ")?;
        self.emit_logic_value(&library_latch.init, true)
    }

    fn emit_library_technology(
        &mut self,
        library_technology: &ast::LibraryTechnology,
    ) -> io::Result<()> {
        match library_technology {
            ast::LibraryTechnology::Gate => Ok(()),
            ast::LibraryTechnology::Latch(library_latch) => self.emit_library_latch(&library_latch),
        }
    }

    fn emit_formal_actual(&mut self, formal_actual: &ast::FormalActual) -> io::Result<()> {
        write!(
            self.sink,
            "{}",
            formal_actual
                .iter()
                .map(|(formal, actual)| format!("{}={}", formal, actual))
                .collect::<Vec<String>>()
                .join(" ")
        )
    }

    fn emit_library_gate(&mut self, library_gate: &ast::LibraryGate) -> io::Result<()> {
        let command = match library_gate.technology {
            ast::LibraryTechnology::Gate => ".gate",
            ast::LibraryTechnology::Latch(_) => ".mlatch",
        };
        write!(self.sink, "{} ", library_gate.name)?;
        self.emit_formal_actual(&library_gate.formal_actual)?;
        match library_gate.technology {
            ast::LibraryTechnology::Gate => (),
            ast::LibraryTechnology::Latch(_) => write!(self.sink, " ")?,
        };
        self.emit_library_technology(&library_gate.technology)
    }

    fn emit_model_reference(&mut self, model_reference: &ast::ModelReference) -> io::Result<()> {
        write!(self.sink, ".subckt {} ", model_reference.name)?;
        self.emit_formal_actual(&model_reference.formal_actual)
    }

    fn emit_subfile_reference(
        &mut self,
        subfile_reference: &ast::SubfileReference,
    ) -> io::Result<()> {
        write!(
            self.sink,
            ".search {}",
            subfile_reference.filename.display()
        )
    }

    fn emit_state_transition(&mut self, state_transition: &ast::StateTransition) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_fsm_description(&mut self, fsm_description: &ast::FsmDescription) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_clock_edge_skew(&mut self, clock_edge_skew: &ast::ClockEdgeSkew) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_clock_edge_kind(&mut self, clock_edge_kind: &ast::ClockEdgeKind) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_event(&mut self, event: &ast::Event) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_clock_event(&mut self, clock_event: &ast::ClockEvent) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_clock_constraint(&mut self, clock_constraint: &ast::ClockConstraint) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_delay_phase_kind(&mut self, delay_phase_kind: &ast::DelayPhaseKind) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_delay(&mut self, delay: &ast::Delay) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_clock_event_position(
        &mut self,
        clock_event_position: &ast::ClockEventPosition,
    ) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_relative_event(&mut self, relative_event: &ast::RelativeEvent) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_input_arrival(&mut self, input_arrival: &ast::InputArrival) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_output_required(&mut self, output_required: &ast::OutputRequired) -> io::Result<()> {
        unimplemented!()
    }

    fn emit_delay_constraint_kind(
        &mut self,
        delay_constraint_kind: &ast::DelayConstraintKind,
    ) -> io::Result<()> {
        match delay_constraint_kind {
            ast::DelayConstraintKind::Area { area } => writeln!(self.sink, ".area {}", area),
            ast::DelayConstraintKind::Delay(delay) => self.emit_delay(&delay),
            ast::DelayConstraintKind::WireLoadSlope { load } => {
                writeln!(self.sink, ".wire_load_slope {}", load)
            }
            ast::DelayConstraintKind::Wire { loads } => {
                writeln!(
                    self.sink,
                    ".wire {}",
                    loads
                        .iter()
                        .map(|load| load.to_string())
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
            ast::DelayConstraintKind::InputArrival(input_arrival) => {
                self.emit_input_arrival(&input_arrival)
            }
            ast::DelayConstraintKind::DefaultInputArrival { rise, fall } => {
                writeln!(self.sink, ".default_input_arrival {} {}", rise, fall)
            }
            ast::DelayConstraintKind::OutputRequired(output_required) => {
                self.emit_output_required(&output_required)
            }
            ast::DelayConstraintKind::DefaultOutputRequired { rise, fall } => {
                writeln!(self.sink, ".default_output_required {} {}", rise, fall)
            }
            ast::DelayConstraintKind::InputDrive {
                in_name,
                rise,
                fall,
            } => writeln!(self.sink, ".input_drive {} {} {}", in_name, rise, fall),
            ast::DelayConstraintKind::DefaultInputDrive { rise, fall } => {
                writeln!(self.sink, ".default_input_drive {} {}", rise, fall)
            }
            ast::DelayConstraintKind::MaxInputLoad { in_name, load } => {
                writeln!(self.sink, ".max_input_load {} {}", in_name, load)
            }
            ast::DelayConstraintKind::DefaultMaxInputLoad { load } => {
                writeln!(self.sink, ".default_max_input_load {}", load)
            }
            ast::DelayConstraintKind::OutputLoad { out_name, load } => {
                writeln!(self.sink, ".output_load {} {}", out_name, load)
            }
            ast::DelayConstraintKind::DefaultOutputLoad { load } => {
                writeln!(self.sink, ".default_output_load {}", load)
            }
        }
    }

    fn emit_delay_constraint(&mut self, delay_constraint: &ast::DelayConstraint) -> io::Result<()> {
        for constraint in &delay_constraint.constraints {
            self.emit_delay_constraint_kind(&constraint)?;
        }
        Ok(())
    }

    fn emit_command(&mut self, command: &ast::Command) -> io::Result<()> {
        match command {
            ast::Command::LogicGate(logic_gate) => self.emit_logic_gate(logic_gate),
            ast::Command::GenericLatch(generic_latch) => self.emit_generic_latch(generic_latch),
            ast::Command::LibraryGate(library_gate) => self.emit_library_gate(library_gate),
            ast::Command::ModelReference(model_reference) => {
                self.emit_model_reference(model_reference)
            }
            ast::Command::SubfileReference(subfile_reference) => {
                self.emit_subfile_reference(subfile_reference)
            }
            ast::Command::FsmDescription(fsm_description) => {
                self.emit_fsm_description(fsm_description)
            }
            ast::Command::ClockConstraint(clock_constraint) => {
                self.emit_clock_constraint(clock_constraint)
            }
            ast::Command::DelayConstraint(delay_constraint) => {
                self.emit_delay_constraint(delay_constraint)
            }
        }
    }

    fn emit_model(&mut self, model: &ast::Model) -> io::Result<()> {
        write!(self.sink, ".model")?;
        if let Some(name) = model.name {
            write!(self.sink, " {}", name)?;
        }
        write!(self.sink, "\n")?;
        if !model.inputs.is_empty() {
            writeln!(self.sink, ".inputs {}", model.inputs.join(" "))?;
        }
        if !model.outputs.is_empty() {
            writeln!(self.sink, ".outputs {}", model.outputs.join(" "))?;
        }
        if !model.clocks.is_empty() {
            writeln!(self.sink, ".clock {}", model.clocks.join(" "))?;
        }
        for command in &model.commands {
            self.emit_command(&command)?;
        }
        write!(self.sink, ".end")?;
        Ok(())
    }
}
