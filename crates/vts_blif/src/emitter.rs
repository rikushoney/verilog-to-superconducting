#![allow(dead_code, unused_variables)]

use crate::ast;

use std::io::{self, Write};

pub struct Emitter<W: Write> {
    sink: W,
}

enum LogicValueStyle {
    Numeric,
    Symbolic,
}

impl<W: Write> Emitter<W> {
    pub fn new(sink: W) -> Self {
        Self { sink }
    }

    fn emit_space(&mut self) -> io::Result<()> {
        write!(self.sink, " ")
    }

    fn emit_newline(&mut self) -> io::Result<()> {
        write!(self.sink, "\n")
    }

    fn emit_single_output(&mut self, single_output: &ast::SingleOutput) -> io::Result<()> {
        for logic_value in &single_output.inputs {
            self.emit_logic_value(&logic_value, LogicValueStyle::Symbolic)?;
        }
        self.emit_space()?;
        self.emit_logic_value(&single_output.output, LogicValueStyle::Symbolic)
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
            self.emit_newline()?;
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

    fn emit_logic_value(
        &mut self,
        logic_value: &ast::LogicValue,
        style: LogicValueStyle,
    ) -> io::Result<()> {
        match logic_value {
            ast::LogicValue::Zero => write!(self.sink, "0"),
            ast::LogicValue::One => write!(self.sink, "1"),
            ast::LogicValue::DontCare => write!(
                self.sink,
                "{}",
                match style {
                    LogicValueStyle::Numeric => "2",
                    LogicValueStyle::Symbolic => "-",
                }
            ),
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
        self.emit_space()?;
        self.emit_latch_control(&generic_latch.control)?;
        self.emit_space()?;
        self.emit_logic_value(&generic_latch.init, LogicValueStyle::Numeric)
    }

    fn emit_library_latch(&mut self, library_latch: &ast::LibraryLatch) -> io::Result<()> {
        self.emit_space()?;
        self.emit_latch_control(&library_latch.control)?;
        self.emit_space()?;
        self.emit_logic_value(&library_latch.init, LogicValueStyle::Numeric)
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
        write!(self.sink, "{} {} ", command, library_gate.name)?;
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
                unimplemented!()
            }
            ast::Command::ClockConstraint(clock_constraint) => {
                unimplemented!()
            }
            ast::Command::DelayConstraint(delay_constraint) => {
                unimplemented!()
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
            self.emit_newline()?;
        }
        write!(self.sink, ".end")?;
        Ok(())
    }
}
