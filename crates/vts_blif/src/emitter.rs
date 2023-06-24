use crate::error::Error;
use crate::*;

use std::io::Write;

pub struct Emitter<W: Write> {
    sink: W,
}

pub enum LogicValueFormat {
    Numeric,
    Symbolic,
}

macro_rules! emit {
    ($dst:expr, $($arg:tt)*) => {
        write!($dst, $($arg)*).map_err(|err| Error::IoError(err))
    }
}

macro_rules! emitln {
    ($dst:expr, $($arg:tt)*) => {
        writeln!($dst, $($arg)*).map_err(|err| Error::IoError(err))
    }
}

impl<W: Write> Emitter<W> {
    pub fn new(sink: W) -> Self {
        Self { sink }
    }

    fn emit_space(&mut self) -> Result<(), Error> {
        emit!(self.sink, " ")
    }

    fn emit_newline(&mut self) -> Result<(), Error> {
        emitln!(self.sink, "")
    }

    pub fn emit_logic_value(
        &mut self,
        value: &LogicValue,
        format: LogicValueFormat,
    ) -> Result<(), Error> {
        match value {
            LogicValue::Zero => {
                emit!(self.sink, "0")
            }
            LogicValue::One => {
                emit!(self.sink, "1")
            }
            LogicValue::DontCare => {
                emit!(
                    self.sink,
                    "{}",
                    match format {
                        LogicValueFormat::Numeric => "2",
                        LogicValueFormat::Symbolic => "-",
                    }
                )
            }
            LogicValue::Unknown => {
                emit!(self.sink, "3")
            }
        }
    }

    pub fn emit_single_output(&mut self, single_output: &SingleOutput) -> Result<(), Error> {
        for input in single_output.inputs.iter() {
            self.emit_logic_value(input, LogicValueFormat::Symbolic)?;
        }
        self.emit_space()?;
        self.emit_logic_value(&single_output.output, LogicValueFormat::Symbolic)
    }

    pub fn emit_logic_gate(&mut self, logic_gate: &LogicGate) -> Result<(), Error> {
        let inputs = logic_gate.inputs.join(" ");
        emit!(self.sink, ".names {inputs} {}", logic_gate.output)?;
        for single_output in logic_gate.pla_description.iter() {
            self.emit_newline()?;
            self.emit_single_output(single_output)?;
        }
        Ok(())
    }

    pub fn emit_generic_latch(&mut self, generic_latch: &GenericLatch) -> Result<(), Error> {
        emit!(
            self.sink,
            ".latch {} {} ",
            generic_latch.input,
            generic_latch.output
        )?;
        if let Some(control) = generic_latch.control.as_ref() {
            let trigger = match control.trigger {
                LatchTrigger::FallingEdge => "fe",
                LatchTrigger::RisingEdge => "re",
                LatchTrigger::ActiveHigh => "ah",
                LatchTrigger::ActiveLow => "al",
                LatchTrigger::Asynchronous => "as",
            };
            let clock = match control.clock {
                LatchControlKind::Global => "NIL",
                LatchControlKind::Clock(signal) => signal,
            };
            emit!(self.sink, "{trigger} {clock} ")?;
        }
        self.emit_logic_value(&generic_latch.init_val, LogicValueFormat::Numeric)
    }

    pub fn emit_formal_actual(&mut self, formal_actual: &FormalActual) -> Result<(), Error> {
        emit!(
            self.sink,
            "{}={}",
            formal_actual.formal,
            formal_actual.actual
        )
    }

    pub fn emit_library_gate(&mut self, library_gate: &LibraryGate) -> Result<(), Error> {
        emit!(self.sink, ".gate {}", library_gate.name)?;
        for formal_actual in library_gate.formal_actual_list.iter() {
            self.emit_space()?;
            self.emit_formal_actual(formal_actual)?;
        }
        Ok(())
    }

    pub fn emit_model_reference(&mut self, model_reference: &ModelReference) -> Result<(), Error> {
        emit!(self.sink, ".subckt {}", model_reference.name)?;
        for formal_actual in model_reference.formal_actual_list.iter() {
            self.emit_space()?;
            self.emit_formal_actual(formal_actual)?;
        }
        Ok(())
    }

    pub fn emit_command(&mut self, command: &Command) -> Result<(), Error> {
        match command {
            Command::LogicGate(logic_gate) => self.emit_logic_gate(logic_gate),
            Command::GenericLatch(generic_latch) => self.emit_generic_latch(generic_latch),
            Command::LibraryGate(library_gate) => self.emit_library_gate(library_gate),
            Command::ModelReference(model_reference) => self.emit_model_reference(model_reference),
        }
    }

    pub fn emit_model(&mut self, model: &Model) -> Result<(), Error> {
        emit!(self.sink, ".model")?;
        if let Some(name) = model.name.as_ref() {
            emit!(self.sink, " {name}")?;
        }
        if !model.inputs.is_empty() {
            self.emit_newline()?;
            emit!(self.sink, ".inputs")?;
            for input in model.inputs.iter() {
                emit!(self.sink, " {input}")?;
            }
        }
        if !model.outputs.is_empty() {
            self.emit_newline()?;
            emit!(self.sink, ".outputs")?;
            for output in model.outputs.iter() {
                emit!(self.sink, " {output}")?;
            }
        }
        for command in model.commands.iter() {
            self.emit_newline()?;
            self.emit_command(command)?;
        }
        emit!(self.sink, ".end")
    }
}
