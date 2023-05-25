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
        let mut iter = logic_gate.pla_description.iter();
        if let Some(first) = iter.next() {
            self.emit_single_output(&first)?;
        }
        for single_output in iter {
            self.emit_newline()?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use std::io::BufWriter;

    macro_rules! test_emitter {
        ($test_name:ident, $test_fn:ident, $test_cases:expr) => {
            #[test]
            fn $test_name() {
                let tests = $test_cases;
                for (ast, expected) in tests {
                    let sink = BufWriter::new(Vec::new());
                    let mut emitter = Emitter::new(sink);
                    if !emitter.$test_fn(&ast).is_ok() {
                        println!("emitter I/O error");
                        panic!();
                    }
                    emitter.sink.flush().unwrap();
                    let emitted = std::str::from_utf8(emitter.sink.get_ref()).unwrap();
                    if emitted != expected {
                        println!("emitted does not equal expected");
                        println!("emitted:\n{}", emitted);
                        println!("expected:\n{}", expected);
                        panic!();
                    }
                }
            }
        };
    }

    test_emitter!(
        test_single_output,
        emit_single_output,
        [
            (
                SingleOutput {
                    inputs: vec![LogicValue::One, LogicValue::One, LogicValue::Zero],
                    output: LogicValue::Zero
                },
                "110 0"
            ),
            (
                SingleOutput {
                    inputs: vec![LogicValue::DontCare, LogicValue::One],
                    output: LogicValue::One
                },
                "-1 1"
            )
        ]
    );

    test_emitter!(
        test_logic_gate,
        emit_logic_gate,
        [(
            LogicGate {
                exdc: false,
                inputs: vec!["a", "b", "c"],
                output: "d",
                pla_description: vec![
                    SingleOutput {
                        inputs: vec![LogicValue::One, LogicValue::DontCare, LogicValue::Zero],
                        output: LogicValue::Zero
                    },
                    SingleOutput {
                        inputs: vec![LogicValue::DontCare, LogicValue::Zero, LogicValue::DontCare],
                        output: LogicValue::One
                    }
                ]
            },
            r#".names a b c d
1-0 0
-0- 1"#
        )]
    );

    test_emitter!(
        test_generic_latch,
        emit_generic_latch,
        [(
            GenericLatch {
                input: "a",
                output: "b",
                kind: LatchKind::RisingEdge,
                control: LatchControl::Clock("clk"),
                init: LogicValue::Unknown
            },
            ".latch a b re clk 3"
        )]
    );

    test_emitter!(
        test_library_gate,
        emit_library_gate,
        [
            (
                LibraryGate {
                    name: "split_01",
                    formal_actual: vec![("a", "b"), ("c", "d")],
                    technology: LibraryTechnology::Gate
                },
                ".gate split_01 a=b c=d"
            ),
            (
                LibraryGate {
                    name: "and_65",
                    formal_actual: vec![("input_1", "wire_x")],
                    technology: LibraryTechnology::Latch(LibraryLatch {
                        control: LatchControl::GlobalClock,
                        init: LogicValue::One
                    })
                },
                ".mlatch and_65 input_1=wire_x NIL 1"
            )
        ]
    );

    test_emitter!(
        test_model_reference,
        emit_model_reference,
        [(
            ModelReference {
                name: "merger",
                formal_actual: vec![("wire_x", "output_y"), ("input_a", "wire_vv")]
            },
            ".subckt merger wire_x=output_y input_a=wire_vv"
        )]
    );

    test_emitter!(
        test_subfile_reference,
        emit_subfile_reference,
        [(
            SubfileReference {
                filename: std::path::Path::new("another.blif")
            },
            ".search another.blif"
        )]
    );

    test_emitter!(
        test_model,
        emit_model,
        [(
            Model {
                name: Some("interesting_model"),
                inputs: vec!["aa", "ab", "ac"],
                outputs: vec!["zz", "zy", "zx"],
                clocks: vec!["clk1", "clk2"],
                commands: vec![Command::LogicGate(LogicGate {
                    exdc: true,
                    inputs: vec!["aa", "ab"],
                    output: "zy",
                    pla_description: vec![SingleOutput {
                        inputs: vec![LogicValue::One, LogicValue::One],
                        output: LogicValue::One
                    }]
                })]
            },
            r#".model interesting_model
.inputs aa ab ac
.outputs zz zy zx
.clock clk1 clk2
.exdc
.names aa ab zy
11 1
.end"#
        )]
    );
}
