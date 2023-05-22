#![allow(dead_code, unused_variables)]

use crate::ast;

use std::io::Write;

pub enum Error {
    Unknown,
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Emitter<W: Write> {
    sink: W,
}

impl<W: Write> Emitter<W> {
    pub fn new(sink: W) -> Self {
        Self { sink }
    }

    fn emit_single_output(&mut self, single_output: &ast::SingleOutput) -> Result<()> {
        unimplemented!()
    }

    fn emit_logic_gate(&mut self, logic_gate: &ast::LogicGate) -> Result<()> {
        unimplemented!()
    }

    fn emit_latch_kind(&mut self, latch_kind: &ast::LatchKind) -> Result<()> {
        unimplemented!()
    }

    fn emit_logic_value(&mut self, logic_value: &ast::LogicValue) -> Result<()> {
        unimplemented!()
    }

    fn emit_latch_control(&mut self, latch_control: &ast::LatchControl) -> Result<()> {
        unimplemented!()
    }

    fn emit_generic_latch(&mut self, generic_latch: &ast::GenericLatch) -> Result<()> {
        unimplemented!()
    }

    fn emit_library_latch(&mut self, library_latch: &ast::LibraryLatch) -> Result<()> {
        unimplemented!()
    }

    fn emit_library_technology(
        &mut self,
        library_technology: &ast::LibraryTechnology,
    ) -> Result<()> {
        unimplemented!()
    }

    fn emit_formal_actual(&mut self, formal_actual: &ast::FormalActual) -> Result<()> {
        unimplemented!()
    }

    fn emit_library_gate(&mut self, library_gate: &ast::LibraryGate) -> Result<()> {
        unimplemented!()
    }

    fn emit_model_reference(&mut self, model_reference: &ast::ModelReference) -> Result<()> {
        unimplemented!()
    }

    fn emit_subfile_reference(&mut self, subfile_reference: &ast::SubfileReference) -> Result<()> {
        unimplemented!()
    }

    fn emit_state_transition(&mut self, state_transition: &ast::StateTransition) -> Result<()> {
        unimplemented!()
    }

    fn emit_fsm_description(&mut self, fsm_description: &ast::FsmDescription) -> Result<()> {
        unimplemented!()
    }

    fn emit_clock_edge_skew(&mut self, clock_edge_skew: &ast::ClockEdgeSkew) -> Result<()> {
        unimplemented!()
    }

    fn emit_clock_edge_kind(&mut self, clock_edge_kind: &ast::ClockEdgeKind) -> Result<()> {
        unimplemented!()
    }

    fn emit_event(&mut self, event: &ast::Event) -> Result<()> {
        unimplemented!()
    }

    fn emit_clock_event(&mut self, clock_event: &ast::ClockEvent) -> Result<()> {
        unimplemented!()
    }

    fn emit_clock_constraint(&mut self, clock_constraint: &ast::ClockConstraint) -> Result<()> {
        unimplemented!()
    }

    fn emit_delay_phase_kind(&mut self, delay_phase_kind: &ast::DelayPhaseKind) -> Result<()> {
        unimplemented!()
    }

    fn emit_delay(&mut self, delay: &ast::Delay) -> Result<()> {
        unimplemented!()
    }

    fn emit_clock_event_position(
        &mut self,
        clock_event_position: &ast::ClockEventPosition,
    ) -> Result<()> {
        unimplemented!()
    }

    fn emit_relative_event(&mut self, relative_event: &ast::RelativeEvent) -> Result<()> {
        unimplemented!()
    }

    fn emit_input_arrival(&mut self, input_arrival: &ast::InputArrival) -> Result<()> {
        unimplemented!()
    }

    fn emit_output_required(&mut self, output_required: &ast::OutputRequired) -> Result<()> {
        unimplemented!()
    }

    fn emit_delay_constraint_kind(
        &mut self,
        delay_constraint_kind: &ast::DelayConstraintKind,
    ) -> Result<()> {
        unimplemented!()
    }

    fn emit_delay_constraint(&mut self, delay_constraint: &ast::DelayConstraint) -> Result<()> {
        unimplemented!()
    }

    fn emit_command(&mut self, command: &ast::Command) -> Result<()> {
        unimplemented!()
    }

    fn emit_model(&mut self, model: &ast::Model) -> Result<()> {
        unimplemented!()
    }
}
