use std::collections::HashMap;
use std::rc::Rc;

/// A reference to a identifier, short for "identifier string"
///
/// There are two kinds of identifiers:
/// - Publically visible identifiers
/// - Auto-generated identifiers
///
/// Publically visible identifiers usually stem from user input while auto-generated identifiers
/// are generated by code.
pub enum IdStr<'a> {
    Public(&'a str),
    Generated(Rc<String>),
}

/// The kind of port associated with a wire. Can be either `Input` or `Output` for input and output
/// ports, respectively.
pub enum PortKind {
    Input,
    Output,
}

/// A named wire in a netlist; used to interconnect cells.
pub struct Wire<'a> {
    pub name: IdStr<'a>,
    pub kind: PortKind,
}

/// A binary logic value.
/// Can be one of
/// - 0
/// - 1
/// - don't care
/// - unknown
pub enum LogicValue {
    Zero,
    One,
    DontCare,
    Unknown,
}

/// A constant value consisting of many bits.
pub struct Const {
    pub value: Vec<LogicValue>,
}

/// The kind of chunk that a `SigChunk` contains.
/// Can be either a chunk inside a wire or a constant value.
pub enum SigChunkKind<'a> {
    Wire { wire: Rc<Wire<'a>>, offset: usize },
    Const(Const),
}

/// A chunk (slice) of signals.
pub struct SigChunk<'a> {
    pub chunk: SigChunkKind<'a>,
    pub width: usize,
}

/// The kind of bit that a `SigBit` contains.
/// Can be either a bit of a wire or a constant value.
pub enum SigBitKind<'a> {
    Wire { wire: Rc<Wire<'a>>, offset: usize },
    Const(LogicValue),
}

/// A single bit signal.
pub struct SigBit<'a> {
    pub bit: SigBitKind<'a>,
    pub index: usize,
}

/// A group of signals that consists of many signal chunks and single bit signals.
pub struct SigSpec<'a> {
    pub chunks: Vec<SigChunk<'a>>,
    pub bits: Vec<SigBit<'a>>,
}

/// The kind of component that a `Cell` contains.
/// Can be either a gate (which models combinational logic) or a latch (which models sequencial
/// logic).
pub enum CellKind {
    Gate,
    Latch { destructive: bool },
}

/// A component used to model combinational or sequencial logic.
pub struct Cell<'a> {
    pub name: IdStr<'a>,
    pub kind: CellKind,
}

pub type Connection<'a> = (SigSpec<'a>, SigSpec<'a>);

/// A collection of cells and wires as well as a description of how these components are
/// interconnected.
pub struct Module<'a> {
    pub name: IdStr<'a>,
    pub wires: HashMap<IdStr<'a>, Wire<'a>>,
    pub cells: HashMap<IdStr<'a>, Cell<'a>>,
    pub connections: Vec<Connection<'a>>,
}
