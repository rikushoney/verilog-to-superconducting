#[derive(Clone, Debug, PartialEq)]
pub struct Port<'a> {
    pub name: &'a str,
    pub is_clock: bool,
    pub clock: Option<&'a str>,
    pub comb_sink_ports: Vec<&'a str>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Model<'a> {
    pub name: &'a str,
    pub never_prune: bool,
    pub input_ports: Vec<Port<'a>>,
    pub output_ports: Vec<Port<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tag<'a> {
    Models(Vec<Model<'a>>),
    Tiles,
    Layout,
    Device,
    SwitchList,
    SegmentList,
    DirectList,
    ComplexBlockList,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Architecture<'a> {
    pub tags: Vec<Tag<'a>>,
}
