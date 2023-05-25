#[derive(Clone, Debug, PartialEq)]
pub enum ArchitectureTag {
    Models,
    Tiles,
    Layout,
    Device,
    SwitchList,
    SegmentList,
    DirectList,
    ComplexBlockList,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Architecture {
    pub tags: Vec<ArchitectureTag>,
}
