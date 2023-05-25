#![allow(dead_code)]

use roxmltree as xml;

use crate::ast::*;

#[derive(Clone, Debug, PartialEq)]
enum ParseError {
    UnknownTag,
    Unknown,
}

type Result<T> = std::result::Result<T, ParseError>;

fn parse_models(_input: &xml::Node) -> Result<ArchitectureTag> {
    unimplemented!()
}

fn parse_tiles(_input: &xml::Node) -> Result<ArchitectureTag> {
    unimplemented!()
}

fn parse_layout(_input: &xml::Node) -> Result<ArchitectureTag> {
    unimplemented!()
}

fn parse_device(_input: &xml::Node) -> Result<ArchitectureTag> {
    unimplemented!()
}
fn parse_switch_list(_input: &xml::Node) -> Result<ArchitectureTag> {
    unimplemented!()
}
fn parse_segment_list(_input: &xml::Node) -> Result<ArchitectureTag> {
    unimplemented!()
}
fn parse_direct_list(_input: &xml::Node) -> Result<ArchitectureTag> {
    unimplemented!()
}
fn parse_complex_block_list(_input: &xml::Node) -> Result<ArchitectureTag> {
    unimplemented!()
}

fn parse_architecture(input: &xml::Document) -> Result<Architecture> {
    let root_element = input.root_element();
    if root_element.tag_name().name() != "architecture" {
        return Err(ParseError::Unknown);
    }
    let mut tags = Vec::new();
    for tag in root_element.children() {
        match tag.tag_name().name() {
            "models" => tags.push(parse_models(&tag)?),
            "tiles" => tags.push(parse_tiles(&tag)?),
            "layout" => tags.push(parse_layout(&tag)?),
            "device" => tags.push(parse_device(&tag)?),
            "switchlist" => tags.push(parse_switch_list(&tag)?),
            "segmentlist" => tags.push(parse_segment_list(&tag)?),
            "directlist" => tags.push(parse_direct_list(&tag)?),
            "complexblocklist" => tags.push(parse_complex_block_list(&tag)?),
            _ => return Err(ParseError::UnknownTag),
        }
    }
    Ok(Architecture { tags })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn smoke_test() {
        let input = xml::Document::parse("<architecture></architecture>").unwrap();
        let parsed = parse_architecture(&input);
        assert_eq!(parsed, Ok(Architecture { tags: vec![] }));
    }
}
