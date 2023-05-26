#![allow(dead_code)]

use roxmltree as xml;

use crate::ast::*;

#[derive(Clone, Debug, PartialEq)]
enum ParseError {
    InvalidValue,
    MissingAttribute,
    UnknownTag,
    Unknown,
}

type Result<T> = std::result::Result<T, ParseError>;

macro_rules! name {
    ($tag:ident) => {
        $tag.attribute("name").ok_or(ParseError::MissingAttribute)
    };
}

macro_rules! ensure_element {
    ($tag:ident, $action:expr) => {
        if !$tag.is_element() {
            $action;
        }
    };
}

fn parse_port<'a>(input: xml::Node<'a, '_>) -> Result<Port<'a>> {
    if input.tag_name().name() != "port" {
        return Err(ParseError::UnknownTag);
    }
    let name = name!(input)?;
    let is_clock = input
        .attribute("is_clock")
        .map_or(Ok(false), |is_clock| match is_clock {
            "0" => Ok(false),
            "1" => Ok(true),
            _ => Err(ParseError::InvalidValue),
        })?;
    let clock = input.attribute("clock");
    let comb_sink_ports = input
        .attribute("combinational_sink_ports")
        .map_or(Vec::new(), |comb_sink_ports| {
            comb_sink_ports.split_whitespace().collect()
        });
    Ok(Port {
        name,
        is_clock,
        clock,
        comb_sink_ports,
    })
}

fn parse_model<'a>(input: xml::Node<'a, '_>) -> Result<Model<'a>> {
    if input.tag_name().name() != "model" {
        return Err(ParseError::UnknownTag);
    }
    let name = name!(input)?;
    let never_prune = input
        .attribute("never_prune")
        .map_or(Ok(false), |never_prune| match never_prune {
            "true" => Ok(true),
            "false" => Ok(false),
            _ => Err(ParseError::InvalidValue),
        })?;
    let mut input_ports = Vec::new();
    let mut output_ports = Vec::new();
    for tag in input.children() {
        ensure_element!(tag, continue);
        match tag.tag_name().name() {
            "input_ports" => {
                for port in tag.children() {
                    ensure_element!(port, continue);
                    input_ports.push(parse_port(port)?);
                }
            }
            "output_ports" => {
                for port in tag.children() {
                    ensure_element!(port, continue);
                    output_ports.push(parse_port(port)?);
                }
            }
            _ => return Err(ParseError::UnknownTag),
        }
    }
    Ok(Model {
        name,
        never_prune,
        input_ports,
        output_ports,
    })
}

fn parse_models<'a>(input: xml::Node<'a, '_>) -> Result<Tag<'a>> {
    let mut models = Vec::new();
    for tag in input.children() {
        ensure_element!(tag, continue);
        models.push(parse_model(tag)?);
    }
    Ok(Tag::Models(models))
}

fn parse_tiles<'a>(_input: xml::Node<'a, '_>) -> Result<Tag<'a>> {
    unimplemented!()
}

fn parse_layout<'a>(_input: xml::Node<'a, '_>) -> Result<Tag<'a>> {
    unimplemented!()
}

fn parse_device<'a>(_input: xml::Node<'a, '_>) -> Result<Tag<'a>> {
    unimplemented!()
}
fn parse_switch_list<'a>(_input: xml::Node<'a, '_>) -> Result<Tag<'a>> {
    unimplemented!()
}
fn parse_segment_list<'a>(_input: xml::Node<'a, '_>) -> Result<Tag<'a>> {
    unimplemented!()
}
fn parse_direct_list<'a>(_input: xml::Node<'a, '_>) -> Result<Tag<'a>> {
    unimplemented!()
}
fn parse_complex_block_list<'a>(_input: xml::Node<'a, '_>) -> Result<Tag<'a>> {
    unimplemented!()
}

fn parse_architecture<'a>(input: &'a xml::Document) -> Result<Architecture<'a>> {
    let root_element = input.root_element();
    if root_element.tag_name().name() != "architecture" {
        return Err(ParseError::Unknown);
    }
    let mut tags = Vec::new();
    for tag in root_element.children() {
        ensure_element!(tag, continue);
        match tag.tag_name().name() {
            "models" => tags.push(parse_models(tag)?),
            "tiles" => tags.push(parse_tiles(tag)?),
            "layout" => tags.push(parse_layout(tag)?),
            "device" => tags.push(parse_device(tag)?),
            "switchlist" => tags.push(parse_switch_list(tag)?),
            "segmentlist" => tags.push(parse_segment_list(tag)?),
            "directlist" => tags.push(parse_direct_list(tag)?),
            "complexblocklist" => tags.push(parse_complex_block_list(tag)?),
            _ => return Err(ParseError::UnknownTag),
        }
    }
    Ok(Architecture { tags })
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_parser {
        ($test_name:ident, $test_fn:ident, $test_cases:expr) => {
            #[test]
            fn $test_name() {
                let tests = $test_cases;
                for (input, expected) in tests {
                    let doc = xml::Document::parse(input).unwrap();
                    let parsed = $test_fn(doc.root_element());
                    assert_eq!(parsed, Ok(expected));
                }
            }
        };
    }

    test_parser!(
        test_port,
        parse_port,
        [
            (
                r#"<port name="port1" clock="clk"/>"#,
                Port {
                    name: "port1",
                    is_clock: false,
                    clock: Some("clk"),
                    comb_sink_ports: vec![]
                }
            ),
            (
                r#"<port name="port2" clock="clk" combinational_sink_ports="sout cout"/>"#,
                Port {
                    name: "port2",
                    is_clock: false,
                    clock: Some("clk"),
                    comb_sink_ports: vec!["sout", "cout"]
                }
            ),
            (
                r#"<port name="clk" is_clock="1"/>"#,
                Port {
                    name: "clk",
                    is_clock: true,
                    clock: None,
                    comb_sink_ports: vec![]
                }
            ),
        ]
    );

    test_parser!(
        test_model,
        parse_model,
        [(
            r#"<model name="single_port_ram">
    <input_ports>
        <port name="we" clock="clk"/>
        <port name="addr" clock="clk" combinational_sink_ports="out"/>
        <port name="data" clock="clk" combinational_sink_ports="out"/>
        <port name="clk" is_clock="1"/>
    </input_ports>
    <output_ports>
        <port name="out" clock="clk"/>
    </output_ports>
</model>"#,
            Model {
                name: "single_port_ram",
                never_prune: false,
                input_ports: vec![
                    Port {
                        name: "we",
                        is_clock: false,
                        clock: Some("clk"),
                        comb_sink_ports: vec![]
                    },
                    Port {
                        name: "addr",
                        is_clock: false,
                        clock: Some("clk"),
                        comb_sink_ports: vec!["out"]
                    },
                    Port {
                        name: "data",
                        is_clock: false,
                        clock: Some("clk"),
                        comb_sink_ports: vec!["out"]
                    },
                    Port {
                        name: "clk",
                        is_clock: true,
                        clock: None,
                        comb_sink_ports: vec![]
                    }
                ],
                output_ports: vec![Port {
                    name: "out",
                    is_clock: false,
                    clock: Some("clk"),
                    comb_sink_ports: vec![]
                },]
            }
        )]
    );
}
