use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace1, satisfy},
    combinator::{map, map_res, opt, recognize},
    multi::{many0, many0_count, separated_list1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use unicode_ident::{is_xid_continue, is_xid_start};

fn unicode_ident(input: &str) -> IResult<&str, &str> {
    let ident_start = alt((satisfy(is_xid_start), char('_')));
    let ident_continue = alt((satisfy(is_xid_continue), char('_')));
    recognize(pair(ident_start, many0_count(ident_continue)))(input)
}

fn dot_command<'a>(command: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    preceded(char('.'), tag(command))
}

#[derive(Clone, Debug, PartialEq)]
pub struct Model<'a> {
    pub name: Option<&'a str>,
    pub inputs: Vec<&'a str>,
    pub outputs: Vec<&'a str>,
    pub clocks: Vec<&'a str>,
}

#[derive(Clone, Debug, PartialEq)]
enum ModelField<'a> {
    Inputs(Vec<&'a str>),
    Outputs(Vec<&'a str>),
    Clock(Vec<&'a str>),
}

impl<'a> ModelField<'a> {
    fn parse(input: &'a str) -> IResult<&'a str, Self> {
        // .<command> <decl-list>
        let field_decl = |name| {
            terminated(
                pair(
                    terminated(dot_command(name), multispace1),
                    separated_list1(multispace1, unicode_ident),
                ),
                multispace1,
            )
        };
        // .inputs <decl-input-list>
        let inputs_decl = field_decl("inputs");
        // .outputs <decl-output-list>
        let outputs_decl = field_decl("outputs");
        // .clock <decl-clock-list>
        let clock_decl = field_decl("clock");
        map_res(
            alt((inputs_decl, outputs_decl, clock_decl)),
            |(name, items)| match name {
                "inputs" => Ok(ModelField::Inputs(items)),
                "outputs" => Ok(ModelField::Outputs(items)),
                "clock" => Ok(ModelField::Clock(items)),
                _ => Err(format!("unexpected declaration \"{}\"", name)),
            },
        )(input)
    }
}

impl<'a> Model<'a> {
    fn parse(input: &'a str) -> IResult<&str, Self> {
        // .model <decl-model-name>
        let mut model_decl = delimited(
            pair(dot_command("model"), multispace1),
            opt(unicode_ident),
            multispace1,
        );
        let (input, model_name) = model_decl(input)?;
        let (input, fields) = many0(ModelField::parse)(input)?;
        let mut inputs = Vec::new();
        let mut outputs = Vec::new();
        let mut clocks = Vec::new();
        for field in fields {
            match field {
                ModelField::Inputs(mut items) => inputs.append(&mut items),
                ModelField::Outputs(mut items) => outputs.append(&mut items),
                ModelField::Clock(mut items) => clocks.append(&mut items),
            }
        }
        let (input, _) = opt(dot_command("end"))(input)?;
        Ok((
            input,
            Self {
                name: model_name,
                inputs,
                outputs,
                clocks,
            },
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unicode_ident() {
        let tests = [
            ("myVariable#", "myVariable", "#"),
            ("veränderlich", "veränderlich", ""),
        ];

        for (input, expected, rest) in tests {
            assert_eq!(unicode_ident(input), Ok((rest, expected)));
        }
    }

    #[test]
    fn test_model() {
        let tests = [
            (
                r#".model myModel
.inputs a b c
.outputs d e f
.clock clk1 clk2
.end"#,
                Model {
                    name: Some("myModel"),
                    inputs: vec!["a", "b", "c"],
                    outputs: vec!["d", "e", "f"],
                    clocks: vec!["clk1", "clk2"],
                },
                "",
            ),
            (
                ".model myModel\n",
                Model {
                    name: Some("myModel"),
                    inputs: vec![],
                    outputs: vec![],
                    clocks: vec![],
                },
                "",
            ),
            (
                r#".model myModel
.outputs d e f
.clock clk1
.inputs a b
.inputs c
.clock clk2
.end"#,
                Model {
                    name: Some("myModel"),
                    inputs: vec!["a", "b", "c"],
                    outputs: vec!["d", "e", "f"],
                    clocks: vec!["clk1", "clk2"],
                },
                "",
            ),
        ];

        for (input, expected, rest) in tests {
            assert_eq!(Model::parse(input), Ok((rest, expected)));
        }
    }
}
