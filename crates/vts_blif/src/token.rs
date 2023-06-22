use crate::blifchar::{is_logic, is_space, is_text};
use crate::cursor::Cursor;
use crate::error::Error;
use crate::strspan::StrSpan;
use crate::Model;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    End(StrSpan<'a>),
    Inputs {
        span: StrSpan<'a>,
        input_list: Vec<StrSpan<'a>>,
    },
    Outputs {
        span: StrSpan<'a>,
        output_list: Vec<StrSpan<'a>>,
    },
    Gate {
        span: StrSpan<'a>,
        name: StrSpan<'a>,
        formal_actual_list: Vec<StrSpan<'a>>,
    },
    Latch {
        span: StrSpan<'a>,
        input: StrSpan<'a>,
        output: StrSpan<'a>,
        // "type" is a reserved keyword (in rust) - use `trigger` instead
        // this is also a better description of what it really is
        trigger: Option<StrSpan<'a>>,
        control: Option<StrSpan<'a>>,
        init_val: Option<StrSpan<'a>>,
    },
    ModelHeader {
        span: StrSpan<'a>,
        model_name: Option<StrSpan<'a>>,
    },
    Names {
        span: StrSpan<'a>,
        inputs: Vec<StrSpan<'a>>,
        output: StrSpan<'a>,
    },
    Newline(StrSpan<'a>),
    PlaDescriptionEnd(StrSpan<'a>),
    SingleOutput {
        span: StrSpan<'a>,
        input_plane: StrSpan<'a>,
        output_plane: StrSpan<'a>,
    },
    Subckt {
        span: StrSpan<'a>,
        name: StrSpan<'a>,
        formal_actual_list: Vec<StrSpan<'a>>,
    },
    Text(StrSpan<'a>),
    Whitespace(StrSpan<'a>),
}

pub type ParseResult<T> = Result<T, Error>;

// Text ::= ([^#] - S)+
fn parse_text<'a>(cur: &mut Cursor<'a>) -> ParseResult<StrSpan<'a>> {
    match cur.peek() {
        Some(ch) if is_text(ch) => {
            let start = cur.pos();
            cur.skip_while(is_text);
            Ok(cur.slice_from(start))
        }
        _ => Err(expected!("text")),
    }
}

// TextList ::= Text (S+ Text)*
fn parse_text_list<'a>(cur: &mut Cursor<'a>) -> ParseResult<Vec<StrSpan<'a>>> {
    let first = parse_text(cur).map_err(|_| expected!("text list"))?;
    let mut text_list = vec![first];
    while let Ok(text) = parse_text(cur) {
        cur.skip_whitespace_and_line_continue();
        text_list.push(text);
    }
    Ok(text_list)
}

// FormalActual ::= Text '=' Text
fn parse_formal_actual<'a>(cur: &mut Cursor<'a>) -> ParseResult<StrSpan<'a>> {
    match parse_text(cur) {
        Ok(text) if text.as_str().contains('=') => Ok(text),
        _ => Err(expected!("formal=actual")),
    }
}

// FormalActualList ::= FormalActual (S+ FormalActual)*
fn parse_formal_actual_list<'a>(cur: &mut Cursor<'a>) -> ParseResult<Vec<StrSpan<'a>>> {
    let first = parse_formal_actual(cur)?;
    let mut formal_actual_list = vec![first];
    while let Ok(formal_actual) = parse_formal_actual(cur) {
        cur.skip_whitespace_and_line_continue();
        formal_actual_list.push(formal_actual);
    }
    Ok(formal_actual_list)
}

// End ::= '.end'
fn parse_end<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const END: &str = ".end";
    debug_assert!(cur.starts_with(END));
    let start = cur.pos();
    cur.advance(END.len());
    Ok(Token::End(cur.slice_from(start)))
}

// ModelHeader ::= '.model' (S+ Text)?
fn parse_model_header<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const MODEL: &str = ".model";
    debug_assert!(cur.starts_with(MODEL));
    let start = cur.pos();
    cur.advance(MODEL.len());
    cur.skip_whitespace_and_line_continue();
    let model_name = parse_text(cur).ok();
    Ok(Token::ModelHeader {
        span: cur.slice_from(start),
        model_name,
    })
}

// Inputs ::= '.inputs' S+ TextList
fn parse_inputs<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const INPUTS: &str = ".inputs";
    debug_assert!(cur.starts_with(INPUTS));
    let start = cur.pos();
    cur.advance(INPUTS.len());
    cur.skip_whitespace_and_line_continue();
    let input_list = parse_text_list(cur).map_err(|_| expected!("list of inputs"))?;
    Ok(Token::Inputs {
        span: cur.slice_from(start),
        input_list,
    })
}

// Outputs ::= '.outputs' S+ TextList
fn parse_outputs<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const OUTPUTS: &str = ".outputs";
    debug_assert!(cur.starts_with(OUTPUTS));
    let start = cur.pos();
    cur.advance(OUTPUTS.len());
    cur.skip_whitespace_and_line_continue();
    let output_list = parse_text_list(cur).map_err(|_| expected!("list of outputs"))?;
    Ok(Token::Outputs {
        span: cur.slice_from(start),
        output_list,
    })
}

// Names ::= '.names' S+ Text S+ Text (S+ Text)*
fn parse_names<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const NAMES: &str = ".names";
    debug_assert!(cur.starts_with(NAMES));
    let start = cur.pos();
    cur.advance(NAMES.len());
    cur.skip_whitespace_and_line_continue();
    let mut inputs = parse_text_list(cur).map_err(|_| expected!("list of inputs"))?;
    if inputs.len() < 2 {
        Err(expected!("at least 1 input and 1 output"))
    } else {
        // SAFETY: `inputs` has at least 2 elements
        let output = inputs.pop().unwrap();
        Ok(Token::Names {
            span: cur.slice_from(start),
            inputs,
            output,
        })
    }
}

// SingleOutput ::= InputPlane+ S+ OutputPlane
// InputPlane   ::= '0' | '1' | '-'
// OutputPlane  ::= '0' | '1'
fn parse_single_output<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    debug_assert!(is_logic(cur.peek().unwrap()));
    let start = cur.pos();
    let input_plane = parse_text(cur).map_err(|_| expected!("input plane"))?;
    if !input_plane.as_str().chars().all(is_logic) {
        return Err(Error::InvalidLogicValue);
    }
    cur.skip_whitespace_and_line_continue();
    let output_plane = parse_text(cur).map_err(|_| expected!("output plane"))?;
    if !output_plane
        .as_str()
        .chars()
        .all(|ch| matches!(ch, '0' | '1'))
    {
        return Err(Error::InvalidLogicValue);
    }
    if output_plane.as_str().chars().count() != 1 {
        return Err(Error::MultipleOutputs);
    }
    Ok(Token::SingleOutput {
        span: cur.slice_from(start),
        input_plane,
        output_plane,
    })
}

// Latch ::= '.latch' S+ Text S+ Text (S+ Text S+ Text)? (S+ Text)?
fn parse_latch<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const LATCH: &str = ".latch";
    debug_assert!(cur.starts_with(LATCH));
    let start = cur.pos();
    cur.advance(LATCH.len());
    cur.skip_whitespace_and_line_continue();
    let input = parse_text(cur).map_err(|_| expected!("latch input"))?;
    cur.skip_whitespace_and_line_continue();
    let output = parse_text(cur).map_err(|_| expected!("latch output"))?;
    cur.skip_whitespace_and_line_continue();
    let mut trigger = None;
    let mut control = None;
    let mut init_val = None;
    // first parameter could be either `trigger` or `init_val`
    if let Ok(trigger_or_init) = parse_text(cur) {
        cur.skip_whitespace_and_line_continue();
        if let Ok(second) = parse_text(cur) {
            // there are at least 2 parameters
            cur.skip_whitespace_and_line_continue();
            trigger = Some(trigger_or_init);
            control = Some(second);
            // 3rd parameter is `init_val` if present
            init_val = parse_text(cur).ok();
        } else {
            // only 1 parameter - must be `init_val`
            init_val = Some(trigger_or_init);
        }
    }
    if init_val.is_some_and(|init_val| init_val.as_str().len() != 1) {
        return Err(Error::MultipleInitValues);
    }
    Ok(Token::Latch {
        span: cur.slice_from(start),
        input,
        output,
        trigger,
        control,
        init_val,
    })
}

// SubcktOrGate ::= ('.subckt' | '.gate') S+ Text S+ FormalActualList
fn parse_subckt_or_gate<'a>(cur: &mut Cursor<'a>, is_subckt: bool) -> ParseResult<Token<'a>> {
    const SUBCKT: &str = ".subckt";
    const GATE: &str = ".gate";
    let start = cur.pos();
    if is_subckt {
        debug_assert!(cur.starts_with(SUBCKT));
        cur.advance(SUBCKT.len());
    } else {
        debug_assert!(cur.starts_with(GATE));
        cur.advance(GATE.len());
    }
    cur.skip_whitespace_and_line_continue();
    let name = parse_text(cur)?;
    cur.skip_whitespace_and_line_continue();
    let formal_actual_list = parse_formal_actual_list(cur)?;
    if is_subckt {
        Ok(Token::Subckt {
            span: cur.slice_from(start),
            name,
            formal_actual_list,
        })
    } else {
        Ok(Token::Gate {
            span: cur.slice_from(start),
            name,
            formal_actual_list,
        })
    }
}

fn parse_subckt<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    parse_subckt_or_gate(cur, true)
}

fn parse_gate<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    parse_subckt_or_gate(cur, false)
}

#[derive(Clone, Copy, PartialEq)]
enum State {
    ModelHeader,
    ModelParameters,
    ModelBody,
    PlaDescription,
}

pub struct Tokenizer<'a> {
    cursor: Cursor<'a>,
    state: State,
    newline_required: bool,
    pla_description_end: StrSpan<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            cursor: Cursor::new(input),
            state: State::ModelHeader,
            newline_required: false,
            pla_description_end: StrSpan::empty(),
        }
    }

    fn parse_next(&mut self) -> Option<ParseResult<Token<'a>>> {
        let cur = &mut self.cursor;
        let ch = cur.peek()?;
        let start = cur.pos();
        match ch {
            '\n' => {
                cur.bump();
                Some(Ok(Token::Newline(cur.slice_from(start))))
            }
            ch if is_space(ch) => {
                cur.skip_whitespace_and_line_continue();
                Some(Ok(Token::Whitespace(cur.slice_from(start))))
            }
            '#' => {
                cur.skip_while(|ch| ch != '\n');
                None
            }
            '\\' => {
                if cur.starts_with("\\\n") {
                    cur.advance(2);
                    None
                } else if cur.starts_with("\\\r\n") {
                    cur.advance(3);
                    None
                } else {
                    Some(parse_text(cur).map(Token::Text))
                }
            }
            ch if is_logic(ch) => {
                if self.state == State::PlaDescription {
                    let single_output = parse_single_output(cur);
                    if single_output.is_ok() {
                        self.pla_description_end = cur.slice_pos();
                    }
                    Some(single_output)
                } else {
                    Some(parse_text(cur).map(Token::Text))
                }
            }
            ch if is_text(ch) => {
                if self.state == State::PlaDescription {
                    self.state = State::ModelBody;
                    return Some(Ok(Token::PlaDescriptionEnd(self.pla_description_end)));
                }
                match self.state {
                    State::ModelHeader => {
                        if cur.starts_with(".model") {
                            Some(parse_model_header(cur))
                        } else {
                            self.state = State::ModelParameters;
                            None
                        }
                    }
                    State::ModelParameters => {
                        if cur.starts_with(".inputs") {
                            Some(parse_inputs(cur))
                        } else if cur.starts_with(".outputs") {
                            Some(parse_outputs(cur))
                        } else {
                            self.state = State::ModelBody;
                            None
                        }
                    }
                    State::ModelBody => {
                        if cur.starts_with(".end") {
                            self.state = State::ModelHeader;
                            Some(parse_end(cur))
                        } else if cur.starts_with(".names") {
                            self.state = State::PlaDescription;
                            Some(parse_names(cur))
                        } else if cur.starts_with(".latch") {
                            Some(parse_latch(cur))
                        } else if cur.starts_with(".gate") {
                            Some(parse_gate(cur))
                        } else if cur.starts_with(".subckt") {
                            Some(parse_subckt(cur))
                        } else if cur.starts_with(".model") {
                            self.state = State::ModelHeader;
                            Some(Ok(Token::End(cur.slice_pos())))
                        } else if cur.starts_with(".inputs") || cur.starts_with(".outputs") {
                            self.state = State::ModelParameters;
                            Some(Ok(Token::End(cur.slice_pos())))
                        } else {
                            let text = parse_text(cur).expect("should be text");
                            if text.as_str().starts_with('.') {
                                // TODO: looks like command - maybe error/warning?
                            }
                            Some(Ok(Token::Text(text)))
                        }
                    }
                    State::PlaDescription => {
                        unreachable!("should be handled separately");
                    }
                }
            }
            _ => {
                unreachable!("unhandled characters are considered text")
            }
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = ParseResult<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token = None;
        while !self.cursor.is_eof() && token.is_none() {
            token = self.parse_next();
            match token {
                Some(Ok(Token::Newline(..))) => {
                    self.newline_required = false;
                    token = None;
                }
                Some(Ok(Token::Whitespace(..))) => {
                    token = None;
                }
                Some(Ok(Token::PlaDescriptionEnd(..))) => {
                    // does not require newline
                    break;
                }
                Some(Ok(_)) => {
                    // could have whitespace right before EOF
                    self.cursor.skip_whitespace_and_line_continue();
                    if self.newline_required && !self.cursor.is_eof() {
                        self.cursor.jump_end();
                        token = Some(Err(expected!("newline")));
                    } else {
                        self.newline_required = true;
                    }
                }
                Some(Err(_)) => {
                    self.cursor.jump_end();
                    break;
                }
                None => {
                    // state transition or EOF
                    continue;
                }
            }
        }
        token
    }
}
