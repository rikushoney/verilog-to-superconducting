use crate::blifchar::{is_logic, is_space, is_text};
use crate::cursor::Cursor;
use crate::error::Error;
use crate::strspan::StrSpan;

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
    Clock {
        span: StrSpan<'a>,
        clock_list: Vec<StrSpan<'a>>,
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
    MLatch {
        span: StrSpan<'a>,
        name: StrSpan<'a>,
        formal_actual_list: Vec<StrSpan<'a>>,
        control: StrSpan<'a>,
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
    Search {
        span: StrSpan<'a>,
        filename: StrSpan<'a>,
    },
    SingleOutput {
        span: StrSpan<'a>,
        input_plane: StrSpan<'a>,
        output_plane: StrSpan<'a>,
    },
    Subckt {
        span: StrSpan<'a>,
        model_name: StrSpan<'a>,
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

// Clock ::= '.clock' S+ TextList
fn parse_clock<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const CLOCK: &str = ".clock";
    debug_assert!(cur.starts_with(CLOCK));
    let start = cur.pos();
    cur.advance(CLOCK.len());
    cur.skip_whitespace_and_line_continue();
    let clock_list = parse_text_list(cur).map_err(|_| expected!("list of clocks"))?;
    Ok(Token::Clock {
        span: cur.slice_from(start),
        clock_list,
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
    if !output_plane.as_str().chars().all(is_logic) {
        return Err(Error::InvalidLogicValue);
    }
    if output_plane.as_str().chars().count() > 1 {
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
    Ok(Token::Latch {
        span: cur.slice_from(start),
        input,
        output,
        trigger,
        control,
        init_val,
    })
}

// Gate ::= '.gate' S+ Text S+ TextList
fn parse_gate<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const GATE: &str = ".gate";
    debug_assert!(cur.starts_with(GATE));
    let start = cur.pos();
    cur.advance(GATE.len());
    cur.skip_whitespace_and_line_continue();
    let name = parse_text(cur)?;
    cur.skip_whitespace_and_line_continue();
    let formal_actual_list = parse_formal_actual_list(cur)?;
    Ok(Token::Gate {
        span: cur.slice_from(start),
        name,
        formal_actual_list,
    })
}

// MLatch ::= '.mlatch' S+ Text S+ FormalActualList S+ Text (S+ Text)?
fn parse_mlatch<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const MLATCH: &str = ".mlatch";
    debug_assert!(cur.starts_with(MLATCH));
    let start = cur.pos();
    cur.advance(MLATCH.len());
    cur.skip_whitespace_and_line_continue();
    let name = parse_text(cur)?;
    cur.skip_whitespace_and_line_continue();
    let formal_actual_list = parse_formal_actual_list(cur)?;
    cur.skip_whitespace_and_line_continue();
    let control = parse_text(cur).map_err(|_| expected!("latch control"))?;
    cur.skip_whitespace_and_line_continue();
    let init_val = parse_text(cur).ok();
    Ok(Token::MLatch {
        span: cur.slice_from(start),
        name,
        formal_actual_list,
        control,
        init_val,
    })
}

// Subckt ::= '.subckt' S+ Text S+ FormalActualList
fn parse_subckt<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const SUBCKT: &str = ".subckt";
    debug_assert!(cur.starts_with(SUBCKT));
    let start = cur.pos();
    cur.advance(SUBCKT.len());
    cur.skip_whitespace_and_line_continue();
    let model_name = parse_text(cur)?;
    cur.skip_whitespace_and_line_continue();
    let formal_actual_list = parse_formal_actual_list(cur)?;
    Ok(Token::Subckt {
        span: cur.slice_from(start),
        model_name,
        formal_actual_list,
    })
}

// Search   ::= '.search' S+ Filename
// Filename ::= [^\n]+
fn parse_search<'a>(cur: &mut Cursor<'a>) -> ParseResult<Token<'a>> {
    const SEARCH: &str = ".search";
    debug_assert!(cur.starts_with(SEARCH));
    let start = cur.pos();
    cur.advance(SEARCH.len());
    cur.skip_whitespace_and_line_continue();
    let filename_start = cur.pos();
    cur.skip_while(|ch| ch != '\n');
    let filename = cur.slice_from(filename_start);
    Ok(Token::Search {
        span: cur.slice_from(start),
        filename,
    })
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
    have_newline: bool,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            cursor: Cursor::new(input),
            state: State::ModelHeader,
            have_newline: false,
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
                    Some(parse_single_output(cur))
                } else {
                    Some(parse_text(cur).map(Token::Text))
                }
            }
            ch if is_text(ch) => {
                // SAFETY: peeked `ch` is text
                let text = parse_text(cur).unwrap();
                match self.state {
                    State::ModelHeader => match text.as_str() {
                        ".model" => Some(parse_model_header(cur)),
                        _ => {
                            self.state = State::ModelParameters;
                            None
                        }
                    },
                    State::ModelParameters => match text.as_str() {
                        ".inputs" => Some(parse_inputs(cur)),
                        ".outputs" => Some(parse_outputs(cur)),
                        ".clock" => Some(parse_clock(cur)),
                        _ => {
                            self.state = State::ModelBody;
                            None
                        }
                    },
                    State::ModelBody => match text.as_str() {
                        ".end" => {
                            self.state = State::ModelHeader;
                            Some(parse_end(cur))
                        }
                        ".names" => {
                            self.state = State::PlaDescription;
                            Some(parse_names(cur))
                        }
                        ".latch" => Some(parse_latch(cur)),
                        ".gate" => Some(parse_gate(cur)),
                        ".mlatch" => Some(parse_mlatch(cur)),
                        ".subckt" => Some(parse_subckt(cur)),
                        ".search" => Some(parse_search(cur)),
                        ".model" => {
                            self.state = State::ModelHeader;
                            None
                        }
                        ".inputs" | ".outputs" | ".clock" => {
                            Some(Err(Error::Unexpected(text.as_str().to_string())))
                        }
                        _ => {
                            if text.as_str().starts_with('.') {
                                // TODO: looks like command - maybe error?
                            }
                            Some(Ok(Token::Text(text)))
                        }
                    },
                    State::PlaDescription => {
                        self.state = State::ModelBody;
                        None
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
                    if self.have_newline {
                        token = None;
                    }
                    self.have_newline = true;
                }
                Some(Ok(Token::Whitespace(..))) => {
                    token = None;
                }
                _ => {}
            }
        }
        token
    }
}
