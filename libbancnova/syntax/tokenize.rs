use std::iter::{Iterator,Peekable};
use std::io::{BufferedReader, IoError, EndOfFile};
use std::io::{BufReader};

#[deriving(Show, PartialEq, Eq)]
pub enum Token {
    IntegerLiteral(String),
    Comma,
    Newline,
    NothingMarker,
    FencedLiteral(char, String),
}

pub struct Tokenizer<'a,'b> {
    breader: &'b mut BufferedReader<BufReader<'a>>,
    ungotch: Option<char>
}

fn escape_character(e: char) -> char {
    match e {
        'n' => '\n',
        't' => '\t',
        e => e,
    }
}

impl<'a,'b> Tokenizer<'a,'b> {
    pub fn new(breader: &'b mut BufferedReader<BufReader<'a>>) -> Tokenizer<'a,'b> {
        Tokenizer { breader: breader, ungotch: None }
    }

    pub fn vectorize(subject: &str) -> Vec<Token> {
        let reader = BufReader::new(subject.as_bytes());
        let mut breader: BufferedReader<BufReader> = BufferedReader::new(reader);
        let mut tokenizer = Tokenizer::new(&mut breader);
        tokenizer.collect()
    }

    fn getch(&mut self) -> Option<char> {
        match self.ungotch {
            Some(c) => {
                self.ungotch = None;
                Some(c)
            },
            _ => match self.breader.read_char() {
                Ok(c) => Some(c),
                _ => None,
            }
        }
    }

    fn ungetch(&mut self, c: char) {
        if self.ungotch.is_some() {
            fail!("can only ungetch one character at a time");
        }

        self.ungotch = Some(c);
    }

    fn read_integer_token(&mut self, c: char) -> Option<Token> {
        let mut buffer = String::new();
        buffer.push_char(c);
        loop {
            match self.breader.read_char() {
                Err(ref s) if s.kind == EndOfFile => {
                    return Some(IntegerLiteral(buffer))
                },
                Err(s) => {
                    println!("<{}>", s);
                    return None;
                },
                Ok(c) if c>='0' && c<='9' => {
                    buffer.push_char(c);
                },
                Ok(c) => {
                    self.ungetch(c);
                    return Some(IntegerLiteral(buffer))
                },
            }
        }
    }

    fn read_fenced_literal(&mut self, lfence: char) -> Option<Token> {
        let mut buffer = String::new();
        loop {
            match self.breader.read_char() {
                Err(_) => { return None; },
                Ok(c) if c == lfence => { return Some(FencedLiteral(lfence, buffer)); },
                Ok('\\') => {
                    let e = self.breader.read_char();
                    if e.is_err() {
                        return None;
                    }
                    buffer.push_char(escape_character(e.unwrap()));
                },
                Ok(c) => {
                    buffer.push_char(c);
                },
            }
        }
    }
}

impl<'a,'b> Iterator<Token> for Tokenizer<'a,'b> {
    fn next(&mut self) -> Option<Token> {
        let c = self.getch();
        if c.is_none() {
            return None;
        }
        let c = c.unwrap();
        match c {
            '-' => self.read_integer_token(c),
            '0'..'9' => self.read_integer_token(c),
            '\'' => self.read_fenced_literal(c),
            ',' => Some(Comma),
            ' '|'\t' => self.next(),
            '\n' => Some(Newline),
            _ => fail!("unexpected character '{}'", c)
        }
    }
}

#[test]
fn comma() {
    let tokens = Tokenizer::vectorize(",");
    assert_eq!(1, tokens.len());
    assert_eq!(&Comma, tokens.get(0));
}

#[test]
fn newline() {
    let tokens = Tokenizer::vectorize("  \n  ");
    assert_eq!(1, tokens.len());
    assert_eq!(&Newline, tokens.get(0));
}

#[test]
fn comma_with_newline() {
    let tokens = Tokenizer::vectorize(",\n,");
    assert_eq!(3, tokens.len());
    assert_eq!(&Comma, tokens.get(0));
    assert_eq!(&Newline, tokens.get(1));
    assert_eq!(&Comma, tokens.get(2));
}

#[test]
fn char_literal() {
    let tokens = Tokenizer::vectorize("'a','\\\\','\\&', '\\n'");
    assert_eq!(7, tokens.len());
    assert_eq!(&FencedLiteral('\'', "a".to_string()), tokens.get(0));
    assert_eq!(&Comma, tokens.get(1));
    assert_eq!(&FencedLiteral('\'', "\\".to_string()), tokens.get(2));
    assert_eq!(&Comma, tokens.get(3));
    assert_eq!(&FencedLiteral('\'', "&".to_string()), tokens.get(4));
    assert_eq!(&Comma, tokens.get(5));
    assert_eq!(&FencedLiteral('\'', "\n".to_string()), tokens.get(6));
}

#[test]
fn integer_literal() {
    let tokens = Tokenizer::vectorize("0 123 -456 7890123456");
    assert_eq!(4, tokens.len());
    assert_eq!(&IntegerLiteral("0".to_string()), tokens.get(0));
    assert_eq!(&IntegerLiteral("123".to_string()), tokens.get(1));
    assert_eq!(&IntegerLiteral("-456".to_string()), tokens.get(2));
    assert_eq!(&IntegerLiteral("7890123456".to_string()), tokens.get(3));
}
