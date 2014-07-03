use std::iter::{Iterator};
use std::io::{BufferedReader, EndOfFile};
use std::io::{BufReader, File};

#[deriving(Show, PartialEq, Eq, Clone)]
pub enum Token {
    IntegerLiteral(String),
    Name(String),
    Comma,
    Newline,
    NothingMarker,
    FencedLiteral(char, String),
    AddressSign,
    OpenBracket(char),
    CloseBracket(char),
    OperToken(String),
}

pub struct Tokenizer<R> {
    breader: Box<BufferedReader<R>>,
    ungotch: Option<char>,
    untok: Option<Token>,
    line: uint,
}

fn escape_character(e: char) -> char {
    match e {
        'n' => '\n',
        't' => '\t',
        e => e,
    }
}

impl<R: Reader> Tokenizer<R> {
    pub fn from_file(file: File) -> Tokenizer<File> {
        let breader = BufferedReader::new(file);
        Tokenizer { breader: box breader, ungotch: None, untok: None, line: 1 }
    }

    pub fn from_str<'a>(text: &'a str) -> Tokenizer<BufReader<'a>> {
        let sreader = BufReader::new(text.as_bytes());
        Tokenizer::<BufReader>::from_buf(sreader)
    }

    pub fn from_buf<'a>(sreader: BufReader<'a>) -> Tokenizer<BufReader<'a>> {
        let breader = BufferedReader::new(sreader);
        Tokenizer { breader: box breader, ungotch: None, untok: None, line: 1 }
    }

    pub fn vectorize(subject: &str) -> Vec<Token> {
        let reader = BufReader::new(subject.as_bytes());
        let mut tokenizer = Tokenizer::<BufReader>::from_buf(reader);
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

    fn read_name(&mut self, c: char) -> Option<Token> {
        let mut buffer = String::new();
        buffer.push_char(c);
        loop {
            match self.breader.read_char() {
                Err(ref s) if s.kind == EndOfFile => {
                    break;
                },
                Err(_) => {
                    return None;
                },
                Ok(c) if (c>='a' && c<='z') || (c>='A' && c<='Z') || c=='_' => {
                    buffer.push_char(c);
                },
                Ok(c) => {
                    self.ungetch(c);
                    break;
                },
            }
        }
        if buffer.as_slice() == "Nothing" {
            Some(NothingMarker)
        } else {
            Some(Name(buffer))
        }
    }

    fn read_integer_token(&mut self, c: char) -> Option<Token> {
        let mut buffer = String::new();
        buffer.push_char(c);
        loop {
            match self.breader.read_char() {
                Err(ref s) if s.kind == EndOfFile => {
                    return Some(IntegerLiteral(buffer));
                },
                Err(_) => {
                    return None;
                },
                Ok(c) if c>='0' && c<='9' => {
                    buffer.push_char(c);
                },
                Ok(c) => {
                    self.ungetch(c);
                    return Some(IntegerLiteral(buffer));
                },
            }
        }
    }

    fn read_oper(&mut self, c: char) -> Option<Token> {
        let mut buffer = String::new();
        buffer.push_char(c);
        loop {
            match self.breader.read_char() {
                Err(ref s) if s.kind == EndOfFile => {
                    return Some(OperToken(buffer));
                },
                Err(_) => {
                    return None;
                },
                Ok(r@'=')|Ok(r@'>')|Ok(r@'<')|Ok(r@'!') => {
                    buffer.push_char(r);
                },
                Ok(c) => {
                    self.ungetch(c);
                    return Some(OperToken(buffer));
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

    pub fn eof(&mut self) -> bool {
        if self.ungotch.is_some() {
            return false;
        }
        match self.breader.read_char() {
            Ok(c) => {
                self.ungotch = Some(c);
                return false;
            },
            _ => {

                return true;
            },
        }
    }

    pub fn get_token(&mut self) -> Option<Token> {
        match self.untok.clone() {
            Some(tk) => {
                self.untok = None;
                if tk == Newline {
                    self.line += 1;
                }
                return Some(tk);
            },
            None => {},
        }
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
            '\n' => {
                self.line += 1;
                Some(Newline)
            },
            '@' => Some(AddressSign),
            'a'..'z' => self.read_name(c),
            'A'..'Z' => self.read_name(c),
            ''|'' => self.next(),
            '('|'['|'{' => Some(OpenBracket(c)),
            ')'|']'|'}' => Some(CloseBracket(c)),
            '='|'!'|'<'|'>' => self.read_oper(c),
            _ => fail!("unexpected character '{}'", c)
        }
    }

    pub fn unget_token(&mut self, token: Token) {
        if self.untok.is_some() {
            fail!("can only unget one token at a time");
        }
        if token == Newline {
            self.line -= 1;
        }

        self.untok = Some(token);
    }

    pub fn peek_token(&mut self) -> Option<Token> {
        let tok = self.get_token();
        if tok.is_some() {
            self.unget_token(tok.clone().unwrap());
        }
        tok
    }

    pub fn current_line(&self) -> uint {
        self.line
    }
}

impl<R: Reader> Iterator<Token> for Tokenizer<R> {
    fn next(&mut self) -> Option<Token> {
        self.get_token()
    }
}

pub fn from_str<'a>(text: &'a str) -> Tokenizer<BufReader<'a>> {
    Tokenizer::<BufReader>::from_str(text)
}

pub fn from_file(file: File) -> Tokenizer<File> {
    Tokenizer::<File>::from_file(file)
}

pub fn vector(text: &str) -> Vec<Token> {
    Tokenizer::<BufReader>::vectorize(text)
}

#[test]
fn comma() {
    let tokens = vector(",");
    assert_eq!(1, tokens.len());
    assert_eq!(&Comma, tokens.get(0));
}

#[test]
fn newline() {
    let tokens = vector("  \n  ");
    assert_eq!(1, tokens.len());
    assert_eq!(&Newline, tokens.get(0));
}

#[test]
fn comma_with_newline() {
    let tokens = vector(",\n,");
    assert_eq!(3, tokens.len());
    assert_eq!(&Comma, tokens.get(0));
    assert_eq!(&Newline, tokens.get(1));
    assert_eq!(&Comma, tokens.get(2));
}

#[test]
fn char_literal() {
    let tokens = vector("'a','\\\\','\\&', '\\n'");
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
    let tokens = vector("0 123 -456 7890123456");
    assert_eq!(4, tokens.len());
    assert_eq!(&IntegerLiteral("0".to_string()), tokens.get(0));
    assert_eq!(&IntegerLiteral("123".to_string()), tokens.get(1));
    assert_eq!(&IntegerLiteral("-456".to_string()), tokens.get(2));
    assert_eq!(&IntegerLiteral("7890123456".to_string()), tokens.get(3));
}
