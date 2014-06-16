use std::io::{BufferedReader, BufferedWriter, BufReader};
use std::fmt::{Show, Formatter, FormatError, WriteError};
use std::container::Container;
use result::BancResult;

#[deriving(PartialEq,Eq)]
pub struct Value {
    x: i16,
}

pub struct SyntaxInstruction {
    values: [Value, ..4],
}

#[deriving(PartialEq,Eq)]
pub struct SyntaxTree {
    instructions: Vec<SyntaxInstruction>,
}

impl Value {
    pub fn new<T: ToPrimitive>(x: T) -> Value {
        Value { x: x.to_i16().unwrap_or(0) }
    }
}

impl Show for Value {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        let wres = if self.x == 0 {
            // empty string
            formatter.write_str("")
        } else {
            formatter.write_int(self.x as int)
        };
        match wres {
            Ok(_) => Ok(()),
            Err(_) => Err(WriteError),
        }
    }
}

impl SyntaxInstruction {
    pub fn new(v0: i16, v1: i16, v2: i16, v3: i16) -> SyntaxInstruction {
        SyntaxInstruction { values: [Value::new(v0), Value::new(v1), Value::new(v2), Value::new(v3)] }
    }

    pub fn parse(line: String) -> BancResult<SyntaxInstruction> {
        let values: Vec<i16> = line.as_slice().splitn(',', 4).map(|s| {
                match s.trim() {
                    "" => Some(0),
                    s => from_str::<i16>(s)
                }
            }).filter(|o| { o.is_some() }).map(|o| { o.unwrap() }).collect();
        if values.len() == 4 {
            Ok(SyntaxInstruction::new(*values.get(0), *values.get(1), *values.get(2), *values.get(3)))
        } else {
            Err("not enough fields")
        }
    }

    pub fn render<W: Writer>(&self, buffer: &mut BufferedWriter<W>) {
        buffer.write_str(self.to_str().as_slice()).unwrap()
    }
}

impl Show for SyntaxInstruction {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        self.values[0].fmt(formatter);
        formatter.write_char(',');
        self.values[1].fmt(formatter);
        formatter.write_char(',');
        self.values[2].fmt(formatter);
        formatter.write_char(',');
        self.values[3].fmt(formatter)
    }
}

impl PartialEq for SyntaxInstruction {
    fn eq(&self, other: &SyntaxInstruction) -> bool {
        (self.values[0] == other.values[0]) &&
        (self.values[1] == other.values[1]) &&
        (self.values[2] == other.values[2]) &&
        (self.values[3] == other.values[3])
    }
}
impl Eq for SyntaxInstruction {}

impl SyntaxTree {
    pub fn new() -> SyntaxTree {
        SyntaxTree{ instructions: vec!() }
    }

    pub fn push(&mut self, inst: SyntaxInstruction) {
        self.instructions.push(inst);
    }

    pub fn get<'a>(&'a self, index: uint) -> &'a SyntaxInstruction {
        self.instructions.get(index)
    }

    pub fn parse<R: Reader>(buffer: &mut BufferedReader<R>) -> BancResult<SyntaxTree> {
        let mut tree = SyntaxTree::new();
        for oline in buffer.lines() {
            match oline {
                Err(_) => { return Err("read error"); },
                Ok(line) => {
                    match SyntaxInstruction::parse(line) {
                        Err(s) => { return Err(s); },
                        Ok(inst) => { tree.push(inst); },
                    }
                },
            }
        }
        Ok(tree)
    }

    pub fn render<W: Writer>(&self, buffer: &mut BufferedWriter<W>) {
        buffer.write_str(self.to_str().as_slice()).unwrap();
    }
}

impl Container for SyntaxTree {
    fn len(&self) -> uint {
        self.instructions.len()
    }
    fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }
}

impl Show for SyntaxTree {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        for inst in self.instructions.iter() {
            inst.fmt(formatter);
            formatter.write_char('\n');
        }
        Ok(())
    }
}


#[test]
fn syntax_tree() {
    let mut tree = SyntaxTree::new();
    tree.push(SyntaxInstruction::new(1, 2, 3, 4));
    tree.push(SyntaxInstruction::new(2, 3, 0, 0));
    let actual = tree.to_str();
    println!("actual:[{}]", actual);
    assert!(actual == "1,2,3,4\n2,3,,\n".to_string());
}

#[test]
fn parse_tree() {
    let strform = "5,6,7,8\n,,,";
    let reader: BufReader = BufReader::new(strform.as_bytes());
    let mut breader: BufferedReader<BufReader> = BufferedReader::new(reader);
    let tree = SyntaxTree::parse(&mut breader).unwrap();
    assert!(tree.len() == 2);
    assert!(*tree.get(0) == SyntaxInstruction::new(5, 6, 7, 8));
    assert!(*tree.get(1) == SyntaxInstruction::new(0, 0, 0, 0));
    let actual = tree.to_str();
    println!("actual:[{}]", actual);
    assert!(actual == "5,6,7,8\n,,,\n".to_string());
}
