use std::io::{BufferedReader, BufferedWriter};
#[cfg(test)]
use std::io::{File, BufReader};
use std::fmt::{Show, Formatter, FormatError, WriteError};
use std::container::Container;
use std::string::String;
use result::BancResult;

#[deriving(PartialEq,Eq)]
pub struct Value {
    x: i16,
}

#[deriving(PartialEq,Eq,Show)]
pub struct CellAddress {
    a: i16,
}

impl CellAddress {
    pub fn new<T: ToPrimitive>(a: T) -> CellAddress {
        CellAddress { a: a.to_i16().unwrap_or(0) }
    }

    pub fn as_value(&self) -> Value {
        Value::new(self.a)
    }
}

impl<B: ToValue> Add<B, Value> for Value {
    fn add(&self, b: &B) -> Value {
        let b = b.as_value();
        Value { x: (self.x + b.x) }
    }
}

impl<B: ToValue> Mul<B, Value> for Value {
    fn mul(&self, b: &B) -> Value {
        let b = b.as_value();
        Value { x: (self.x)*(b.x) }
    }
}

impl<B: ToValue> Add<B, CellAddress> for CellAddress {
    fn add(&self, b: &B) -> CellAddress {
        let b = b.as_value();
        CellAddress { a: (self.a)+(b.x) }
    }
}

impl<B: ToValue> Mul<B, CellAddress> for CellAddress {
    fn mul(&self, b: &B) -> CellAddress {
        let b = b.as_value();
        CellAddress { a: (self.a)*(b.x) }
    }
}

pub struct Instruction {
    values: [Value, ..4],
}

#[deriving(PartialEq,Eq)]
pub struct Tree {
    instructions: Vec<Instruction>,
}

impl Value {
    pub fn new<T: ToPrimitive>(x: T) -> Value {
        Value { x: x.to_i16().unwrap_or(0) }
    }

    pub fn parse<R: Reader>(buffer: &mut BufferedReader<R>) -> BancResult<Value> {
        Err("not implemented")
    }

    pub fn is_zero(&self) -> bool {
        self.x == 0
    }

    pub fn as_i16(&self) -> i16 {
        self.x
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

impl Instruction {
    pub fn new<T0: ToValue, T1: ToValue, T2: ToValue, T3: ToValue>(v0: T0, v1: T1, v2: T2, v3: T3) -> Instruction {
        Instruction { values: [v0.as_value(), v1.as_value(), v2.as_value(), v3.as_value()] }
    }

    pub fn parse(line: String) -> BancResult<Instruction> {
        let values: Vec<i16> = line.as_slice().splitn(',', 4).map(|s| {
                match s.trim() {
                    "" => Some(0),
                    s => from_str::<i16>(s)
                }
            }).filter(|o| { o.is_some() }).map(|o| { o.unwrap() }).collect();
        if values.len() == 4 {
            Ok(Instruction::new(
                    *values.get(0) as int,
                    *values.get(1) as int,
                    *values.get(2) as int,
                    *values.get(3) as int
                        ))
        } else {
            Err("not enough fields")
        }
    }

    pub fn get<'a>(&'a self, index: uint) -> &'a Value {
        self.values.get(index).unwrap()
    }

    pub fn render<W: Writer>(&self, buffer: &mut BufferedWriter<W>) {
        buffer.write_str(self.to_str().as_slice()).unwrap()
    }
}

impl Show for Instruction {
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

impl PartialEq for Instruction {
    fn eq(&self, other: &Instruction) -> bool {
        (self.values[0] == other.values[0]) &&
        (self.values[1] == other.values[1]) &&
        (self.values[2] == other.values[2]) &&
        (self.values[3] == other.values[3])
    }
}
impl Eq for Instruction {}

impl Tree {
    pub fn new() -> Tree {
        Tree{ instructions: vec!() }
    }

    pub fn push(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }

    pub fn get<'a>(&'a self, index: uint) -> &'a Instruction {
        self.instructions.get(index)
    }

    pub fn parse<R: Reader>(buffer: &mut BufferedReader<R>) -> BancResult<Tree> {
        let mut tree = Tree::new();
        for oline in buffer.lines() {
            match oline {
                Err(_) => { return Err("read error"); },
                Ok(line) => {
                    if line.as_slice()!="\x1a" {
                        match Instruction::parse(line) {
                            Err(s) => { return Err(s); },
                            Ok(inst) => { tree.push(inst); },
                        }
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

impl Container for Tree {
    fn len(&self) -> uint {
        self.instructions.len()
    }
    fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }
}

impl Show for Tree {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        for inst in self.instructions.iter() {
            inst.fmt(formatter);
            formatter.write_char('\n');
        }
        Ok(())
    }
}

pub trait ToValue {
    fn as_value(&self) -> Value;
}

impl ToValue for Value {
    fn as_value(&self) -> Value {
        *self
    }
}

impl ToValue for CellAddress {
    fn as_value(&self) -> Value {
        Value::new(self.a)
    }
}

impl ToValue for int {
    fn as_value(&self) -> Value {
        Value::new(*self)
    }
}

#[test]
fn syntax_tree() {
    let mut tree = Tree::new();
    tree.push(Instruction::new(1, 2, 3, 4));
    tree.push(Instruction::new(2, 3, 0, 0));
    let actual = tree.to_str();
    assert_eq!(actual, "1,2,3,4\n2,3,,\n".to_string());
}

#[test]
fn parse_tree() {
    let strform = "5,6,7,8\n,,,";
    let reader: BufReader = BufReader::new(strform.as_bytes());
    let mut breader: BufferedReader<BufReader> = BufferedReader::new(reader);
    let tree = Tree::parse(&mut breader).unwrap();
    assert_eq!(tree.len(), 2);
    assert_eq!(tree.get(0), &Instruction::new(5, 6, 7, 8));
    assert_eq!(tree.get(1), &Instruction::new(0, 0, 0, 0));
    let actual = tree.to_str();
    assert_eq!(actual, "5,6,7,8\n,,,\n".to_string());
}

#[test]
fn parse_file() {
    let file = File::open(&Path::new("../test_data/MM1SM1.SCN"));
    let mut reader = BufferedReader::new(file);
    let tree = Tree::parse(&mut reader).unwrap();
    assert_eq!(tree.len(), 706);
    assert_eq!(tree.get(  0), &Instruction::new(31521,10001,700,108));
    assert_eq!(tree.get(100), &Instruction::new(3100,1528,1,10011));
    assert_eq!(tree.get(200), &Instruction::new(11530,22012,22002,22002));
    assert_eq!(tree.get(300), &Instruction::new(3100,285,3,30088));
    assert_eq!(tree.get(400), &Instruction::new(3000,1533,6,10000));
    assert_eq!(tree.get(500), &Instruction::new(3000,1538,6,30045));
    assert_eq!(tree.get(600), &Instruction::new(3000,1540,3,30032));
    assert_eq!(tree.get(700), &Instruction::new(8500,0,3,0));
}
