use std::io::{BufferedWriter};
use std::fmt::{Show, Formatter, FormatError, WriteError};
use std::string::String;
use result::BancResult;
use syntax::tree::{TreeNode};
use syntax::tokenize::{Tokenizer, Newline, IntegerLiteral, Comma};
#[cfg(test)]
use std::io::{File};
#[cfg(test)]
use syntax::tree::Tree;
use std::num::{Zero, One};


#[deriving(PartialEq,Eq)]
pub struct Value {
    x: i16,
}

#[deriving(PartialEq,Eq)]
pub struct CellAddress {
    a: i16,
}

impl CellAddress {
    pub fn new<T: ToPrimitive>(a: T) -> CellAddress {
        CellAddress { a: a.to_i16().unwrap_or(0) }
    }

    pub fn parse(text: String) -> BancResult<CellAddress> {
        match Value::parse(text) {
            Ok(v) => {
                let i = v.as_i16();
                if i < 0 || i >= 2000 {
                    Err("cell address must be in the range [0, 2000]")
                } else {
                    Ok(CellAddress::new(i))
                }
            },
            Err(e) => Err(e),
        }
    }

    pub fn as_value(&self) -> Value {
        Value::new(self.a)
    }
}

impl Show for CellAddress {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        if self.a == 0 {
            "0".fmt(formatter)
        } else {
            formatter.write_char('@');
            self.as_value().fmt(formatter)
        }
    }
}

impl<B: ToValue> Add<B, Value> for Value {
    fn add(&self, b: &B) -> Value {
        let b = b.as_value();
        Value { x: (self.x + b.x) }
    }
}

impl<B: ToValue> Sub<B, Value> for Value {
    fn sub(&self, b: &B) -> Value {
        let b = b.as_value();
        Value { x: (self.x - b.x) }
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

impl Value {
    pub fn new<T: ToPrimitive>(x: T) -> Value {
        Value { x: x.to_i16().unwrap_or(0) }
    }

    pub fn parse(text: String) -> BancResult<Value> {
        match from_str::<i16>(text.as_slice()) {
            Some(i) => Ok(Value::new(i)),
            None => Err("invalid value"),
        }
    }

    pub fn as_i16(&self) -> i16 {
        self.x
    }
}

impl Zero for Value {
    fn zero() -> Value {
        Value::new(0)
    }
    fn is_zero(&self) -> bool {
        self.x == 0
    }
}

impl One for Value {
    fn one() -> Value {
        Value::new(1)
    }
}

impl Zero for CellAddress {
    fn zero() -> CellAddress {
        CellAddress::new(0)
    }
    fn is_zero(&self) -> bool {
        self.a == 0
    }
}

impl ToPrimitive for Value {
    fn to_i64(&self) -> Option<i64> {
        Some(self.as_i16() as i64)
    }
    fn to_u64(&self) -> Option<u64> {
        let i = self.as_i16();
        if i < 0 {
            None
        } else {
            Some(i as u64)
        }
    }
}

impl Show for Value {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        let wres = if self.x == 0 {
            // zero could be shown as the empty string
            // but that would be a pain
            formatter.write_str("0")
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

    pub fn parse_elem(s0: String, s1: String, s2: String, s3: String) -> BancResult<Instruction> {
        let v0 = Value::parse(s0);
        let v1 = Value::parse(s1);
        let v2 = Value::parse(s2);
        let v3 = Value::parse(s3);

        if v0.is_ok() && v1.is_ok() && v2.is_ok() && v3.is_ok() {
            Ok(Instruction::new(v0.unwrap(), v1.unwrap(), v2.unwrap(), v3.unwrap()))
        } else {
            Err("not enough fields")
        }
    }

    pub fn parse_vec(elements: &mut Vec<String>) -> BancResult<Option<Instruction>> {
        match elements.len() {
            0 => { return Ok(None); },
            1|2|3 => { return Err("not enough arguments"); },
            4 => {},
            _ => { return Err("too many arguments"); },
        }

        let inst = Instruction::parse_elem(elements.get(0).clone(), elements.get(1).clone(), elements.get(2).clone(), elements.get(3).clone());
        elements.truncate(0);
        match inst {
            Err(msg) => { Err(msg) },
            Ok(inst) => { Ok(Some(inst)) },
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

impl TreeNode for Instruction {
    fn parse<R: Reader>(tokenizer: &mut Tokenizer<R>) -> BancResult<Option<Instruction>> {
        let mut tokenizer = tokenizer;
        let mut elements: Vec<String> = vec!();
        let mut lasttoken = Newline;
        let empty: String = "0".to_string();
        for token in tokenizer {
            match token.clone() {
                Newline => { break; },
                IntegerLiteral(s) => {
                    elements.push(s);
                },
                Comma => {
                    if lasttoken == Newline || lasttoken == Comma {
                        elements.push(empty.clone());
                    }
                },
                _ => {
                    return Err("unexpected token");
                },
            }
            lasttoken = token;
        }
        if lasttoken == Comma {
            elements.push(empty.clone());
        }
        match Instruction::parse_vec(&mut elements) {
            Ok(None) => Ok(None),
            Ok(Some(inst)) => Ok(Some(inst)),
            Err(e) => Err(e),
        }
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
    assert_eq!(actual, "1,2,3,4\n2,3,0,0\n".to_string());
}

#[test]
fn parse_tree() {
    let strform = "5,6,7,8\n,,,";
    let tree = Tree::parse_string(strform).unwrap();
    assert_eq!(tree.len(), 2);
    assert_eq!(tree.get(0), &Instruction::new(5, 6, 7, 8));
    assert_eq!(tree.get(1), &Instruction::new(0, 0, 0, 0));
    let actual = tree.to_str();
    assert_eq!(actual, "5,6,7,8\n0,0,0,0\n".to_string());
}

#[test]
fn parse_file() {
    let file = File::open(&Path::new("test_data/MM1SM1.SCN")).unwrap();
    let tree = Tree::parse_file(file).unwrap();
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
