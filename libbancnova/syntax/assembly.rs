use std::io::{BufferedReader};
#[cfg(test)]
use std::io::{BufReader};
use syntax::bscode;
use syntax::bscode::{Value, CellAddress, ToValue};
use result::BancResult;

#[deriving(PartialEq,Eq,Show)]
pub enum Expression {
    Immediate(Value),
    ImmChar(char),
    Cell(CellAddress),
    Nothing,
}
#[deriving(PartialEq,Eq,Show)]
pub enum UnaryComparator {
    IsNull,
    IsNotNull,
}
#[deriving(PartialEq,Eq,Show)]
pub enum BinaryComparator {
    LessOrEqual,
    Equal,
    GreaterOrEqual,
    Greater,
    NotEqual,
}
#[deriving(PartialEq,Eq,Show)]
pub enum ArithOperator {
    Length,
    Subtract,
    Add,
    Multiply,
    Divide,
    Substring,
    System,
    Logarithm,
    TruncateInt,
    Date,
}
#[deriving(PartialEq,Eq,Show)]
pub enum ArithTerm {
    ArithCell(ArithOperator, CellAddress),
    ArithImmediate(ArithOperator, Value),
}
#[deriving(PartialEq,Eq,Show)]
pub enum Comparison {
    UnaryComparison(Expression, UnaryComparator),
    BinaryComparison(Expression, BinaryComparator, Expression),
}

#[deriving(PartialEq,Eq,Show)]
pub enum Instruction {
    Unrecognised(Value, Value, Value, Value),
    NewPage,
    SimpleConditional(Comparison),
    BlockConditional(Comparison),
    BlockEnd,
    ReverseBlockConditional(Comparison),
    ReverseBlockEnd,
    SaveAddress,
    GotoPage(Value),
    AutoSave,
    Arithmetic(CellAddress, ArithTerm, ArithTerm, ArithTerm),
}

impl ToValue for Expression {
    fn as_value(&self) -> Value {
        match self {
            &Immediate(x) => x+10000,
            &ImmChar(c) => Value::new(c as int) + 30000,
            &Cell(addr) => addr.as_value(),
            &Nothing => Value::new(0),
        }
    }
}

fn parse_value<R: Reader>(buffer: &mut BufferedReader<R>) -> BancResult<Value> {
    //Value::parse(buffer)
    Ok(Value::new(42))
}

fn parse_chardata_1<R: Reader>(buffer: &mut BufferedReader<R>) -> BancResult<char> {
    match buffer.read_char() {
        Ok(c) => Ok(c),
        Err(e) => Err(e.desc),
    }
}

impl Expression {
    pub fn from_bscode(expr: Value) -> Option<Expression> {
        match expr.as_i16() {
            0 => Some(Nothing),
            10000..29999 => Some(Immediate(expr)),
            30000..30255 => Some(ImmChar((expr + (-30000)).as_i16() as u8 as char)),
            1..2000 => Some(Cell(CellAddress::new(expr.as_i16()))),
            _ => None,
        }
    }

    pub fn parse<R: Reader>(buffer: &mut BufferedReader<R>) -> BancResult<Expression> {
        match buffer.read_char() {
            Err(x) => Err(x.desc),
            Ok(x) => match x {
                '\'' => {
                    let ret = parse_chardata_1(buffer).map(|v| {ImmChar(v)});
                    match buffer.read_char() {
                        Ok('\'') => ret,
                        _ => Err("unclosed character"),
                    }
                },
                '-' => {
                    parse_value(buffer).map(|v| {Immediate(v)})
                },
                '0'..'9' => {
                    parse_value(buffer).map(|v| {Immediate(v)})
                },
                '@' => Ok(Nothing),
                _ => Err("read error"),
            },
        }
    }
}

impl ToValue for UnaryComparator {
    fn as_value(&self) -> Value {
        match self {
            &IsNull => 1.as_value(),
            &IsNotNull => 2.as_value(),
        }
    }
}

impl UnaryComparator {
    pub fn from_bscode(op: Value) -> Option<UnaryComparator> {
        match op.as_i16() {
            1 => Some(IsNull),
            2 => Some(IsNotNull),
            _ => None
        }
    }
}

impl ToValue for BinaryComparator {
    fn as_value(&self) -> Value {
        match self {
            &LessOrEqual => 2.as_value(),
            &Equal => 3.as_value(),
            &GreaterOrEqual => 4.as_value(),
            &Greater => 5.as_value(),
            &NotEqual => 6.as_value(),
        }
    }
}

impl BinaryComparator {
    pub fn from_bscode(op: Value) -> Option<BinaryComparator> {
        match op.as_i16() {
            2 => Some(LessOrEqual),
            3 => Some(Equal),
            4 => Some(GreaterOrEqual),
            5 => Some(Greater),
            6 => Some(NotEqual),
            _ => None
        }
    }
}

impl ArithTerm {
    fn from_bscode(n: Value) -> Option<ArithTerm> {
        let val = n.as_i16();
        let aoper = ArithOperator::from_bscode(val % 10);
        if aoper.is_none() { return None; }
        let aoper = aoper.unwrap();
        let val = val - (val%10);
        match val {
            0..20000 => {
                Some(ArithCell(aoper, CellAddress::new(val/10)))
            },
            20001..21999 => None,
            _ => {
                let val = (val-22000)/10;
                Some(ArithImmediate(aoper, (val as int).as_value()))
            },
        }
    }
}

impl ToValue for ArithTerm {
    fn as_value(&self) -> Value {
        match self {
            &ArithCell(op, addr) => ((addr*10) + op).as_value(),
            &ArithImmediate(op, val) => (val*10) + 22000 + op,
        }
    }
}

impl ArithOperator {
    pub fn from_bscode(val: i16) -> Option<ArithOperator> {
        match val {
            0 => Some(Length),
            1 => Some(Subtract),
            2 => Some(Add),
            3 => Some(Multiply),
            4 => Some(Divide),
            5 => Some(Substring),
            6 => Some(System),
            7 => Some(Logarithm),
            8 => Some(TruncateInt),
            9 => Some(Date),
            _ => None
        }
    }
}

impl ToValue for ArithOperator {
    fn as_value(&self) -> Value {
        match self {
            &Length => 0.as_value(),
            &Subtract => 1.as_value(),
            &Add => 2.as_value(),
            &Multiply => 3.as_value(),
            &Divide => 4.as_value(),
            &Substring => 5.as_value(),
            &System => 6.as_value(),
            &Logarithm => 7.as_value(),
            &TruncateInt => 8.as_value(),
            &Date => 9.as_value(),
        }
    }
}

impl Comparison {
    pub fn from_bscode(expr1: Value, op: Value, expr2: Value) -> Option<Comparison> {
        let o_expr1 = Expression::from_bscode(expr1);
        let o_uop = UnaryComparator::from_bscode(op);
        let o_bop = BinaryComparator::from_bscode(op);
        let o_expr2 = Expression::from_bscode(expr2);
        match (o_expr1, o_uop, o_bop, o_expr2) {
            (Some(expr1), Some(op), _, Some(Nothing)) => Some(UnaryComparison(expr1, op)),
            (Some(expr1), _, Some(op), Some(expr2)) => Some(BinaryComparison(expr1, op, expr2)),
            _ => None,
        }
    }
}

fn conditional_as_bscode(inst: &Instruction) -> Option<bscode::Instruction> {
    let (opcode, comparison) = match inst {
        &SimpleConditional(comp) => (Some(3000), Some(comp)),
        &BlockConditional(comp) => (Some(3001), Some(comp)),
        &ReverseBlockConditional(comp) => (Some(3101), Some(comp)),
        _ => (None, None),
    };
    if opcode.is_none() { return None; }
    let opcode = opcode.unwrap();
    match comparison {
        Some(UnaryComparison(expr, op)) => Some(bscode::Instruction::new(opcode, expr, op, 0)),
        Some(BinaryComparison(expr1, op, expr2)) => Some(bscode::Instruction::new(opcode, expr1, op, expr2)),
        _ => None,
    }
}

fn arithmetic_from_bscode(opcode: Value, a: Value, b: Value, c: Value) -> Option<Instruction> {
    let iopcode = opcode.as_i16();
    if iopcode <= 10000 || iopcode >= 12000 {
        return None;
    }
    let addr = CellAddress::new((opcode + (-10000)).as_i16());
    let oa = ArithTerm::from_bscode(a);
    let ob = ArithTerm::from_bscode(b);
    let oc = ArithTerm::from_bscode(c);
    match (oa, ob, oc) {
        (Some(a), Some(b), Some(c)) => Some(Arithmetic(addr, a, b, c)),
        _ => None,
    }
}

impl Instruction {
    pub fn from_bscode(inst: &bscode::Instruction) -> Instruction {
        let opcode = inst.get(0).as_i16() as int;
        let (a, b, c) = (*inst.get(1), *inst.get(2), *inst.get(3));
        let all_args_zero = a.is_zero() && b.is_zero() && c.is_zero();
        match opcode {
            2999 if all_args_zero => NewPage,
            3000|3001|3101 => {
                let ocomp = Comparison::from_bscode(a, b, c);
                if all_args_zero && opcode != 3000 {
                    if opcode == 3001 {
                        BlockEnd
                    } else {
                        ReverseBlockEnd
                    }
                } else if ocomp.is_some() {
                    if opcode == 3000 {
                        SimpleConditional(ocomp.unwrap())
                    } else if opcode == 3001 {
                        BlockConditional(ocomp.unwrap())
                    } else {
                        ReverseBlockConditional(ocomp.unwrap())
                    }
                } else {
                    Unrecognised(opcode.as_value(), a, b, c)
                }
            },
            8400 if all_args_zero => SaveAddress,
            8500 if a.is_zero() && c.is_zero() => GotoPage(b),
            9001 if all_args_zero => AutoSave,
            10000..11999 => match arithmetic_from_bscode(opcode.as_value(), a, b, c) {
                Some(a) => a,
                None => Unrecognised(opcode.as_value(), a, b, c),
            },
            _ => Unrecognised(opcode.as_value(), a, b, c),
        }
    }

    pub fn as_bscode(&self) -> bscode::Instruction {
        match self {
            &NewPage => bscode::Instruction::new(2999,0,0,0),
            &BlockEnd => bscode::Instruction::new(3001,0,0,0),
            &ReverseBlockEnd => bscode::Instruction::new(3101,0,0,0),
            &SaveAddress => bscode::Instruction::new(8400,0,0,0),
            &GotoPage(n) => bscode::Instruction::new(8500,0,n,0),
            &AutoSave => bscode::Instruction::new(9001,0,0,0),
            &Arithmetic(addr, t1, t2, t3) => bscode::Instruction::new(addr+10000,t1,t2,t3),//(addr + 10000).as_value(),
            &Unrecognised(a,b,c,d) => bscode::Instruction::new(a,b,c,d),
            x => conditional_as_bscode(x).unwrap(),
        }
    }
}

#[test]
fn parse_newpage() {
    let bsinst = bscode::Instruction::new(2999,0,0,0);
    let inst = Instruction::from_bscode(&bsinst);
    assert_eq!(inst, NewPage);
}
#[test]
fn parse_block_end() {
    let bsinst = bscode::Instruction::new(3001,0,0,0);
    let inst = Instruction::from_bscode(&bsinst);
    assert_eq!(inst, BlockEnd);
}
#[test]
fn parse_reverse_block_end() {
    let bsinst = bscode::Instruction::new(3101,0,0,0);
    let inst = Instruction::from_bscode(&bsinst);
    assert_eq!(inst, ReverseBlockEnd);
}

#[test]
fn render_newpage_inst() {
    let inst = NewPage;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(2999,0,0,0));
}
#[test]
fn render_simple_cond_unary() {
    let inst = SimpleConditional(UnaryComparison(Immediate(42.as_value()), IsNull));
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(3000,10042,1,0));
}
#[test]
fn render_simple_cond_binary() {
    let inst = SimpleConditional(BinaryComparison(Immediate(42.as_value()), GreaterOrEqual, Cell(CellAddress::new(69))));
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(3000,10042,4,69));
}
#[test]
fn render_block_end() {
    let inst = BlockEnd;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(3001,0,0,0));
}
#[test]
fn render_reverse_block_end() {
    let inst = ReverseBlockEnd;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(3101,0,0,0));
}
#[test]
fn render_save_address() {
    let inst = SaveAddress;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(8400,0,0,0));
}
#[test]
fn render_goto_page() {
    let inst = GotoPage(42.as_value());
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(8500,0,42,0));
}
#[test]
fn render_autosave() {
    let inst = AutoSave;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(9001,0,0,0));
}
#[test]
fn render_arith_substr() {
    let inst = Arithmetic(CellAddress::new(350), ArithImmediate(Add, 42.as_value()), ArithCell(Multiply, CellAddress::new(269)), ArithImmediate(Logarithm, 53.as_value()));
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(10350,22422,2693,22537));
}

#[test]
fn parse_expr_imm_char() {
    let strform = "'@'";
    let reader = BufReader::new(strform.as_bytes());
    let mut breader = BufferedReader::new(reader);
    let expr = Expression::parse(&mut breader).unwrap();
    assert_eq!(expr, ImmChar('@'));
}
