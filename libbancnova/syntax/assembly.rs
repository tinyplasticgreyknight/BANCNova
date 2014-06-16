use syntax::bscode;
use syntax::bscode::{Value, CellAddress, ToValue};

pub enum Expression {
    Immediate(Value),
    Cell(CellAddress),
    Nothing,
}
pub enum UnaryComparator {
    IsNull,
    IsNotNull,
}
pub enum BinaryComparator {
    LessOrEqual,
    Equal,
    GreaterOrEqual,
    Greater,
    NotEqual,
}
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
pub enum ArithTerm {
    ArithCell(ArithOperator, CellAddress),
    ArithImmediate(ArithOperator, Value),
}
pub enum Comparison {
    UnaryComparison(Expression, UnaryComparator),
    BinaryComparison(Expression, BinaryComparator, Expression),
}

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
            &Immediate(x) => x,
            &Cell(addr) => addr.as_value(),
            &Nothing => Value::new(0),
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

impl ToValue for ArithTerm {
    fn as_value(&self) -> Value {
        match self {
            &ArithCell(op, addr) => ((addr*10) + op).as_value(),
            &ArithImmediate(op, val) => (val*10) + 22000 + op,
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

impl Instruction {
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
fn render_newpage_inst() {
    let inst = NewPage;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(2999,0,0,0));
}
#[test]
fn render_simple_cond_unary() {
    let inst = SimpleConditional(UnaryComparison(Immediate(42.as_value()), IsNull));
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(3000,42,1,0));
}
#[test]
fn render_simple_cond_binary() {
    let inst = SimpleConditional(BinaryComparison(Immediate(42.as_value()), GreaterOrEqual, Cell(CellAddress::new(69))));
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(3000,42,4,69));
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
