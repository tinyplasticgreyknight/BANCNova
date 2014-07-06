use std::io::{BufferedWriter};
use std::fmt::{Show, Formatter, FormatError};
use syntax::bscode;
use syntax::bscode::{Value, CellAddress, ToValue};
use result::BancResult;
use syntax::tokenize;
use syntax::tokenize::{Tokenizer, Token, Name, IntegerLiteral, Comma, Newline, OpenBracket, CloseBracket, OperToken, NothingMarker};
use syntax::tree::{TreeNode};
#[cfg(test)]
use syntax::tree::{Tree};
use std::num::Zero;
#[cfg(test)]
use std::num::One;

#[deriving(PartialEq,Eq)]
pub enum Expression {
    Immediate(Value),
    ImmChar(char),
    Cell(CellAddress),
    Nothing,
}
#[deriving(PartialEq,Eq)]
pub enum AddressOrValue {
    EAddress(CellAddress),
    EValue(Value),
}
#[deriving(PartialEq,Eq)]
pub enum UnaryComparator {
    IsNull,
    IsNotNull,
}
#[deriving(PartialEq,Eq)]
pub enum BinaryComparator {
    Less,
    LessOrEqual,
    Equal,
    GreaterOrEqual,
    Greater,
    NotEqual,
}
#[deriving(PartialEq,Eq)]
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
struct ArithRaw {
    op: i16,
    main: i16,
}
#[deriving(PartialEq,Eq)]
pub enum ArithTerm {
    ArithEmpty,
    ArithCell(ArithOperator, CellAddress),
    ArithImmediate(ArithOperator, Value),
}
#[deriving(PartialEq,Eq)]
pub enum Comparison {
    UnaryComparison(Expression, UnaryComparator),
    BinaryComparison(Expression, BinaryComparator, Expression),
}

#[deriving(PartialEq,Eq)]
pub struct Position {
    x: Value,
    y: Value,
}

#[deriving(PartialEq,Eq)]
pub enum GotoSpecialTarget {
    ProductSales,
    MainMenu,
    ExitSystem,
    Storage,
    MultitaskMenu,
}

#[deriving(PartialEq,Eq)]
pub enum DataModelField {
    DataModelNumber,
    DataModelLabel,
    DataModelValue,
}

#[deriving(PartialEq,Eq)]
pub enum TableMode {
    TableSearch,
    TableSearchRange,
    TableSearchRangePairs,
    TableTransfer,
    TableTransferReverse,
}

#[deriving(PartialEq,Eq)]
pub enum Instruction {
    Unrecognised(Value, Value, Value, Value),
    ShowPrompt(CellAddress, Position, Value, Position),
    NewPage(Value),
    SimpleConditional(Comparison),
    BlockConditional(Comparison),
    BlockEnd,
    ReverseSimpleConditional(Comparison),
    ReverseBlockConditional(Comparison),
    ReverseBlockEnd,
    Window(Value, Position, Position),
    SaveAddress,
    GotoPage(Value),
    GotoPrompt(CellAddress),
    GotoFKey(Value),
    GotoTransaction(Value),
    GotoSpecial(GotoSpecialTarget),
    SystemCall(CellAddress),
    SetVideo(Value),
    AutoSolve,
    AutoSave,
    DataRun(bool, DataModelField, Option<CellAddress>, Option<CellAddress>),
    DataPut(bool, DataModelField, Option<CellAddress>, Option<CellAddress>),
    DataGet(bool, DataModelField, Option<CellAddress>, Option<CellAddress>),
    Arithmetic(CellAddress, ArithTerm, ArithTerm, ArithTerm),
    ArithmeticSet(CellAddress, AddressOrValue, ArithTerm, ArithTerm),
    ArithmeticSetNeg(CellAddress, AddressOrValue, ArithTerm, ArithTerm),
    SetZero(CellAddress),
    Clear(CellAddress, CellAddress, ArithTerm, ArithTerm),
    ArithmeticLen(CellAddress, AddressOrValue, ArithTerm, ArithTerm),
    ArithmeticPow(CellAddress, AddressOrValue, AddressOrValue, ArithTerm),
    ArithmeticSubstr(CellAddress, CellAddress, AddressOrValue, AddressOrValue),
    GetSystemTime(CellAddress, CellAddress, ArithTerm),
    ArithmeticLog(CellAddress, AddressOrValue, ArithTerm, ArithTerm),
    ArithmeticTrunc(CellAddress, AddressOrValue, ArithTerm, ArithTerm),
    Date365(CellAddress, AddressOrValue, AddressOrValue),
    Date360(CellAddress, AddressOrValue, AddressOrValue),
    DateDiff(CellAddress, AddressOrValue, AddressOrValue),
    DateFoo(CellAddress, AddressOrValue, AddressOrValue),
    Table(bool, TableMode, CellAddress, AddressOrValue, Value, Value),
}

#[deriving(PartialEq,Eq,Show)]
pub enum Argument {
    ArgArith(ArithTerm),
    ArgComp(Comparison),
    ArgExpr(Expression),
    ArgEmpty,
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

impl Expression {
    pub fn from_bscode(expr: Value) -> Option<Expression> {
        match expr.as_i16() {
            0 => Some(Nothing),
            r@ 10000..29999 => Some(Immediate(Value::new(r-10000))),
            r@30000..30255 => Some(ImmChar((r-30000) as u8 as char)),
            r@1..2000 => Some(Cell(CellAddress::new(r))),
            _ => None,
        }
    }


    pub fn parse_string(text: &str) -> BancResult<Expression> {
        let mut tokenizer = tokenize::from_str(text);
        let tok = tokenizer.next();

        Expression::parse(tok, &mut tokenizer)
    }

    pub fn parse<R: Reader>(init_token: Option<Token>, tokenizer: &mut Tokenizer<R>) -> BancResult<Expression> {
        let tok = match init_token {
            Some(tk) => Some(tk),
            None => tokenizer.next(),
        };

        if tok.is_none() {
            return Err("read error");
        }

        match tok.unwrap() {
            tokenize::FencedLiteral('\'', s) => {
                if s.len() == 1 {
                    let c = s.as_slice().char_at(0);
                    Ok(ImmChar(c))
                } else {
                    Err("character literal too long")
                }
            },
            tokenize::AddressSign => {
                tokenizer.unget_token(tokenize::AddressSign);
                CellAddress::parse_tokens(tokenizer).map(|a| Cell(a))
            },
            tokenize::NothingMarker => Ok(Nothing),
            tokenize::IntegerLiteral(s) => {
                Value::parse(s).map(|v| { Immediate(v) })
            },
            _ => Err("unexpected token"),
        }
    }
}

impl Show for Expression {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        match self {
            &Immediate(v) => {
                v.fmt(formatter)
            },
            &ImmChar(c) => {
                formatter.write_char('\'');
                formatter.write_char(c);
                formatter.write_char('\'');
                Ok(())
            },
            &Cell(a) => {
                a.fmt(formatter)
            },
            &Nothing => {
                "Nothing".fmt(formatter)
            }
        }
    }
}

impl Position {
    pub fn new<T0: ToValue, T1: ToValue>(v0: T0, v1: T1) -> Position {
        Position { x: v0.as_value(), y: v1.as_value() }
    }
    pub fn from_packed(packed: Value) -> Position {
        let v = packed.as_i16();
        let x = v % 100;
        let y = v / 100;
        Position::new(x as int, y as int)
    }
    pub fn as_packed(&self) -> Value {
        let x = self.x.as_i16();
        let y = self.y.as_i16();
        Value::new(x + 100*y)
    }
}

impl Add<Position, Position> for Position {
    fn add(&self, rhs: &Position) -> Position {
        Position::new(self.x+rhs.x, self.y+rhs.y)
    }
}

impl Zero for Position {
    fn zero() -> Position {
        Position::new(0, 0)
    }
    fn is_zero(&self) -> bool {
        self.x.is_zero() && self.y.is_zero()
    }
}

impl Show for Position {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        let v = self.as_packed();
        v.fmt(formatter)
    }
}

impl AddressOrValue {
    fn from_value(v: Value) -> Option<AddressOrValue> {
        match v.as_i16() {
            0 => Some(EValue(Zero::zero())),
            1..2000 => Some(EAddress(CellAddress::new(v))),
            10000..19999 => Some(EValue(Value::new(v - 10000))),
            _ => None,
        }
    }

    fn as_term_value(&self) -> Value {
        match self {
            &EAddress(addr) => addr.as_value(),
            &EValue(v) => v+2200
        }
    }
}

impl Show for AddressOrValue {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        match self {
            &EAddress(addr) => addr.fmt(formatter),
            &EValue(v) => v.fmt(formatter),
        }
    }
}

impl ToValue for AddressOrValue {
    fn as_value(&self) -> Value {
        match self {
            &EAddress(addr) => addr.as_value(),
            &EValue(v) =>
                if v.is_zero() {
                    v
                } else {
                    v+10000
                },
        }
    }
}

impl Add<AddressOrValue, AddressOrValue> for AddressOrValue {
    fn add(&self, rhs: &AddressOrValue) -> AddressOrValue {
        match (self, rhs) {
            (&EAddress(a1), &EAddress(a2)) => EAddress(a1+a2),
            _ => {
                let a = self.as_value();
                let b = rhs.as_value();
                EValue(a+b)
            },
        }
    }
}

impl Zero for AddressOrValue {
    fn zero() -> AddressOrValue {
        EValue(Zero::zero())
    }
    fn is_zero(&self) -> bool {
        match self {
            &EAddress(addr) => addr.is_zero(),
            &EValue(v) => v.is_zero(),
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
            &Less => 1,
            &LessOrEqual => 2,
            &Equal => 3,
            &GreaterOrEqual => 4,
            &Greater => 5,
            &NotEqual => 6,
        }.as_value()
    }
}

impl BinaryComparator {
    pub fn from_bscode(op: Value) -> Option<BinaryComparator> {
        match op.as_i16() {
            1 => Some(Less),
            2 => Some(LessOrEqual),
            3 => Some(Equal),
            4 => Some(GreaterOrEqual),
            5 => Some(Greater),
            6 => Some(NotEqual),
            _ => None
        }
    }
}

impl ArithRaw {
    fn new(n: Value) -> ArithRaw {
        let val = n.as_i16();
        let op = val % 10;
        let aval = (val - op)/10;
        ArithRaw { op: op, main: aval }
    }

    fn combine<T1: ToPrimitive>(op: T1, main: AddressOrValue) -> Option<ArithRaw> {
        match (op.to_i16(), main.as_term_value().to_i16()) {
            (Some(op), Some(main)) => Some(ArithRaw { op: op, main: main }),
            _ => None
        }
    }

    fn as_address_or_value(&self) -> Option<AddressOrValue> {
        match (self.as_address(), self.as_maybe_value()) {
            (Some(addr), _) => Some(EAddress(addr)),
            (_, Some(v)) => Some(EValue(v)),
            _ => None,
        }
    }

    fn as_address(&self) -> Option<CellAddress> {
        match self.main {
            r@1..2000 => Some(CellAddress::new(r)),
            _ => None
        }
    }

    fn as_maybe_value(&self) -> Option<Value> {
        match self.as_address() {
            Some(_) => None,
            None => Some(Value::new(self.main - 2200)),
        }
    }

    fn as_term(&self) -> Option<ArithTerm> {
        match (self.main, ArithOperator::from_bscode(self.op), self.as_maybe_value(), self.as_address()) {
            (0, _, _, _) => Some(ArithEmpty),
            (1..2000, Some(op), _, Some(addr)) => Some(ArithCell(op, addr)),
            (2001..2199, _, _, _) => None,
            (_, Some(op), Some(v), _) => Some(ArithImmediate(op, v)),
            _ => None,
        }
    }
}

impl ToValue for ArithRaw {
    fn as_value(&self) -> Value {
        Value::new(self.op + 10*self.main)
    }
}

impl ArithTerm {
    fn parse<R: Reader>(init_token: Token, tokenizer: &mut Tokenizer<R>) -> BancResult<ArithTerm> {
        if init_token == NothingMarker {
            return Ok(ArithEmpty);
        }
        let op = ArithOperator::parse_token(init_token);
        let open_paren = tokenizer.get_token();
        let subarg = Expression::parse(None, tokenizer);
        let close_paren = tokenizer.get_token();
        match open_paren {
            Some(OpenBracket('(')) => {},
            _ => { return Err("expected left parenthesis"); },
        }
        match close_paren {
            Some(CloseBracket(')')) => {},
            _ => { return Err("expected right parenthesis"); },
        }

        match (op, subarg) {
            (Ok(op), Ok(expr)) => {
                match expr {
                    Cell(addr) => Ok(ArithCell(op, addr)),
                    Immediate(val) => Ok(ArithImmediate(op, val)),
                    ImmChar(_) => Err("don't know what to do with a character literal in this position"),
                    Nothing => Err("no argument to arithterm?"),
                }
            },
            (Ok(_), Err(e)) => Err(e),
            (Err(e), _) => Err(e),
        }
    }

    fn new(op: ArithOperator, content: AddressOrValue) -> ArithTerm {
        match content {
            EAddress(addr) => ArithCell(op, addr),
            EValue(v) => ArithImmediate(op, v),
        }
    }

    fn effective(&self) -> bool {
        match self {
            &ArithEmpty => false,
            &ArithCell(_, _) => true,
            &ArithImmediate(Add, v) if v.as_i16() == 0 => false,
            &ArithImmediate(Subtract, v) if v.as_i16() == 0 => false,
            &ArithImmediate(Multiply, v) if v.as_i16() == 1 => false,
            &ArithImmediate(Divide, v) if v.as_i16() == 1 => false,
            _ => true,
        }
    }

    fn null() -> ArithTerm {
        ArithImmediate(Add, Zero::zero())
    }

    fn is_null(&self) -> bool {
        ! self.effective()
    }
}

impl ToValue for ArithTerm {
    fn as_value(&self) -> Value {
        match self {
            &ArithEmpty => ArithTerm::null().as_value(),
            &ArithCell(op, addr) => ((addr*10) + op).as_value(),
            &ArithImmediate(op, val) => (val*10) + 22000 + op,
        }
    }
}

impl Show for ArithTerm {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        match self {
            &ArithEmpty => {
                NothingMarker.fmt(formatter)
            },
            &ArithCell(op, addr) => {
                op.fmt(formatter);
                formatter.write_char('(');
                addr.fmt(formatter);
                formatter.write_char(')');
                Ok(())
            },
            &ArithImmediate(op, val) => {
                op.fmt(formatter);
                formatter.write_char('(');
                val.fmt(formatter);
                formatter.write_char(')');
                Ok(())
            },
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

    pub fn parse_token(token: Token) -> BancResult<ArithOperator> {
        match token {
            Name(s) => match s.as_slice() {
                "LEN" => Ok(Length),
                "SUB" => Ok(Subtract),
                "ADD" => Ok(Add),
                "MUL" => Ok(Multiply),
                "DIV" => Ok(Divide),
                "SUBSTR" => Ok(Substring),
                "SYSTEM" => Ok(System),
                "LOG" => Ok(Logarithm),
                "TRUNC" => Ok(TruncateInt),
                "DATE" => Ok(Date),
                _ => Err("name is not a valid arith operator"),
            },
            _ => Err("token is not a valid arith operator"),
        }
    }
}

impl ToValue for ArithOperator {
    fn as_value(&self) -> Value {
        match self {
            &Length => 0,
            &Subtract => 1,
            &Add => 2,
            &Multiply => 3,
            &Divide => 4,
            &Substring => 5,
            &System => 6,
            &Logarithm => 7,
            &TruncateInt => 8,
            &Date => 9,
        }.as_value()
    }
}

impl Show for ArithOperator {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        match self {
            &Length => "LEN",
            &Subtract => "SUB",
            &Add => "ADD",
            &Multiply => "MUL",
            &Divide => "DIV",
            &Substring => "SUBSTR",
            &System => "SYSTEM",
            &Logarithm => "LOG",
            &TruncateInt => "TRUNC",
            &Date => "DATE",
        }.fmt(formatter)
    }
}

impl Show for UnaryComparator {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        match self {
            &IsNull => "ISNULL",
            &IsNotNull => "ISNOTNULL",
        }.fmt(formatter)
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

    pub fn parse<R: Reader>(tokenizer: &mut Tokenizer<R>) -> BancResult<Comparison> {
        let tok = tokenizer.get_token();
        match tok {
            Some(Name(op)) => {
                let lparen = tokenizer.get_token();
                let expr = Expression::parse(None, tokenizer);
                let rparen = tokenizer.get_token();
                match (expr, lparen, rparen) {
                    (Ok(expr), Some(OpenBracket('(')), Some(CloseBracket(')'))) => {
                        match op.as_slice() {
                            "ISNULL" => Ok(UnaryComparison(expr, IsNull)),
                            "ISNOTNULL" => Ok(UnaryComparison(expr, IsNotNull)),
                            _ => Err("not a valid unary comparator"),
                        }
                    },
                    _ => Err("invalid unary condition syntax"),
                }
            },
            Some(tok) => {
                let expr1 = Expression::parse(Some(tok), tokenizer);
                let op = tokenizer.get_token();
                let expr2 = Expression::parse(None, tokenizer);
                match (expr1, op, expr2) {
                    (Ok(e1), Some(OperToken(op)), Ok(e2)) => {
                        match op.as_slice() {
                            "<"  => Ok(BinaryComparison(e1, Less, e2)),
                            "<=" => Ok(BinaryComparison(e1, LessOrEqual, e2)),
                            "==" => Ok(BinaryComparison(e1, Equal, e2)),
                            ">=" => Ok(BinaryComparison(e1, GreaterOrEqual, e2)),
                            ">"  => Ok(BinaryComparison(e1, Greater, e2)),
                            "!=" => Ok(BinaryComparison(e1, NotEqual, e2)),
                            _ => {
                                Err("not a valid binary comparator")
                            },
                        }
                    },
                    _ => Err("invalid condition syntax"),
                }
            },
            None => Err("read error"),
        }
    }
}

impl Show for Comparison {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        match self {
            &UnaryComparison(expr, comp) => {
                comp.fmt(formatter);
                formatter.write_char('(');
                expr.fmt(formatter);
                formatter.write_char(')');
                Ok(())
            },
            &BinaryComparison(expr1, Less, expr2) => {
                expr1.fmt(formatter);
                formatter.write_str(" < ");
                expr2.fmt(formatter);
                Ok(())
            },
            &BinaryComparison(expr1, LessOrEqual, expr2) => {
                expr1.fmt(formatter);
                formatter.write_str(" <= ");
                expr2.fmt(formatter);
                Ok(())
            },
            &BinaryComparison(expr1, Equal, expr2) => {
                expr1.fmt(formatter);
                formatter.write_str(" == ");
                expr2.fmt(formatter);
                Ok(())
            },
            &BinaryComparison(expr1, GreaterOrEqual, expr2) => {
                expr1.fmt(formatter);
                formatter.write_str(" >= ");
                expr2.fmt(formatter);
                Ok(())
            },
            &BinaryComparison(expr1, Greater, expr2) => {
                expr1.fmt(formatter);
                formatter.write_str(" > ");
                expr2.fmt(formatter);
                Ok(())
            },
            &BinaryComparison(expr1, NotEqual, expr2) => {
                expr1.fmt(formatter);
                formatter.write_str(" != ");
                expr2.fmt(formatter);
                Ok(())
            },
        }
    }
}

fn conditional_as_bscode(inst: &Instruction) -> Option<bscode::Instruction> {
    let (opcode, comparison) = match inst {
        &SimpleConditional(comp) => (Some(3000), Some(comp)),
        &BlockConditional(comp) => (Some(3001), Some(comp)),
        &ReverseSimpleConditional(comp) => (Some(3100), Some(comp)),
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

fn arithmetic_from_raw(addr: CellAddress, a: ArithRaw, b: ArithRaw, c: ArithRaw) -> Option<Instruction> {
    let (aop, bop) = (a.op, b.op);
    match aop {
        5 => match (a.as_address(), b.as_term(), c.as_term()) {
            (Some(a), Some(b), Some(c)) => Some(Clear(addr, a, b, c)),
            _ => None
        },
        6 => match bop {
            1..4 => match (a.as_address_or_value(), b.as_address_or_value(), c.as_term()) {
                (Some(a), Some(b), Some(c)) => Some(ArithmeticPow(addr, a, b, c)),
                _ => None
            },
            5 => match (a.as_address(), b.as_address_or_value(), c.as_address_or_value()) {
                (Some(a), Some(b), Some(c)) => Some(ArithmeticSubstr(addr, a, b, c)),
                _ => None
            },
            6 => match (a.as_address(), c.as_term()) {
                (Some(a), Some(c)) => Some(GetSystemTime(addr, a, c)),
                _ => None
            },
            _ => None
        },
        0..4|7|8 => match (aop, a.as_address_or_value(), b.as_term(), c.as_term()) {
            (0, Some(a), Some(b), Some(c)) => Some(ArithmeticLen(addr, a, b, c)),
            (1, Some(a), Some(b), Some(c)) => Some(ArithmeticSetNeg(addr, a, b, c)),
            (2..4, Some(a), Some(b), Some(c)) => Some(ArithmeticSet(addr, a, b, c)),
            (7, Some(a), Some(b), Some(c)) => Some(ArithmeticLog(addr, a, b, c)),
            (8, Some(a), Some(b), Some(c)) => Some(ArithmeticTrunc(addr, a, b, c)),
            _ => None
        },
        9 => match (a.as_address_or_value(), b.as_address_or_value()) {
            (Some(a), Some(b)) => match bop {
                1 => Some(Date365(addr, a, b)),
                2 => Some(Date360(addr, a, b)),
                3 => Some(DateDiff(addr, a, b)),
                4 => Some(DateFoo(addr, a, b)),
                _ => None
            },
            _ => None
        },
        _ => None
    }
}

fn arithmetic_from_bscode(opcode: Value, a: Value, b: Value, c: Value) -> Option<Instruction> {
    let iopcode = opcode.as_i16();
    if iopcode <= 10000 || iopcode >= 12000 {
        return None;
    }
    let addr = CellAddress::new((opcode + (-10000)).as_i16());
    let a = ArithRaw::new(a);
    let b = ArithRaw::new(b);
    let c = ArithRaw::new(c);
    arithmetic_from_raw(addr, a, b, c)
}

impl DataModelField {
    pub fn new<T: ToPrimitive>(v: T) -> Option<DataModelField> {
        match v.to_u64() {
            Some(0) => Some(DataModelNumber),
            Some(1) => Some(DataModelLabel),
            Some(2) => Some(DataModelValue),
            _ => None
        }
    }
}

impl ToValue for DataModelField {
    fn as_value(&self) -> Value {
        match self {
            &DataModelNumber => 0,
            &DataModelLabel => 1,
            &DataModelValue => 2,
        }.as_value()
    }
}

impl Instruction {
    pub fn from_bscode(inst: &bscode::Instruction) -> Instruction {
        let opcode = inst.get(0).as_i16() as int;
        let (a, b, c) = (*inst.get(1), *inst.get(2), *inst.get(3));
        let all_args_zero = a.is_zero() && b.is_zero() && c.is_zero();
        let give_up = Unrecognised(opcode.as_value(), a, b, c);
        match opcode {
            1..2000 => {
                let p1 = Position::from_packed(a);
                let r = b;
                let p2 = Position::from_packed(c);
                ShowPrompt(CellAddress::new(opcode), p1, r, p2)
            },
            2999 if a.is_zero() && b.is_zero() => NewPage(c),
            3000|3001|3100|3101 => {
                let ocomp = Comparison::from_bscode(a, b, c);
                if all_args_zero && (opcode==3001 || opcode==3101) {
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
                    } else if opcode == 3100 {
                        ReverseSimpleConditional(ocomp.unwrap())
                    } else {
                        ReverseBlockConditional(ocomp.unwrap())
                    }
                } else {
                    give_up
                }
            },
            8000 => {
                let clrpair = a;
                let p1 = Position::from_packed(b);
                let p2 = Position::from_packed(c);
                Window(clrpair, p1, p2)
            },
            8400 if all_args_zero => SaveAddress,
            8500 if a.is_zero() && c.is_zero() => {
                match b.as_i16() {
                    0..500 => GotoPage(b),
                    501..520 => GotoFKey(b - 500),
                    1001..3000 => GotoPage(b - 1000),
                    4001..4024 => GotoTransaction(b - 4000),
                    4080 => GotoSpecial(ProductSales),
                    4077 => GotoSpecial(MainMenu),
                    4069 => GotoSpecial(ExitSystem),
                    4083 => GotoSpecial(Storage),
                    4084 => GotoSpecial(MultitaskMenu),
                    _ => give_up,
                }
            },
            8560 => SystemCall(CellAddress::new(c)),
            8650 => SetVideo(c),
            8700 if all_args_zero => AutoSolve,
            9001 if all_args_zero => AutoSave,
            9200|9201|9300|9301|9400|9401 => {
                let flag = opcode%10 == 1;
                match (DataModelField::new(a), CellAddress::from_value(b), CellAddress::from_value(c)) {
                    (Some(field), addr1, addr2) => match opcode {
                        9200|2301 => DataRun(flag, field, addr1, addr2),
                        9300|9301 => DataPut(flag, field, addr1, addr2),
                        9400|9401 => DataGet(flag, field, addr1, addr2),
                        _ => give_up,
                    },
                    _ => give_up,
                }
            },
            10000..11999 => match arithmetic_from_bscode(opcode.as_value(), a, b, c) {
                Some(a) => a,
                None => give_up,
            },
            30001..32000 => {
                let p = CellAddress::new(opcode - 30000);
                let c = c.as_i16();
                let op = c % 10;
                let flag = op % 2 == 1;
                let y = b;
                let z = Value::new(c / 10);
                match AddressOrValue::from_value(a) {
                    Some(x) => match op {
                        1|2  => Table(flag, TableSearch, p, x, y, z),
                        3|4  => Table(flag, TableSearchRange, p, x, y, z),
                        5|6  => Table(flag, TableSearchRangePairs, p, x, y, z),
                        7|8  => Table(flag, TableTransfer, p, x, y, z),
                        9|10 => Table(flag, TableTransferReverse, p, x, y, z),
                        _ => give_up,
                    },
                    None => give_up,
                }
            },
            _ => give_up,
        }
    }

    pub fn as_bscode(&self) -> bscode::Instruction {
        match self {
            &ShowPrompt(addr, p1, r, p2) => bscode::Instruction::new(addr.as_value(), p1.as_packed(), r, p2.as_packed()),
            &NewPage(v) => bscode::Instruction::new(2999,0,0,v),
            &BlockEnd => bscode::Instruction::new(3001,0,0,0),
            &ReverseBlockEnd => bscode::Instruction::new(3101,0,0,0),
            &Window(clr, p1, p2) => bscode::Instruction::new(8000, clr, p1.as_packed(), p2.as_packed()),
            &SaveAddress => bscode::Instruction::new(8400,0,0,0),
            &GotoPage(n) => bscode::Instruction::new(8500,0,n,0),
            &GotoFKey(f) => bscode::Instruction::new(8500,0,f+500,0),
            &GotoPrompt(addr) => bscode::Instruction::new(8500,0,addr.as_value()+1000,0),
            &GotoTransaction(t) => bscode::Instruction::new(8500,0,t+4000,0),
            &GotoSpecial(MainMenu) => bscode::Instruction::new(8500,0,(4000+'M' as int).as_value(),0),
            &GotoSpecial(ProductSales) => bscode::Instruction::new(8500,0,(4000+'P' as int).as_value(),0),
            &GotoSpecial(ExitSystem) => bscode::Instruction::new(8500,0,(4000+'E' as int).as_value(),0),
            &GotoSpecial(Storage) => bscode::Instruction::new(8500,0,(4000+'S' as int).as_value(),0),
            &GotoSpecial(MultitaskMenu) => bscode::Instruction::new(8500,0,(4000+'T'as int).as_value(),0),
            &SystemCall(addr) => bscode::Instruction::new(8560,0,0,addr.as_value()),
            &SetVideo(mode) => bscode::Instruction::new(8650,0,0,mode),
            &AutoSolve => bscode::Instruction::new(8700,0,0,0),
            &AutoSave => bscode::Instruction::new(9001,0,0,0),
            &DataRun(flag,field,addr1,addr2) => bscode::Instruction::new(flag.as_value()+9200, field.as_value(), addr1.as_value(), addr2.as_value()),
            &DataPut(flag,field,addr1,addr2) => bscode::Instruction::new(flag.as_value()+9300, field.as_value(), addr1.as_value(), addr2.as_value()),
            &DataGet(flag,field,addr1,addr2) => bscode::Instruction::new(flag.as_value()+9400, field.as_value(), addr1.as_value(), addr2.as_value()),
            &Arithmetic(addr, t1, t2, t3) => bscode::Instruction::new(addr+10000,t1,t2,t3),
            &Clear(dest, addr, t1, t2) => bscode::Instruction::new(dest+10000, ArithRaw::combine(5, EAddress(addr)), t1, t2),
            &ArithmeticSet(dest, av1, t2, t3) => bscode::Instruction::new(dest+10000, ArithRaw::combine(2,av1),t2,t3),
            &ArithmeticSetNeg(dest, av1, t2, t3) => bscode::Instruction::new(dest+10000, ArithRaw::combine(1,av1),t2,t3),
            &ArithmeticLen(dest, av1, t2, t3) => bscode::Instruction::new(dest+10000, ArithRaw::combine(0,av1),t2,t3),
            &ArithmeticSubstr(dest, addr, av1, av2) => bscode::Instruction::new(dest+10000, ArithRaw::combine(6, EAddress(addr)), ArithRaw::combine(5, av1), ArithRaw::combine(2, av2)),
            &GetSystemTime(dest, addr, t1) => bscode::Instruction::new(dest+10000, ArithRaw::combine(6, EAddress(addr)), ArithRaw::combine(6, EValue(Zero::zero())), t1),
            &Date365(dest, av1, av2) => bscode::Instruction::new(dest+10000, ArithRaw::combine(9, av1), ArithRaw::combine(1, av2), ArithTerm::null()),
            &Date360(dest, av1, av2) => bscode::Instruction::new(dest+10000, ArithRaw::combine(9, av1), ArithRaw::combine(2, av2), ArithTerm::null()),
            &DateDiff(dest, av1, av2) => bscode::Instruction::new(dest+10000, ArithRaw::combine(9, av1), ArithRaw::combine(3, av2), ArithTerm::null()),
            &DateFoo(dest, av1, av2) => bscode::Instruction::new(dest+10000, ArithRaw::combine(9, av1), ArithRaw::combine(4, av2), ArithTerm::null()),
            &Table(flag,TableSearch,p,x,y,z) => bscode::Instruction::new(p.as_value()+30000, x.as_value(), y, z*10+2-flag.as_value()),
            &Table(flag,TableSearchRange,p,x,y,z) => bscode::Instruction::new(p.as_value()+30000, x.as_value(), y, z*10+4-flag.as_value()),
            &Table(flag,TableSearchRangePairs,p,x,y,z) => bscode::Instruction::new(p.as_value()+30000, x.as_value(), y, z*10+6-flag.as_value()),
            &Table(flag,TableTransfer,p,x,y,z) => bscode::Instruction::new(p.as_value()+30000, x.as_value(), y, z*10+8-flag.as_value()),
            &Table(flag,TableTransferReverse,p,x,y,z) => bscode::Instruction::new(p.as_value()+30000, x.as_value(), y, z*10+10-flag.as_value()),
            &Unrecognised(a,b,c,d) => bscode::Instruction::new(a,b,c,d),
            x => match conditional_as_bscode(x) {
                Some(inst) => inst,
                None => {
                    bscode::Instruction::new(0,0,0,0)
                }
            },
        }
    }

    pub fn render<W: Writer>(&self, buffer: &mut BufferedWriter<W>) {
        buffer.write_str(self.to_str().as_slice()).unwrap()
    }

    pub fn parse_noarg(name: String) -> BancResult<Instruction> {
        match name.as_slice() {
            "SAVEADDR" => Ok(SaveAddress),
            "AUTOSOLVE" => Ok(AutoSolve),
            "AUTOSAVE" => Ok(AutoSave),
            "ENDCOND" => Ok(BlockEnd),
            "ENDRCOND" => Ok(ReverseBlockEnd),
            "EXIT" => Ok(GotoSpecial(ExitSystem)),
            "MAINMENU" => Ok(GotoSpecial(MainMenu)),
            "MTASKMENU" => Ok(GotoSpecial(MultitaskMenu)),
            "GOSTORAGE" => Ok(GotoSpecial(Storage)),
            "GOPRODUCT" => Ok(GotoSpecial(ProductSales)),
            _ => Err("not a valid nullary opcode"),
        }
    }

    pub fn parse_cond(name: String, comp: Comparison) -> BancResult<Instruction> {
        match name.as_slice() {
            "COND" => Ok(SimpleConditional(comp)),
            "RCOND" => Ok(ReverseSimpleConditional(comp)),
            "STARTCOND" => Ok(BlockConditional(comp)),
            "STARTRCOND" => Ok(ReverseBlockConditional(comp)),
            _ => Err("not a valid conditional opcode"),
        }
    }

    pub fn parse_raw(args: Vec<Argument>) -> BancResult<Instruction> {
        if args.len() != 4 {
            return Err("expected RAW statement to have exactly four arguments");
        }
        let v0 = args.get(0).as_maybe_value();
        let v1 = args.get(1).as_maybe_value();
        let v2 = args.get(2).as_maybe_value();
        let v3 = args.get(3).as_maybe_value();
        match (v0, v1, v2, v3) {
            (Some(v0), Some(v1), Some(v2), Some(v3)) => Ok(Unrecognised(v0, v1, v2, v3)),
            _ => Err("expected RAW <int>, <int>, <int>, <int>")
        }
    }

    pub fn parse_datamodel(name: String, arg1: &Argument, arg2: &Argument) -> BancResult<Instruction> {
        let addr1 = arg1.as_address();
        let addr2 = arg2.as_address();
        let name = name.as_slice();
        let field =
        if name.ends_with("N") {
            Some(DataModelNumber)
        } else if name.ends_with("L") {
            Some(DataModelLabel)
        } else if name.ends_with("V") {
            Some(DataModelValue)
        } else {
            None
        };
        let flag =
        match name.char_at(7) {
            '0' => Some(false),
            '1' => Some(true),
            _ => None,
        };
        match (flag, field, addr1, addr2) {
            (Some(flag), Some(field), addr1, addr2) =>
                if name.starts_with("DATARUN") {
                    Ok(DataRun(flag, field, addr1, addr2))
                } else if name.starts_with("DATAPUT") {
                    Ok(DataPut(flag, field, addr1, addr2))
                } else if name.starts_with("DATAGET") {
                    Ok(DataGet(flag, field, addr1, addr2))
                } else {
                    Err("expected DATA{RUN|PUT|GET}")
                },
            _ => Err("expected DATA{RUN|PUT|GET}{0|1}{N|L|V} [<cell>[, <cell>]]")
        }
    }

    pub fn parse_table(name: String, p: &Argument, x: &Argument, y: &Argument, z: &Argument) -> BancResult<Instruction> {
        let p = p.as_address();
        let x = x.as_address_or_value();
        let y = y.as_maybe_value();
        let z = z.as_maybe_value();
        let name = name.as_slice();
        let flag =
        if name.ends_with("D") {
            Some(false)
        } else if name.ends_with("V") {
            Some(true)
        } else {
            None
        };

        let mode =
        if name.starts_with("TABLESEARCH") {
            Some(TableSearch)
        } else if name.starts_with("TABLERANGE") {
            Some(TableSearchRange)
        } else if name.starts_with("TABLERANGEP") {
            Some(TableSearchRangePairs)
        } else if name.starts_with("TABLEXFER") {
            Some(TableTransfer)
        } else if name.starts_with("TABLERXFER") {
            Some(TableTransferReverse)
        } else {
            None
        };

        match (flag, mode, p, x, y, z) {
            (Some(flag), Some(mode), Some(p), Some(x), Some(y), Some(z)) => Ok(Table(flag, mode, p, x, y, z)),
            _ => Err("expected TABLE{SEARCH|RANGE|RANGEP|XFER|RXFER}{D|V} <cell>, {<int>|<cell>}, <int>, <int>"),
        }
    }

    pub fn parse_general(name: String, args: Vec<Argument>) -> BancResult<Instruction> {
        if args.len() != 4 {
            return Err("wrong number of arguments");
        }
        match name.as_slice() {
            "SHOW" => {
                let addr = args.get(0).as_address();
                let p1 = args.get(1).as_packed_position();
                let r = args.get(2).as_maybe_value();
                let p2 = args.get(3).as_packed_position();
                match (addr, p1, r, p2) {
                    (Some(addr), Some(p1), Some(r), Some(p2)) => Ok(ShowPrompt(addr, p1, r, p2)),
                    _ => Err("expected SHOW <cell>, <packedpos>, <int>, <packedpos>"),
                }
            },
            "NEWPAGE" => {
                match args.get(0).as_maybe_value() {
                    Some(v) => Ok(NewPage(v)),
                    _ => Err("expected NEWPAGE [<int>]"),
                }
            },
            "SET" => {
                let addr = args.get(0).as_address();
                let av1 = args.get(1).as_address_or_value();
                let t1 = args.get(1).as_arithterm();
                let t2 = args.get(2).as_arithterm();
                let t3 = args.get(3).as_arithterm();
                match (addr, av1, t1, t2, t3) {
                    (Some(addr), Some(av1), None, Some(t2), Some(t3)) => Ok(ArithmeticSet(addr, av1, t2, t3)),
                    (Some(addr), None, Some(ArithCell(Subtract, a1)), Some(t2), Some(t3)) => Ok(ArithmeticSetNeg(addr, EAddress(a1), t2, t3)),
                    (Some(addr), None, Some(ArithImmediate(Subtract, v1)), Some(t2), Some(t3)) => Ok(ArithmeticSetNeg(addr, EValue(v1), t2, t3)),
                    (Some(addr), None, Some(t1), Some(t2), Some(t3)) => Ok(Arithmetic(addr, t1, t2, t3)),
                    _ => Err("expected SET <cell>, {<cell>|<int>|<arithterm>}[, <arithterm>[, <arithterm>]]")
                }
            },
            "WINDOW" => {
                let clr = args.get(0).as_maybe_value();
                let p1 = args.get(1).as_packed_position();
                let p2 = args.get(2).as_packed_position();
                match (clr, p1, p2) {
                    (Some(clr), Some(p1), Some(p2)) => Ok(Window(clr, p1, p2)),
                    _ => Err("expected WINDOW <clrpair>, <packedpos>, <packedpos>")
                }
            },
            "GOTO" => {
                // can be either an address or a value
                let addr = args.get(0).as_address();
                let num = args.get(0).as_maybe_value();
                match (addr, num) {
                    (Some(addr), None) => Ok(GotoPrompt(addr)),
                    (None, Some(v)) => match v.as_i16() {
                        0..500 => Ok(GotoPage(v)),
                        _ => Err("expected GOTO argument in range [0, 500]")
                    },
                    _ => Err("expected GOTO {<int>|<cell>}")
                }
            },
            "GOTOF" => {
                match args.get(0).as_maybe_value() {
                    Some(v) => match v.as_i16() {
                        1..20 => Ok(GotoFKey(v)),
                        _ => Err("expected GOTOF argument in range [1, 20]")
                    },
                    _ => Err("expected GOTOF <int>")
                }
            },
            "GOTOTR" => {
                match args.get(0).as_maybe_value() {
                    Some(v) => match v.as_i16() {
                        1..24 => Ok(GotoTransaction(v)),
                        _ => Err("expected GOTOTR argument in range [1, 24]")
                    },
                    _ => Err("expected GOTOTR <int>")
                }
            },
            "SYSCALL" => {
                match args.get(0).as_address() {
                    Some(addr) => Ok(SystemCall(addr)),
                    _ => Err("expected SYSCALL <cell>")
                }
            },
            "VIDEOMODE" => {
                match args.get(0).as_maybe_value() {
                    Some(m) => Ok(SetVideo(m)),
                    _ => Err("expected VIDEO <int>")
                }
            },
            "SUBSTR" => {
                let dest = args.get(0).as_address();
                let addr = args.get(1).as_address();
                let av2 = args.get(2).as_address_or_value();
                let av3 = args.get(3).as_address_or_value();
                match (dest, addr, av2, av3) {
                    (Some(dest), Some(addr), Some(av2), Some(av3)) => Ok(ArithmeticSubstr(dest, addr, av2, av3)),
                    _ => Err("expected SUBSTR")
                }
            },
            "LEN" => {
                let dest = args.get(0).as_address();
                let av1 = args.get(1).as_address_or_value();
                let t2 = args.get(2).as_arithterm();
                let t3 = args.get(3).as_arithterm();
                match (dest, av1, t2, t3) {
                    (Some(dest), Some(av1), Some(t2), Some(t3)) => Ok(ArithmeticLen(dest, av1, t2, t3)),
                    _ => Err("expected LEN <dest>, {<cell>|<int>}[, <arithterm>[, <arithterm>]]")
                }
            },
            "CLEAR" => {
                let dest = args.get(0).as_address();
                let a = args.get(1).as_address();
                let t2 = args.get(2).as_arithterm();
                let t3 = args.get(3).as_arithterm();
                match (dest, a, t2, t3) {
                    (Some(dest), Some(a), Some(t2), Some(t3)) => Ok(Clear(dest, a, t2, t3)),
                    _ => Err("expected CLEAR <dest>, <cell>[, <arithterm>[, <arithterm>]]")
                }
            },
            "TIME" => {
                let dest = args.get(0).as_address();
                let a = args.get(1).as_address();
                let t2 = args.get(2).as_arithterm();
                match (dest, a, t2) {
                    (Some(dest), Some(a), Some(t2)) => Ok(GetSystemTime(dest, a, t2)),
                    _ => Err("expected TIME <dest>, <cell>[, <arithterm>]")
                }
            },
            "DATE360"|"DATE365"|"DATEDIFF"|"DATEFOO" => {
                let dest = args.get(0).as_address();
                let a1 = args.get(1).as_address_or_value();
                let a2 = args.get(2).as_address_or_value();
                match (dest, a1, a2) {
                    (Some(dest), Some(a1), Some(a2)) => match name.as_slice() {
                        "DATE365"  => Ok(Date365(dest, a1, a2)),
                        "DATE360"  => Ok(Date360(dest, a1, a2)),
                        "DATEDIFF" => Ok(DateDiff(dest, a1, a2)),
                        "DATEFOO"  => Ok(DateFoo(dest, a1, a2)),
                        _ => Err("unexpected DATE* command")
                    },
                    _ => Err("expected DATE* <dest>, <a>, <b>")
                }
            },
            _ => {
                if name.as_slice().starts_with("DATA") {
                    Instruction::parse_datamodel(name, args.get(0), args.get(1))
                } else if name.as_slice().starts_with("TABLE") {
                    Instruction::parse_table(name, args.get(0), args.get(1), args.get(2), args.get(3))
                } else {
                    let count = args.iter().count(|a| a != &ArgEmpty);
                    if count == 0 {
                        Instruction::parse_noarg(name)
                    } else {
                        Err("not a valid opcode")
                    }
                }
            },
        }
    }
}

fn consume_newline<R: Reader>(tokenizer: &mut Tokenizer<R>) -> BancResult<()> {
    match tokenizer.get_token() {
        Some(Newline) => Ok(()),
        _ => Err("missing end-of-line"),
    }
}

impl Argument {
    pub fn parse<R: Reader>(init_token: Option<Token>, tokenizer: &mut Tokenizer<R>) -> BancResult<Argument> {
        let tok = match init_token {
            Some(tk) => Some(tk),
            None => tokenizer.next(),
        };
        if tok.is_none() {
            return Err("read error");
        }

        let tok = tok.unwrap();
        match tok {
            Name(_) => ArithTerm::parse(tok, tokenizer).map(|x| ArgArith(x) ),
            a => Expression::parse(Some(a), tokenizer).map(|x| ArgExpr(x) ),
        }
    }

    fn as_arithterm(&self) -> Option<ArithTerm> {
        match self {
            &ArgEmpty => Some(ArithEmpty),
            &ArgArith(at) => Some(at),
            _ => None,
        }
    }

    fn as_packed_position(&self) -> Option<Position> {
        match self {
            &ArgExpr(Immediate(v)) => Some(Position::from_packed(v)),
            &ArgEmpty => Some(Zero::zero()),
            _ => None,
        }
    }

    fn as_address_or_value(&self) -> Option<AddressOrValue> {
        match self {
            &ArgExpr(Cell(addr)) => Some(EAddress(addr)),
            &ArgExpr(Immediate(v)) => Some(EValue(v)),
            &ArgEmpty => Some(Zero::zero()),
            _ => None,
        }
    }

    fn as_maybe_value(&self) -> Option<Value> {
        match self {
            &ArgExpr(Immediate(v)) => Some(v),
            &ArgEmpty => Some(Zero::zero()),
            _ => None,
        }
    }

    fn as_address(&self) -> Option<CellAddress> {
        match self {
            &ArgExpr(Cell(addr)) => Some(addr),
            _ => None,
        }
    }
}

fn parse_argument<R: Reader>(tokenizer: &mut Tokenizer<R>) -> BancResult<Option<Argument>> {
    let tok = tokenizer.get_token();
    if tok.is_none() {
        return Ok(None);
    }
    let tok = tok.unwrap();
    match tok.clone() {
        Comma => {
            Ok(Some(ArgEmpty))
        },
        Newline => {
            Ok(None)
        },
        tk => {
            let result = match Argument::parse(Some(tk), tokenizer) {
                Ok(arg) => Ok(Some(arg)),
                Err(e) => Err(e),
            };
            let nexttok = tokenizer.get_token();
            match nexttok {
                Some(Comma) | None => {},
                Some(Newline) => {
                    tokenizer.unget_token(Newline);
                },
                _ => {
                    return Err("expected comma/newline/eof after expression");
                },
            }
            result
        }
    }
}

fn parse_arguments<R: Reader>(tokenizer: &mut Tokenizer<R>) -> BancResult<Vec<Argument>> {
    let mut args = vec!();
    loop {
        match parse_argument(tokenizer) {
            Ok(Some(arg)) => args.push(arg),
            Ok(None) => { break; }
            Err(e) => { return Err(e); }
        }
    }
    if args.len() > 4 {
        return Err("too many arguments");
    }
    while args.len() < 4 {
        args.push(ArgEmpty);
    }
    Ok(args)
}

impl TreeNode for Instruction {
    fn parse<R: Reader>(tokenizer: &mut Tokenizer<R>) -> BancResult<Option<Instruction>> {
        let token = tokenizer.next();
        if token.is_none() {
            return Ok(None);
        }
        let rinst = match token.unwrap() {
            Name(name) => match name.as_slice() {
                "RCOND" | "COND" | "STARTCOND" | "STARTRCOND" => {
                    match Comparison::parse(tokenizer) {
                        Ok(comp) => {
                            match consume_newline(tokenizer) {
                                Ok(_) => Instruction::parse_cond(name, comp),
                                Err(e) => Err(e),
                            }
                        },
                        Err(e) => Err(e),
                    }
                },
                "RAW" => {
                    match parse_arguments(tokenizer) {
                        Ok(args) => Instruction::parse_raw(args),
                        Err(e) => Err(e),
                    }
                },
                _ => {
                    match parse_arguments(tokenizer) {
                        Ok(args) => Instruction::parse_general(name, args),
                        Err(e) => Err(e),
                    }
                },
            },
            _ => {
                Err("unexpected name as first token on line")
            }
        };
        return match rinst {
            Ok(a) => Ok(Some(a)),
            Err(e) => Err(e),
        };
    }
}

#[allow(unused_must_use)]
fn render_common_arith<T: Show>(name: &'static str, addr: CellAddress, arg: T, term1: ArithTerm, term2: ArithTerm, formatter: &mut Formatter) -> Result<(), FormatError> {
    name.fmt(formatter);
    formatter.write_char(' ');
    addr.fmt(formatter);
    ", ".fmt(formatter);
    let mut result = arg.fmt(formatter);
    if term1.is_null() && term2.is_null() {
        return result;
    }
    result = ", ".fmt(formatter);
    if ! term1.is_null() {
        result = term1.fmt(formatter);
    }
    if ! term2.is_null() {
        ", ".fmt(formatter);
        result = term2.fmt(formatter);
    }
    return result;
}

#[allow(unused_must_use)]
fn render_arith_terms(t1: ArithTerm, t2: ArithTerm, t3: ArithTerm, formatter: &mut Formatter) -> Result<(), FormatError> {
    let p2 = t2.effective();
    let p3 = t3.effective();

    formatter.write_str(", ");
    t1.fmt(formatter);
    if p2 || p3 {
        formatter.write_str(", ");
        t2.fmt(formatter);
        if p3 {
            formatter.write_str(", ");
            t3.fmt(formatter);
        }
    }
    Ok(())
}

#[allow(unused_must_use)]
fn format_data_model_opcode(name: &'static str, flag: bool, field: DataModelField, addr1: Option<CellAddress>, addr2: Option<CellAddress>, formatter: &mut Formatter) -> Result<(), FormatError> {
    "DATA".fmt(formatter);
    name.fmt(formatter);
    flag.as_value().fmt(formatter);
    let status = match field {
        DataModelNumber => "N",
        DataModelLabel => "L",
        DataModelValue => "V",
    }.fmt(formatter);

    match (addr1, addr2) {
        (None, None) => status,
        (Some(addr1), Some(addr2)) => {
            formatter.write_char(' ');
            addr1.fmt(formatter);
            ", ".fmt(formatter);
            addr2.fmt(formatter)
        },
        (None, Some(addr2)) => {
            formatter.write_char(' ');
            "0, ".fmt(formatter);
            addr2.fmt(formatter)
        },
        (Some(addr1), None) => {
            formatter.write_char(' ');
            addr1.fmt(formatter)
        },
    }
}

#[allow(unused_must_use)]
fn format_table_opcode(name: &'static str, flag: bool, p: CellAddress, x: AddressOrValue, y: Value, z: Value, formatter: &mut Formatter) -> Result<(), FormatError> {
    "TABLE".fmt(formatter);
    name.fmt(formatter);
    if flag {
        "V".fmt(formatter);
    } else {
        "D".fmt(formatter);
    }
    formatter.write_char(' ');
    p.fmt(formatter);
    ", ".fmt(formatter);
    x.fmt(formatter);
    ", ".fmt(formatter);
    y.fmt(formatter);
    ", ".fmt(formatter);
    z.fmt(formatter)
}

impl Show for Instruction {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        match self {
            &ShowPrompt(addr, p1, r, p2) => {
                "SHOW".fmt(formatter);
                formatter.write_char(' ');
                let mut status = addr.fmt(formatter);
                let (p1z, rz, p2z) = (p1.is_zero(), r.is_zero(), p2.is_zero());
                if p1z && rz && p2z {
                    return status;
                }
                formatter.write_str(", ");
                if ! p1z {
                    status = p1.fmt(formatter);
                }
                if r.is_zero() && p2.is_zero() {
                    return status;
                }
                formatter.write_str(", ");
                if !rz {
                    status = r.fmt(formatter);
                }
                if !p2z {
                    formatter.write_str(", ");
                    status = p2.fmt(formatter);
                }
                return status;
            },
            &Arithmetic(addr, t1, t2, t3) => {
                "SET".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter);
                render_arith_terms(t1, t2, t3, formatter)
            },
            &SetZero(addr) => {
                "SETZERO".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter)
            },
            &Clear(addr, srcaddr, t1, t2) => {
                render_common_arith("CLEAR", addr, srcaddr, t1, t2, formatter)
            },
            &ArithmeticLen(addr, arg, term1, term2) => {
                render_common_arith("LEN", addr, arg, term1, term2, formatter)
            },
            &ArithmeticSet(addr, arg, term1, term2) => {
                render_common_arith("SET", addr, arg, term1, term2, formatter)
            },
            &ArithmeticSetNeg(addr, arg, term1, term2) => {
                render_common_arith("SET", addr, ArithTerm::new(Subtract, arg), term1, term2, formatter)
            },
            &ArithmeticPow(addr, arg1, arg2, term) => {
                "POW".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter);
                ", ".fmt(formatter);
                arg1.fmt(formatter);
                ", ".fmt(formatter);
                let mut result = arg2.fmt(formatter);
                if term != ArithEmpty {
                    ", ".fmt(formatter);
                    result = term.fmt(formatter);
                }
                return result;
            },
            &ArithmeticSubstr(addr, arg1, arg2, arg3) => {
                "SUBSTR".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter);
                ", ".fmt(formatter);
                arg1.fmt(formatter);
                ", ".fmt(formatter);
                arg2.fmt(formatter);
                ", ".fmt(formatter);
                arg3.fmt(formatter)
            },
            &GetSystemTime(addr, arg1, term) => {
                "TIME".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter);
                ", ".fmt(formatter);
                let mut result = arg1.fmt(formatter);
                if term != ArithEmpty {
                    ", ".fmt(formatter);
                    result = term.fmt(formatter);
                }
                return result;
            },
            &ArithmeticLog(addr, arg, term1, term2) => {
                render_common_arith("LOG", addr, arg, term1, term2, formatter)
            },
            &ArithmeticTrunc(addr, arg, term1, term2) => {
                render_common_arith("TRUNC", addr, arg, term1, term2, formatter)
            },
            &Date365(addr, arg1, arg2) => {
                "DATE365".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter);
                ", ".fmt(formatter);
                arg1.fmt(formatter);
                ", ".fmt(formatter);
                arg2.fmt(formatter)
            },
            &Date360(addr, arg1, arg2) => {
                "DATE360".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter);
                ", ".fmt(formatter);
                arg1.fmt(formatter);
                ", ".fmt(formatter);
                arg2.fmt(formatter)
            },
            &DateDiff(addr, arg1, arg2) => {
                "DATEDIFF".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter);
                ", ".fmt(formatter);
                arg1.fmt(formatter);
                ", ".fmt(formatter);
                arg2.fmt(formatter)
            },
            &DateFoo(addr, arg1, arg2) => {
                "DATEFOO".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter);
                ", ".fmt(formatter);
                arg1.fmt(formatter);
                ", ".fmt(formatter);
                arg2.fmt(formatter)
            },
            &NewPage(v) => {
                let mut status = "NEWPAGE".fmt(formatter);
                if ! v.is_zero() {
                    formatter.write_char(' ');
                    status = v.fmt(formatter);
                }
                return status;
            },
            &Window(clr, p1, p2) => {
                "WINDOW".fmt(formatter);
                formatter.write_char(' ');
                clr.fmt(formatter);
                formatter.write_str(", ");
                p1.fmt(formatter);
                formatter.write_str(", ");
                p2.fmt(formatter)
            },
            &SaveAddress => {
                "SAVEADDR".fmt(formatter)
            },
            &SetVideo(mode) => {
                "VIDEOMODE".fmt(formatter);
                formatter.write_char(' ');
                mode.fmt(formatter)
            },
            &AutoSolve => {
                "AUTOSOLVE".fmt(formatter)
            },
            &AutoSave => {
                "AUTOSAVE".fmt(formatter)
            },
            &SimpleConditional(cmp) => {
                "COND".fmt(formatter);
                formatter.write_char(' ');
                cmp.fmt(formatter)
            },
            &ReverseSimpleConditional(cmp) => {
                "RCOND".fmt(formatter);
                formatter.write_char(' ');
                cmp.fmt(formatter)
            },
            &BlockConditional(cmp) => {
                "STARTCOND".fmt(formatter);
                formatter.write_char(' ');
                cmp.fmt(formatter);
                Ok(())
            },
            &BlockEnd => {
                "ENDCOND".fmt(formatter)
            },
            &ReverseBlockConditional(cmp) => {
                "STARTRCOND".fmt(formatter);
                formatter.write_char(' ');
                cmp.fmt(formatter)
            },
            &ReverseBlockEnd => {
                "ENDRCOND".fmt(formatter)
            },
            &GotoPage(v) => {
                "GOTO".fmt(formatter);
                formatter.write_char(' ');
                v.fmt(formatter)
            },
            &GotoFKey(v) => {
                "GOTOF".fmt(formatter);
                formatter.write_char(' ');
                v.fmt(formatter)
            },
            &GotoPrompt(addr) => {
                "GOTO".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter)
            },
            &GotoTransaction(v) => {
                "GOTOTR".fmt(formatter);
                formatter.write_char(' ');
                v.fmt(formatter)
            },
            &GotoSpecial(MainMenu) => "MAINMENU".fmt(formatter),
            &GotoSpecial(MultitaskMenu) => "MTASKMENU".fmt(formatter),
            &GotoSpecial(ProductSales) => "GOPRODUCT".fmt(formatter),
            &GotoSpecial(Storage) => "GOSTORAGE".fmt(formatter),
            &GotoSpecial(ExitSystem) => "EXIT".fmt(formatter),
            &SystemCall(addr) => {
                "SYSCALL".fmt(formatter);
                formatter.write_char(' ');
                addr.fmt(formatter)
            },
            &DataRun(flag,field,addr1,addr2) => format_data_model_opcode("RUN", flag, field, addr1, addr2, formatter),
            &DataPut(flag,field,addr1,addr2) => format_data_model_opcode("PUT", flag, field, addr1, addr2, formatter),
            &DataGet(flag,field,addr1,addr2) => format_data_model_opcode("GET", flag, field, addr1, addr2, formatter),
            &Table(flag,TableSearch,p,x,y,z)           => format_table_opcode("SEARCH", flag, p, x, y, z, formatter),
            &Table(flag,TableSearchRange,p,x,y,z)      => format_table_opcode("RANGE",  flag, p, x, y, z, formatter),
            &Table(flag,TableSearchRangePairs,p,x,y,z) => format_table_opcode("RANGEP", flag, p, x, y, z, formatter),
            &Table(flag,TableTransfer,p,x,y,z)         => format_table_opcode("XFER",   flag, p, x, y, z, formatter),
            &Table(flag,TableTransferReverse,p,x,y,z)  => format_table_opcode("RXFER",  flag, p, x, y, z, formatter),
            &Unrecognised(v0, v1, v2, v3) => {
                "RAW".fmt(formatter);
                formatter.write_char(' ');
                v0.fmt(formatter);
                formatter.write_str(", ");
                v1.fmt(formatter);
                formatter.write_str(", ");
                v2.fmt(formatter);
                formatter.write_str(", ");
                v3.fmt(formatter)
            },
        }
    }
}

#[test]
fn parse_newpage() {
    let bsinst = bscode::Instruction::new(2999,0,0,1);
    let inst = Instruction::from_bscode(&bsinst);
    assert_eq!(inst, NewPage(One::one()));
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
fn renderbs_newpage_inst() {
    let inst = NewPage(One::one());
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(2999,0,0,1));
}
#[test]
fn renderbs_simple_cond_unary() {
    let inst = SimpleConditional(UnaryComparison(Immediate(42.as_value()), IsNull));
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(3000,10042,1,0));
}
#[test]
fn renderbs_simple_cond_binary() {
    let inst = SimpleConditional(BinaryComparison(Immediate(42.as_value()), GreaterOrEqual, Cell(CellAddress::new(69))));
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(3000,10042,4,69));
}
#[test]
fn renderbs_block_end() {
    let inst = BlockEnd;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(3001,0,0,0));
}
#[test]
fn renderbs_reverse_block_end() {
    let inst = ReverseBlockEnd;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(3101,0,0,0));
}
#[test]
fn renderbs_save_address() {
    let inst = SaveAddress;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(8400,0,0,0));
}
#[test]
fn renderbs_goto_page() {
    let inst = GotoPage(42.as_value());
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(8500,0,42,0));
}
#[test]
fn renderbs_autosave() {
    let inst = AutoSave;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(9001,0,0,0));
}
#[test]
fn renderbs_arith_substr() {
    let inst = Arithmetic(CellAddress::new(350), ArithImmediate(Add, 42.as_value()), ArithCell(Multiply, CellAddress::new(269)), ArithImmediate(Logarithm, 53.as_value()));
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(10350,22422,2693,22537));
}

#[test]
fn parse_expr_imm_char() {
    let strform = "'@'";
    let expr = Expression::parse_string(strform).unwrap();
    assert_eq!(expr, ImmChar('@'));
}

#[test]
fn parse_expr_imm_int() {
    let strform = "12345";
    let expr = Expression::parse_string(strform).unwrap();
    assert_eq!(expr, Immediate(Value::new(12345)));
}

#[test]
fn parse_expr_imm_int2() {
    let strform = "-12345";
    let expr = Expression::parse_string(strform).unwrap();
    assert_eq!(expr, Immediate(Value::new(-12345)));
}

#[test]
fn parse_expr_imm_int3() {
    let strform = "1234b";
    let expr = Expression::parse_string(strform).unwrap();
    assert_eq!(expr, Immediate(Value::new(1234)));
}

#[test]
fn parse_expr_cell_addr() {
    let strform = "@1234";
    let expr = Expression::parse_string(strform).unwrap();
    assert_eq!(expr, Cell(CellAddress::new(1234)));
}

#[test]
fn parse_expr_cell_bad_addr() {
    let strform = "@'_'";
    assert!(Expression::parse_string(strform).is_err());
}

#[test]
fn parse_expr_cell_bad_addr2() {
    let strform = "@2001";
    assert!(Expression::parse_string(strform).is_err());
}

#[test]
fn parse_expr_nothing() {
    let strform = "Nothing";
    let expr = Expression::parse_string(strform).unwrap();
    assert_eq!(expr, Nothing);
}

#[test]
fn render_arith() {
    let inst = Arithmetic(CellAddress::new(350), ArithImmediate(Add, 42.as_value()), ArithCell(Multiply, CellAddress::new(269)), ArithImmediate(Logarithm, 53.as_value()));
    let expected_form = "SET @350, ADD(42), MUL(@269), LOG(53)";
    let rendered_form = inst.to_str();
    assert_eq!(rendered_form.as_slice(), expected_form);
}

#[test]
fn disassemble_line() {
    let strform = "10350,22422,2693,22531";
    let bstree = Tree::<bscode::Instruction>::parse_string(strform).unwrap();
    assert_eq!(bstree.len(), 1);
    let inst = Instruction::from_bscode(bstree.get(0));
    let disassembled = inst.to_str();
    assert_eq!("SET @350, 42, MUL(@269), SUB(53)", disassembled.as_slice());
}
