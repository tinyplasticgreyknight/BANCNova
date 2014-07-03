use std::io::{BufferedWriter};
use std::fmt::{Show, Formatter, FormatError};
use syntax::bscode;
use syntax::bscode::{Value, CellAddress, ToValue};
use result::BancResult;
use syntax::tokenize;
use syntax::tokenize::{Tokenizer, Token, Name, IntegerLiteral, Comma, Newline, OpenBracket, CloseBracket, OperToken};
use syntax::tree::{TreeNode};
#[cfg(test)]
use syntax::tree::{Tree};
use std::num::Zero;

#[deriving(PartialEq,Eq)]
pub enum Expression {
    Immediate(Value),
    ImmChar(char),
    Cell(CellAddress),
    Nothing,
}
#[deriving(PartialEq,Eq)]
pub enum UnaryComparator {
    IsNull,
    IsNotNull,
}
#[deriving(PartialEq,Eq)]
pub enum BinaryComparator {
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
#[deriving(PartialEq,Eq)]
pub enum ArithTerm {
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

#[deriving(PartialEq,Eq,Show)]
pub enum GotoSpecialTarget {
    ProductSales,
    MainMenu,
    ExitSystem,
    Storage,
    MultitaskMenu,
}

#[deriving(PartialEq,Eq)]
pub enum Instruction {
    Unrecognised(Value, Value, Value, Value),
    NewPage,
    SimpleConditional(Comparison),
    BlockConditional(Comparison),
    BlockEnd,
    ReverseSimpleConditional(Comparison),
    ReverseBlockConditional(Comparison),
    ReverseBlockEnd,
    SaveAddress,
    GotoPage(Value),
    GotoPrompt(CellAddress),
    GotoFKey(Value),
    GotoTransaction(Value),
    GotoSpecial(GotoSpecialTarget),
    SystemCall(CellAddress),
    AutoSolve,
    AutoSave,
    Arithmetic(CellAddress, ArithTerm, ArithTerm, ArithTerm),
    ShowPrompt(CellAddress, Position, Value, Position),
    Window(Value, Position, Position),
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
                match tokenizer.next() {
                    Some(tokenize::IntegerLiteral(a)) => {
                        CellAddress::parse(a).map(|a| { Cell(a) })
                    },
                    _ => Err("expected integer after address-sign")
                }
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

    fn parse<R: Reader>(init_token: Token, tokenizer: &mut Tokenizer<R>) -> BancResult<ArithTerm> {
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

    fn effective(&self) -> bool {
        match self {
            &ArithCell(_, _) => true,
            &ArithImmediate(Add, v) if v.as_i16() == 0 => false,
            &ArithImmediate(Subtract, v) if v.as_i16() == 0 => false,
            &ArithImmediate(Multiply, v) if v.as_i16() == 1 => false,
            &ArithImmediate(Divide, v) if v.as_i16() == 1 => false,
            _ => true,
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

impl Show for ArithTerm {
    #[allow(unused_must_use)]
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), FormatError> {
        match self {
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
            1..2000 => {
                let p1 = Position::from_packed(a);
                let r = b;
                let p2 = Position::from_packed(c);
                ShowPrompt(CellAddress::new(opcode), p1, r, p2)
            },
            2999 if all_args_zero => NewPage,
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
                    Unrecognised(opcode.as_value(), a, b, c)
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
                    _ => Unrecognised(opcode.as_value(), a, b, c),
                }
            },
            8560 => SystemCall(CellAddress::new(c)),
            8700 if all_args_zero => AutoSolve,
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
            &ShowPrompt(addr, p1, r, p2) => bscode::Instruction::new(addr.as_value(), p1.as_packed(), r, p2.as_packed()),
            &NewPage => bscode::Instruction::new(2999,0,0,0),
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
            &AutoSolve => bscode::Instruction::new(8700,0,0,0),
            &AutoSave => bscode::Instruction::new(9001,0,0,0),
            &Arithmetic(addr, t1, t2, t3) => bscode::Instruction::new(addr+10000,t1,t2,t3),
            &Unrecognised(a,b,c,d) => bscode::Instruction::new(a,b,c,d),
            x => conditional_as_bscode(x).unwrap(),
        }
    }

    pub fn render<W: Writer>(&self, buffer: &mut BufferedWriter<W>) {
        buffer.write_str(self.to_str().as_slice()).unwrap()
    }

    pub fn parse_noarg(name: String) -> BancResult<Instruction> {
        match name.as_slice() {
            "NEWPAGE" => Ok(NewPage),
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
            _ => Err("not a valid opcode"),
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
            return Err("RAW statement must have exactly four arguments");
        }
        let mut vargs: Vec<Value> = vec!();
        for expr in args.iter() {
            match expr {
                &ArgExpr(Immediate(v)) => vargs.push(v),
                _ => { return Err("RAW statement must use explicit numeric arguments"); },
            }
        }
        Ok(Unrecognised(
                *vargs.get(0),
                *vargs.get(1),
                *vargs.get(2),
                *vargs.get(3)
        ))
    }

    pub fn parse_general(name: String, args: Vec<Argument>) -> BancResult<Instruction> {
        if args.len() != 4 {
            return Err("wrong number of arguments");
        }
        let targs = (args.get(0), args.get(1), args.get(2), args.get(3));
        let vargs: Vec<Option<Value>> = args.iter().map(|a| match a {
                &ArgExpr(Immediate(v)) => Some(v),
                &ArgEmpty => Some(Zero::zero()),
                _ => None,
            }).collect();
        match name.as_slice() {
            "SHOW" => {
                let addr = match args.get(0) {
                    &ArgExpr(Cell(addr)) => addr,
                    _ => { return Err("SHOW must have an address for its first argument") },
                };
                let p1 = match args.get(1) {
                    &ArgExpr(Immediate(v)) => Position::from_packed(v),
                    &ArgEmpty => Zero::zero(),
                    _ => { return Err("SHOW must have a number for its second argument"); }
                };
                let r = match vargs.get(2) {
                    &Some(v) => v,
                    _ => { return Err("SHOW must have a number for its third argument") },
                };
                let p2 = match args.get(3) {
                    &ArgExpr(Immediate(v)) => Position::from_packed(v),
                    &ArgEmpty => Zero::zero(),
                    _ => { return Err("SHOW must have a number for its fourth argument"); }
                };
                Ok(ShowPrompt(addr, p1, r, p2))
            },
            "SET" => {
                let addr = match args.get(0) {
                    &ArgExpr(Cell(addr)) => addr,
                    _ => { return Err("SET must have an address for its first argument"); },
                };
                let mut arithargs: Vec<ArithTerm> = vec!();
                for arg in args.tail().iter() {
                    match arg {
                        &ArgEmpty => arithargs.push(ArithImmediate(Add, Zero::zero() )),
                        &ArgArith(at) => arithargs.push(at),
                        _ => { return Err("SET must have arithterms for arguments"); },
                    }
                }
                Ok(Arithmetic(addr, *arithargs.get(0), *arithargs.get(1), *arithargs.get(2)))
            },
            "WINDOW" => {
                let clr = match vargs.get(0) {
                    &Some(v) => v,
                    _ => { return Err("WINDOW must have a number for its first argument") },
                };
                let p1 = match args.get(1) {
                    &ArgExpr(Immediate(v)) => Position::from_packed(v),
                    &ArgEmpty => Zero::zero(),
                    _ => { return Err("SHOW must have a number for its second argument"); }
                };
                let p2 = match args.get(2) {
                    &ArgExpr(Immediate(v)) => Position::from_packed(v),
                    &ArgEmpty => Zero::zero(),
                    _ => { return Err("SHOW must have a number for its third argument"); }
                };
                Ok(Window(clr, p1, p2))
            },
            "GOTO" => {
                match targs {
                    (&ArgExpr(Immediate(v)), &ArgEmpty, &ArgEmpty, &ArgEmpty) => {
                        match v.as_i16() {
                            0..500 => Ok(GotoPage(v)),
                            _ => Err("GOTO can only go to pages up to 500"),
                        }
                    },
                    (&ArgExpr(Cell(addr)), &ArgEmpty, &ArgEmpty, &ArgEmpty) => {
                        Ok(GotoPrompt(addr))
                    },
                    _ => Err("GOTO takes only a single argument (number or cell)"),
                }
            },
            "GOTOF" => {
                match targs {
                    (&ArgExpr(Immediate(v)), &ArgEmpty, &ArgEmpty, &ArgEmpty) => {
                        match v.as_i16() {
                            1..20 => Ok(GotoFKey(v)),
                            _ => Err("GOTOF argument must be in [1, 20]"),
                        }
                    },
                    _ => Err("GOTOF takes only a single numeric argument"),
                }
            },
            "GOTOTR" => {
                match targs {
                    (&ArgExpr(Immediate(v)), &ArgEmpty, &ArgEmpty, &ArgEmpty) => {
                        match v.as_i16() {
                            1..24 => Ok(GotoTransaction(v)),
                            _ => Err("GOTOTR argument must be in [1, 24]"),
                        }
                    },
                    _ => Err("GOTOTR takes only a single numeric argument"),
                }
            },
            "SYSCALL" => {
                match args.get(0) {
                    &ArgExpr(Cell(addr)) => {
                        Ok(SystemCall(addr))
                    },
                    _ => Err("SYSCALL needs a cell argument"),
                }
            },
            _ => Err("unrecognised name"),
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
}

fn parse_argument<R: Reader>(tokenizer: &mut Tokenizer<R>) -> BancResult<Option<Argument>> {
    let tok = tokenizer.get_token();
    if tok.is_none() {
        return Ok(None);
    }
    let tok = tok.unwrap();
    match tok {
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
                _ => { return Err("expected comma/newline/eof after expression"); },
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
                        Ok(args) => {
                            let count = args.iter().count(|a| a != &ArgEmpty);
                            if count == 0 {
                                Instruction::parse_noarg(name)
                            } else {
                                Instruction::parse_general(name, args)
                            }
                        },
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
fn render_arith_terms(t1: ArithTerm, t2: ArithTerm, t3: ArithTerm, formatter: &mut Formatter) -> Result<(), FormatError> {
    let p1 = t1.effective();
    let p2 = t2.effective();
    let p3 = t3.effective();

    if p1 || p2 || p3 {
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
    }
    Ok(())
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
                if p2z {
                    return status;
                }
                formatter.write_str(", ");
                if !p2z {
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
            &NewPage => {
                "NEWPAGE".fmt(formatter)
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
fn renderbs_newpage_inst() {
    let inst = NewPage;
    let bscode = inst.as_bscode();
    assert_eq!(bscode, bscode::Instruction::new(2999,0,0,0));
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
    let strform = "10350,22422,2693,22537";
    let bstree = Tree::<bscode::Instruction>::parse_string(strform).unwrap();
    assert_eq!(bstree.len(), 1);
    let inst = Instruction::from_bscode(bstree.get(0));
    let disassembled = inst.to_str();
    assert_eq!("SET @350, ADD(42), MUL(@269), LOG(53)", disassembled.as_slice());
}
