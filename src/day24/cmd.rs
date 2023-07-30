pub type Int = i64;

// there are only 4 variables, so a 'pointer' only needs 2 bits, but this is as small as I can go
pub type Variable = u8;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Ord, PartialOrd)]
pub enum Value {
    Variable(Variable),
    Integer(Int),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub enum Command {
    TwoArg(Variable, TwoArgOperand, Value),
    OneArg(Variable, OneArgOperand),
    Inp(Variable),
}

// Note we rely on the derived ordering (two independent instructions will be sorted according to it)
// so don't reorder these
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub enum TwoArgOperand {
    Set,
    Add,
    Mul,
    Div,
    Mod,
    Eql,
}

impl ToString for TwoArgOperand {
    fn to_string(&self) -> String {
        match self {
            TwoArgOperand::Set => "set to",
            TwoArgOperand::Add => "+",
            TwoArgOperand::Mul => "*",
            TwoArgOperand::Div => "/",
            TwoArgOperand::Mod => "%",
            TwoArgOperand::Eql => "==",
        }
        .to_string()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub enum OneArgOperand {
    // literally just "set val to val % 26;" for whatever reason every mod is mod 26
    Mod26,
}
