#[derive(Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum Op {
    // 2 stack operands
    Add = 0,
    Sub,
    Mul,
    Eq,
    Lte,
    // 2 stack operands
    Intersect,
    Union,
    // N stack operands
    Print,
    // 1 instr operand
    Constant,
    Pop,
    // N stack operands (args) + 1 stack operand (fn ident) + 1 instr for count
    Call,
    TailCall,
    CallMain,
    // 2 args, 2 jump instrs
    Extends,
    ExtendsNoPopLeft,
    PanicExtends,
    Jump,
    Number,
    Boolean,
    String,
    Object,
    PopCallFrame,
    // next instr is fields
    MakeObj,
    EmptyTuple,
    MakeArray,
    MakeTuple,
    MakeTupleSpread,

    Index,
    IndexLit,

    WriteFile,
    ToTypescriptSource,
    ParseInt,
    Panic,

    SetLocal,
    GetLocal,
    SetGlobal,
    GetGlobal,

    FormatString,
    Any,
    Length,

    Negate,
    Update,

    Exit,
}

const OP_MAX_VAL: u8 = Op::GetGlobal as u8;

impl From<u8> for Op {
    fn from(value: u8) -> Self {
        return unsafe { std::mem::transmute(value) };
    }
}
