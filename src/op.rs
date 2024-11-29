#[derive(Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum Op {
    // 2 stack operands
    Add = 0,
    Sub,
    Mul,
    Div,
    Exp,
    Floor,
    Min,
    Max,
    Mod,
    Eq,
    Lte,
    Lt,
    Gte,
    And,
    Or,
    Intersect,
    Union,
    Constant,
    Pop,
    Call,
    TailCall,
    CallMain,
    CallNative,
    Extends,
    ExtendsTrue,
    ExtendsNoPopLeft,
    PanicExtends,
    Jump,
    JumpGameState,
    Number,
    Boolean,
    String,
    Object,
    PopCallFrame,
    MakeObj,
    EmptyTuple,
    MakeArray,
    MakeTuple,
    MakeTupleSpread,

    Index,
    IndexLit,

    Panic,

    SetLocal,
    GetLocal,
    SetGlobal,
    GetGlobal,
    SetInitialState,

    FormatString,
    Any,
    Length,

    Negate,
    Update,
    SetArray,
    Fill,
    NewArray,

    Exit,
}

const _OP_MAX_VAL: u8 = Op::Exit as u8;

impl From<u8> for Op {
    fn from(value: u8) -> Self {
        return unsafe { std::mem::transmute(value) };
    }
}
