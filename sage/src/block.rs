use std::convert::TryFrom;

#[derive(Clone, Debug)]
pub enum Value
{
    String { val: String },
    Number { val: i32 },
    Bool   { val: bool },
    Function {
        name: String,
        arity: usize,
        closure: Closure,
        argument_type_names:
        Vec<Option<String>>,
        return_type_name: String
    },
    Closure { val: Closure },
    Unit
}

#[derive(Clone, Debug)]
pub struct Closure
{
    pub code: Block
}

#[repr(u8)]
pub enum Op
{
    Pop,
    True, False,
    Not,
    Add, Subtract, Multiply, Divide,
    Constant,
    Equal, Less, Greater,
    GreaterEqual, LessEqual,
    DeclareVariable, SetLocal, GetLocal,
    SetUpvalue, GetUpvalue,
    Frame, Return,
    Jump, CondJump, LoopJump,
    Call
}

impl TryFrom<u8> for Op
{
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error>
    {
        match v {
            x if x == Op::Pop as u8             => Ok(Op::Pop),
            x if x == Op::True as u8            => Ok(Op::True),
            x if x == Op::False as u8           => Ok(Op::False),
            x if x == Op::Not as u8             => Ok(Op::Not),
            x if x == Op::Add as u8             => Ok(Op::Add),
            x if x == Op::Subtract as u8        => Ok(Op::Subtract),
            x if x == Op::Multiply as u8        => Ok(Op::Multiply),
            x if x == Op::Divide as u8          => Ok(Op::Divide),
            x if x == Op::Constant as u8        => Ok(Op::Constant),
            x if x == Op::Equal as u8           => Ok(Op::Equal),
            x if x == Op::Less as u8            => Ok(Op::Less),
            x if x == Op::Greater as u8         => Ok(Op::Greater),
            x if x == Op::GreaterEqual as u8    => Ok(Op::GreaterEqual),
            x if x == Op::LessEqual as u8       => Ok(Op::LessEqual),
            x if x == Op::DeclareVariable as u8 => Ok(Op::DeclareVariable),
            x if x == Op::SetLocal as u8        => Ok(Op::SetLocal),
            x if x == Op::GetLocal as u8        => Ok(Op::GetLocal),
            x if x == Op::SetUpvalue as u8      => Ok(Op::SetUpvalue),
            x if x == Op::GetUpvalue as u8      => Ok(Op::GetUpvalue),
            x if x == Op::Frame as u8           => Ok(Op::Frame),
            x if x == Op::Return as u8          => Ok(Op::Return),
            x if x == Op::Jump as u8            => Ok(Op::Jump),
            x if x == Op::CondJump as u8        => Ok(Op::CondJump),
            x if x == Op::LoopJump as u8        => Ok(Op::LoopJump),
            x if x == Op::Call as u8            => Ok(Op::Call),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub struct TypeInfo
{
    name: String,
    kind: TypeKind,
}

#[derive(Clone, Debug)]
pub enum TypeKind
{
    Bool,
    Number,
    String,
    // Struct,
}

#[derive(Clone, Debug)]
pub struct Block
{
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub type_info: Vec<TypeInfo>,
}

impl Block
{
    pub fn new(capacity: usize) -> Block
    {
        Block {
            code: Vec::with_capacity(capacity),
            constants: vec![],
            type_info: vec![],
        }
    }

    pub fn write_op(&mut self, op: Op) -> usize
    {
        self.code.push(op as u8);
        self.code.len() - 1
    }

    pub fn write(&mut self, val: u8) -> usize
    {
        self.code.push(val);
        self.code.len() - 1
    }

    pub fn write_at(&mut self, index: usize, val: u8)
    {
        self.code[index] = val;
    }

    pub fn write_constant(&mut self, constant: Value) -> u8
    {
        self.constants.push(constant);
        (self.constants.len() - 1) as u8
    }

    pub fn write_constant_at(&mut self, index: usize, constant: Value)
    {
        self.constants[index] = constant;
    }
}
