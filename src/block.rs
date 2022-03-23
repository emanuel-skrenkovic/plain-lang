use std::convert::TryFrom;

#[derive(Clone, Copy, Debug)]
pub enum Value {
    // String { val: String },
    Number { val: i32 },
    Bool   { val: bool },
    Unit
}

#[repr(u8)]
pub enum Op {
    Pop,
    True, False,
    Not,
    Add, Subtract, Multiply, Divide,
    Constant,
    Equal, Less, Greater,
    GreaterEqual, LessEqual
}

impl TryFrom<u8> for Op {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == Op::Pop as u8          => Ok(Op::Pop),
            x if x == Op::True as u8         => Ok(Op::True),
            x if x == Op::False as u8        => Ok(Op::False),
            x if x == Op::Not as u8          => Ok(Op::Not),
            x if x == Op::Add as u8          => Ok(Op::Add),
            x if x == Op::Subtract as u8     => Ok(Op::Subtract),
            x if x == Op::Multiply as u8     => Ok(Op::Multiply),
            x if x == Op::Divide as u8       => Ok(Op::Divide),
            x if x == Op::Constant as u8     => Ok(Op::Constant),
            x if x == Op::Equal as u8        => Ok(Op::Equal),
            x if x == Op::Less as u8         => Ok(Op::Less),
            x if x == Op::Greater as u8      => Ok(Op::Greater),
            x if x == Op::GreaterEqual as u8 => Ok(Op::GreaterEqual),
            x if x == Op::LessEqual as u8    => Ok(Op::LessEqual),
            _ => Err(()),
        }
    }
}

pub struct Block {
    pub code: Vec<u8>,
    pub values: Vec<Value>
}

impl Block {
    pub fn new(capacity: usize) -> Block {
        Block {
            code: Vec::with_capacity(capacity),
            values: vec![]
        }
    }

    pub fn write_op(&mut self, op: Op) {
        self.code.push(op as u8);
    }

    pub fn write(&mut self, val: u8) {
        self.code.push(val);
    }

    pub fn write_constant(&mut self, constant: Value) -> u8 {
        self.values.push(constant);
        (self.values.len() - 1) as u8
    }
}
