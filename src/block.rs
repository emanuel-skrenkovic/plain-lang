pub enum Value {
    String { val: String },
    Number { val: i32 }
}

#[repr(u8)]
pub enum Op {
    Pop,
    True, False,
    Not,
    Add, Subtract,
    Constant,
    Equal, GreaterEqual, LessEqual
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
