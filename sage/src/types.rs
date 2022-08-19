use crate::block::Value;

pub trait Equatable {
    fn equals(&self, other: &Self) -> Result<bool, ()>;
}

impl Equatable for Value {
    fn equals(&self, other: &Value) -> Result<bool, ()> {
        match self {
            Value::Number { val } => {
                match other {
                    Value::Number { val: other_val } => Ok(val == other_val),
                    _ => { Err(()) }
                }
            },
            Value::Bool { val } => {
                match other {
                    Value::Bool { val: other_val } => Ok(val == other_val),
                    _ => { Err(()) }
                }
            },
            _ => { Err(()) }
        }
    }
}
