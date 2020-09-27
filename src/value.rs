
#[derive(Clone)]
pub enum Value {
    Float(f64),
    Integer(i64),
}

impl Value {
    pub fn print(&self) -> () {
        match self {
            Self::Float(v) => print!("{}", v),
            Self::Integer(v) => print!("{}", v),
        }
    }
}
