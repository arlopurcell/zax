
#[derive(Clone)]
pub enum Value {
    Float(f64),
}

impl Value {
    pub fn print(&self) -> () {
        match self {
            Self::Float(v) => print!("'{}'", v),
        }
    }
}
