use std::convert::TryInto;

#[derive(PartialEq, Eq)]
pub struct Object {
    pub obj_type: ObjType,
}

#[derive(PartialEq, Eq)]
pub enum ObjType {
    Str(String),
}

pub struct Heap {
    objects: Vec<Object>,
}

impl Object {
    pub fn new(obj_type: ObjType) -> Self {
        Self { obj_type }
    }

    pub fn to_string(&self) -> &str {
        match &self.obj_type {
            ObjType::Str(string) => &string,
            _ => panic!("object at heap index non-string"),
        }
    }
}

// TODO string interning

impl Heap {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }

    pub fn allocate(&mut self, o: Object) -> usize {
        self.objects.push(o);
        self.objects.len() - 1
    }

    pub fn get(&self, idx: usize) -> &Object {
        self.objects.get(idx).unwrap()
    }

    pub fn get_with_bytes(&self, idx: &[u8]) -> &Object {
        let idx = usize::from_be_bytes(idx.try_into().unwrap());
        self.get(idx)
    }

    #[cfg(feature = "debug-logging")]
    pub fn print(&self) -> () {
        print!(" heap: ");
        for slot in self.objects.iter() {
            print!("[ {} ]", slot.to_string());
        }
        println!();
    }
}
