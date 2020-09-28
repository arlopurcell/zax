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
        Self {
            obj_type
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
}
