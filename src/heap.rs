use std::convert::TryInto;
use fnv::FnvHashMap;

#[derive(PartialEq, Eq)]
pub struct Object {
    pub obj_type: ObjType,
}

#[derive(PartialEq, Eq)]
pub enum ObjType {
    Str(String),
}

pub struct Heap {
    objects: FnvHashMap<usize, Object>,
    counter: usize,
    interned_strings: FnvHashMap<String, usize>,
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
            objects: FnvHashMap::default(),
            counter: 0,
            interned_strings: FnvHashMap::default(),
        }
    }

    pub fn allocate_string(&mut self, s: String) -> usize {
        if let Some(key) = self.interned_strings.get(&s) {
            *key
        } else {
            let c = s.clone();
            let key = self.allocate(Object::new(ObjType::Str(s)));
            self.interned_strings.insert(c, key);
            key
        }
    }

    pub fn allocate(&mut self, o: Object) -> usize {
        if self.objects.len() >= usize::MAX {
            panic!("Heap overflow");
        }
        while self.objects.contains_key(&self.counter) {
            let (counter, _) = self.counter.overflowing_add(1);
            self.counter = counter;
        }
        self.objects.insert(self.counter, o);
        self.counter
    }

    pub fn get(&self, idx: usize) -> &Object {
        self.objects.get(&idx).unwrap()
    }

    pub fn get_with_bytes(&self, idx: &[u8]) -> &Object {
        let idx = usize::from_be_bytes(idx.try_into().unwrap());
        self.get(idx)
    }

    #[cfg(feature = "debug-logging")]
    pub fn print(&self) -> () {
        print!(" heap: ");
        for slot in self.objects.values() {
            print!("[ {} ]", slot.to_string());
        }
        println!();
    }
}
