use fnv::FnvHashMap;
use std::convert::TryInto;

use crate::object::{ObjType, Object};

pub struct Heap {
    objects: FnvHashMap<usize, Object>,
    counter: usize,
    interned_strings: FnvHashMap<String, usize>,
}

impl<'a> Heap {
    pub fn new() -> Self {
        Self {
            objects: FnvHashMap::default(),
            counter: 0,
            interned_strings: FnvHashMap::default(),
        }
    }

    pub fn allocate_string(&mut self, s: &str) -> usize {
        if let Some(key) = self.interned_strings.get(s) {
            *key
        } else {
            let key = self.allocate(Object::new(ObjType::Str(Box::new(s.to_string()))));
            self.interned_strings.insert(s.to_string(), key);
            key
        }
    }

    pub fn allocate(&mut self, o: Object) -> usize {
        if self.objects.len() >= usize::MAX {
            panic!("Heap overflow");
        }
        while self.objects.contains_key(&self.counter) {
            self.counter = self.counter.wrapping_add(1);
        }
        self.objects.insert(self.counter, o);
        self.counter
    }

    pub fn get(&self, idx: usize) -> &Object {
        self.objects.get(&idx).unwrap()
    }

    pub fn get_mut(&mut self, idx: usize) -> &mut Object {
        self.objects.get_mut(&idx).unwrap()
    }

    pub fn get_with_bytes(&'a self, idx: &[u8]) -> &'a Object {
        let idx = usize::from_be_bytes(idx.try_into().unwrap());
        self.get(idx)
    }

    #[cfg(feature = "debug-logging")]
    pub fn print(&self) -> () {
        eprint!(" heap: ");
        for (index, slot) in self.objects.iter() {
            eprint!("[ {}: {} ]", index, slot.print(self));
        }
        eprintln!();
    }
}
