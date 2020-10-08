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

    pub fn allocate_string(&mut self, s: String) -> usize {
        if let Some(key) = self.interned_strings.get(&s) {
            *key
        } else {
            let c = s.clone();
            let key = self.allocate(Object::new(ObjType::Str(Box::new(s))));
            self.interned_strings.insert(c, key);
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

    pub fn get_upvalue(&self, closure_index: usize, upvalue_index: usize) -> usize {
        let closure = self.get(closure_index);
        if let ObjType::Closure(closure) = &closure.value {
            closure.upvalue_indexes[upvalue_index]
        } else {
            panic!("Closure ref must be a ClosureObj")
        }
    }

    pub fn update_upvalue(&mut self, closure_index: usize, upvalue_index: usize, value: &[u8]) -> () {
        // This function is really wonky, but it's this way to make sure we're not borrowing stuff
        // as mutable and immutable at the same time
        let closure = self.get_mut(closure_index);
        if let ObjType::Closure(closure) = &mut closure.value {
            closure.upvalue_indexes[upvalue_index] = usize::from_be_bytes(value.try_into().unwrap());
        } else {
            panic!("Closure ref must be a ClosureObj")
        }
    }

    #[cfg(feature = "debug-logging")]
    pub fn print(&self) -> () {
        eprint!(" heap: ");
        for slot in self.objects.values() {
            eprint!("[ {} ]", slot.print(self));
        }
        eprintln!();
    }
}
