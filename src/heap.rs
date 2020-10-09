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

    pub fn get_upvalue(&self, closure_index: usize, upvalue_index: usize) -> &[u8] {
        let closure = self.get(closure_index);
        if let ObjType::Closure(closure) = &closure.value {
            let upvalue_obj = self.get(closure.upvalues[upvalue_index]);
            if let ObjType::Upvalue(bytes) = &upvalue_obj.value {
                &bytes
            } else {
                panic!("upvalue ref must be a Upvalue")
            }
        } else {
            panic!("Closure ref must be a ClosureObj")
        }
    }

    pub fn update_upvalue(&mut self, closure_index: usize, upvalue_index: usize, value: &[u8]) -> () {
        let closure = if let ObjType::Closure(closure) = &self.get_mut(closure_index).value {
            closure
        } else {
            panic!("should be closure");
        };
        let heap_index = closure.upvalues[upvalue_index];
        
        let upvalue_obj = self.get_mut(heap_index);
        upvalue_obj.value = ObjType::Upvalue(value.to_vec());
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
