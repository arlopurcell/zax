use fnv::FnvHashMap;
use std::convert::TryInto;

use crate::object::Object;

pub struct Heap {
    pub objects: FnvHashMap<i64, Object>,
    counter: i64,
    pub interned_strings: FnvHashMap<String, i64>,
}

impl<'a> Heap {
    pub fn new() -> Self {
        Self {
            objects: FnvHashMap::default(),
            counter: i64::MIN,
            interned_strings: FnvHashMap::default(),
        }
    }

    pub fn allocate(&mut self, o: Object) -> i64 {
        if self.objects.len() >= usize::MAX >> 1 {
            // Since only indexes with most significant bit == 1 are allowed, the max number of
            // heap objects is half of usize::MAX
            //TODO InterpretError::Runtime
            panic!("Heap overflow");
        }
        while self.objects.contains_key(&self.counter) {
            // Mask most significant bit. we want all heap indexes to be negative so that the
            // conservative garbage collector interprets less primitives as references
            // This also wraps from 0 to i64::MIN
            self.counter = (self.counter + 1) | i64::MIN;
        }

        #[cfg(feature = "debug-log-gc")]
        eprintln!("Allocate {} for {:?}", o.size(), o.print(self));

        self.objects.insert(self.counter, o);
        self.counter
    }

    pub fn get(&self, idx: &i64) -> &Object {
        self.objects.get(idx).unwrap()
    }

    pub fn try_get_mut(&mut self, idx: &i64) -> Option<&mut Object> {
        self.objects.get_mut(idx)
    }

    pub fn get_mut(&mut self, idx: &i64) -> &mut Object {
        self.objects.get_mut(idx).unwrap()
    }

    /*
    pub fn get_with_bytes(&'a self, idx: &[u8]) -> &'a Object {
        let idx = i64::from_be_bytes(idx.try_into().unwrap());
        self.get(&idx)
    }
    */

    pub fn print_object(&self, idx: &i64) -> String {
        let object = self.get(idx);
        object.print(self)
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
