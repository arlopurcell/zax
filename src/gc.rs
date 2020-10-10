use std::iter::once;

use crate::heap::Heap;
use crate::object::ObjType;
use crate::vm::VM;

pub fn collect_garbage(vm: &mut VM) -> () {
    #[cfg(feature = "debug-log-gc")]
    {
        eprintln!("-- gc begin");
        let size_before = vm.bytes_allocated;
    }

    let gray_stack = mark_roots(vm);
    trace_refs(gray_stack, &mut vm.heap);
    sweep_interned(&mut vm.heap);
    sweep(vm);

    #[cfg(feature = "debug-log-gc")]
    {
        eprintln!("-- gc end");
        eprintln!(
            "    collected {} bytes (from {} to {}) next at {}",
            size_before - vm.bytes_allocated,
            size_before,
            vm.bytes_allocated,
            vm.next_gc
        );
    }
}

fn mark_roots(vm: &mut VM) -> Vec<i64> {
    let mut roots = Vec::new();
    for value in vm
        .stack
        .0
        .iter()
        .chain(vm.globals.values())
        .chain(vm.frames.iter().map(|frame| &frame.function_heap_index))
        .chain(vm.upvalue_allocations.values())
        .chain(
            vm.chunk_generators
                .iter()
                //.map(|gen| gen.chunk.constants.iter())
                .map(|gen| gen.chunk.constants.iter())
                .flatten(),
        )
    {
        if let Some(idx) = mark_value(value, &mut vm.heap) {
            roots.push(idx);
        }
    }
    roots
}

fn mark_value(value: &i64, heap: &mut Heap) -> Option<i64> {
    // try_get_mut will not return anything for positive numbers, which should cover most
    // non-references. Some erroneous refs may be marked, but that's conservative gc
    if let Some(object) = heap.try_get_mut(value) {
        if object.marked {
            // It's already been marked, so avoid the cycle here
            None
        } else {
            object.marked = true;

            #[cfg(feature = "debug-log-gc")]
            eprintln!("{} mark", heap.print_object(value));

            Some(*value)
        }
    } else {
        None
    }
}

fn trace_refs(mut gray_stack: Vec<i64>, heap: &mut Heap) -> () {
    while let Some(gray_index) = gray_stack.pop() {
        gray_stack.append(&mut blacken(&gray_index, heap));
    }
}

fn blacken(value: &i64, heap: &mut Heap) -> Vec<i64> {
    if let Some(object) = &heap.try_get(value) {
        #[cfg(feature = "debug-log-gc")]
        eprintln!("{} blacken", object.print(heap));

        let ref_values = match &object.value {
            ObjType::NativeFunction(_) | ObjType::Nil | ObjType::Str(_) => Vec::new(),
            ObjType::Upvalue(ref_idx) => vec![*ref_idx],
            ObjType::Function(func) => func
                .chunk
                .constants
                .iter()
                .chain(once(&func.name_index))
                .cloned()
                .collect(),
        };

        for value in ref_values.iter() {
            mark_value(value, heap);
        }
        ref_values
    } else {
        // If the value isn't in the heap, then it's a primitive and doesn't need gc'ing
        Vec::new()
    }
}

fn sweep_interned(heap: &mut Heap) {
    let objects = &mut heap.objects;
    heap.interned_strings
        .retain(|_, ref_idx| objects.get(ref_idx).unwrap().marked)
}

fn sweep(vm: &mut VM) {
    vm.heap.objects.retain(|_, object| object.marked);
    let bytes_allocated = &mut vm.bytes_allocated;
    *bytes_allocated = 0;
    vm.heap.objects.values_mut().for_each(|object| {
        object.marked = false;
        *bytes_allocated += object.size();
    });
    vm.next_gc = vm.bytes_allocated * 2; // arbitrary grow factor
}
