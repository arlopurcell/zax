use crate::vm::VM;

fn collect_garbage(vm: &mut VM) -> () {
    #[cfg(feature = "debug-log-gc")]
    eprintln!("-- gc begin");

    mark_roots(vm);

    #[cfg(feature = "debug-log-gc")]
    eprintln!("-- gc end");
}

fn mark_roots(vm: &mut VM) -> () {
}
