use std::collections::HashMap;

pub struct Scope {
    variables: HashMap<String, Variable>,
    pub scope_depth: usize,
}

enum Variable {
    Local {
        depth: usize,
        index: usize,
        size: u8,
    },
    Heap(usize),
}

#[derive(Debug)]
struct Local {
    name: String,
    depth: usize,
    index: usize,
    size: u8,
}
