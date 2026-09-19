use std::cell::Cell;

#[derive(Debug, Default)]
pub struct GlobalIdGenerator {
    next_id: Cell<usize>,
}

impl GlobalIdGenerator {
    pub fn new() -> Self {
        Self {
            next_id: Cell::new(0),
        }
    }

    pub fn next(&self) -> GlobalId {
        let id = self.next_id.get();
        self.next_id.set(id + 1);
        GlobalId(id)
    }

    pub fn count(&self) -> usize {
        self.next_id.get()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlobalId(usize);

impl GlobalId {
    pub fn get(&self) -> usize {
        self.0
    }
}
