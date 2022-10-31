use std::cell::Cell;

use crate::oref::{Reify, ORef, Gc};
use crate::heap_obj::NonIndexed;
use crate::r#type::NonIndexedType;
use crate::mutator::Mutator;

#[repr(C)]
pub struct Box {
    v: Cell<ORef>
}

impl Reify for Box {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<NonIndexedType> { mt.types().r#box }
}

unsafe impl NonIndexed for Box {}

impl Box {
    pub fn new(v: ORef) -> Self { Self {v: Cell::new(v)} }

    pub fn get(&self) -> ORef { self.v.get() }

    pub fn set(&self, v: ORef) { self.v.set(v); }
}
