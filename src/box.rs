use crate::oref::{Reify, ORef, Gc};
use crate::heap_obj::NonIndexed;
use crate::r#type::NonIndexedType;
use crate::mutator::Mutator;

#[repr(C)]
pub struct Box {
    pub v: ORef
}

impl Reify for Box {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<NonIndexedType> { mt.types().r#box }
}

unsafe impl NonIndexed for Box {}

impl Box {
    pub const TYPE_LEN: usize = 1;
}
