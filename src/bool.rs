use crate::mutator::Mutator;
use crate::heap_obj::NonIndexed;
use crate::oref::{Reify, Gc};
use crate::r#type::BitsType;

#[repr(C)]
pub struct Bool(pub bool);

impl Reify for Bool {
    type Kind = BitsType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().bool }
}

unsafe impl NonIndexed for Bool {}

impl Bool {
    pub fn instance(mt: &Mutator, v: bool) -> Gc<Self> {
        if v { mt.singletons().r#true } else { mt.singletons().r#false }
    }
}
