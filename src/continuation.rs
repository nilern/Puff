use crate::heap_obj::Indexed;
use crate::oref::{Reify, ORef, Gc};
use crate::mutator::Mutator;
use crate::r#type::IndexedType;

#[repr(C)]
pub struct Continuation;

impl Reify for Continuation {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().continuation }
}

unsafe impl Indexed for Continuation {
    type Item = ORef;
}

impl Continuation {
    // OPTIMIZE: The simplest, most inefficient continuation capture:
    pub fn current(mt: &mut Mutator) -> Gc<Self> {
        let len = mt.stack().len();
        unsafe {
            let nptr = mt.alloc_indexed(Self::reify(mt), len).cast::<Self>();
            (nptr.as_ptr() as *mut ORef).copy_from_nonoverlapping(mt.stack().as_ptr(), len);
            Gc::new_unchecked(nptr)
        }
    }
}
