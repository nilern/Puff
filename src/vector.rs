use std::marker::PhantomData;

use crate::heap_obj::Indexed;
use crate::oref::{Reify, ORef, Gc};
use crate::handle::Handle;
use crate::mutator::Mutator;
use crate::r#type::IndexedType;

/// Immutable vector
#[repr(C)]
pub struct Vector<T>{
    phantom: PhantomData<T>
}

unsafe impl<T> Indexed for Vector<T> {
    type Item = T;
}

impl Reify for Vector<ORef> {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().vector_of_any }
}

impl Vector<ORef> {
    pub fn from_handles(mt: &mut Mutator, handles: &[Handle]) -> Gc<Self> {
        unsafe {
            let mut nptr = mt.alloc_indexed(Self::reify(mt), handles.len()).cast::<Self>();

            nptr.as_ptr().write(Vector {phantom: Default::default()});
            let mut v = nptr.as_mut().indexed_field_ptr_mut();
            for handle in handles {
                v.write(**handle);
                v = v.add(1);
            }

            Gc::new_unchecked(nptr)
        }
    }
}
