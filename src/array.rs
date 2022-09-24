use std::marker::PhantomData;

use crate::heap_obj::Indexed;
use crate::oref::{Reify, ORef, Gc};
use crate::handle::Handle;
use crate::mutator::Mutator;
use crate::r#type::IndexedType;

#[repr(C)]
pub struct Array<T>{
    phantom: PhantomData<T>
}

unsafe impl<T> Indexed for Array<T> {
    type Item = T;
}

impl Reify for Array<ORef> {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().array_of_any }
}

impl<T> Array<T> {
    // The indexed field, .phantom is just for Rust typing:
    pub const TYPE_LEN: usize = 1;
}

impl Array<ORef> {
    pub fn from_handles(mt: &mut Mutator, handles: &[Handle]) -> Gc<Self> {
        unsafe {
            if let Some(nptr) = mt.alloc_indexed(Self::reify(mt), handles.len())
            {
                let mut nptr = nptr.cast::<Self>();

                nptr.as_ptr().write(Array {phantom: Default::default()});
                let mut v = nptr.as_mut().indexed_field_ptr_mut();
                for handle in handles {
                    v.write(**handle);
                    v = v.add(1);
                }

                Gc::new_unchecked(nptr)
            } else {
                todo!() // Need to GC, then retry
            }
        }
    }
}
