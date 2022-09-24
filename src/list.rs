use crate::oref::{Reify, ORef, Gc};
use crate::handle::Handle;
use crate::heap_obj::{NonIndexed, Singleton};
use crate::mutator::Mutator;
use crate::r#type::NonIndexedType;

#[repr(C)]
pub struct EmptyList;

impl Reify for EmptyList {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().empty_list }
}

impl Singleton for EmptyList {
    fn instance(mt: &Mutator) -> Gc<Self> { mt.singletons().empty_list }
}

impl EmptyList {
    pub const TYPE_LEN: usize = 0;
}

#[repr(C)]
pub struct Pair {
    pub car: ORef,
    pub cdr: ORef
}

impl Reify for Pair {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<NonIndexedType> { mt.types().pair }
}

unsafe impl NonIndexed for Pair {}

impl Pair {
    pub const TYPE_LEN: usize = 2;

    pub fn new(mt: &mut Mutator, car: Handle, cdr: Handle) -> Gc<Self> {
        unsafe {
            if let Some(nptr) = mt.alloc_static::<Self>() {
                nptr.as_ptr().write(Pair {
                    car: *car,
                    cdr: *cdr
                });

                Gc::new_unchecked(nptr)
            } else {
                todo!() // Need to GC, then retry
            }
        }
    }
}
