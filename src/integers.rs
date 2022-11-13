use crate::oref::{Reify, Gc};
use crate::heap_obj::NonIndexed;
use crate::mutator::Mutator;
use crate::r#type::BitsType;

impl Reify for isize {
    type Kind = BitsType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().isize }
}

unsafe impl NonIndexed for isize {}

impl Gc<isize> {
    pub fn new(mt: &mut Mutator, bits: isize) -> Self {
        unsafe {
            let nptr = mt.alloc_static::<isize>();
            nptr.as_ptr().write(bits);
            Gc::new_unchecked(nptr)
        }
    }
}

impl Reify for usize {
    type Kind = BitsType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().usize }
}

unsafe impl NonIndexed for usize {}

impl Gc<usize> {
    pub fn new(mt: &mut Mutator, bits: usize) -> Self {
        unsafe {
            let nptr = mt.alloc_static::<usize>();
            nptr.as_ptr().write(bits);
            Gc::new_unchecked(nptr)
        }
    }
}

