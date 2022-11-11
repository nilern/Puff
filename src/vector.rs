use std::marker::PhantomData;
use std::cell::Cell;

use crate::heap_obj::Indexed;
use crate::oref::{Reify, ORef, Gc};
use crate::fixnum::Fixnum;
use crate::handle::HandleAny;
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
    pub fn from_handles(mt: &mut Mutator, handles: &[HandleAny]) -> Gc<Self> {
        unsafe {
            let mut nptr = mt.alloc_indexed(Self::reify(mt), handles.len()).cast::<Self>();

            nptr.as_ptr().write(Vector {phantom: Default::default()});
            let mut v = nptr.as_mut().indexed_field_ptr_mut();
            for handle in handles {
                v.write(handle.oref());
                v = v.add(1);
            }

            Gc::new_unchecked(nptr)
        }
    }
}

#[repr(C)]
pub struct VectorMut<T> {
    phantom: PhantomData<T>
}

unsafe impl<T: Copy> Indexed for VectorMut<T> {
    type Item = Cell<T>;
}

impl Reify for VectorMut<ORef> {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().vector_mut_of_any }
}

impl Reify for VectorMut<u8> {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().vector_mut_of_byte }
}

impl VectorMut<ORef> {
    // OPTIMIZE: Make Fixnum::TAG 0b00, so that allocator has already initialized this:
    pub fn zeros(mt: &mut Mutator, len: usize) -> Gc<Self> {
        unsafe {
            let mut nptr = mt.alloc_indexed(Self::reify(mt), len).cast::<Self>();

            nptr.as_ptr().write(VectorMut {phantom: Default::default()});
            let mut v = nptr.as_mut().indexed_field_ptr_mut();
            for _ in 0..len {
                v.write(Cell::new(Fixnum::from(0u8).into()));
                v = v.add(1);
            }

            Gc::new_unchecked(nptr)
        }
    }
}
