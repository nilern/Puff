use std::marker::PhantomData;
use std::cell::Cell;
use std::slice;
use std::ops::Range;

use crate::heap_obj::Indexed;
use crate::oref::{Reify, ORef, Gc};
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
    pub fn from_regs(mt: &mut Mutator, indices: Range<usize>) -> Gc<Self> {
        unsafe {
            let mut nptr = mt.alloc_indexed(Self::reify(mt), indices.len()).cast::<Self>();

            nptr.as_ptr().write(Vector {phantom: Default::default()});
            let mut v = nptr.as_mut().indexed_field_ptr_mut();
            for &oref in &mt.regs().as_slice()[indices] {
                v.write(oref);
                v = v.add(1);
            }

            Gc::new_unchecked(nptr)
        }
    }

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
    pub fn zeros(mt: &mut Mutator, len: usize) -> Gc<Self> {
        unsafe {
            let nptr = mt.alloc_indexed(Self::reify(mt), len).cast::<Self>();

            nptr.as_ptr().write(VectorMut {phantom: Default::default()});

            Gc::new_unchecked(nptr)
        }
    }
}

impl VectorMut<u8> {
    pub fn zeros(mt: &mut Mutator, len: usize) -> Gc<Self> {
        unsafe {
            let nptr = mt.alloc_indexed(Self::reify(mt), len).cast::<Self>();

            nptr.as_ptr().write(VectorMut {phantom: Default::default()});

            Gc::new_unchecked(nptr)
        }
    }

    pub unsafe fn as_bytes(&self) -> &[u8] {
        let byte_cells = self.indexed_field();
        slice::from_raw_parts(byte_cells.as_ptr() as *const u8, byte_cells.len())
    }

    pub unsafe fn as_bytes_mut(&self) -> &mut [u8] {
        let byte_cells = self.indexed_field();
        slice::from_raw_parts_mut(byte_cells.as_ptr() as *mut u8, byte_cells.len())
    }
}
