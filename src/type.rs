use std::mem::{size_of, align_of};
use std::alloc::Layout;
use std::slice;

use super::oref::{Gc, Header};

#[repr(C)]
pub struct Type {
    align: usize,
    min_size: usize
}

impl Type {
    pub fn align(&self) -> usize { self.align }

    pub fn min_size(&self) -> usize { self.min_size }

    fn fields(&self) -> &[Gc<NonIndexedType>] {
        let ptr = self as *const Self;

        unsafe {
            let field_align = align_of::<Gc<NonIndexedType>>();

            let mut fields_addr = ptr.add(1) as usize;
            fields_addr = (fields_addr + field_align - 1) & !(field_align - 1);
            let fields_ptr = fields_addr as *const Gc<NonIndexedType>;

            let header_len = ((ptr as *const Header).offset(-1) as *const usize)
                .offset(-1);

            slice::from_raw_parts(fields_ptr, *header_len)
        }
    }
}

#[repr(C)]
pub struct NonIndexedType(Type);

impl NonIndexedType {
    pub fn align(&self) -> usize { self.0.align() }

    pub fn min_size(&self) -> usize { self.0.min_size() }

    pub fn stride(&self) -> usize {
        (self.min_size() + self.align() - 1) & !(self.align() - 1)
    }

    pub fn layout(&self) -> Layout {
        unsafe {
            Layout::from_size_align_unchecked(self.min_size(), self.align())
        }
    }
}

#[repr(C)]
pub struct BitsType(Type);

impl BitsType {
    pub fn from_static<T>() -> Self {
        Self(Type {
            align: align_of::<T>(),
            min_size: size_of::<T>()
        })
    }
}

#[repr(C)]
pub struct IndexedType(Type);

impl IndexedType {
    fn align(&self) -> usize { self.0.align() }

    fn min_size(&self) -> usize { self.0.min_size() }

    fn indexed_field(&self) -> Gc<NonIndexedType> {
        let fields = self.0.fields();
        fields[fields.len() - 1]
    }

    pub fn layout(&self, len: usize) -> Layout {
        unsafe {
            Layout::from_size_align_unchecked(
                self.min_size() + len * self.indexed_field().as_ref().stride(),
                self.align()
            )
        }
   }
}
