use std::mem::{size_of, align_of};

#[repr(C)]
pub struct Type {
    align: usize,
    min_size: usize
}

impl Type {
    pub fn align(&self) -> usize { self.align }

    pub fn min_size(&self) -> usize { self.min_size }
}

#[repr(C)]
pub struct NonIndexedType(Type);

impl NonIndexedType {
    pub fn align(&self) -> usize { self.0.align() }

    pub fn min_size(&self) -> usize { self.0.min_size() }

    pub fn stride(&self) -> usize {
        (self.min_size() + self.align() - 1) & !(self.align() - 1)
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
