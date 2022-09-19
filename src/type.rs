use std::mem::{transmute, size_of, align_of};
use std::alloc::Layout;
use std::ptr::NonNull;

use crate::heap::Heap;
use crate::oref::Gc;
use crate::heap_obj::{HeapObj, Indexed, min_size_of_indexed, align_of_indexed,
    item_stride};

#[repr(C)]
pub struct Field<T> {
    pub r#type: Gc<T>,
    pub offset: usize
}

impl<T> Clone for Field<T> {
    fn clone(&self) -> Self {
        Self {
            r#type: self.r#type,
            offset: self.offset
        }
    }
}

impl<T> Copy for Field<T> {}

impl<T> Field<T> {
    pub const TYPE_LEN: usize = 2;

    pub const TYPE_SIZE: usize = min_size_of_indexed::<Type>()
        + Self::TYPE_LEN * item_stride::<Type>();

    pub const TYPE_LAYOUT: Layout = unsafe {
        Layout::from_size_align_unchecked(
            Self::TYPE_SIZE, align_of_indexed::<Type>()
        )
    };
}

#[repr(C)]
pub struct Type {
    pub align: usize,
    pub min_size: usize
}

impl Type {
    pub const TYPE_LEN: usize = 3;

    pub const TYPE_SIZE: usize = min_size_of_indexed::<Type>()
        + Self::TYPE_LEN * item_stride::<Type>();

    pub const TYPE_LAYOUT: Layout = unsafe {
        Layout::from_size_align_unchecked(
            Self::TYPE_SIZE, align_of_indexed::<Type>()
        )
    };

    pub unsafe fn bootstrap_new<T>(heap: &mut Heap, type_type: Gc<IndexedType>,
        len: usize
    ) -> Option<NonNull<T>>
    {
        heap.alloc_indexed(type_type, len)
            .map(NonNull::cast)
    }

    fn fields(&self) -> &[Field<Type>] { self.indexed_field() }
}

unsafe impl HeapObj for Type {}

unsafe impl Indexed for Type {
    type Item = Field<Type>;
}

#[repr(C)]
pub struct NonIndexedType(Type);

unsafe impl HeapObj for NonIndexedType {}

impl NonIndexedType {
    pub fn new_unchecked(r#type: Type) -> Self { Self(r#type) }
    
    pub fn from_static<T>() -> Self {
        Self(Type {
            align: align_of::<T>(),
            min_size: size_of::<T>()
        })
    }

    pub fn align(&self) -> usize { self.0.align }

    pub fn min_size(&self) -> usize { self.0.min_size }

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

unsafe impl HeapObj for BitsType {}

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

unsafe impl HeapObj for IndexedType {}

impl IndexedType {
    pub fn new_unchecked(r#type: Type) -> Self { Self(r#type) }

    fn align(&self) -> usize { self.0.align }

    fn min_size(&self) -> usize { self.0.min_size }

    fn indexed_field(&self) ->&Field<NonIndexedType> {
        let fields = self.0.fields();
        unsafe { transmute::<&Field<Type>, &Field<NonIndexedType>>(
            &fields[fields.len() - 1]
        ) }
    }

    pub fn layout(&self, len: usize) -> Layout {
        unsafe {
            let item_stride = self.indexed_field().r#type.as_ref().stride();
            Layout::from_size_align_unchecked(
                self.min_size() + len * item_stride,
                self.align()
            )
        }
   }
}
