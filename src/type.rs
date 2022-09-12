use std::mem::{transmute, size_of, align_of};
use std::alloc::Layout;
use std::slice;
use std::ptr::NonNull;

use crate::heap::Heap;
use crate::oref::{Gc, Header};

pub unsafe trait Indexed: Sized {
    type Item;

    fn indexed_field(&self) -> &[Self::Item] {
        let ptr = self as *const Self;

        unsafe {
            let field_align = align_of::<Self::Item>();

            let mut fields_addr = ptr.add(1) as usize;
            fields_addr = (fields_addr + field_align - 1) & !(field_align - 1);
            let fields_ptr = fields_addr as *const Self::Item;

            let header_len = ((ptr as *const Header).offset(-1) as *const usize)
                .offset(-1);

            slice::from_raw_parts(fields_ptr, *header_len)
        }
    }

    fn indexed_field_mut(&mut self) -> &mut[Self::Item] {
        let ptr = self as *mut Self;

        unsafe {
            let field_align = align_of::<Self::Item>();

            let mut fields_addr = ptr.add(1) as usize;
            fields_addr = (fields_addr + field_align - 1) & !(field_align - 1);
            let fields_ptr = fields_addr as *mut Self::Item;

            let header_len = ((ptr as *const Header).offset(-1) as *const usize)
                .offset(-1);

            slice::from_raw_parts_mut(fields_ptr, *header_len)
        }
    }
}

pub const fn min_size_of_indexed<T: Indexed>() -> usize {
    let static_size = size_of::<T>();
    let item_align = align_of::<T::Item>();
    (static_size + item_align - 1) & !(item_align - 1)
}

pub const fn align_of_indexed<T: Indexed>() -> usize {
    let align = align_of::<T>();
    let item_align = align_of::<T::Item>();
    if align > item_align { align } else { item_align }
}

pub const fn item_stride<T: Indexed>() -> usize {
    let item_size = size_of::<T::Item>();
    let item_align = align_of::<T::Item>();
    (item_size + item_align - 1) & !(item_align - 1)
}

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

unsafe impl Indexed for Type {
    type Item = Field<Type>;
}

#[repr(C)]
pub struct NonIndexedType(Type);

impl NonIndexedType {
    pub fn new_unchecked(r#type: Type) -> Self { Self(r#type) }

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
