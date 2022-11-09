use std::ptr::NonNull;
use std::mem::{size_of, align_of};
use std::slice;

use crate::r#type::{Type, NonIndexedType};
use crate::oref::{Reify, Gc};
use crate::mutator::Mutator;

pub struct Header(usize);

impl Header {
    const TAG_SIZE: usize = 1;

    const TAG_BITS: usize = (1 << Self::TAG_SIZE) - 1;

    const MARK_BIT: usize = 1;

    pub fn new(r#type: Gc<Type>) -> Self { Self(r#type.as_ptr() as usize) }

    pub fn r#type(&self) -> Gc<Type> {
        unsafe { Gc::new_unchecked(NonNull::new_unchecked(((self.0 & !Self::TAG_BITS) | Gc::<()>::TAG) as *mut Type)) }
    }

    pub unsafe fn set_type(&mut self, r#type: Gc<Type>) {
        self.0 = (r#type.as_ptr() as usize) | (self.0 & Self::TAG_BITS);
    }

    pub unsafe fn initialize_indexed<T>(obj: NonNull<T>, header: Self, len: usize) {
        let header_ptr = (obj.as_ptr() as *mut Header).offset(-1);
        let header_len = (header_ptr as *mut usize).offset(-1);
        header_len.write(len);
        header_ptr.write(header);
    }

    pub fn forwarding_address(&self) -> Option<Gc<()>> {
        if (self.0 & Self::MARK_BIT) == 1 {
            Some(unsafe {
                Gc::new_unchecked(NonNull::new_unchecked(((self.0 & !Self::TAG_BITS) | Gc::<()>::TAG) as *mut ()))
            })
        } else {
            None
        }
    }

    pub unsafe fn set_forwarding_address(&mut self, copy: Gc<()>) {
        self.0 = (copy.as_ptr() as usize) | Self::MARK_BIT;
    }
}

pub unsafe trait HeapObj {
    fn header(&self) -> &Header {     
        unsafe { &*((self as *const Self) as *const Header).offset(-1) }
    }

    fn r#type(&self) -> Gc<Type> { self.header().r#type() }
}

// HACK:
unsafe impl HeapObj for () {}

unsafe impl HeapObj for usize {}

pub unsafe trait NonIndexed: Reify {
    fn reify_nonindexed(mt: &Mutator) -> Gc<NonIndexedType> {
        unsafe { Self::reify(mt).unchecked_cast::<NonIndexedType>() }
    }
}

pub unsafe trait Indexed: Sized {
    type Item;

    fn indexed_field_ptr(&self) -> *const Self::Item {
        let ptr = self as *const Self;

        unsafe {
            let field_align = align_of::<Self::Item>();

            let mut fields_addr = ptr.add(1) as usize;
            fields_addr = (fields_addr + field_align - 1) & !(field_align - 1);
            fields_addr as *const Self::Item
        }
    }

    fn indexed_field_ptr_mut(&mut self) -> *mut Self::Item {
        let ptr = self as *mut Self;

        unsafe {
            let field_align = align_of::<Self::Item>();

            let mut fields_addr = ptr.add(1) as usize;
            fields_addr = (fields_addr + field_align - 1) & !(field_align - 1);
            fields_addr as *mut Self::Item
        }
    }

    fn indexed_field(&self) -> &[Self::Item] {
        let ptr = self as *const Self;

        let fields_ptr = self.indexed_field_ptr();

        unsafe {
            let header_len = ((ptr as *const Header).offset(-1) as *const usize)
                .offset(-1);

            slice::from_raw_parts(fields_ptr, *header_len)
        }
    }

    fn indexed_field_mut(&mut self) -> &mut[Self::Item] {
        let ptr = self as *mut Self;

        let fields_ptr = self.indexed_field_ptr_mut();

        unsafe {
            let header_len = ((ptr as *const Header).offset(-1) as *const usize)
                .offset(-1);

            slice::from_raw_parts_mut(fields_ptr, *header_len)
        }
    }
}

pub trait Singleton: Sized {
    fn instance(mt: &Mutator) -> Gc<Self>;
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
