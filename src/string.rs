use std::str;
use std::ptr::NonNull;
use std::slice;

use crate::mutator::Mutator;
use crate::oref::{Reify, Gc};
use crate::heap_obj::{Indexed, HeapObj};
use crate::r#type::{IndexedType, NonIndexedType};
use crate::fixnum::Fixnum;
use crate::vector::VectorMut;

#[repr(C)]
pub struct String {
    char_len: Fixnum
}

impl Reify for String {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().string }
}

unsafe impl HeapObj for String {}

unsafe impl Indexed for String {
    type Item = u8;
}

impl String {
    pub fn new(mt: &mut Mutator, chars: &str) -> Gc<String> {
        unsafe {
            let nptr: NonNull<Self> = mt.alloc_indexed(Self::reify(mt), chars.len()).cast();

            let char_len = Fixnum::try_from(chars.chars().count()).unwrap_or_else(|_| {
                todo!("String length overflows fixnum");
            });

            nptr.as_ptr().write(String {char_len});
            (*nptr.as_ptr()).indexed_field_ptr_mut()
                .copy_from_nonoverlapping(chars.as_ptr(), chars.len());

            Gc::new_unchecked(nptr)
        }
    }

    pub fn as_str(&self) -> &str { unsafe { str::from_utf8_unchecked(self.indexed_field()) } }
}

#[repr(C)]
pub struct StringMut {
    char_len: Fixnum,
    chars: Gc<VectorMut<u8>>
}

impl Reify for StringMut {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().string_mut }
}

impl StringMut {
    pub fn as_str(&self) -> &str {
        unsafe {
            let byte_cells = self.chars.as_ref().indexed_field();
            let bytes = slice::from_raw_parts(byte_cells.as_ptr() as *const u8, byte_cells.len());
            str::from_utf8_unchecked(bytes)
        }
    }
}
