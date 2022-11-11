use std::str;
use std::ptr::NonNull;

use crate::mutator::Mutator;
use crate::oref::{Reify, Gc};
use crate::heap_obj::{Indexed, HeapObj};
use crate::r#type::IndexedType;
use crate::fixnum::Fixnum;

#[repr(C)]
pub struct String {
    char_len: Fixnum
}

unsafe impl HeapObj for String {}

unsafe impl Indexed for String {
    type Item = u8;
}

impl Reify for String {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().string }
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
