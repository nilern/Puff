use std::str;
use std::ptr::NonNull;
use std::slice;
use std::cmp::Ordering;
use std::cell::Cell;

use crate::mutator::Mutator;
use crate::oref::{Reify, Gc};
use crate::heap_obj::{Indexed, NonIndexed, HeapObj};
use crate::r#type::{IndexedType, NonIndexedType};
use crate::fixnum::Fixnum;
use crate::vector::VectorMut;
use crate::handle::HandleRef;

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
    byte_len: Cell<Fixnum>,
    chars: Cell<Gc<VectorMut<u8>>>
}

impl Reify for StringMut {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().string_mut }
}

unsafe impl NonIndexed for StringMut {}

impl StringMut {
    pub fn new(mt: &mut Mutator, char_len: Fixnum, byte_len: Fixnum, chars: HandleRef<VectorMut<u8>>) -> Gc<Self> {
        unsafe {
            let nptr = mt.alloc_static::<Self>();

            nptr.as_ptr().write(Self {
                char_len,
                byte_len: Cell::new(byte_len),
                chars: Cell::new(chars.oref())
            });

            Gc::new_unchecked(nptr)
        }
    }

    fn buf_as_bytes(&self) -> &[u8] {
        unsafe {
            let chars = self.chars.get();
            let byte_cells = chars.as_ref().indexed_field();
            slice::from_raw_parts(byte_cells.as_ptr() as *const u8, byte_cells.len())
        }
    }

    fn buf_as_mut_bytes(&self) -> &mut [u8] {
        unsafe {
            let chars = self.chars.get();
            let byte_cells = chars.as_ref().indexed_field();
            slice::from_raw_parts_mut(byte_cells.as_ptr() as *mut u8, byte_cells.len())
        }
    }

    pub fn as_str(&self) -> &str {
        let bytes = self.buf_as_bytes();
        let byte_len = isize::from(self.byte_len.get()) as usize; // `self.byte_len` should always be >= 0
        unsafe { str::from_utf8_unchecked(slice::from_raw_parts_mut(bytes.as_ptr() as *mut u8, byte_len)) }
    }
}

impl<'a> HandleRef<'a, StringMut> {
    pub fn set_nth(self, mt: &mut Mutator, i: usize, c: char) -> Result<(), ()> {
        let (start, old_c) = self.as_str().char_indices().nth(i).ok_or(())?;
        let c_len = c.len_utf8();
        let old_c_len = old_c.len_utf8();
        let delta = c_len as isize - old_c_len as isize;

        match delta.cmp(&0) {
            Ordering::Equal => { c.encode_utf8(&mut self.buf_as_mut_bytes()[start..]); },

            Ordering::Less => {
                let bytes = self.buf_as_mut_bytes();

                c.encode_utf8(&mut bytes[start..]);
                bytes.copy_within((start + old_c_len).., start + c_len);

                self.byte_len.set(self.byte_len.get().checked_add(Fixnum::try_from(delta).unwrap()).unwrap());
            },

            Ordering::Greater => {
                let bytes = self.buf_as_mut_bytes();

                let byte_len = isize::from(self.byte_len.get()) as usize; // `self.byte_len` should always be >= 0
                if delta <= bytes.len() as isize - byte_len as isize {
                    bytes.copy_within((start + old_c_len).., start + c_len);
                    c.encode_utf8(&mut bytes[start..]);

                    self.byte_len.set(self.byte_len.get().checked_add(Fixnum::try_from(delta).unwrap()).unwrap());
                } else {
                    let chars = VectorMut::<u8>::zeros(mt, (byte_len as isize + delta) as usize);
                    let byte_cells = unsafe { chars.as_ref().indexed_field() };
                    let bytes = unsafe { slice::from_raw_parts_mut(byte_cells.as_ptr() as *mut u8, byte_cells.len()) };

                    // Reload in case GC happened:
                    let old_bytes = self.buf_as_mut_bytes();

                    bytes[..start].copy_from_slice(&old_bytes[..start]);
                    c.encode_utf8(&mut bytes[start..]);
                    bytes[(start + c_len)..].copy_from_slice(&old_bytes[(start + old_c_len)..]);

                    self.chars.set(chars);
                    self.byte_len.set(self.byte_len.get().checked_add(Fixnum::try_from(delta).unwrap()).unwrap());
                }
            }
        }

        Ok(())
    }
}
