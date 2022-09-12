use std::str;
use std::collections::hash_map::DefaultHasher;

use crate::oref::{Gc, Fixnum};
use crate::heap_obj::Indexed;
use crate::mutator::Mutator;
use crate::util::hash;

#[repr(C)]
pub struct Symbol {
    hash: Fixnum,
}

unsafe impl Indexed for Symbol {
    type Item = u8;
}

impl Symbol {
    pub const TYPE_LEN: usize = 2;

    /// Safety: May GC
    unsafe fn new(mt: &mut Mutator, cs: &str) -> Gc<Self> {
        let len = cs.len();

        if let Some(nptr) = mt.alloc_indexed(mt.types().symbol, len) {
            let nptr = nptr.cast::<Self>();
            let ptr = nptr.as_ptr();

            ptr.write(Symbol {
                hash: hash::<DefaultHasher, _>(cs)
            });
            (*ptr).indexed_field_mut().copy_from_slice(cs.as_bytes());

            Gc::new_unchecked(nptr)
        } else {
            todo!() // Need to GC, then retry
        }
    }

    fn name<'a>(&'a self) -> &'a str {
        unsafe { str::from_utf8_unchecked(self.indexed_field()) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::oref::AsType;

    #[test]
    fn symbol_new() {
        let mut mt = Mutator::new(1 << 20 /* 1 MiB */).unwrap();

        let sym = unsafe { Symbol::new(&mut mt, "foo") };

        unsafe {
            assert_eq!(sym.r#type(), mt.types().symbol.as_type());
            assert_eq!(sym.as_ref().hash, hash::<DefaultHasher, _>("foo"));
            assert_eq!(sym.as_ref().name(), "foo");
        }
    }
}
