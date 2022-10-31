use std::str;
use std::collections::hash_map::DefaultHasher;
use std::alloc::{Layout, alloc, dealloc};
use std::mem::{size_of, align_of};
use std::ptr;
use std::slice;

use crate::oref::{Reify,  Gc, Fixnum};
use crate::heap_obj::{HeapObj, Indexed};
use crate::mutator::Mutator;
use crate::util::hash;
use crate::r#type::IndexedType;

pub struct SymbolTable {
    len: usize,
    capacity: usize,
    symbols: *mut Option<Gc<Symbol>>
}

impl Drop for SymbolTable {
    fn drop(&mut self) {
        unsafe {
            dealloc(self.symbols as *mut u8, Layout::from_size_align_unchecked (
                self.capacity * size_of::<Option<Gc<Symbol>>>(),
                align_of::<Option<Gc<Symbol>>>()
            ))
        }
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        let capacity = 2;

        let symbols = unsafe {
            let size = capacity * size_of::<Option<Gc<Symbol>>>();
            let symbols = alloc(Layout::from_size_align_unchecked(
                size,
                align_of::<Option<Gc<Symbol>>>()
            ));
            ptr::write_bytes(symbols, 0, size);
            symbols as *mut Option<Gc<Symbol>>
        };

        Self { len: 0, capacity, symbols }
    }

    fn rehash(&mut self) {
        let new_capacity = self.capacity * 2;

        let new_symbols: &mut [Option<Gc<Symbol>>] = unsafe {
            let size = new_capacity * size_of::<Option<Gc<Symbol>>>();
            let new_symbols = alloc(Layout::from_size_align_unchecked(
                size,
                align_of::<Option<Gc<Symbol>>>()
            ));
            ptr::write_bytes(new_symbols, 0, size);
            slice::from_raw_parts_mut(new_symbols as _, new_capacity)
        };

        unsafe { slice::from_raw_parts(self.symbols, self.capacity).iter() }
            .filter_map(|&osym| osym)
            .for_each(|sym| {
                let hash = isize::from(unsafe { sym.as_ref().hash }) as usize;

                let max_index = new_capacity - 1;
                let mut collisions = 0;
                let mut i = hash & max_index;
                while new_symbols[i].is_some() {
                    collisions += 1;
                    i = (i + collisions) & max_index;
                }
                new_symbols[i] = Some(sym);
            });

        self.capacity = new_capacity;
        unsafe { dealloc(
            self.symbols as *mut u8,
            Layout::from_size_align_unchecked (
                self.capacity * size_of::<Option<Gc<Symbol>>>(),
                align_of::<Option<Gc<Symbol>>>()
            )
        ) };
        self.symbols = new_symbols.as_mut_ptr();
    }
}

#[repr(C)]
pub struct Symbol {
    hash: Fixnum,
}

unsafe impl HeapObj for Symbol {}

unsafe impl Indexed for Symbol {
    type Item = u8;
}

impl Reify for Symbol {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().symbol }
}

impl Symbol {
    pub fn new(mt: &mut Mutator, cs: &str) -> Gc<Self> {
        let symbols = mt.symbols_mut();
        let hash = hash::<DefaultHasher, _>(cs);

        loop {
            unsafe {
                let symbols_slice =
                    slice::from_raw_parts_mut((*symbols).symbols,
                        (*symbols).capacity);

                let max_index = (*symbols).capacity - 1;
                let mut collisions = 0;
                let mut i = (isize::from(hash) as usize) & max_index;
                loop {
                    match symbols_slice[i] {
                        Some(isymbol) =>
                            if isymbol.as_ref().hash == hash
                                && isymbol.as_ref().name() == cs
                            {
                                return isymbol
                            } else {
                                // continue inner loop
                            },
                        None => 
                            if ((*symbols).len + 1) * 2 > (*symbols).capacity {
                                (*symbols).rehash();
                                break; // continue outer loop
                            } else {
                                let len = cs.len();

                                let nptr = mt.alloc_indexed(mt.types().symbol, len).cast::<Self>();
                                let ptr = nptr.as_ptr();

                                ptr.write(Symbol { hash });
                                (*ptr).indexed_field_mut()
                                    .copy_from_slice(cs.as_bytes());

                                let sym = Gc::new_unchecked(nptr);

                                (*symbols).len += 1;
                                symbols_slice[i] = Some(sym);

                                return sym;
                            }
                    }

                    collisions += 1;
                    i = (i + collisions) & max_index;
                }
            }
        }
    }

    pub fn name<'a>(&'a self) -> &'a str {
        unsafe { str::from_utf8_unchecked(self.indexed_field()) }
    }

    pub fn hash(&self) -> Fixnum { self.hash }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::oref::AsType;

    #[test]
    fn symbol_new() {
        let mut mt = Mutator::new(1 << 20 /* 1 MiB */, false).unwrap();
        let bootstrap_symbols_len = mt.symbols().len;

        let sym1 = Symbol::new(&mut mt, "foo");
        unsafe {
            assert_eq!(sym1.r#type(), mt.types().symbol.as_type());
            assert_eq!(sym1.as_ref().hash, hash::<DefaultHasher, _>("foo"));
            assert_eq!(sym1.as_ref().name(), "foo");
        }

        let sym2 = Symbol::new(&mut mt, "bar");
        unsafe {
            assert_eq!(sym2.r#type(), mt.types().symbol.as_type());
            assert_eq!(sym2.as_ref().hash, hash::<DefaultHasher, _>("bar"));
            assert_eq!(sym2.as_ref().name(), "bar");
        }

        let sym3 = Symbol::new(&mut mt, "foo");
        assert_eq!(sym3, sym3);

        assert_eq!(mt.symbols().len, bootstrap_symbols_len + 2);
    }
}
