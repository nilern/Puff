use std::str;
use std::collections::hash_map::DefaultHasher;
use std::alloc::{Layout, alloc, dealloc};
use std::mem::{size_of, align_of};
use std::slice;

use crate::oref::{Reify,  Gc, Fixnum};
use crate::heap_obj::{HeapObj, Indexed};
use crate::mutator::Mutator;
use crate::util::hash;
use crate::r#type::IndexedType;
use crate::heap::{self, Heap};

#[derive(Clone, Copy)]
enum SymbolTableEntry {
    Present(Gc<Symbol>),
    Vacant,
    Tombstone
}

pub struct SymbolTable {
    len: usize,
    capacity: usize,
    symbols: *mut SymbolTableEntry
}

impl Drop for SymbolTable {
    fn drop(&mut self) {
        unsafe { dealloc(self.symbols as *mut u8, Self::entries_layout(self.capacity)); }
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        let capacity = 2;

        Self { len: 0, capacity, symbols: Self::new_entries(capacity) }
    }

    fn entries_layout(capacity: usize) -> Layout {
        unsafe {
            Layout::from_size_align_unchecked(capacity * size_of::<SymbolTableEntry>(), align_of::<SymbolTableEntry>())
        }
    }

    fn new_entries(capacity: usize) -> *mut SymbolTableEntry {
        unsafe {
            let entries = alloc(Self::entries_layout(capacity)) as *mut SymbolTableEntry;

            let mut entry = entries;
            for _ in 0..capacity {
                entry.write(SymbolTableEntry::Vacant);
                entry = entry.add(1);
            }

            entries
        }
    }

    fn rehash(&mut self) {
        let mut new_len = 0;
        let new_capacity = self.capacity * 2;

        let new_symbols = unsafe { slice::from_raw_parts_mut(Self::new_entries(new_capacity), new_capacity) };

        for entry in unsafe { slice::from_raw_parts(self.symbols, self.capacity).iter() } {
            if let SymbolTableEntry::Present(sym) = *entry {
                let hash = isize::from(unsafe { sym.as_ref().hash }) as usize;

                let max_index = new_capacity - 1;
                let mut collisions = 0;
                let mut i = hash & max_index;
                loop {
                    match new_symbols[i] {
                        SymbolTableEntry::Present(_) => {
                            collisions += 1;
                            i = (i + collisions) & max_index;
                        },

                        SymbolTableEntry::Vacant => {
                            new_symbols[i] = SymbolTableEntry::Present(sym);
                            new_len += 1;
                            break;
                        },

                        SymbolTableEntry::Tombstone => unreachable!()
                    }
                }
            }
        }

        unsafe { dealloc(self.symbols as *mut u8, Self::entries_layout(self.capacity)); }

        self.len = new_len;
        self.capacity = new_capacity;
        self.symbols = new_symbols.as_mut_ptr();
    }

    pub unsafe fn scan(&mut self) {
        for entry in slice::from_raw_parts_mut(self.symbols, self.capacity).iter_mut() {
            if let SymbolTableEntry::Present(sym) = *entry {
                *entry = match sym.forwarding_address() {
                    Some(copy) => SymbolTableEntry::Present(copy),
                    None => SymbolTableEntry::Tombstone
                };
            }
        }
    }

    pub unsafe fn verify(&self, heap: &Heap) -> Result<(), heap::VerificationError> {
        for entry in slice::from_raw_parts(self.symbols, self.capacity).iter() {
            if let SymbolTableEntry::Present(sym) = *entry {
                heap.verify_oref(sym.into())?;
            }
        }

        Ok(())
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
            let entries = unsafe { (*symbols).symbols };

            let max_index = unsafe { (*symbols).capacity } - 1;
            let mut collisions = 0;
            let mut i = (isize::from(hash) as usize) & max_index;
            loop {
                match unsafe { *entries.add(i) } {
                    SymbolTableEntry::Present(isymbol) =>
                        if mt.borrow(isymbol).hash == hash && mt.borrow(isymbol).name() == cs {
                            return isymbol
                        } else {
                            collisions += 1;
                            i = (i + collisions) & max_index;
                        },

                    SymbolTableEntry::Vacant =>
                        if unsafe { ((*symbols).len + 1) * 2 > (*symbols).capacity } {
                            unsafe { (*symbols).rehash(); }
                            break; // continue outer loop
                        } else {
                            unsafe {
                                let mut nptr = mt.alloc_indexed(Self::reify(mt), cs.len()).cast::<Self>();
                                nptr.as_ptr().write(Symbol { hash });
                                nptr.as_mut().indexed_field_mut().copy_from_slice(cs.as_bytes());

                                let sym = Gc::new_unchecked(nptr);

                                (*symbols).len += 1;
                                *entries.add(i) = SymbolTableEntry::Present(sym);

                                return sym;
                            }
                        },

                    SymbolTableEntry::Tombstone => unsafe {
                        let mut nptr = mt.alloc_indexed(Self::reify(mt), cs.len()).cast::<Self>();
                        nptr.as_ptr().write(Symbol { hash });
                        nptr.as_mut().indexed_field_mut().copy_from_slice(cs.as_bytes());

                        let sym = Gc::new_unchecked(nptr);

                        *entries.add(i) = SymbolTableEntry::Present(sym);

                        return sym;
                    }
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

    use quickcheck_macros::quickcheck;

    use crate::oref::AsType;
    use crate::handle::{HandleT, Root, root};

    #[test]
    fn symbol_new() {
        let mut mt = Mutator::new(1 << 20 /* 1 MiB */, false).unwrap();
        let bootstrap_symbols_len = mt.symbols().len;

        let sym1 = Symbol::new(&mut mt, "foo");
        assert_eq!(sym1.r#type(), mt.types().symbol.as_type());
        assert_eq!(mt.borrow(sym1).hash, hash::<DefaultHasher, _>("foo"));
        assert_eq!(mt.borrow(sym1).name(), "foo");

        let sym2 = Symbol::new(&mut mt, "bar");
        assert_eq!(sym2.r#type(), mt.types().symbol.as_type());
        assert_eq!(mt.borrow(sym2).hash, hash::<DefaultHasher, _>("bar"));
        assert_eq!(mt.borrow(sym2).name(), "bar");

        let sym3 = Symbol::new(&mut mt, "foo");
        assert_eq!(sym3, sym3);

        assert_eq!(mt.symbols().len, bootstrap_symbols_len + 2);
    }

    #[quickcheck]
    fn interned(name: String) -> bool {
        let mut mt = Mutator::new(1 << 20 /* 1 MiB */, false).unwrap();

        let sym1 = root!(&mut mt, Symbol::new(&mut mt, &name));
        let sym2 = Symbol::new(&mut mt, &name);

        sym1.oref() == sym2
    }

    #[quickcheck]
    fn multi_interned(names: Vec<String>) -> bool {
        let mut mt = Mutator::new(1 << 20 /* 1 MiB */, false).unwrap();

        let syms1: Vec<HandleT<Symbol>> = names.iter().map(|name| root!(&mut mt, Symbol::new(&mut mt, name))).collect();
        let syms2: Vec<HandleT<Symbol>> = names.iter().map(|name| root!(&mut mt, Symbol::new(&mut mt, name))).collect();

        syms1.iter().zip(syms2.iter()).all(|(sym1, sym2)| sym1.oref() == sym2.oref())
    }
}
