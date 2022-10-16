use std::alloc::{Layout, alloc, dealloc};
use std::mem::{size_of, align_of};
use std::slice;

use crate::symbol::Symbol;
use crate::oref::{Reify, Gc, ORef};
use crate::mutator::Mutator;
use crate::handle::Handle;
use crate::heap_obj::NonIndexed;
use crate::r#type::NonIndexedType;

#[repr(C)]
pub struct Var {
    value: ORef
}

unsafe impl NonIndexed for Var {}

impl Reify for Var {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().var }
}

impl Var {
    pub const TYPE_LEN: usize = 1;

    pub unsafe fn new_uninitialized(mt: &mut Mutator) -> Gc<Self> { Gc::new_unchecked(mt.alloc_static::<Self>()) }

    pub fn new(mt: &mut Mutator, value: Handle) -> Gc<Self> {
        unsafe {
            let nptr = mt.alloc_static::<Self>();
            nptr.as_ptr().write(Self { value: *value });
            Gc::new_unchecked(nptr)
        }
    }

    pub fn value(&self) -> ORef { self.value }

    pub fn init(&mut self, v: ORef) { self.value = v; }

    pub fn redefine(&mut self, v: ORef) { self.value = v; }

    pub fn set(&mut self, v: ORef) { self.value = v; }
}

type Key = Option<Gc<Symbol>>;
type Value = Option<Gc<Var>>;

pub struct Namespace {
    len: usize,
    capacity: usize,
    keys: *mut Key,
    values: *mut Value
}

impl Drop for Namespace {
    fn drop(&mut self) {
        unsafe {
            dealloc(self.keys as *mut u8, Layout::from_size_align_unchecked(
                self.capacity * size_of::<Key>(), align_of::<Key>()));
            dealloc(self.values as *mut u8, Layout::from_size_align_unchecked(
                self.capacity * size_of::<Value>(), align_of::<Value>()))
        }
    }
}

impl Namespace {
    pub fn new() -> Self {
        let capacity = 2;

        let keys = unsafe {
            let size = capacity * size_of::<Key>();
            let keys = alloc(Layout::from_size_align_unchecked(size, align_of::<Key>()));
            keys.write_bytes(0, size);
            keys as *mut Key
        };
        let values = unsafe {
            let size = capacity * size_of::<Value>();
            let values = alloc(Layout::from_size_align_unchecked(size, align_of::<Value>()));
            values.write_bytes(0, size);
            values as *mut Value
        };

        Self { len: 0, capacity, keys, values }
    }

    pub fn get(&self, name: Gc<Symbol>) -> Option<Gc<Var>> {
        let hash = unsafe { name.as_ref().hash() };

        let max_index = self.capacity - 1;
        let mut collisions = 0;
        let mut i = (isize::from(hash) as usize) & max_index;
        loop {
            match unsafe { *self.keys.add(i) } {
                Some(k) if k == name => return unsafe { *self.values.add(i) },
                Some(_) => {
                    collisions += 1;
                    i = (i + collisions) & max_index;
                },
                None => return None
            }
        }
    }

    pub fn add(&mut self, name: Gc<Symbol>, v: Gc<Var>) {
        let hash = unsafe { name.as_ref().hash() };

        loop {
            let max_index = self.capacity - 1;
            let mut collisions = 0;
            let mut i = (isize::from(hash) as usize) & max_index;
            loop {
                match unsafe { *self.keys.add(i) } {
                    Some(k) => {
                        debug_assert!(k != name);
                        collisions += 1;
                        i = (i + collisions) & max_index;
                    },
                    None =>
                        if (self.len + 1) * 2 > self.capacity {
                            self.rehash();
                            break; // continue outer loop
                        } else {
                            self.len += 1;
                            unsafe {
                                *self.keys.add(i) = Some(name);
                                *self.values.add(i) = Some(v);
                            }
                            return;
                        }
                }
            }
        }
    }

    fn rehash(&mut self) {
        let new_capacity = self.capacity * 2;

        let new_keys = unsafe {
            let size = new_capacity * size_of::<Key>();
            let new_keys = alloc(Layout::from_size_align_unchecked(size, align_of::<Key>()));
            new_keys.write_bytes(0, size);
            slice::from_raw_parts_mut(new_keys as *mut Key, new_capacity)
        };
        let new_values = unsafe {
            let size = new_capacity * size_of::<Value>();
            let new_values = alloc(Layout::from_size_align_unchecked(size, align_of::<Value>()));
            new_values.write_bytes(0, size);
            slice::from_raw_parts_mut(new_values as *mut Value, new_capacity)
        };

        unsafe { slice::from_raw_parts(self.keys, self.capacity).iter() }
            .enumerate()
            .filter_map(|(old_i, &ok)| ok.map(|k| (old_i, k)))
            .for_each(|(old_i, k)| {
                let hash = isize::from(unsafe { k.as_ref().hash() }) as usize;

                let max_index = new_capacity - 1;
                let mut collisions = 0;
                let mut i = hash & max_index;
                while new_keys[i].is_some() {
                    collisions += 1;
                    i = (i + collisions) & max_index;
                }
                new_keys[i] = Some(k);
                new_values[i] = unsafe { *self.values.add(old_i) };
            });

        self.capacity = new_capacity;
        unsafe { dealloc(
            self.keys as *mut u8,
            Layout::from_size_align_unchecked (self.capacity * size_of::<Key>(), align_of::<Key>())
        ) };
        self.keys = new_keys.as_mut_ptr();
        unsafe { dealloc(
            self.values as *mut u8,
            Layout::from_size_align_unchecked (self.capacity * size_of::<Value>(), align_of::<Value>())
        ) };
        self.values = new_values.as_mut_ptr();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::oref::Fixnum;

    #[test]
    fn test_add_get() {
        let mut mt = Mutator::new(1 << 20).unwrap();
        let mut ns = Namespace::new();

        let foo = Symbol::new(&mut mt, "foo");
        let bar = Symbol::new(&mut mt, "bar");
        let baz = Symbol::new(&mut mt, "baz");

        let one = mt.root(Fixnum::try_from(1isize).unwrap().into());
        let two = mt.root(Fixnum::try_from(2isize).unwrap().into());
        let three = mt.root(Fixnum::try_from(3isize).unwrap().into());

        assert_eq!(ns.get(foo), None);

        ns.add(foo, Var::new(&mut mt, one.clone()));
        assert_eq!(unsafe { ns.get(foo).unwrap().as_ref().value() }, *one);
        assert_eq!(ns.get(bar), None);
        assert_eq!(ns.get(baz), None);

        ns.add(bar, Var::new(&mut mt, two.clone()));
        assert_eq!(unsafe { ns.get(foo).unwrap().as_ref().value() }, *one);
        assert_eq!(unsafe { ns.get(bar).unwrap().as_ref().value() }, *two);
        assert_eq!(ns.get(baz), None);

        ns.add(baz, Var::new(&mut mt, three.clone()));
        assert_eq!(unsafe { ns.get(foo).unwrap().as_ref().value() }, *one);
        assert_eq!(unsafe { ns.get(bar).unwrap().as_ref().value() }, *two);
        assert_eq!(unsafe { ns.get(baz).unwrap().as_ref().value() }, *three);
    }
}
