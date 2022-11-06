use std::cell::Cell;

use crate::symbol::Symbol;
use crate::oref::{Reify, Gc, ORef, Fixnum};
use crate::mutator::Mutator;
use crate::handle::{Handle, HandleT, Root, root};
use crate::heap_obj::{NonIndexed, Indexed};
use crate::r#type::NonIndexedType;
use crate::vector::VectorMut;

#[repr(C)]
pub struct Var {
    value: Cell<ORef>
}

unsafe impl NonIndexed for Var {}

impl Reify for Var {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().var }
}

impl Var {
    pub unsafe fn new_uninitialized(mt: &mut Mutator) -> Gc<Self> { Gc::new_unchecked(mt.alloc_static::<Self>()) }

    pub fn new(mt: &mut Mutator, value: Handle) -> Gc<Self> {
        unsafe {
            let nptr = mt.alloc_static::<Self>();
            nptr.as_ptr().write(Self { value: Cell::new(value.oref()) });
            Gc::new_unchecked(nptr)
        }
    }

    pub fn value(&self) -> ORef { self.value.get() }

    pub fn init(&self, v: ORef) { self.value.set(v); }

    pub fn redefine(&self, v: ORef) { self.value.set(v); }

    pub fn set(&self, v: ORef) { self.value.set(v); }
}

#[repr(C)]
pub struct Namespace {
    len: Cell<usize>,
    capacity: Cell<usize>,
    keys: Cell<Gc<VectorMut<ORef>>>,
    values: Cell<Gc<VectorMut<ORef>>>
}

impl Reify for Namespace {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<NonIndexedType> { mt.types().namespace }
}

unsafe impl NonIndexed for Namespace {}

impl Namespace {
    pub fn new(mt: &mut Mutator) -> Gc<Self> {
        let capacity = 2;

        let keys = root!(mt, VectorMut::<ORef>::zeros(mt, capacity));
        let values = root!(mt, VectorMut::<ORef>::zeros(mt, capacity));

        unsafe {
            let nptr = mt.alloc_static::<Self>();

            nptr.as_ptr().write(Self {
                len: Cell::new(0),
                capacity: Cell::new(capacity),
                keys: Cell::new(keys.oref()),
                values: Cell::new(values.oref())
            });

            Gc::new_unchecked(nptr)
        }
    }

    pub fn get(&self, name: Gc<Symbol>) -> Option<Gc<Var>> {
        let hash = unsafe { name.as_ref().hash() };

        let max_index = self.capacity.get() - 1;
        let mut collisions = 0;
        let mut i = (isize::from(hash) as usize) & max_index;
        loop {
            let k = unsafe { self.keys.get().as_ref().indexed_field()[i].get() };

            if k == name.into() {
                return Some(unsafe { self.values.get().as_ref().indexed_field()[i].get().unchecked_cast::<Var>() });
            } else if k != Fixnum::from(0u8).into() {
                collisions += 1;
                i = (i + collisions) & max_index;
            } else {
                return None;
            }
        }
    }
}

impl HandleT<Namespace> {
    pub fn add(self, mt: &mut Mutator, name: HandleT<Symbol>, v: HandleT<Var>) {
        let hash = name.hash();

        loop {
            let max_index = self.capacity.get() - 1;
            let mut collisions = 0;
            let mut i = (isize::from(hash) as usize) & max_index;
            loop {
                let k = unsafe { self.keys.get().as_ref().indexed_field()[i].get() };

                if k != Fixnum::from(0u8).into() {
                    debug_assert!(k != name.oref().into());
                    collisions += 1;
                    i = (i + collisions) & max_index;
                } else {
                    if (self.len.get() + 1) * 2 > self.capacity.get() {
                        unsafe { self.clone().rehash(mt); }
                        break; // continue outer loop
                    } else {
                        unsafe {
                            self.len.set(self.len.get() + 1);
                            self.keys.get().as_ref().indexed_field()[i].set(name.oref().into());
                            self.values.get().as_ref().indexed_field()[i].set(v.oref().into());
                        }
                        return;
                    }
                }
            }
        }
    }

    unsafe fn rehash(self, mt: &mut Mutator) {
        let new_capacity = self.capacity.get() * 2;

        let new_keys = root!(mt, VectorMut::<ORef>::zeros(mt, new_capacity));
        let new_values = VectorMut::<ORef>::zeros(mt, new_capacity);
        let new_keys = new_keys.oref();

        for (old_i, k) in self.keys.get().as_ref().indexed_field().iter().enumerate() {
            if let Some(k) = k.get().try_cast::<Symbol>(mt) {
                let hash = isize::from(k.as_ref().hash()) as usize;

                let max_index = new_capacity - 1;
                let mut collisions = 0;
                let mut i = hash & max_index;
                while new_keys.as_ref().indexed_field()[i].get() != Fixnum::from(0u8).into() {
                    collisions += 1;
                    i = (i + collisions) & max_index;
                }
                new_keys.as_ref().indexed_field()[i].set(k.into());
                new_values.as_ref().indexed_field()[i]
                    .set(self.values.get().as_ref().indexed_field()[old_i].get());
            }
        }

        self.capacity.set(new_capacity);
        self.keys.set(new_keys);
        self.values.set(new_values);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use quickcheck_macros::quickcheck;

    use crate::oref::Fixnum;
    use crate::handle::{Root, root};

    #[test]
    fn test_add_get() {
        let mut mt = Mutator::new(1 << 20, false).unwrap();
        let ns = root!(&mut mt, Namespace::new(&mut mt));

        let foo = root!(&mut mt, Symbol::new(&mut mt, "foo"));
        let bar = root!(&mut mt, Symbol::new(&mut mt, "bar"));
        let baz = root!(&mut mt, Symbol::new(&mut mt, "baz"));

        let one = root!(&mut mt, ORef::from(Fixnum::from(1u8)));
        let two = root!(&mut mt, ORef::from(Fixnum::from(2u8)));
        let three = root!(&mut mt, ORef::from(Fixnum::from(3u8)));

        assert_eq!(ns.get(foo.oref()), None);

        let var = root!(&mut mt, Var::new(&mut mt, one.clone()));
        ns.clone().add(&mut mt, foo.clone(), var);
        assert_eq!(unsafe { ns.get(foo.oref()).unwrap().as_ref().value() }, one.oref());
        assert_eq!(ns.get(bar.oref()), None);
        assert_eq!(ns.get(baz.oref()), None);

        let var = root!(&mut mt, Var::new(&mut mt, two.clone()));
        ns.clone().add(&mut mt, bar.clone(), var);
        assert_eq!(unsafe { ns.get(foo.oref()).unwrap().as_ref().value() }, one.oref());
        assert_eq!(unsafe { ns.get(bar.oref()).unwrap().as_ref().value() }, two.oref());
        assert_eq!(ns.get(baz.oref()), None);

        let var = root!(&mut mt, Var::new(&mut mt, three.clone()));
        ns.clone().add(&mut mt, baz.clone(), var);
        assert_eq!(unsafe { ns.get(foo.oref()).unwrap().as_ref().value() }, one.oref());
        assert_eq!(unsafe { ns.get(bar.oref()).unwrap().as_ref().value() }, two.oref());
        assert_eq!(unsafe { ns.get(baz.oref()).unwrap().as_ref().value() }, three.oref());
    }

    #[quickcheck]
    fn add_get(k: String, v: u8) -> bool {
        let mut mt = Mutator::new(1 << 20, false).unwrap();
        let ns = root!(&mut mt, Namespace::new(&mut mt));

        let k = root!(&mut mt, Symbol::new(&mut mt, &k));
        let v = root!(&mut mt, ORef::from(Fixnum::from(v)));
        let var = root!(&mut mt, Var::new(&mut mt, v.clone()));
        ns.clone().add(&mut mt, k.clone(), var);

        unsafe { ns.get(k.oref()).unwrap().as_ref().value() == v.oref() }
    }

    #[quickcheck]
    fn multi_add_get(mut kvs: Vec<(String, u8)>) -> bool {
        let mut mt = Mutator::new(1 << 20, false).unwrap();
        let ns = root!(&mut mt, Namespace::new(&mut mt));

        // HACK:
        kvs.sort_by_key(|(k, _)| k.clone());
        kvs.dedup_by_key(|(k, _)| k.clone());

        for (k, v) in kvs.iter() {
            let k = root!(&mut mt, Symbol::new(&mut mt, k));
            let v = root!(&mut mt, ORef::from(Fixnum::from(*v)));
            let var = root!(&mut mt, Var::new(&mut mt, v.clone()));
            ns.clone().add(&mut mt, k.clone(), var);
        }

        kvs.iter().all(|(k, v)| {
            let k = root!(&mut mt, Symbol::new(&mut mt, k));
            let v = root!(&mut mt, ORef::from(Fixnum::from(*v)));
            unsafe { ns.get(k.oref()).unwrap().as_ref().value() == v.oref() }
        })
    }
}
