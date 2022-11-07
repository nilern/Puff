use std::cell::Cell;

use crate::oref::{Reify, ORef, Gc};
use crate::handle::HandleRefAny;
use crate::heap_obj::{NonIndexed, Singleton};
use crate::mutator::Mutator;
use crate::r#type::NonIndexedType;

#[repr(C)]
pub struct EmptyList;

impl Reify for EmptyList {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().empty_list }
}

impl Singleton for EmptyList {
    fn instance(mt: &Mutator) -> Gc<Self> { mt.singletons().empty_list }
}

#[repr(C)]
pub struct Pair {
    car: Cell<ORef>,
    cdr: Cell<ORef>
}

impl Reify for Pair {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<NonIndexedType> { mt.types().pair }
}

unsafe impl NonIndexed for Pair {}

impl Pair {
    pub fn new(car: ORef, cdr: ORef) -> Self { Self {car: Cell::new(car), cdr: Cell::new(cdr)} }

    pub fn car(&self) -> ORef { self.car.get() }

    pub fn cdr(&self) -> ORef { self.cdr.get() }

    pub fn set_car(&self, v: ORef) { self.car.set(v); }

    pub fn set_cdr(&self, v: ORef) { self.cdr.set(v); }
}

impl Gc<Pair> {
    pub fn new(mt: &mut Mutator, car: HandleRefAny, cdr: HandleRefAny) -> Self {
        unsafe {
            let nptr = mt.alloc_static::<Pair>();
            nptr.as_ptr().write(Pair::new(car.oref(), cdr.oref()));
            Gc::new_unchecked(nptr)
        }
    }
}
