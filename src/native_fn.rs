use crate::mutator::Mutator;
use crate::oref::{Reify, Gc};
use crate::heap_obj::NonIndexed;
use crate::r#type::NonIndexedType;

#[must_use]
pub enum Answer {
    Ret {retc: usize},
    TailCall {argc: usize}
}

pub type Code = fn(&mut Mutator) -> Answer;

#[repr(C)]
pub struct NativeFn {
    pub min_arity: usize,
    pub varargs: bool,
    pub code: Code
}

impl Reify for NativeFn {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<NonIndexedType> { mt.types().native_fn }
}

unsafe impl NonIndexed for NativeFn {}

impl NativeFn {
    pub fn new(mt: &mut Mutator, f: Self) -> Gc<Self> {
        unsafe {
            let nptr = mt.alloc_static::<Self>();
            nptr.as_ptr().write(f);
            Gc::new_unchecked(nptr)
        }
    }
}
