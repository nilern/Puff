use crate::mutator::Mutator;
use crate::oref::{Reify, Gc, ORef};
use crate::heap_obj::NonIndexed;
use crate::r#type::NonIndexedType;
use crate::handle::{Root, root};

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
    pub domain: ORef, // Vector<Gc<Type> | #f> | #f
    pub code: Code
}

impl Reify for NativeFn {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<NonIndexedType> { mt.types().native_fn }
}

unsafe impl NonIndexed for NativeFn {}

pub struct Builder {
    pub min_arity: usize,
    pub varargs: bool,
    pub make_param_types: fn(&mut Mutator) -> ORef /* Vector<Gc<Type> | #f> | #f */,
    pub code: Code
}

impl Builder {
    pub fn build(self, mt: &mut Mutator) -> Gc<NativeFn> {
        let param_types = root!(mt, (self.make_param_types)(mt));

        unsafe {
            let nptr = mt.alloc_static::<NativeFn>();
            nptr.as_ptr().write(NativeFn {
                min_arity: self.min_arity,
                varargs: self.varargs,
                domain: param_types.oref(),
                code: self.code
            });
            Gc::new_unchecked(nptr)
        }
    }
}
