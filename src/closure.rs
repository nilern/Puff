use crate::oref::{Reify, ORef, Gc};
use crate::bytecode::Bytecode;
use crate::heap_obj::Indexed;
use crate::mutator::Mutator;
use crate::r#type::IndexedType;

#[repr(C)]
pub struct Closure {
    code: Gc<Bytecode>
}

impl Closure {
    pub fn clovers(&self) -> &[ORef] { self.indexed_field() }
}

unsafe impl Indexed for Closure {
    type Item = ORef;
}

impl Reify for Closure {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().closure }
}

impl Closure {
    pub const TYPE_LEN: usize = 2;

    /// Safety: `code` and `clovers` must point into `mt` registers, not the managed heap.
    pub fn new(mt: &mut Mutator, len: usize) -> Gc<Self> {
        unsafe {
            if let Some(nptr) = mt.alloc_indexed(Self::reify(mt), len)
            {
                let mut nptr = nptr.cast::<Self>();

                let regs = mt.regs();
                let code_reg = regs.len().checked_sub(len + 1).unwrap();
                nptr.as_ptr().write(Closure {code: regs[code_reg].unchecked_cast()});
                let mut v = nptr.as_mut().indexed_field_ptr_mut();
                for clover in &regs[(code_reg + 1)..] {
                    v.write(*clover);
                    v = v.add(1);
                }

                Gc::new_unchecked(nptr)
            } else {
                todo!() // Need to GC, then retry
            }
        }
    }
}
