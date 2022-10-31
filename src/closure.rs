use crate::oref::{Reify, ORef, Gc};
use crate::bytecode::Bytecode;
use crate::heap_obj::Indexed;
use crate::mutator::Mutator;
use crate::r#type::IndexedType;

#[repr(C)]
pub struct Closure {
    pub code: Gc<Bytecode>
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
    pub fn new(mt: &mut Mutator, len: usize) -> Gc<Self> {
        unsafe {
            let mut nptr = mt.alloc_indexed(Self::reify(mt), len).cast::<Self>();

            let regs = mt.regs();
            let first_clover_reg = regs.len().checked_sub(len + 1).unwrap();
            nptr.as_ptr().write(Closure {code: regs[regs.len() - 1].unchecked_cast()});
            let mut v = nptr.as_mut().indexed_field_ptr_mut();
            for clover in &regs.as_slice()[first_clover_reg..(first_clover_reg + len)] {
                v.write(*clover);
                v = v.add(1);
            }

            Gc::new_unchecked(nptr)
        }
    }
}
