use crate::oref::{Reify, Gc, ORef};
use crate::heap_obj::Indexed;
use crate::mutator::Mutator;
use crate::r#type::IndexedType;

#[repr(C)]
pub struct CaseFn;

unsafe impl Indexed for CaseFn {
    type Item = ORef; // Gc<Closure> | Gc<TypedClosure>
}

impl Reify for CaseFn {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().case_fn }
}

impl CaseFn {
    pub unsafe fn new(mt: &mut Mutator, len: usize) -> Gc<Self> {
        let mut nptr = mt.alloc_indexed(Self::reify(mt), len).cast::<Self>();

        let regs = mt.regs();
        let first_clause_reg = regs.len().checked_sub(len).unwrap();
        nptr.as_ptr().write(CaseFn);
        let mut v = nptr.as_mut().indexed_field_ptr_mut();
        for &clause in &regs.as_slice()[first_clause_reg..(first_clause_reg + len)] {
            v.write(clause);
            v = v.add(1);
        }

        Gc::new_unchecked(nptr)
    }
}
