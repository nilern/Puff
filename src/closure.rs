use crate::oref::{Reify, ORef, Gc};
use crate::handle::{Root, root};
use crate::bytecode::Bytecode;
use crate::heap_obj::Indexed;
use crate::mutator::Mutator;
use crate::r#type::IndexedType;
use crate::vector::Vector;

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
            nptr.as_ptr().write(Self {code: regs[regs.len() - 1].unchecked_cast()});
            let mut v = nptr.as_mut().indexed_field_ptr_mut();
            for clover in &regs.as_slice()[first_clover_reg..(first_clover_reg + len)] {
                v.write(*clover);
                v = v.add(1);
            }

            Gc::new_unchecked(nptr)
        }
    }
}

#[repr(C)]
pub struct TypedClosure {
    pub domain: Gc<Vector<ORef>>,
    pub code: Gc<Bytecode>,
}

impl TypedClosure {
    pub fn clovers(&self) -> &[ORef] { self.indexed_field() }
}

unsafe impl Indexed for TypedClosure {
    type Item = ORef;
}

impl Reify for TypedClosure {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().typed_closure }
}

impl TypedClosure {
    pub fn new(mt: &mut Mutator, arity: usize, len: usize) -> Gc<Self> {
        let first_type_reg = mt.regs().len() - arity - len - 1;
        let domain = root!(mt, Vector::<ORef>::from_regs(mt, first_type_reg..(first_type_reg + arity)));

        unsafe {
            let mut nptr = mt.alloc_indexed(Self::reify(mt), len).cast::<Self>();

            let regs = mt.regs();
            let first_clover_reg = regs.len().checked_sub(len + 1).unwrap();
            nptr.as_ptr().write(Self {domain: domain.oref(), code: regs[regs.len() - 1].unchecked_cast()});
            let mut v = nptr.as_mut().indexed_field_ptr_mut();
            for clover in &regs.as_slice()[first_clover_reg..(first_clover_reg + len)] {
                v.write(*clover);
                v = v.add(1);
            }

            Gc::new_unchecked(nptr)
        }
    }
}
