use crate::oref::{AsType, Reify, ORef, Gc};
use crate::heap_obj::Indexed;
use crate::array::Array;
use crate::mutator::Mutator;
use crate::handle::HandleT;
use crate::r#type::{Type, IndexedType};

#[repr(C)]
pub struct Bytecode {
    consts: Gc<Array<ORef>>
}

unsafe impl Indexed for Bytecode {
    type Item = u8;
}

impl Reify for Bytecode {
    fn reify(mt: &Mutator) -> Gc<Type> { mt.types().bytecode.as_type() }
}

impl Bytecode {
    pub const TYPE_LEN: usize = 2;

    pub fn new(mt: &mut Mutator, consts: HandleT<Array<ORef>>, instrs: &[u8])
        -> Gc<Self>
    {
        unsafe {
            let r#type = Self::reify(mt).unchecked_cast::<IndexedType>();
            if let Some (nptr) = mt.alloc_indexed(r#type, instrs.len()) {
                let mut nptr = nptr.cast::<Self>();

                nptr.as_ptr().write(Bytecode {
                    consts: *consts
                });
                nptr.as_mut().indexed_field_mut().copy_from_slice(instrs);

                Gc::new_unchecked(nptr)
            } else {
                todo!() // Need to GC, then retry
            }
        }
    }
}
