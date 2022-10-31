use crate::oref::{Reify, ORef, Gc, Fixnum};
use crate::handle::{Handle, HandleT, Root, root};
use crate::mutator::Mutator;
use crate::heap_obj::{NonIndexed, Indexed};
use crate::r#type::NonIndexedType;
use crate::bool::Bool;
use crate::string::String;
use crate::list::Pair;
use crate::vector::Vector;

#[repr(C)]
pub struct Syntax {
    pub expr: ORef,
    pub pos: ORef
}

impl Reify for Syntax {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().syntax }
}

unsafe impl NonIndexed for Syntax {}

impl Syntax {
    pub fn new(mt: &mut Mutator, expr: Handle, pos: Option<HandleT<Pos>>) -> Gc<Self> {
        unsafe {
            let nptr = mt.alloc_static::<Self>();
            nptr.as_ptr().write(Self {
                expr: *expr,
                pos: match pos {
                    Some(pos) => (*pos).into(),
                    None => Bool::instance(mt, false).into()
                }
            });
            Gc::new_unchecked(nptr)
        }
    }

    pub fn to_datum(&self, mt: &mut Mutator) -> ORef { self.expr.to_datum(mt) }
}

impl ORef {
    pub fn to_datum(self, mt: &mut Mutator) -> ORef {
        if let Some(stx) = self.try_cast::<Syntax>(mt) {
            unsafe { stx.as_ref().to_datum(mt) }
        } else if let Some(pair) = self.try_cast::<Pair>(mt) {
            let pair = root!(mt, pair);

            let car = root!(mt, unsafe { pair.as_ref().car() }.to_datum(mt));
            let cdr = root!(mt, unsafe { pair.as_ref().cdr() }.to_datum(mt));

            if *car == unsafe { pair.as_ref().car() } && *cdr == unsafe { pair.as_ref().cdr() } {
                (*pair).into()
            } else {
                Gc::<Pair>::new(mt, car, cdr).into()
            }
        } else if let Some(vector) = self.try_cast::<Vector<ORef>>(mt) {
            let vector = root!(mt, vector);

            let vs: Vec<Handle> = unsafe {
                vector.as_ref().indexed_field().iter()
                    .map(|v| root!(mt, v.to_datum(mt)))
                    .collect()
            };

            if unsafe { vector.as_ref().indexed_field().iter().zip(vs.iter()).all(|(v, u)| *v == **u) } {
                (*vector).into()
            } else {
                Vector::<ORef>::from_handles(mt, &vs).into()
            }
        } else {
            self
        }
    }
}

#[repr(C)]
pub struct Pos {
    pub filename: ORef,
    pub line: Fixnum,
    pub column: Fixnum
}

impl Reify for Pos {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().pos }
}

unsafe impl NonIndexed for Pos {}

impl Pos {
    pub fn new(mt: &mut Mutator, filename: Option<HandleT<String>>, line: Fixnum, column: Fixnum) -> Gc<Self> {
        unsafe {
            let nptr = mt.alloc_static::<Self>();
            nptr.as_ptr().write(Self {
                filename: match filename {
                    Some(filename) => (*filename).into(),
                    None => Bool::instance(mt, false).into()
                },
                line, column
            });
            Gc::new_unchecked(nptr)
        }
    }
}
