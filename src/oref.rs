use std::fmt::{self, Debug};
use std::mem::{size_of, transmute};
use std::ptr::NonNull;
use std::marker::PhantomData;
use pretty::RcDoc;

use crate::r#type::{Type, IndexedType};
use crate::heap_obj::{Singleton, HeapObj, Header};
use crate::mutator::{Mutator, WithinMt};
use crate::list::{Pair, EmptyList};
use crate::bool::Bool;
use crate::fixnum::Fixnum;
use crate::flonum::Flonum;
use crate::char::Char;

pub const FIXNUM_TAG: usize = 0;
pub const OBJ_TAG: usize = FIXNUM_TAG + 1;
pub const FLONUM_TAG: usize = OBJ_TAG + 1;
pub const CHAR_TAG: usize = FLONUM_TAG + 1;

pub trait Tagged {
    const TAG: usize;
}

// TODO: Enforce `usize` at least 32 bits:
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ORef(usize);

impl ORef {
    pub const TAG_SIZE: usize = 2;

    pub const TAG_BITS: usize = (1 << Self::TAG_SIZE) - 1;

    pub const PAYLOAD_BITS: usize = 8*size_of::<Self>() - Self::TAG_SIZE;

    pub const SHIFT: usize = Self::TAG_SIZE;

    pub fn tag(self) -> usize { self.0 & Self::TAG_BITS }

    pub fn is_tagged<T: Tagged>(self) -> bool { self.tag() == T::TAG }

    pub fn within(self, mt: &Mutator) -> WithinMt<Self> {
        WithinMt {v: self, mt}
    }

    pub fn r#type(self, mt: &Mutator) -> Gc<Type> {
        if let Ok(obj) = Gc::<()>::try_from(self) {
            obj.r#type()
        } else {
            mt.tag_type(self.tag())
        }
    }

    pub unsafe fn unchecked_cast<T>(self) -> Gc<T> {
        Gc::new_unchecked(NonNull::new_unchecked(self.0 as *mut T))
    }

    pub fn try_cast<T: Reify>(self, mt: &Mutator) -> Option<Gc<T>> where Gc<T::Kind>: Into<Gc<Type>> {
        if let Ok(obj) = Gc::<()>::try_from(self) {
            obj.try_cast::<T>(mt)
        } else {
            None // FIXME: casting to e.g. Fixnum
        }
    }

    pub fn instance_of_dyn(self, mt: &Mutator, sup: Gc<Type>) -> bool { self.r#type(mt).extends(mt, sup) }

    pub fn instance_of<T: Reify>(self, mt: &Mutator) -> bool where Gc<T::Kind>: Into<Gc<Type>> {
        self.instance_of_dyn(mt, T::reify(mt).into())
    }

    pub fn is_truthy(self, mt: &Mutator) -> bool { self != Bool::instance(mt, false).into() }

    pub fn to_doc(self, mt: &Mutator) -> RcDoc<()> {
        match Gc::<()>::try_from(self) {
            Ok(obj) => obj.to_doc(mt),
            Err(_) => RcDoc::as_string(self.within(mt))
        }
    }
}

pub enum ORefEnum {
    Gc(Gc<()>),
    Fixnum(isize),
    Flonum(f64),
    Char(char)
}

impl From<ORef> for ORefEnum {
    fn from(oref: ORef) -> Self {
        match oref.tag() {
            Gc::<()>::TAG => Self::Gc(unsafe { Gc::from_oref_unchecked(oref) }),
            Fixnum::TAG => Self::Fixnum(unsafe { Fixnum::from_oref_unchecked(oref) }.into()),
            Flonum::TAG => Self::Flonum(unsafe { Flonum::from_oref_unchecked(oref) }.into()),
            Char::TAG => Self::Char(unsafe { Char::from_oref_unchecked(oref) }.into()),
            _ => unreachable!()
        }
    }
}

pub trait Reify {
    type Kind;

    fn reify(mt: &Mutator) -> Gc<Self::Kind>;
}

pub struct Gc<T> {
    oref: usize,
    phantom: PhantomData<NonNull<T>>
}

impl<T> Gc<T> {
    pub const TAG: usize = OBJ_TAG;
}

impl<T: HeapObj> Gc<T> {
    pub fn instance_of_dyn(self, mt: &Mutator, sup: Gc<Type>) -> bool { self.r#type().extends(mt, sup) }

    pub fn instance_of<U: Reify>(self, mt: &Mutator) -> bool where Gc<U::Kind>: Into<Gc<Type>> {
        self.instance_of_dyn(mt, U::reify(mt).into())
    }

    pub fn try_cast<U: Reify>(self, mt: &Mutator) -> Option<Gc<U>> where Gc<U::Kind>: Into<Gc<Type>> {
        if self.instance_of::<U>(mt) {
            Some(unsafe { self.unchecked_cast::<U>() })
        } else {
            None
        }
    }

    pub fn size(self) -> usize {
        unsafe {
            let r#type = self.as_ref().r#type();

            if let Ok(r#type) = Gc::<IndexedType>::try_from(r#type) {
                let len = *((self.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1);
                r#type.as_ref().size(len)
            } else {
                r#type.as_ref().min_size
            }
        }
    }
}

impl<T> Debug for Gc<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Gc")
            .field(&self.oref)
            .finish()
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self { Gc {oref: self.oref, phantom: self.phantom} }
}

impl<T> Copy for Gc<T> {}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool { self.oref == other.oref }
}

impl<T> From<Gc<T>> for ORef {
    fn from(obj: Gc<T>) -> Self { Self(obj.oref) }
}

impl TryFrom<ORef> for Gc<()> {
    type Error = ();

    fn try_from(obj: ORef) -> Result<Self, Self::Error> {
        if obj.tag() == Gc::<()>::TAG {
            Ok(unsafe { obj.unchecked_cast::<()>() })
        } else {
            Err(())
        }
    }
}

impl<T> Gc<T> {
    unsafe fn from_oref_unchecked(oref: ORef) -> Self { Gc {oref: oref.0, phantom: PhantomData::default()} }

    pub unsafe fn new_unchecked(nptr: NonNull<T>) -> Self {
        Gc {
            oref: (nptr.as_ptr() as usize) | Self::TAG,
            phantom: PhantomData::default()
        }
    }

    pub fn as_ptr(self) -> *const T { (self.oref & !ORef::TAG_BITS) as *const T }

    pub unsafe fn as_ref(&self) -> &T { transmute::<*const T, &T>(self.as_ptr()) }

    fn header(&self) -> &Header { unsafe { &*((self.as_ptr() as *const Header).offset(-1)) } }

    pub fn header_mut(self) -> *mut Header { unsafe { (self.as_ptr() as *mut Header).offset(-1) } }

    pub fn r#type(self) -> Gc<Type> { self.header().r#type() }

    pub fn forwarding_address(self) -> Option<Gc<T>> {
        self.header().forwarding_address().map(|obj| unsafe { obj.unchecked_cast::<T>() })
    }

    pub unsafe fn set_forwarding_address(self, copy: Gc<T>) {
        (*self.header_mut()).set_forwarding_address(copy.unchecked_cast::<()>());
    }

    pub unsafe fn unchecked_cast<R>(self) -> Gc<R> { Gc {oref: self.oref, phantom: PhantomData::default()} }

    pub fn within(self, mt: &Mutator) -> WithinMt<Self> { WithinMt {v: self, mt} }
}

impl Gc<()> {
    pub fn to_doc(self, mt: &Mutator) -> RcDoc<()> {
        if let Some(pair) = self.try_cast::<Pair>(mt) {
            let mut doc = RcDoc::text("(").append(mt.borrow(pair).car().to_doc(mt));
            
            let mut ls = mt.borrow(pair).cdr();
            loop {
                if let Some(pair) = ls.try_cast::<Pair>(mt) {
                    doc = doc.append(RcDoc::line())
                        .append(mt.borrow(pair).car().to_doc(mt));
                    ls = mt.borrow(pair).cdr();
                } else if ls == EmptyList::instance(mt).into() {
                    doc = doc.append(RcDoc::text(")"));
                    break;
                } else {
                    doc = doc.append(RcDoc::line()).append(RcDoc::text(".")).append(RcDoc::line())
                        .append(ls.to_doc(mt)).append(RcDoc::text(")"));
                    break;
                }
            }

            doc
        } else {
            RcDoc::text(format!("{}", self.within(mt)))
        }
    }
}
