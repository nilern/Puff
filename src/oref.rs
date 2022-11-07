use std::fmt::{self, Debug};
use std::mem::{size_of, transmute};
use std::ptr::NonNull;
use pretty::RcDoc;

use crate::r#type::{Type, NonIndexedType, IndexedType, BitsType};
use crate::heap_obj::{Singleton, HeapObj, Header};
use crate::mutator::{Mutator, WithinMt};
use crate::list::{Pair, EmptyList};
use crate::bool::Bool;
use crate::fixnum::Fixnum;

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

    pub unsafe fn unchecked_cast<T>(self) -> Gc<T> {
        Gc::new_unchecked(NonNull::new_unchecked(self.0 as *mut T))
    }

    pub fn try_cast<T: Reify>(self, mt: &Mutator) -> Option<Gc<T>> where Gc<T::Kind>: AsType {
        if let Ok(obj) = Gc::<()>::try_from(self) {
            obj.try_cast::<T>(mt)
        } else {
            None // FIXME: casting to e.g. Fixnum
        }
    }

    pub fn instance_of<T: Reify>(self, mt: &Mutator) -> bool where Gc<T::Kind>: AsType {
        if let Ok(obj) = Gc::<()>::try_from(self) {
            obj.instance_of::<T>(mt)
        } else {
            false // FIXME: instance_of e.g. Fixnum
        }
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
            Gc::<()>::TAG => Self::Gc(unsafe { Gc(NonNull::new_unchecked(oref.0 as *mut ())) }),
            Fixnum::TAG => Self::Fixnum(unsafe { Fixnum::from_oref_unchecked(oref) }.into()),
            Flonum::TAG => Self::Flonum(Flonum(oref.0).into()),
            Char::TAG => Self::Char(Char(oref.0).into()),
            _ => unreachable!()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Flonum(usize);

impl Tagged for Flonum {
    const TAG: usize = Fixnum::TAG + 1;
}

impl From<Flonum> for ORef {
    fn from(n: Flonum) -> Self { ORef(n.0) }
}

impl From<f64> for Flonum {
    // Loses `ORef::TAG_SIZE` bits of precision:
    fn from(n: f64) -> Self {
        Flonum(unsafe { transmute::<f64, usize>(n) } | Self::TAG)
    }
}

impl From<Flonum> for f64 {
    fn from(n: Flonum) -> Self {
        unsafe { transmute::<usize, f64>(n.0 & !ORef::TAG_BITS) }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Char(usize);

impl Tagged for Char {
    const TAG: usize = Flonum::TAG + 1;
}

impl From<Char> for ORef {
    fn from(c: Char) -> Self { ORef(c.0) }
}

// `ORef::PAYLOAD_BITS >= 30` so even `char::MAX` always fits:
impl From<char> for Char {
    fn from(c: char) -> Self { Char(((c as usize) << ORef::SHIFT) | Char::TAG) }
}

impl From<Char> for char {
    fn from(c: Char) -> Self {
        unsafe { char::from_u32_unchecked((c.0 >> ORef::SHIFT) as u32) }
    }
}

pub trait Reify {
    type Kind;

    fn reify(mt: &Mutator) -> Gc<Self::Kind>;
}

pub struct Gc<T>(NonNull<T>);

impl<T> Gc<T> {
    pub const TAG: usize = 0;
}

impl<T: HeapObj> Gc<T> {
    pub fn instance_of<U: Reify>(self, mt: &Mutator) -> bool where Gc<U::Kind>: AsType {
        mt.borrow(self).r#type() == U::reify(mt).as_type()
    }

    pub fn try_cast<U: Reify>(self, mt: &Mutator) -> Option<Gc<U>> where Gc<U::Kind>: AsType {
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
            .field(&self.0)
            .finish()
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self { Self(self.0) }
}

impl<T> Copy for Gc<T> {}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl<T> From<Gc<T>> for ORef {
    fn from(obj: Gc<T>) -> Self { Self(obj.0.as_ptr() as usize) }
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
    pub unsafe fn new_unchecked(ptr: NonNull<T>) -> Self { Self(ptr) }

    pub fn as_ptr(self) -> *const T { self.0.as_ptr() }

    pub unsafe fn as_ref(&self) -> &T { self.0.as_ref() }

    fn header(&self) -> &Header { unsafe { &*((self.0.as_ptr() as *const Header).offset(-1)) } }

    fn header_mut(self) -> *mut Header { unsafe { (self.0.as_ptr() as *mut Header).offset(-1) } }

    pub fn r#type(self) -> Gc<Type> { self.header().r#type() }

    pub fn forwarding_address(self) -> Option<Gc<T>> {
        self.header().forwarding_address().map(|obj| unsafe { obj.unchecked_cast::<T>() })
    }

    pub unsafe fn set_forwarding_address(self, copy: Gc<T>) {
        (*self.header_mut()).set_forwarding_address(copy.unchecked_cast::<()>());
    }

    pub unsafe fn unchecked_cast<R>(self) -> Gc<R> { Gc::<R>(self.0.cast()) }

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

pub unsafe trait AsType {
    fn as_type(self) -> Gc<Type>;
}

unsafe impl AsType for Gc<NonIndexedType> {
    fn as_type(self) -> Gc<Type> {
        unsafe { self.unchecked_cast::<Type>() }
    }
}

unsafe impl AsType for Gc<IndexedType> {
    fn as_type(self) -> Gc<Type> {
        unsafe { self.unchecked_cast::<Type>() }
    }
}

unsafe impl AsType for Gc<BitsType> {
    fn as_type(self) -> Gc<Type> {
        unsafe { self.unchecked_cast::<Type>() }
    }
}

impl Gc<BitsType> {
    pub fn as_nonindexed(self) -> Gc<NonIndexedType> {
        unsafe { self.unchecked_cast::<NonIndexedType>() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flonum_from_f64() {
        assert_eq!(f64::from(Flonum::from(0f64)), 0f64);
        assert!(ORef::from(Flonum::from(0f64)).is_tagged::<Flonum>());

        assert_eq!(f64::from(Flonum::from(5f64)), 5f64);

        assert_eq!(f64::from(Flonum::from(-5f64)), -5f64);
    }

    #[test]
    fn char_from() {
        assert_eq!(
            char::from(Char::from(char::from_u32(0u32).unwrap())),
            char::from_u32(0u32).unwrap()
        );
        assert!(ORef::from(Char::from(char::from_u32(0u32).unwrap()))
            .is_tagged::<Char>());

        assert_eq!(
            char::from(Char::from(char::from_u32(5u32).unwrap())),
            char::from_u32(5u32).unwrap()
        );

        assert_eq!(
            char::from(Char::from(char::from_u32(char::MAX as u32).unwrap())),
            char::from_u32(char::MAX as u32).unwrap()
        );
    }
}
