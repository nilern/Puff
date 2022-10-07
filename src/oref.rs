use std::fmt::{self, Debug, Display};
use std::mem::{size_of, transmute};
use std::ptr::NonNull;

use crate::r#type::{Type, NonIndexedType, IndexedType, BitsType};
use crate::heap_obj::{HeapObj, Header};
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::list::{Pair, EmptyList};
use crate::closure::Closure;
use crate::bytecode::Bytecode;

trait Tagged {
    const TAG: usize;
}

pub trait DisplayWithin {
    fn fmt_within(&self, mt: &Mutator, fmt: &mut fmt::Formatter) -> fmt::Result;
}

pub struct WithinMt<'a, T> {
    pub v: T,
    pub mt: &'a Mutator
}

impl<'a, T: DisplayWithin> Display for WithinMt<'a, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.v.fmt_within(self.mt, fmt)
    }
}

// TODO: Enforce `usize` at least 32 bits:
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ORef(usize);

impl ORef {
    pub const TAG_SIZE: usize = 2;

    const TAG_BITS: usize = (1 << Self::TAG_SIZE) - 1;

    const PAYLOAD_BITS: usize = 8*size_of::<Self>() - Self::TAG_SIZE;

    const SHIFT: usize = Self::TAG_SIZE;

    pub fn tag(self) -> usize { self.0 & Self::TAG_BITS }

    fn is_tagged<T: Tagged>(self) -> bool { self.tag() == T::TAG }

    pub fn within(self, mt: &Mutator) -> WithinMt<Self> {
        WithinMt {v: self, mt}
    }

    pub unsafe fn unchecked_cast<T>(self) -> Gc<T> {
        Gc::new_unchecked(NonNull::new_unchecked(self.0 as *mut T))
    }

    pub fn try_cast<T: Reify>(self, mt: &Mutator) -> Option<Gc<T>>
        where Gc<T::Kind>: AsType
    {
        if let Ok(obj) = Gc::<()>::try_from(self) {
            obj.try_cast::<T>(mt)
        } else {
            None
        }
    }

    pub fn is_truthy(self, _: &Mutator) -> bool { true } // TODO: false
}

impl DisplayWithin for ORef {
    fn fmt_within(&self, mt: &Mutator, fmt: &mut fmt::Formatter) -> fmt::Result
    {
        match self.tag() {
            Gc::<()>::TAG => unsafe {
                let ptr = NonNull::new_unchecked(self.0 as *mut ());
                Gc::new_unchecked(ptr).fmt_within(mt, fmt)
            },
            Fixnum::TAG =>
                Display::fmt(&isize::from(Fixnum(self.0)), fmt),
            Flonum::TAG =>
                Display::fmt(&f64::from(Flonum(self.0)), fmt),
            Char::TAG =>
                Display::fmt(&char::from(Char(self.0)), fmt),
            _ => unreachable!()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Fixnum(usize);

impl Tagged for Fixnum {
    const TAG: usize = Gc::<()>::TAG + 1;
}

impl Fixnum {
    const MIN: isize = -(1 << (ORef::PAYLOAD_BITS - 1));

    const MAX: isize = (1 << (ORef::PAYLOAD_BITS - 1)) - 1;

    pub unsafe fn from_oref_unchecked(oref: ORef) -> Self { Self(oref.0) }
}

impl From<Fixnum> for ORef {
    fn from(n: Fixnum) -> Self { ORef(n.0) }
}

impl TryFrom<isize> for Fixnum {
    type Error = ();

    fn try_from(n: isize) -> Result<Self, Self::Error> {
        // Bounds check `MIN <= n <= MAX` from Hacker's Delight 4-1:
        if (n - Fixnum::MIN) as usize <= (Fixnum::MAX - Fixnum::MIN) as usize {
            Ok(Fixnum(((n as usize) << ORef::SHIFT) | Fixnum::TAG))
        } else {
            Err(())
        }
    }
}

impl From<Fixnum> for isize {
    fn from(n: Fixnum) -> Self { (n.0 as isize) >> ORef::SHIFT }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct Flonum(usize);

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
struct Char(usize);

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
    pub fn try_cast<U: Reify>(self, mt: &Mutator) -> Option<Gc<U>>
        where Gc<U::Kind>: AsType
    {
        if unsafe { self.as_ref() }.r#type() == U::reify(mt).as_type() {
            Some(unsafe { self.unchecked_cast::<U>() })
        } else {
            None
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

impl<T> Display for Gc<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "#<object {:p}>", self.0)
    }
}

impl DisplayWithin for Gc<()> {
    fn fmt_within(&self, mt: &Mutator, fmt: &mut fmt::Formatter) -> fmt::Result
    {
        unsafe {
            if let Some(this) = self.try_cast::<Symbol>(mt) {
                write!(fmt, "{}", this.as_ref().name())
            } else if let Some(this) = self.try_cast::<Pair>(mt) {
                write!(fmt, "({}", this.as_ref().car.within(mt))?;

                let mut ls = this.as_ref().cdr;
                loop {
                    if let Ok(ls_obj) = Gc::<()>::try_from(ls) {
                        if let Some(pair) = ls_obj.try_cast::<Pair>(mt) {
                            write!(fmt, " {}", pair.as_ref().car.within(mt))?;

                            ls = pair.as_ref().cdr;
                            continue;
                        } else if let Some(_) = ls_obj.try_cast::<EmptyList>(mt) {
                            break;
                        }
                    }

                    write!(fmt, " . {}", ls.within(mt))?;
                    break;
                }

                write!(fmt, ")")
            } else if let Some(_) = self.try_cast::<EmptyList>(mt) {
                write!(fmt, "()")
            } else if let Some(_) = self.try_cast::<Closure>(mt) {
                write!(fmt, "#<fn>")
            } else if let Some(code) = self.try_cast::<Bytecode>(mt) {
                write!(fmt, "{}", code.within(mt))
            } else {
                write!(fmt, "#<object {:p}>", self.0)
            }
        }
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

    pub unsafe fn as_ref(&self) -> &T { self.0.as_ref() }

    pub unsafe fn as_mut(&mut self) -> &mut T { self.0.as_mut() }

    fn header(&self) -> &Header {
        unsafe { &*((self.0.as_ptr() as *const Header).offset(-1)) }
    }

    pub fn r#type(self) -> Gc<Type> { self.header().r#type() }

    pub fn is_marked(self) -> bool { self.header().is_marked() }

    pub unsafe fn unchecked_cast<R>(self) -> Gc<R> { Gc::<R>(self.0.cast()) }

    pub fn within(self, mt: &Mutator) -> WithinMt<Self> {
        WithinMt {v: self, mt}
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
    fn fixnum_try_from_isize() {
        assert!(Fixnum::try_from(0isize).is_ok());
        assert!(ORef::from(Fixnum::try_from(0isize).unwrap())
            .is_tagged::<Fixnum>());

        assert!(Fixnum::try_from(5isize).is_ok());
        assert!(Fixnum::try_from(-5isize).is_ok());

        assert!(Fixnum::try_from(Fixnum::MIN).is_ok());
        assert!(Fixnum::try_from(Fixnum::MAX).is_ok());
    	
        assert!(Fixnum::try_from(Fixnum::MIN - 1).is_err());
        assert!(Fixnum::try_from(Fixnum::MAX + 1).is_err());
    }

    #[test]
    fn isize_from_fixnum() {
        assert_eq!(isize::from(Fixnum::try_from(0isize).unwrap()), 0);

        assert_eq!(isize::from(Fixnum::try_from(5isize).unwrap()), 5);
        assert_eq!(isize::from(Fixnum::try_from(-5isize).unwrap()), -5);

        assert_eq!(isize::from(Fixnum::try_from(Fixnum::MIN).unwrap()),
            Fixnum::MIN);
        assert_eq!(isize::from(Fixnum::try_from(Fixnum::MAX).unwrap()),
            Fixnum::MAX);
    }

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
