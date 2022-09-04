use std::fmt::{self, Debug};
use std::mem::{size_of, transmute};
use std::ptr::NonNull;

use super::r#type::{Type, NonIndexedType, BitsType};

trait Tagged {
    const TAG: usize;
}

// TODO: Enforce `usize` at least 32 bits:
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ORef(usize);

impl ORef {
    const TAG_SIZE: usize = 2;

    const TAG_BITS: usize = (1 << Self::TAG_SIZE) - 1;

    const PAYLOAD_BITS: usize = 8*size_of::<Self>() - Self::TAG_SIZE;

    const SHIFT: usize = Self::TAG_SIZE;

    fn tag(self) -> usize { self.0 & Self::TAG_BITS }

    fn is_tagged<T: Tagged>(self) -> bool { self.tag() == T::TAG }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Fixnum(usize);

impl Tagged for Fixnum {
    const TAG: usize = 1;
}

impl Fixnum {
    const MIN: isize = -(1 << (ORef::PAYLOAD_BITS - 1));

    const MAX: isize = (1 << (ORef::PAYLOAD_BITS - 1)) - 1;
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

pub struct Header(usize);

impl Header {
    const TAG_SIZE: usize = 1;

    const TAG_BITS: usize = (1 << Self::TAG_SIZE) - 1;

    const MARK_BIT: usize = 1;

    pub fn new(r#type: Gc<Type>) -> Self { Self(r#type.0.as_ptr() as usize) }

    pub fn r#type(&self) -> Gc<Type> {
        unsafe {
            Gc::new_unchecked(
                NonNull::new_unchecked((self.0 & !Self::TAG_BITS) as *mut Type)
            )
        }
    }

    fn is_marked(&self) -> bool { (self.0 & Self::MARK_BIT) == 1 }
}

pub struct Gc<T>(NonNull<T>);

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

impl<T> Gc<T> {
    pub unsafe fn new_unchecked(ptr: NonNull<T>) -> Self { Self(ptr) }

    pub unsafe fn as_ref(&self) -> &T { self.0.as_ref() }

    fn header(&self) -> &Header {
        unsafe { &*((self.0.as_ptr() as *const Header).offset(-1)) }
    }

    pub fn r#type(self) -> Gc<Type> { self.header().r#type() }

    pub fn is_marked(self) -> bool { self.header().is_marked() }

    unsafe fn unchecked_cast<R>(self) -> Gc<R> { Gc::<R>(self.0.cast()) }
}

impl Gc<NonIndexedType> {
    pub fn as_type(self) -> Gc<Type> {
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
