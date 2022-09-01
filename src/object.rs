use std::mem::size_of;

trait Tagged {
    const TAG: usize;
}

// TODO: Enforce `usize` at least 32 bits:
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ORef(usize);

impl ORef {
    const TAG_SIZE: usize = 2;

    const TAG_BITS: usize = (1 << Self::TAG_SIZE) - 1;

    const PAYLOAD_BITS: usize = 8*size_of::<Self>() - Self::TAG_SIZE;

    const SHIFT: usize = Self::TAG_SIZE;

    fn tag(self) -> usize { self.0 & Self::TAG_BITS }

    fn is_tagged<T: Tagged>(self) -> bool { self.tag() == T::TAG }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Fixnum(usize);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Char(usize);

impl Tagged for Char {
    const TAG: usize = 3;
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
