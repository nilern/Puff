use std::mem::transmute;

use crate::oref::{Tagged, ORef, Gc};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Fixnum(usize);

impl Tagged for Fixnum {
    const TAG: usize = Gc::<()>::TAG + 1;
}

impl Fixnum {
    const MIN: isize = -(1 << (ORef::PAYLOAD_BITS - 1));

    const MAX: isize = (1 << (ORef::PAYLOAD_BITS - 1)) - 1;

    pub unsafe fn from_oref_unchecked(oref: ORef) -> Self { transmute(oref) }

    pub fn checked_add(self, other: Self) -> Option<Self> {
        self.0.checked_add(other.0)
            .map(|n| Self(n - 1))
    }

    pub fn checked_sub(self, other: Self) -> Option<Self> {
        self.0.checked_sub(other.0)
            .map(|n| Self(n | Self::TAG))
    }

    pub fn checked_mul(self, other: Self) -> Option<Self> {
        (self.0 & !ORef::TAG_BITS).checked_mul(other.0 & !ORef::TAG_BITS)
            .map(|n| Self((n >> ORef::SHIFT) | Self::TAG))
    }
}

impl From<Fixnum> for ORef {
    fn from(n: Fixnum) -> Self { unsafe { transmute(n) } }
}

impl TryFrom<ORef> for Fixnum {
    type Error = ();

    fn try_from(oref: ORef) -> Result<Self, Self::Error> {
        if oref.tag() == Self::TAG {
            Ok(unsafe { transmute(oref) })
        } else {
            Err(())
        }
    }
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

impl TryFrom<usize> for Fixnum {
    type Error = ();

    fn try_from(n: usize) -> Result<Self, Self::Error> {
        if n <= Fixnum::MAX as usize {
            Ok(Fixnum((n << ORef::SHIFT) | Fixnum::TAG))
        } else {
            Err(())
        }
    }
}

impl From<u8> for Fixnum {
    fn from(n: u8) -> Self { Self((n as usize) << ORef::SHIFT | Fixnum::TAG) }
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
}
