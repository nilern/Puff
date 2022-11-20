use std::mem::transmute;

use crate::oref::{FromORefUnchecked, ORef, Tagged, FLONUM_TAG};
use crate::fixnum::Fixnum;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Flonum(usize);

impl Tagged for Flonum {
    const TAG: usize = FLONUM_TAG;
}

impl From<Flonum> for ORef {
    fn from(n: Flonum) -> Self { unsafe { transmute(n) } }
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

impl From<Fixnum> for Flonum {
    fn from(n: Fixnum) -> Self { Self::from(isize::from(n) as f64) }
}

impl FromORefUnchecked for Flonum {
    unsafe fn from_oref_unchecked(oref: ORef) -> Self { transmute(oref) }
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
}
