use std::mem::transmute;

use crate::oref::{ORef, Tagged};
use crate::flonum::Flonum;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Char(usize);

impl Tagged for Char {
    const TAG: usize = Flonum::TAG + 1;
}

impl From<Char> for ORef {
    fn from(c: Char) -> Self { unsafe { transmute(c) } }
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

impl Char {
    pub unsafe fn from_oref_unchecked(oref: ORef) -> Self { transmute(oref) }
}

#[cfg(test)]
mod tests {
    use super::*;

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
