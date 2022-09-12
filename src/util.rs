use std::hash::{Hasher, Hash};

use crate::oref::{ORef, Fixnum};

pub fn hash<H: Hasher + Default, T: Hash + ?Sized>(t: &T) -> Fixnum {
    let mut s = H::default();
    t.hash(&mut s);
    let h = s.finish();
    Fixnum::try_from(((h as isize) << ORef::TAG_SIZE) >> ORef::TAG_SIZE)
        .unwrap()
}
