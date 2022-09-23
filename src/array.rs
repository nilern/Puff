use std::marker::PhantomData;

use crate::heap_obj::Indexed;

#[repr(C)]
pub struct Array<T>{
    phantom: PhantomData<T>
}

unsafe impl<T> Indexed for Array<T> {
    type Item = T;
}

impl<T> Array<T> {
    pub const TYPE_LEN: usize = 1;
}
