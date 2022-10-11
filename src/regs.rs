use std::mem::{size_of, align_of, transmute};
use std::alloc::{Layout, alloc, dealloc};
use std::slice;
use std::ops::{Index, IndexMut};

use crate::mutator::Mutator;
use crate::oref::ORef;

pub struct Regs {
    start: *mut ORef,
    end: *mut ORef,
    base: *mut ORef,
    top: *mut ORef
}

impl Drop for Regs {
    fn drop(&mut self) {
        unsafe {
            dealloc(self.start as *mut u8, Layout::from_size_align_unchecked(
                self.end as usize - self.start as usize,
                Self::ALIGN
            ));
        }
    }
}

impl Index<usize> for Regs {
    type Output = ORef;

    fn index<'a>(&'a self, index: usize) -> &'a Self::Output {
        unsafe { transmute::<*mut ORef, &'a ORef>(self.base.add(index)) }
    }
}

impl IndexMut<usize> for Regs {
    fn index_mut<'a>(&'a mut self, index: usize) -> &'a mut Self::Output {
        unsafe { transmute::<*mut ORef, &'a mut ORef>(self.base.add(index)) }
    }
}

impl Regs {
    const ALIGN: usize = align_of::<ORef>();

    pub fn with_capacity(cap: usize) -> Self {
        unsafe {
            let start = alloc(Layout::from_size_align_unchecked(cap * size_of::<ORef>(), Self::ALIGN)) as *mut ORef;
            Self {
                start,
                end: start.add(cap),
                base: start,
                top: start
            }
        }
    }

    pub fn len(&self) -> usize { (self.top as usize - self.base as usize) / size_of::<ORef>() }

    pub fn ensure(&mut self, required: usize) {
        let required_bytes = required * size_of::<ORef>();

        if (self.end as usize - self.base as usize) < required_bytes {
            let len = self.len();

            if (self.end as usize - self.start as usize) >= required_bytes {
                unsafe {
                    self.base.copy_to(self.start, len);

                    self.base = self.start;
                    self.top = self.base.add(len);
                }
            } else {
                unsafe {
                    let start = alloc(Layout::from_size_align_unchecked(required_bytes, Self::ALIGN))
                        as *mut ORef;
                    self.base.copy_to_nonoverlapping(start, len);

                    self.start = start;
                    self.end = start.add(required);
                    self.base = start;
                    self.top = start.add(len);
                }
            }
        }
    }

    fn reserve(&mut self, additional: usize) { self.ensure(self.len() + additional); }

    pub fn as_slice(&self) -> &[ORef] { unsafe { slice::from_raw_parts(self.base, self.len()) } }

    pub fn pop(&mut self) -> Option<ORef> {
        if self.base < self.top {
            unsafe {
                self.top = self.top.sub(1);
                Some(*self.top)
            }
        } else {
            None
        }
    }

    pub fn popn(&mut self, n: usize) {
        unsafe { self.top = self.top.sub(n); }
        debug_assert!(self.top >= self.base);
    }

    pub fn popnnt(&mut self, n: usize) {
        unsafe {
            let top = *self.top.sub(1);
            self.top = self.top.sub(n);
            debug_assert!(self.top >= self.base);
            *self.top.sub(1) = top;
        }
    }

    pub unsafe fn push_unchecked(&mut self, v: ORef) {
        debug_assert!(self.top < self.end);
        self.top.write(v);
        self.top = self.top.add(1);
    }

    pub fn push(&mut self, v: ORef) {
        self.reserve(1);
        unsafe { self.push_unchecked(v); }
    }

    pub fn extend(&mut self, vs: &[ORef]) {
        self.reserve(vs.len());
        unsafe {
            self.top.copy_from_nonoverlapping(vs.as_ptr(), vs.len());
            self.top = self.top.add(vs.len());
        }
    }

    pub fn enter(&mut self, new_len: usize) {
        unsafe { self.base = self.top.sub(new_len); }
    }

    pub fn truncate(&mut self, n: usize) {
        if n < self.len() {
            unsafe { self.top = self.base.add(n); }
        }
    }

    pub fn dump(&self, mt: &Mutator) {
        print!("[");
        for v in self.as_slice() { print!("{}, ", v.within(mt)); }
        println!("]");
    }
}
